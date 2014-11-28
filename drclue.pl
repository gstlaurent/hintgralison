:- expects_dialect(sicstus).

:- dynamic has/2, lacks/2, room/1, weapon/1, character/1, player/1,
   me/1, next/2, firstPlayer/1, potential/1, numCards/2,
   playerRoom/1.

%% Start Dr. Clue here!    
clue :-
    % remove all previous facts.
    retractall(has(_,_)),
    retractall(lacks(_,_)),    
    retractall(room(_)),
    retractall(weapon(_)),
    retractall(character(_)),
    retractall(player(_)),
    retractall(me(_)),
    retractall(next(_,_)),
    retractall(firstPlayer(_)),
    retractall(potential(_)),
    retractall(numCards(_,_)),
    retractall(playerRoom(_)),

    nl, write('Welcome to Dr. Clue!'), nl, nl,
    write('To begin, Dr. Clue will lead you through the initialization of the game.'), nl, nl,
    setup,!,
    write('Setup is complete. It\'s time to begin the game!'), nl,
    write('As soon as you are able to make a correct accusation, Dr. Clue will let you know.'), nl,
    write('Dr. Clue will also help you make a suggestion by offering cards you should suggest.'), nl, nl,
    firstPlayer(X),
    gameLoop(X). % start the game with the first player's turn.

%%%%%%%%%%%%%%%%%%%%%%%% GAME SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Lead the user through the steps to initialize the game. This includes entering
%% all of the cards in the game, the names of the players, and the user's cards.
setup :-
    write('Start by entering the names of all the rooms on your clue board.'), nl,
    getInfo(room), nl,

    write('Enter the names of all the weapons in your game.'), nl,
    getInfo(weapon), nl,
    
    write('Enter the names of all the suspects in your game.'), nl,
    getInfo(character), nl,

    write('Next, enter the players, followed by how many cards they each have, starting with the player who will go first.'), nl,
    getPlayers, nl,

    getMyName,

    write('Now, enter your cards.'), nl,
    getInfo(card), nl.

%% Prompt the user to enter the name of a type of card (for example, a weapon card).
getInfo(Type) :-
    write('Enter the name of a '), write(Type), write(' or hit ENTER if there are no more '),
    write(Type), write('s: '),
    readline(Entry),
    input(Type, Entry).

%% Store card X of the given type in the database.
%% Used both to input the cards in the game and the cards in the user's hand.
input(_, '') :- !.
input(room,X) :- assert(room(X)), assert(potential(X)), getInfo(room).
input(weapon,X) :- assert(weapon(X)), assert(potential(X)), getInfo(weapon).
input(character,X) :- assert(character(X)), assert(potential(X)),
                      getInfo(character).
input(card,X) :- room(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,X) :- weapon(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,X) :- character(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,_) :- write('That\'s not a valid card. '), nl, nl, listAllCards, getInfo(card).

%% Prompt the user to enter the names and number of cards for each player.
getPlayers :-
    write('Enter the first player: '), readline(First),
    write('How many cards does this player have? '), readnumber(NumCards),
    write('Enter the next player: '), readline(Next),
    assert(player(First)),
    assert(numCards(First,NumCards)),
    assert(firstPlayer(First)),
    assert(next(First, Next)),
    assertNextPlayer(First, First, Next).

%% Continue the process of entering the names and number of cards for each player.
assertNextPlayer(First, Last, '') :- assert(next(Last, First)).
assertNextPlayer(First, Previous, Current) :-
    write('How many cards does this player have? '), readnumber(NumCards),
    assert(numCards(Current, NumCards)),
    assert(next(Previous, Current)),
    assert(player(Current)),
    write('Enter the next player or hit ENTER if there are no more players: '),
    readline(Next), assertNextPlayer(First, Current, Next).

getMyName :- write('Which player are you? '), readline(Character), inputMyName(Character).
inputMyName(Character) :- player(Character),!, assert(me(Character)).
inputMyName(_) :- write('That\'s not a valid player name. '), listPlayers, nl, getMyName.


%%%%%%%%%%%%%%%%%%%%%%%% SHOW DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Display all the information Dr. Clue knows.
showDatabase :- write('******************* Database Contents **********************'), nl,
                listAllCards, nl, listPlayers, listMe, nl, listAllPlayerCards,
                listAllPotential, listAllKnown,
                write('************************************************************'), nl.

%% Print out all the valid names for suspects, weapons, and rooms.
listAllCards :- listCards(character), listCards(weapon), listCards(room).

%% Print out all the cards that satisfy the predicate Type.
listCards(Type) :- allType(Type, Cards), write('The '), write(Type), write('s are: '), writeln(Cards).

%% Print out all the names of the people playing this round of clue.
listPlayers :- allPlayers(Players), write('The players are: '), writeln(Players).

%% Print out the name of the player Dr. Clue is helping.
listMe :- me(Player), write('You are '), write(Player), nl.

%% Print out everything Dr. Clue knows about the cards in each player's hand.
listAllPlayerCards :- allPlayers(Players), forall(member(P,Players), listPlayerCards(P)).

%% Print out everything Dr. Clue knows about the cards in Player's hand.
listPlayerCards(Player) :- hasAll(Player, HasCards), write(Player), write(' is known to have these cards: '),
                           writeln(HasCards),
                           lacksAll(Player, LacksCards), write(Player),
                           write(' does not have any of these cards: '), writeln(LacksCards), nl.

%% Print all the cards that could be in the envelope. That is, all cards that are not
%% known to be in any player's hand.
listAllPotential :-
    allType(potential, Cards),
    write('These cards may be in the envelope, or they may be in another player\'s hand: '), writeln(Cards). 

%% Print all the cards that must be in the envelope.
listAllKnown :-
    allType(inEnvelope, Known), 
    write('These cards are DEFINITELY in the envelope: '), writeln(Known).

%% allType(Type,Items) is true if Items is a list of all the atoms of type Type.
allType(Type, Items) :- findall(I, call(Type, I), Items).

%% True if Cards is all the cards Dr. Clue knows Player has.
hasAll(Player, Cards) :- findall(C, has(Player, C), Cards).

%% True if Cards is all the cards Dr. Clue knows Player does not have.
lacksAll(Player, Cards) :- findall(C, lacks(Player, C), Cards).


%%%%%%%%%%%%%%%%%%%%%%%% GAME LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Start Player's turn.
gameLoop(Player) :-
    me(Player), !,
    write('It\'s your turn!'), nl,
    %% Player can make an accusation if it is their turn, even if not in a room.
    checkForAccusation(Player),
    write('To make a suggestion, type "suggest".'), nl,
    write('To move to the next player without making a suggestion, hit enter.'), nl,
    write('To see the database, type "db". '),
    readline(X), nl,
    suggestionPrompt(Player,X).
gameLoop(Player) :-
    write('It\'s '), write(Player), write('\'s turn.'),nl,
    write('Type "suggest" if '), write(Player), write(' made a suggestion or accusation.'), nl,
    write('Type "db" to see the database.'), nl,
    write('Hit enter to move to the next player. '),
    readline(X), 
    suggestionPrompt(Player,X).

%% If Player is the user, offer a suggestion to make, then get the player's suggestion,
%% If Player is another player, get their suggestion.
suggestionPrompt(Player, db) :- showDatabase, nl, gameLoop(Player).
% If an accusation can be made after the suggestion, let the player know
suggestionPrompt(Player, suggest) :-
    me(Player),
    offerSuggestion, !,
    getSuggestion(Player).
suggestionPrompt(Player, suggest) :-
    getSuggestion(Player).
suggestionPrompt(Player,'') :- nl, next(Player, Next), gameLoop(Next).
suggestionPrompt(Player,_) :-
    write('Invalid input. Try again'), nl, gameLoop(Player).

%% Prompt for the cards Player suggested, making sure that each card entered is
%% a valid card name.
getSuggestion(Player) :-
    me(Player), !,
    playerRoom(Room),
    %% Do not prompt user for room since they've already told us what room they're in.
    write('Enter the CHARACTER suggestion: '), readline(Character),
    write('Enter the WEAPON suggestion: '), readline(Weapon),
    validateSuggestion(Player,Character,Weapon,Room).
getSuggestion(Player) :-
    write('Enter the CHARACTER suggestion: '), readline(Character),
    write('Enter the WEAPON suggestion: '), readline(Weapon),
    write('Enter the ROOM suggestion: '), readline(Room),
    validateSuggestion(Player,Character,Weapon,Room).

%% Ensures a valid suggestion was made before moving on.
validateSuggestion(Player,Character, Weapon, Room) :-
    character(Character), weapon(Weapon), room(Room), !, makeSuggestion(Player,Character,Weapon,Room).
validateSuggestion(Player,_,_,_) :- nl, write('That is not a valid suggestion!'), nl, listAllCards, getSuggestion(Player).

%% Prompt for the player who disproved a suggestion and the card they showed, making
%% sure the player name and card are valid before recording them.
makeSuggestion(CurrentPlayer,Character,Weapon,Room) :-
    %me(CurrentPlayer),
    write('Name the PLAYER who showed a card (or just hit ENTER if no one could show anything): '),
    readline(DisprovingPlayer),
    write('Name the CARD that was shown (or just hit ENTER if you didn\'t see it or no card was shown: '),
    readline(Card),
    validateAndRecordSuggestion(CurrentPlayer,Character,Weapon,Room,DisprovingPlayer,Card).

%% Check that DisprovingPlayer and Card are valid, and enter this information
%% into the database before continuing with the next player's turn.
%% No one showed a card.
validateAndRecordSuggestion(Current,Character,Weapon,Room,'','') :-
    suggestion(Current,Character,Weapon,Room,'',''), !,
    next(Current,Next), checkForAccusation(Current), gameLoop(Next).
%% Valid values of DisprovingPlayer and Card were given.
validateAndRecordSuggestion(Current,Character,Weapon,Room,DisprovingPlayer,Card) :-
    player(DisprovingPlayer),
    (==('',Card);==(Character,Card); ==(Weapon,Card);==(Room,Card)),!,
    suggestion(Current,Character,Weapon,Room,DisprovingPlayer,Card), !,
    next(Current,Next), checkForAccusation(Current), gameLoop(Next).
%% Invalid value given for either DisprovingPlayer or Card.
validateAndRecordSuggestion(Current,Character,Weapon,Room,_,_) :-
    nl, write('Invalid player or card entered! Please try again.'),nl,nl,
    listPlayers,
    write('The possible cards shown are: '), write(Character), write(', '),
    write(Weapon), write(', '), write(Room), nl,nl,
    makeSuggestion(Current,Character,Weapon,Room).


%% Add to database knowledge gained from this round of suggestion.
%%  suggestion/6:(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard)
%% No one showed a card. Inspecting Player could be bluffing and have some of the cards, but no other
%% players can have any of them.
suggestion(InspectingPlayer, Character, Weapon, Room, '', '') :- !,
    allPlayers(Players), delete(Players, InspectingPlayer, Others),
    forall(member(P, Others), assertLacksTrio(P, Character, Weapon, Room)), !.
suggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, '') :- !,
    playersBetween(InspectingPlayer,DisprovingPlayer,Between),
    forall(member(P,Between), assertLacksTrio(P, Character, Weapon, Room)),
    deduceCardShown(DisprovingPlayer, Character, Weapon, Room),!.
suggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard) :-
    playersBetween(InspectingPlayer, DisprovingPlayer, Between),
    forall(member(P, Between), assertLacksTrio(P, Character, Weapon, Room)),
    assertHas(DisprovingPlayer, DisprovingCard), !.

%% Try to figure out what card DisprovingPlayer showed to another player.
%% We already know the player has one of the cards. No new information gathered.
deduceCardShown(DisprovingPlayer, Character, Room, Weapon) :- 
    has(DisprovingPlayer,Character); has(DisprovingPlayer,Room); has(DisprovingPlayer,Weapon), !.
%% We know DisprovingPlayer lacks two of the cards suggested. Therefore, DisprovingPlayer must have
%% shown the other card!
deduceCardShown(DisprovingPlayer, Character, Room, Weapon) :-
    lacks(DisprovingPlayer, Character),
    lacks(DisprovingPlayer, Room),
    assertHas(DisprovingPlayer, Weapon), !.
deduceCardShown(DisprovingPlayer, Character, Room, Weapon) :-
    lacks(DisprovingPlayer, Character),
    lacks(DisprovingPlayer, Weapon),
    assertHas(DisprovingPlayer, Room), !.
deduceCardShown(DisprovingPlayer, Character, Room, Weapon) :-
    lacks(DisprovingPlayer, Weapon),
    lacks(DisprovingPlayer, Room),
    assertHas(DisprovingPlayer, Character), !.
%% We cannot deduce the card shown.
deduceCardShown(_,_,_,_).


%% Prompt for user's current room so that a valid suggestion can be offered.
offerSuggestion :-
    write('What room are you in? '),
    readline(Room),
    verifyAndSuggest(Room).

%% If room given is in the database, offer the player a suggestion. Otherwise,
%% try again to get a valid room.
verifyAndSuggest(Room) :-
    room(Room), !, assert(playerRoom(Room)), writeSuggestion(Room).
verifyAndSuggest(_) :-
    write('That is not a valid room.'), nl,
    listCards(room), nl,
    % Get the player to enter the room again.
    offerSuggestion. 

%% Offer the player a suggestion based on the information in the database.
writeSuggestion(Room) :-
    findSuggestion(Character, Weapon),
    write('You should suggest that '), write(Character),
    write(' murdered somebody in the '), write(Room),
    write(' with the '), write(Weapon), write('.'), nl.

%% Suggest cards that may be in the envelope. Does not bluff or strategize.
findSuggestion(Character, Weapon) :-
    character(Character), potential(Character),
    weapon(Weapon), potential(Weapon).

%% Check if an accusation is possible and inform the player if this is the case
checkForAccusation(Player) :-
    me(Player), 
    accusation(Character, Weapon, Room), !,
    accusescript(Character, Weapon, Room).
checkForAccusation(_) :-
    accusation(Character, Weapon, Room), !,
    accuseLaterScript(Character, Weapon, Room).
checkForAccusation(_).

%% Produce True when the given variables are indisputedly in the Clue envelope.
accusation(Character, Weapon, Room) :-
   character(Character), potential(Character), inEnvelope(Character),
   weapon(Weapon), potential(Weapon), inEnvelope(Weapon),
   room(Room), potential(Room), inEnvelope(Room).

%% Let the user know the accusation they should make.
accusescript(Character, Weapon, Room) :-
  upcase_atom(Character, CHARACTER), upcase_atom(Weapon, WEAPON), upcase_atom(Room, ROOM),
  nl,
  writeln('You should make an accusation! Shout out the following now:'),
  writeln('***********************************************************************************************'),
  write('********I accuse '), write(CHARACTER), write(' of murdering somebody in the '),
  write(ROOM), write(' with the '), write(WEAPON), write('!********'),
  nl, writeln('*******************************************************************************************').

%% Let the user know they should make an accusation on their next turn.
accuseLaterScript(Character, Weapon, Room) :-
  nl,
  write('We\'ve solved the case! On your next turn, accuse '), write(Character),
  write(' of murdering somebody in the '), write(Room), write(' with the '), write(Weapon),
  write('.'), nl.

%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Store in the database that the given player does not have the 3 given cards.
assertLacksTrio(Player, Character, Weapon, Room) :-
    assertLacks(Player, Character), assertLacks(Player, Weapon), assertLacks(Player, Room).

%% assertLacks and assertHas create proper assertions if necessary.
assertLacks(Player, Card) :- lacks(Player, Card), !.
assertLacks(Player, Card) :- assert(lacks(Player, Card)), !.

%% True when there are no players holding the given card, meaning it must be in the envelope.
inEnvelope(Card) :- potential(Card), allPlayers(Players), foreach(member(Player, Players), lacks(Player, Card)).

%% Ensure other players lack the card, if neccessary, and remove card from potential.
assertHas(Player, Card) :- has(Player, Card), !.
assertHas(Player, Card) :-
    assert(has(Player, Card)),
    retract(potential(Card)),
    allPlayers(Players), delete(Players, Player, Others),
    forall(member(P, Others), assertLacks(P, Card)),
    checkNumKnownCards(Player),
    deduceSolution.

%% Deduce the murderer, murder weapon, or murder location by process of elimination:
%% if we know that all the cards but one of a certain type are in players' hands,
%% the final card must be in the envelope, and all the players lack this card.
%% Add this information to the database.
deduceSolution :- deduce(character), deduce(weapon), deduce(room).
deduce(Type) :-
    allType(potential, Potentials), allType(Type, Cards),
    intersection(Potentials, Cards, [Suspect|Rest]), length([Suspect|Rest], 1),
    allPlayers(Players), forall(member(P, Players), assertLacks(P, Suspect)).
deduce(_).

%% Check if Dr. Clue knows all the cards a player has. If so, assert that they lack
%% all the other cards. 
checkNumKnownCards(Player) :-
    % update the cards the player lacks.
    deduceSolution, validNumCards(N),
    numCards(Player, N), hasAll(Player, PlayerCards), length(PlayerCards, N), !,
    allType(potential, Cards), subtract(Cards, PlayerCards, RestCards),
    forall(member(C, RestCards), assertLacks(Player, C)).
checkNumKnownCards(_).

getNumCards(Player, NumCards) :-
    validNumCards(NumCards), numCards(Player, NumCards).
    
allPlayers(Players) :- findall(P, player(P), Players).

%% True if the list contains all players whose turn comes between StartPlayer and EndPlayer.
playersBetween(StartPlayer, EndPlayer, []) :- next(StartPlayer, EndPlayer), !.
playersBetween(StartPlayer, EndPlayer, [Prev|Tweens]) :-
    next(Prev, EndPlayer), !, playersBetween(StartPlayer, Prev, Tweens).

%% Read input from the user, allowing them to use punctuation, spaces, etc.
readline(Atom) :- read_line(Input), string_codes(String, Input), string_to_atom(String, Atom).
%% Read a number from the user.
readnumber(Number) :- read_line(Input), string_codes(String, Input), string_to_atom(String, Atom),
                      atom_number(Atom, Number).

% Not sure about the max number of cards a player could have in each
% of the various clue boards, but 15 seems like plenty.
validNumCards(1).
validNumCards(2).
validNumCards(3).
validNumCards(4).
valdiNumCards(5).
validNumCards(6).
validNumCards(7).
validNumCards(8).
validNumCards(9).
validNumCards(10).
validNumCards(11).
validNumCards(12).
validNumCards(13).
validNumCards(14).
validNumCards(15).


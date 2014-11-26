:- expects_dialect(sicstus).

:- dynamic has/2, lacks/2, maybe/2.

:- dynamic room/1,
   weapon/1,
   character/1,
   player/1,
   me/1,
   dead/1,
   location/2,
   next/2,
   firstPlayer/1,
   potential/1,
   numCards/2.


%% IF LOTS LOF EXTRA TIME:
%% auto generate number of cards per player
%% Make generic change so ENTER can finish things.

% TODO write better intro to the game. 
clue :-
    write('Welcome to Dr. Clue!'), nl,
    write('To begin, we will lead you through the initialization of the game.'), nl,
    setup,!,
    write('Setup is complete. It\'s time to begin the game!'), nl,
    write('Dr. Clue will inform you if it is your turn and you are able to make a correct accusation.'), nl,
    firstPlayer(X),
    gameLoop(X). % start the game with the first player's turn.

%%%%%%%%%%%%%%%%%%%%%%%% GAME SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO better wording of instructions.
setup :-
    write('Start by entering the names of all the rooms on your clue board.'), nl,
    retractall(potential(_)),
    retractall(room(_)),
    getInfo(room), nl,

    write('Enter the names of all the weapons in the game.'), nl,
    retractall(weapon(_)),
    getInfo(weapon), nl,

    write('Enter the names of all the characters in your game.'), nl,
    write('These are the names of all the people who may have committed the murder, not just the current players.'), nl,
    retractall(character(_)),
    getInfo(character), nl,

    write('Next, enter the players, and how many cards each has, starting with the player who will go first.'), nl,
    retractall(next(_,_)),
    retractall(player(_)),
    retractall(firstPlayer(_)),
    retractall(numCards(_,_)),
    getPlayers, nl,

    write('Which player are you? '),
    retractall(me(_)),
    getMyName,

    write('Now, enter your cards.'), nl,
    retractall(has(_,_)),
    retractall(lacks(_,_)),
    getInfo(card), nl.


getInfo(Type) :-
    write('Enter the name of a '), write(Type), write(' or hit ENTER if there are no more '),
    write(Type), write('s: '),
    readline(Entry),
    input(Type, Entry).

input(_, '') :- !.
input(room,X) :- assert(room(X)), assert(potential(X)), getInfo(room).
input(weapon,X) :- assert(weapon(X)), assert(potential(X)), getInfo(weapon).
input(character,X) :- assert(character(X)), assert(potential(X)),
                      getInfo(character).
input(card,X) :- room(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,X) :- weapon(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,X) :- character(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,_) :- write('That\'s not a valid card. '), nl, listAllCards, getInfo(card).

getPlayers :-
    write('Enter first player: '), readline(First),
    write('How many cards does this player have? '), readline(NumCards),
    write('Enter next player: '), readline(Next),
    assert(player(First)),
    atom_number(NumCards, N), assert(numCards(First,N)), 
    assert(firstPlayer(First)),
    assert(next(First, Next)),
    assertNextPlayer(First, First, Next).

assertNextPlayer(First, Last, '') :- assert(next(Last, First)).
assertNextPlayer(First, Previous, Current) :-
    write('How many cards does this player have? '), readline(NumCards),
    atom_number(NumCards, N), assert(numCards(Previous, N)),
    assert(next(Previous, Current)),
    assert(player(Current)),
    write('Enter next player or hit ENTER if there are no more players: '),
    readline(Next), assertNextPlayer(First, Current, Next).

getMyName :- readline(Character), inputMyName(Character).
inputMyName(Character) :- player(Character),!, assert(me(Character)).
inputMyName(_) :- write('That\'s not a valid player name. '), listPlayers, getMyName.


%%%%%%%%%%%%%%%%%%%%%%%% SHOW DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showDatabase :- listAllCards, listPlayers, listMe, nl, listAllPlayerCards,listAllPotentials.

listAllCards :- listCards(character), listCards(weapon), listCards(room).

listCards(Type) :- allType(Type, Cards), write('The '), write(Type), write('s are: '), writeln(Cards),nl.

listPlayers :- allPlayers(Players), write('The players are: '), writeln(Players),nl.

listMe :- me(Player), write('You are '), write(Player), nl.

listAllPlayerCards :- allPlayers(Players), forall(member(P,Players), listPlayerCards(P)).

listPlayerCards(Player) :- hasAll(Player, HasCards), write(Player), write(' has these cards: '),
                           writeln(HasCards),
                           lacksAll(Player, LacksCards), write(Player),
                           write(' does not have any of these cards: '), writeln(LacksCards), nl.

%% Write all the cards that are not known to be in any player's hand
listAllPotentials :-
    allType(potential,Cards),
    write('Cards that may be in the envelope: '), writeln(Cards). 

%% Items is all things of type Type
allType(Type, Items) :- findall(I, call(Type, I), Items).
    
hasAll(Player, Cards) :- findall(C, has(Player, C), Cards).
lacksAll(Player, Cards) :- findall(C, lacks(Player, C), Cards).


%%%%%%%%%%%%%%%%%%%%%%%% GAME LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gameLoop(Player) :-
    me(Player), !,
    write('It\'s your turn!'), nl,
    %% Player can make an accusation if it is their turn, even if not in a room.
    checkForAccusation,
    write('To make a suggestion, type "suggest".'), nl,
    write('To move to the next player without making a suggestion, hit enter.'), nl,
    write('To see the database, type "db". '),
    readline(X), nl,
    suggestionPrompt(Player,X).
gameLoop(Player) :-
    write('It\'s '), write(Player), write('\'s turn.'),nl,
    write('Type "suggest" if the player made a suggestion or accusation.'), nl,
    write('Type "db" to see the database.'), nl,
    write('Hit enter to move to the next player. '),
    readline(X), 
    suggestionPrompt(Player,X).


suggestionPrompt(Player, db) :- showDatabase, nl, gameLoop(Player).
% If an accusation can be made after the suggestion, let the player know
suggestionPrompt(Player, suggest) :-
    me(Player),
    % TODO suggestCards for the player - check what room the player is in.
    offerSuggestion, !,
    getSuggestion(Player),
    checkForAccusation,
    next(Player, Next),
    gameLoop(Next).
suggestionPrompt(Player, suggest) :-
    getSuggestion(Player),
    next(Player, Next),
    gameLoop(Next).
suggestionPrompt(Player,'') :- nl, next(Player, Next), gameLoop(Next).
suggestionPrompt(Player,_) :-
    write('Invalid input. Try again'), nl, gameLoop(Player).

getSuggestion(Player) :-
    write('Enter the CHARACTER suggestion: '), readline(Character),
    write('Enter the WEAPON suggestion: '), readline(Weapon),
    write('Enter the ROOM suggestion: '), readline(Room),
    validateSuggestion(Player,Character,Weapon,Room).

%% Ensures a valid suggestion was made before moving on.
validateSuggestion(Player,Character, Weapon, Room) :-
    character(Character), weapon(Weapon), room(Room), !, makeSuggestion(Player,Character,Weapon,Room).
validateSuggestion(Player,_,_,_) :- write('That is not a valid suggestion'), listAllCards, getSuggestion(Player).

makeSuggestion(CurrentPlayer,Character,Weapon,Room) :-
    %me(CurrentPlayer),
    write('Name the PLAYER who showed a card (or just hit ENTER if no one could show anything): '),
    readline(DisprovingPlayer),
    write('Name the CARD that was shown (or just hit ENTER if you didn\'t see it or no card was shown: '),
    readline(Card),
    validateAndRecordSuggestion(CurrentPlayer,Character,Weapon,Room,DisprovingPlayer,Card).


%% No one showed a card.
validateAndRecordSuggestion(Current,Character,Weapon,Room,'','') :- !,
    mysuggestion(Current,Character,Weapon,Room,none,none), !,
    next(Current,Next), gameLoop(Next).
%% Valid values of DisprovingPlayer and Card were given.
validateAndRecordSuggestion(Current,Character,Weapon,Room,DisprovingPlayer,Card) :-
    player(DisprovingPlayer),
    (==('',Card);==(Character,Card); ==(Weapon,Card);==(Room,Card)),!,
    mysuggestion(Current,Character,Weapon,Room,DisprovingPlayer,Card), !,
    next(Current,Next), gameLoop(Next).
%% Invalid value given for either DisprovingPlayer or Card.
validateAndRecordSuggestion(Current,Character,Weapon,Room,_,_) :-
    write('Invalid player or card entered. Please try again.'),nl,
    listAllPlayers,
    write('The possible cards shown are: '), write(Character), write(', '),
    write(Weapon), write(', '), write(Room), nl,
    makeSuggestion(Current,Character,Weapon,Room).


%% Adds to DB knowledge gained from this round of my suggestion.
% TODO: make generic 'suggestion' that can assert maybes. Have shorter version, mysuggestion based on generic
%%  mysuggestion/6:(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard)
%% No one showed a card. Inspecting Player could be bluffing and have some of the cards, but no other
%% players can have any of them.
mysuggestion(InspectingPlayer, Character, Weapon, Room, '', '') :-
    allPlayers(Players), delete(Players, InspectingPlayer, Others),
    forall(member(P, Others), assertLacksTrio(P, Character,Weapon,Room)), !.
mysuggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, '') :-
    playersBetween(InspectingPlayer,DisprovingPlayer,Between),
    forall(member(P,Between), assertLacksTrio(P,Character,Weapon,Room)), !.
%TODO. record that one of these cards is in DisprovingPlayer's hands.
mysuggestion(_, _, _, _, DisprovingPlayer, DisprovingCard) :-
    assertHas(DisprovingPlayer, DisprovingCard), !. 


offerSuggestion :-
    write('What room are you in? '),
    readline(Room),
    verifyAndSuggest(Room).

verifyAndSuggest(Room) :-
    room(Room), !, writeSuggestion(Room).
verifyAndSuggest(_) :-
    write('That is not a valid room.'),
    listCards(room),
    % Get the player to enter the room again.
    offerSuggestion. 

writeSuggestion(Room) :-
    findSuggestion(Character, Weapon, Room),
    write('You should suggest that '), write(Character),
    write(' murdered somebody in the '), write(Room),
    write(' with the '), write(Weapon), nl.

%% Very simple suggester. Does not bluff or strategize. TODO Improve this!
findSuggestion(Character, Weapon, Room) :-
    character(Character), potential(Character),
    weapon(Weapon), potential(Weapon).


%% Checks if an accusation is possible and informs the player if this is the case
checkForAccusation :- accusation(Character, Weapon, Room),
                      accusescript(Character, Weapon, Room).
checkForAccusation.

%% Produce True when the given variables are indisputedly in the Clue envelope.
accusation(Character, Weapon, Room) :-
   character(Character), allLack(Character),
   weapon(Weapon), allLack(Weapon),
   room(Room), allLack(Room).

accusescript(Character, Weapon, Room) :-
  upcase_atom(Character, CHARACTER), upcase_atom(Weapon, WEAPON), upcase_atom(Room, ROOM),
  nl,
  writeln('You should make an accusation! Shout out the following now:'),
  writeln('***********************************************************************************************'),
  write('********I accuse '), write(CHARACTER), write(' of murdering somebody in the '),
  write(ROOM), write(' with the '), write(WEAPON), write('!********'),
  nl, writeln('*******************************************************************************************').


%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%normalizePlayer(PotentialPlayer, PlayerOrNone) :-
%    not(player(PotentialPlayer)), PlayerOrNone = none.
%normalizePlayer(PotentialPlayer, PlayerOrNone) :- PlayerOrNone = PotentialPlayer.

assertLacksTrio(Player, Character, Weapon, Room) :-
    assertLacks(Player, Character), assertLacks(Player, Weapon), assertLacks(Player, Room).

%% assertLacks and assertHas creates proper assertions if necessary.
assertLacks(Player, Card) :- lacks(Player, Card), !.
assertLacks(Player, Card) :- assert(lacks(Player, Card)), !.

%% True when there are no players holding the given cardd
allLack(Card) :- allPlayers(Players), foreach(member(Player, Players), lacks(Player, Card)).

%% Ensures other players lack the card, if neccessary, and removes card from potentials.
assertHas(Player, Card) :- has(Player, Card), !.
assertHas(Player, Card) :-
    assert(has(Player, Card)),
    allPlayers(Players), delete(Players, Player, Others),
    forall(member(P, Others), assertLacks(P, Card)),
    checkNumKnownCards(Player),
    retract(potential(Card)).

%% Check to see if we know all the cards a player has. If so, assert that they lack
%% all the other cards.
checkNumKnownCards(Player) :-
    numCards(Player, N), hasAll(Player, PlayerCards), length(PlayerCards, N), !,
    allType(potential, Cards), subtract(Cards, PlayerCards, RestCards),
    forall(member(C, RestCards), assertLacks(Player, C)).
checkNumKnownCards(_).


allPlayers(Players) :- findall(P, player(P), Players).

%% Third argument is all players whose turn comes between Player1 and Player2.
%% TODO make this function not so stupid
playersBetween(Player1,Player1,[]) :- !.
playersBetween(Player1,Player2,[]) :- next(Player1,Player2), !.
playersBetween(Player1,Player2,[Next]) :-
    next(Player1,Next), next(Next,Player2), !.
playersBetween(Player1,Player2,[Next1,Next2]) :-
    next(Player1,Next1), next(Next1,Next2), next(Next2,Player2), !.
playersBetween(Player1,Player2,[Next1,Next2,Next3]) :-
    next(Player1,Next1), next(Next1,Next2), next(Next2,Next3), next(Next3,Player2), !.
playersBetween(Player1,Player2,[Next1,Next2,Next3,Next4]) :-
    next(Player1,Next1), next(Next1,Next2), next(Next2,Next3),
    next(Next3,Next4), next(Next4,Player2), !.
    

readline(Atom) :- read_line(Input), string_codes(String, Input), string_to_atom(String, Atom).
%% writeline(String) :- writef("%s", [String]).
writeline(String) :- write(String).

%% listings/0 is handy for testing.
listings :- listing(lacks(_, _)), listing(has(_, _)).


%%%%%%%%% Test facts %%%%%%%%%%%%%%%%%%%%%%%%%%%

character('scarlett').
character('plum').
character('peacock').
character('green').
character('mustard').
character('white').

weapon('candlestick').
weapon('knife').
weapon('leadpipe').
weapon('revolver').
weapon('rope').
weapon('wrench').

room('kitchen').
room('ballroom').
room('conservatory').
room('dining').
room('billiard').
room('library').
room('study').
room('hall').
room('lounge').

% our test game has only four players
player('scarlett').
player('plum').
player('white').
player('mustard').

dead('mustard').

me('plum').

% next(CurrentPlayer, NextPlayer).
next('scarlett', 'plum').
next('plum', 'white').
next('white', 'mustard').
next('mustard', 'scarlett').

%% has(Player, Card) :- fail.
%% lacks(Player, Card) :- fail.
%% maybe(Player, Card) :- fail.

%% sample solution:
lacks('white', 'green').
lacks('white', 'study').
lacks('white', 'knife').
lacks('mustard', 'green').
lacks('mustard', 'study').
lacks('mustard', 'knife').
lacks('scarlett', 'green').
lacks('scarlett', 'study').
lacks('scarlett', 'knife').

lacks('plum', 'green').
lacks('plum', 'study').
lacks('plum', 'knife').

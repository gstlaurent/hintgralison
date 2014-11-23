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
   firstPlayer/1.
  


%TODO:
% switch from read to readline

%% IF LOTS LOF EXTRA TIME:
%% auto generate number of cards per player



/*
Examples of all the types of facts

weapon(knife).
room(library).
character(scarlett).
player(scarlett).
firstPlayer(plum).
next(scarlett,plum).

me(alison).

*/

%%%%%%%%%%%%%%%%%%%%%%%% INTRO TEXT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO write better intro to the game. 
clue :-
    write('Welcome to Dr. Clue!'), nl,
    write('To begin, we will lead you through the initialization of the game.'), nl,
    setup,
    write('Setup is complete. Whenever you wish to see the database, type "db"'), nl,
    gameLoop.

%%%%%%%%%%%%%%%%%%%%%%%% GAME SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO better wording of instructions.
setup :-
    write('It\'s time to set up the game. When entering names for the rooms, weapons, and characters, make sure to begin with a lowercase letter, end with a period, and avoid using any punctuation or spaces before the final period.'), nl,
    write('Start by entering the names of all the rooms on your clue board.'), nl,
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
    retractall(numCards(_)),
    getPlayers, nl,

    write('Which player are you? '),
    retractall(me(_)),
    getMyName,

    write('Now, enter your cards.'), nl,
    retractall(has(_,_)),
    retractall(lacks(_,_)),
    getInfo(card), nl, 

    write('It\'s time to begin the game!'), nl. %TODO lead into the gameplay here*/

getInfo(Type) :-
    write('Enter the name of a '), write(Type), write(' or "done." if there are no more '),
    write(Type), write('s: '),
    read(Entry),
    input(Type, Entry).

input(_, done) :- !.
input(room,X) :- assert(room(X)), getInfo(room).
input(weapon,X) :- assert(weapon(X)), getInfo(weapon).
input(character,X) :- assert(character(X)), getInfo(character).
input(card,X) :- room(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,X) :- weapon(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,X) :- character(X),!, me(Y), assertHas(Y,X), getInfo(card).
input(card,_) :- write('That\'s not a valid card. '), nl, listAllCards, getInfo(card).

getPlayers :-
    write('Enter first player: '), readline(First),
    write('How many cards does this player have? '), readline(N1),
    write('Enter next player: '), readline(Next),
    assert(player(First)),
    assert(numCards(First,N1)),
    assert(firstPlayer(First)),
    assert(next(First, Next)),
    assertNextPlayer(First, First, Next).

assertNextPlayer(First, Last, done) :- assert(next(Last, First)).
assertNextPlayer(First, Previous, Current) :-
    write('How many cards does this player have? '), readline(N),
    assert(numCards(Previous, N)),
    assert(next(Previous, Current)),
    assert(player(Current)),
    write('Enter next player (or "done." if no more): '),
    read(Next), assertNextPlayer(First, Current, Next).

getMyName :- read(Character), inputMyName(Character).
inputMyName(Character) :- player(Character),!, assert(me(Character)).
inputMyName(_) :- write('That\'s not a valid player name. '), listPlayers, getMyName.

%%%%%%%%%%%%%%%%%%%%%%%% SHOW DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help :-
    write('You may enter any of the following commands:'), nl,nl,
    write('"showDatabase." - List all the information that is currently known.'), nl,nl,
    write('"setup." - This command only needs to be run once. It will let you initialize the game with the rooms, weapons, and characters, and players in your game.'), nl,nl,
    write('"listPlayers." - Lists all the players in the game'), nl,nl,
    write('"listAllCards." - Lists all the cards in the game'), nl,nl.
    
%etc. add commands as they are created.

showDatabase :- listAllCards, listPlayers, listHasCards, listAllPlayerCards.

listAllCards :- listCards(room), listCards(weapon), listCards(character).

listCards(Type) :- findall(C, call(Type, C), Cards), write('The '), write(Type), write('s are: '), writeln(Cards),nl.

listPlayers :- allPlayers(Players), write('The players are: '), writeln(Players),nl.

listAllPlayerCards :- allPlayers(Players), forall(member(P,Players), listPlayerCards(P)).

listPlayerCards(Player) :- allHas(Player, HasCards), write(Player), write(' has these cards: '),
                           writeln(HasCards),
                           allLacking(Player, LacksCards), write(Player),
                           write(' does not have any of these cards: '), writeln(LacksCards), nl.

allHas(Player, Cards) :- findall(C, has(Player, C), Cards).
allLacking(Player, Cards) :- findall(C, lacks(Player, C), Cards).


ps :- listing(player(X)).

readline(String) :- read_line(Input), string_codes(String, Input).
%% writeline(String) :- writef("%s", [String]).
writeline(String) :- write(String).



% Test facts.

character(scarlett).
character(plum).
character(peacock).
character(green).
character(mustard).
character(white).

weapon(candlestick).
weapon(knife).
weapon(leadpipe).
weapon(revolver).
weapon(rope).
weapon(wrench).

room(kitchen).
room(ballroom).
room(conservatory).
room(dining).
room(billiard).
room(library).
room(study).
room(hall).
room(lounge).

% our test game has only four players
%% player(scarlett).
%% player(plum).
%% player(white).
%% player(mustard).

dead(mustard).

me(plum).

%% has(plum, )




% mext(CurrentPlayer, NextPlayer).
next(scarlett, plum).
next(plum, white).
next(white, mustard).
next(mustard, scarlett).

%% has(Player, Card) :- fail.
%% lacks(Player, Card) :- fail.
%% maybe(Player, Card) :- fail.

%% sample solution:
lacks(white, green).
lacks(white, study).
lacks(white, knife).
lacks(mustard, green).
lacks(mustard, study).
lacks(mustard, knife).
lacks(scarlett, green).
lacks(scarlett, study).
lacks(scarlett, knife).
lacks(plum, green).
lacks(plum, study).
lacks(plum, knife).



card(X) :- character(X).
card(X) :- weapon(X).
card(X) :- room(X).

assertLacksTrio(Player, Character, Weapon, Room) :-
   assertLacks(Player, Character), assertLacks(Player, Weapon), assertLacks(Player, Room).


%% assertLacks and assertHas creates proper assertions if necessary.
assertLacks(Player, Card) :- lacks(Player, Card), !.
assertLacks(Player, Card) :- assert(lacks(Player, Card)), !.

%% Ensures other players lack the card, if neccessary.
assertHas(Player, Card) :- has(Player, Card), !.
assertHas(Player, Card) :- assert(has(Player, Card)), allPlayers(Players),
   delete(Players, Player, Others), forall(member(P, Others), assertLacks(P, Card)).


allPlayers(Players) :- findall(P, player(P), Players).


%% Adds to DB knowledge gained from this round of my suggestion.
% TODO: make generic 'suggestion' that can assert maybes. Have shorter version, mysuggestion based on generic
%%  mysuggestion/6:(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard)
mysuggestion(InspectingPlayer,_,_,_,none,_) :- me(InspectingPlayer), !.
mysuggestion(DisprovingPlayer, _, _, _, DisprovingPlayer, DisprovingCard) :- assertHas(DisprovingPlayer, DisprovingCard), !.
mysuggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard) :-
   assertLacksTrio(InspectingPlayer, Character, Weapon, Room),
   next(InspectingPlayer, NextPlayer), !,
   mysuggestion(NextPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard).


%% Produce True when the given variables are indisputedly in the Clue envelope.
accusation(Character, Weapon, Room) :-
   character(Character), allLack(Character),
   weapon(Weapon), allLack(Weapon),
   room(Room), allLack(Room).

%% True when there are no players holding the given cardd
allLack(Card) :- allPlayers(Players), foreach(member(Player, Players), lacks(Player, Card)).



%% listings/0 is handy for testing.
listings :- listing(lacks(_, _)), listing(has(_, _)).



%% Starting the fun game!

makesuggestion :-
    write('When it is your turn to make a suggestion, hit enter.'),
    readline(Ignore),
    promptTilValid('Enter your CHARACTER suggestion: ', character, Character).

    %% write('Enter your CHARACTER suggestion: '), readline(Character),
    %% write('Enter your WEAPON suggestion: '), readline(Weapon),
    %% write('Enter your ROOM suggestion: '), readline(Room),

listCards(CardType) :-
    findall(Card, call(CardType, Card), Cards),
    write('The '), write(CardType), write('s are: '), writeln(Cards), nl.



promptTilValid(Prompt, Goal, Input) :-
    write(Prompt),
    readline(Input),
    verified(Prompt, Goal, Input).

verified(Prompt, Goal, Input) :-
    

    inputIsGoal(Goal, Input),


    call(Goal, Input), !.

promptTilValid(Prompt, Goal, Input) :-
    write(Prompt),
    readline(Input), !,
    not(call(Goal, Input)),
    write('Sorry but that is not a valid '), write(Goal),
    listCards(Goal), !,  promptTilValid(Prompt, Goal, Input).



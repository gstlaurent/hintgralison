%% :- expects_dialect(sicstus).

:- dynamic has/2, lacks/2, maybe/2.

:- dynamic room/1,
   weapon/1,
   character/1,
   player/1,
   numPlayers/1,
   playerNum/2,
   me/1,
   dead/1,
   location/2,
   next/2.

/*
Examples of all the types of facts

weapon(knife).
room(library).
character(scarlett).
player(scarlett).

numPlayers(3).
playerNum(1, scarlett).
validNum(2).
me(scarlett).

*/

clue :- intro.

%%%%%%%%%%%%%%%%%%%%%%%% INTRO TEXT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO write better intro to the game. explain the commands you can type. Tell them to type "setup." to initialize the game.
intro :-
    write('Welcome to Dr. Clue!'), nl,
    write('If you need help, type "help." This will let you know what commands are available.'), nl,
    write('To begin the game, type "setup." This will lead you through the initialization of the game.'), nl.    

%%%%%%%%%%%%%%%%%%%%%%%% GAME SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO better wording of instructions.
setup :-
    write('It\'s time to set up the game. When entering names for the rooms, weapons, and characters, make sure to begin with a lowercase letter, end with a period, and avoid using any punctuation or spaces before the final period.'), nl,
    write('Start by entering the names of all the rooms on your clue board.'), nl,
    getInfo(room), nl,

    write('Enter the names of all the weapons in the game.'), nl,
    getInfo(weapon), nl,

    write('Enter the names of all the characters in your game.'), nl,
    write('These are the names of all the people who may have committed the murder, not just the current players.'), nl,
    getInfo(character), nl,

    write('How many people are playing? Enter a number from 2 to 6.'), nl,
    getNumPlayers, nl,

    write('Next, enter the players, starting with the player who will go first.'), nl,
    write('Use the name of a person\'s character to identify them.'), nl, 
    getPlayer(1), nl,

    write('Which character are you playing?'), nl,
    getMyName,

    write('Now, enter your cards.'), nl,
    getInfo(card), nl, 
    % TODO enter the number of cards each player has
    write('It\'s time to begin the game!'), nl. %TODO lead into the gameplay here*/

getInfo(Type) :-
    % why does writeln write the commas?!?!
    %writeln(['Enter the name of a ', Type, ' or "done." if there are no more ', Type, 's.']), nl,nl.
    write('Enter the name of a '), write(Type), write(' or "done." if there are no more '),
    write(Type), write('s.'), nl, nl,
    read(Entry),
    input(Type, Entry).

input(_, done) :- !.
input(room,X) :- assert(room(X)), getInfo(room).
input(weapon,X) :- assert(weapon(X)), getInfo(weapon).
input(character,X) :- assert(character(X)), getInfo(character).
input(player,X) :- character(X),!, assert(player(X)), getInfo(player).
input(player,_) :- write('That\'s not a valid character name. Please try again.'), nl, %TODO write the valid character names.
                   getInfo(player).
input(card,X) :- room(X),!, me(Y), hasCard(Y,X), getInfo(card).
input(card,X) :- weapon(X),!, me(Y), hasCard(Y,X), getInfo(card).
input(card,X) :- character(X),!, me(Y), hasCard(Y,X), getInfo(card).
input(card,_) :- write('That\'s not a valid card. Please try again.'), nl, getInfo(card). %TODO write the valid card names

getNumPlayers :- read(Number), setPlayersTo(Number).

setPlayersTo(Number) :- validNum(Number),!, assert(numPlayers(Number)).
setPlayersTo(_) :- write('That is not a valid number. Please try again.'), nl,
                   getNumPlayers.

% How many players can play a game of clue.
validNum(2).
validNum(3).
validNum(4).
validNum(5).
validNum(6).

getPlayer(N) :-
    numPlayers(X),
    N == X, !,
    write('Which character is player '), write(N), write('?'), nl,
    read(Player),
    input(Player,N).
getPlayer(N) :-
    nextPlayer(N,M),
    write('Which character is player '), write(N), write('?'), nl,
    read(Player),
    input(Player,N),
    getPlayer(M).

inputPlayer(Player,N) :- character(Player), !, assert(playerNum(N, Player)).
inputPlayer(_,N) :-
    write('That is not a valid player name. Please try again.'), nl,
    listPlayers,
    getPlayer(N).

nextPlayer(N,M) :-
    validNum(N),
    numPlayers(X),
    N < X,
    M =:= N + 1.
nextPlayer(N,M) :-
    numPlayers(X),
    ==(N,X),
    ==(M,1).

getMyName :- read(Character), inputMyName(Character).
inputMyName(Character) :- player(Character),!, assert(me(Character)).
inputMyName(_) :- write('That\'s not a valid player name. Please try again.'), nl, %TODO write the player names
    getMyName.

%%%%%%%%%%%%%%%%%%%%%%%% HELP COMMAND %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help :-
    write('You may enter any of the following commands:'), nl,nl,
    write('"showDatabase." - List all the information that is currently known.'), nl,nl,
    write('"setup." - This command only needs to be run once. It will let you initialize the game with the rooms, weapons, and characters, and players in your game.'), nl,nl,
    write('"listPlayers." - Lists all the players in the game'), nl,nl. %etc. add commands as they are created.


showDatabase :- true. %TODO

listPlayers :- allPlayers(Players), write('The players are: '), writeln(Players),nl.




ps :- listing(player(X)).

readline(X) :- read(X).

addPlayers :-
    write('Enter first player:'), readline(First),
    write('Enter next player:'), readline(Next),
    assertNextPlayer(First, First, Next).

assertNextPlayer(First, Last, []) :- assert(next(Last, First)).
assertNextPlayer(First, Previous, Current) :-
    assert(next(Previous, Current)),
    write('Enter next player (or just hit enter if no more):'),
    readline(Next), assertNextPlayer(First, Current, Next).

%% addPlayer(Previous) :-
%%     write('Enter the next player:'),
%%     readln(Next), assert(next(Previous, Next)), addPlayer(Next).




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

%% :- expects_dialect(sicstus).

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

    write('Which player are you?'), nl,
    retractall(me(_)),
    getMyName,

    write('Now, enter your cards.'), nl,
    retractall(has(_,_)),
    getInfo(card), nl, 
    % TODO enter the number of cards each player has


    write('It\'s time to begin the game!'), nl. %TODO lead into the gameplay here*/

getInfo(Type) :-
    % why does writeln write the commas?!?!
    %writeln(['Enter a ', Type, ' or "done." if there are no more ', Type, 's.']), nl,nl.
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
input(card,_) :- write('That\'s not a valid card. '), nl, listCards, getInfo(card).

getPlayers :-
    write('Enter first player: '), readline(First),
    write('How many cards does this player have? '), readline(N1),
    write('Enter next player: '), readline(Next),
    write('How many cards does this player have? '), readline(N2),
    assert(player(First)),
    assert(numCards(First,N1)),
    assert(firstPlayer(First)),
    assert(next(First, Next)),
    assert(numCards(Next, N2)),
    assertNextPlayer(First, First, Next).

assertNextPlayer(First, Last, done) :- assert(next(Last, First)).
assertNextPlayer(First, Previous, Current) :-
    assert(next(Previous, Current)),
    assert(player(Current)),
    write('Enter next player (or "done." if no more): '),
    write('How many cards does this player have? '), readline(N),
    assert(numCards(current, N)),
    read(Next), assertNextPlayer(First, Current, Next).

getMyName :- read(Character), inputMyName(Character).
inputMyName(Character) :- player(Character),!, assert(me(Character)).
inputMyName(_) :- write('That\'s not a valid player name. '), listPlayers, getMyName.

%%%%%%%%%%%%%%%%%%%%%%%% HELP COMMAND %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help :-
    write('You may enter any of the following commands:'), nl,nl,
    write('"showDatabase." - List all the information that is currently known.'), nl,nl,
    write('"setup." - This command only needs to be run once. It will let you initialize the game with the rooms, weapons, and characters, and players in your game.'), nl,nl,
    write('"listPlayers." - Lists all the players in the game'), nl,nl,
    write('"listCards." - Lists all the cards in the game'), nl,nl.
    
%etc. add commands as they are created.

showDatabase :- true.

listCards :- listRooms, listWeapons, listCharacters.
listRooms :- findall(R, room(R), Rooms), write('The rooms are: '), writeln(Rooms),nl.
listWeapons :- findall(W, weapon(W), Weapons), write('The weapons are: '), writeln(Weapons),nl.
listCharacters :- findall(C, character(C), Characters), write('The characters are: '), writeln(Characters),nl.
listPlayers :- allPlayers(Players), write('The players are: '), writeln(Players),nl.

ps :- listing(player(X)).

readline(X) :- read(X).


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

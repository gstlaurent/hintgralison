:- dynamic has/2, lacks/2, maybe/2.

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
player(scarlett).
player(plum).
player(white).
player(mustard).

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

%% True when there are no players holding the given card
allLack(Card) :- allPlayers(Players), foreach(member(Player, Players), lacks(Player, Card)).



%% listings/0 is handy for testing.
listings :- listing(lacks(_, _)), listing(has(_, _)).
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

lacksTrio(Player, Character, Weapon, Room) :-
   lacksCard(Player, Character), lacksCard(Player, Weapon), lacksCard(Player, Room).


%% lacksCard and hasCard creates proper assertions if necessary.
lacksCard(Player, Card) :- lacks(Player, Card), !.
lacksCard(Player, Card) :- assert(lacks(Player, Card)), !.

%% Ensures other players lack the card, if neccessary.
hasCard(Player, Card) :- has(Player, Card), !.
hasCard(Player, Card) :- assert(has(Player, Card)), allPlayers(Players),
   delete(Players, Player, Others), forall(member(P, Others), lacksCard(P, Card)).


allPlayers(Players) :- findall(P, player(P), Players).


%% Adds to DB knowledge gained from this round of my suggestion. TODO: make generic 'suggestion'. Have shorter version of mysuggestion that assumes me as the initiator.
%% mysuggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlauer, DisprovingCard).


%%  mysuggestion/6:(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard)
mysuggestion(InspectingPlayer,_,_,_,none,_) :- me(InspectingPlayer), !.

mysuggestion(DisprovingPlayer, _, _, _, DisprovingPlayer, DisprovingCard) :- hasCard(DisprovingPlayer, DisprovingCard), !.
mysuggestion(DisprovingPlayer, _, _, _, DisprovingPlayer, DisprovingCard) :- assert(has(DisprovingPlayer, DisprovingCard)), !.

mysuggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard) :-
   lacksTrio(InspectingPlayer, Character, Weapon, Room),
   next(InspectingPlayer, NextPlayer), !,
   mysuggestion(NextPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard).

%% Produce True when the given variables are indisputedly in the Clue envelope.
accusation(Character, Weapon, Room) :-
   character(Character), allLack(Character),
   weapon(Weapon), allLack(Weapon),
   room(Room), allLack(Room).

%% True when there are no players holding the given card
allLack(Card) :- allPlayers(Players), foreach(member(Player, Players), lacks(Player, Card)).



%% Listing might be handing for testing.
%% listing(lacks(X,Y)).
begin :- (dynamic has/2), (dynamic lacks/2), (dynamic maybe/2).
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

me(plum).
dead(mustard).

% mext(CurrentPlayer, NextPlayer).
next(scarlett, plum).
next(plum, white).
next(white, mustard).
next(mustard, scarlett).

%% has(Player, Card) :- fail.
%% lacks(Player, Card) :- fail.
%% maybe(Player, Card) :- fail.


card(X) :- character(X).
card(X) :- weapon(X).
card(X) :- room(X).


lacksCard(Player, Card) :- lacks(Player, Card), !.
lacksCard(Player, Card) :- assert(lacks(Player, Card)), !.

lacksAll(Player, Character, Weapon, Room) :-
   lacksCard(Player, Character), lacksCard(Player, Weapon), lacksCard(Player, Room).

%% Adds to DB knowledge gained from this round of my suggestion.
%% mysuggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlauer, DisprovingCard).
mysuggestion(InspectingPlayer,_,_,_,none,_) :- me(InspectingPlayer), !.

mysuggestion(DisprovingPlayer, _, _, _, DisprovingPlayer, DisprovingCard) :- has(DisprovingPlayer, DisprovingCard), !.
mysuggestion(DisprovingPlayer, _, _, _, DisprovingPlayer, DisprovingCard) :- assert(has(DisprovingPlayer, DisprovingCard)), !.

mysuggestion(InspectingPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard) :-
   lacksAll(InspectingPlayer, Character, Weapon, Room),
   next(InspectingPlayer, NextPlayer), !,
   mysuggestion(NextPlayer, Character, Weapon, Room, DisprovingPlayer, DisprovingCard).

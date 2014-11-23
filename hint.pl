%% Welcome to Dr. Clue!

begin :- dynamic joe/1.

joe(X) :- fail.
behold :- write_ln('Gobble'), read(X), write('hey '), write(X).

bill(3).

test(X) :- bill(X), asserta(joe(X)).



%% must have:
%%     easy documention describing
%%         what can and can't do
%%         how to use it
%%         how it works
%%     easy to use interface


%% initialize game setup
%%     - set room names
%%     - set weapon names
%%     - set num players
%%     - set player order  (... perhaps together, including setting player names, or just colours?)
%%     - set held cards

%% minimum
%%     - input my suggestion (name, weapon, room)
%%         - input what learned from suggestion (player, card) | nothing
%%     - view database contents (dismissed/exonerated cardsbytype, pending/suspect/curious cardsbytype)
%%     - output an accusation when known

%% betterassert
%%     - input other players suggestions and if a player showed them a card
%%         (output what card should I show them?)
%%     - output what my next suggestion should be

%% best
%%     - monitor other players (build models for them too)
%%         - output red-herring suggestions
%%         - output preemtpive accusations (shows all possibilities, and lets player choose one; or just most likely?)
%%     - AND BEYOND...


% nice opening


room(convservatory).
weapon(knife).
character(scarlett). % input

player(scarlett).   % input these in order, then ask if there are any characters not represented by players
player(plum).
player(white).

next(plum, scarlett).

card(knife).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source(Card, Player).   %  ?

potentia(Card).             % ??

maybeHas(Player, Card). % ????????


has(Player, Card).
maybe(Player, Card).    % we have reason to believe might have (ie, showed something to someone else)
%% unkown(Player, Card).
lacks(Player, Card).

cardInfo(Player, Card, Status).

showTrios(Player, Room, Weapon, Character).  % this player showed these things. It's a fact.

%% suggestShowedTrio(SuggestingPlayer, ShowingPlayer, Room, Weapon, Character). %

%% maybeHas(Player) :-

%% Alison: will think about/implement UI -- not LOTS!
%% Graham: will think about/implement actual logic -- not LOTS!

%% playerCard(Player, Card, maybe) :- maybe(Player, Card).


%% next(1,2).
next(FromPlayer, ToPlayer.

%% characterRepresentedBy(Player, Character).

dead(mustard).


%% assert(room(convservatory)).





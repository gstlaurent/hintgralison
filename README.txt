# Graham St-Laurent, i5l8, 23310121
# Alison Clark, m0v8, 21464136

*******************************
DR. CLUE DOCUMENTATION
*******************************

Congratulations on your acquisition of the new Clue solving program, Dr. Clue!

Dr. Clue is a very user-friendly program. To start it, open prolog and type:

    [drclue]. <ENTER>

That loads up the program. To run it, type:

    clue. <ENTER>

That's it! It'll walk you through the rest. First you'll setup the game, telling
Dr. Clue things like what the rooms, weapons, and characters are, and who the
players are and what their order is. Once you're done that, it's ready to help
you play Clue! Just follow along with what it says, inputting information as
different players have their different turns. The more info you give it, the
better it will be at helping you Win at Clue! Eventually, when it knows enough,
it'll tell you to make an accusation. Do that as soon as you can and WIN THE
GAME! Congratulations again!

You can type things the way you always do: use capitals and lowercase, spaces,
punctuation -- you name it! Just hit <ENTER> when you're done typing, and Dr.
Clue will understand. But be careful: every time you type something, you have
to type it exactly the same way, so make sure you type it the way you want it
the first time. Don't worry though, if you happen to forget exactly how you
typed things, Dr. Clue will notice and it'll let you know what your options
really are. Sometimes Dr. Clue will even ask you preemptively if you want to
see everything it knows; then you can type

    db <ENTER>

and it'll print out its database contents. But it'll tell you all that when the
time comes, so for now, just rest easy and Clue away!



****************************************************
FEATURES OF DR. CLUE: WHAT DOES IT DO?
****************************************************

1.  Easy-to-use interface.
It prompts you throughout and offers you suggestions when it doesn't
understand what you tell it -- i.e., you made a typo -- or it recognizes your
input as otherwise invalid -- i.e., you might be lying to it. At the beginning
of each turn, you can easily ask Dr. Clue to display its database contents.

2.  Let's you input your suggestions.
When you tell Dr. Clue about a suggestion you made, along with the player and
card that you were shown (if any), then it will remember that that player has
that card. Additionally, it will infer some other information, which is
feature #3.

3.  Infers card disownership.
Any time Dr. Clue finds out that a player has a card -- either from your
suggestion or during the setup -- it realizes that no one else can have that
card, so it adds that info to its database. But it also infers that players
are lacking certain cards if, during a suggestion -- whether it's your or
someone else's -- they had to give a pass without showing anything.

4.  Provides Suggestion Suggestions.
Dr. Clue keeps track of cards you haven't seen yet. When you ask it to show
you the database, it will tell you what these are; it calls those 'Cards that
may be in the envelope'. So when it is your turn to make a suggestion, all you
have to do is tell Dr. Clue what room you are in, and it will provide you with
a recommended suggestion consisting of as many cards that you haven't seen yet
as it can.

5.  Makes Accusations!
If Dr. Clue has learned enough to know for certain what the three cards are that
are in the envelope, then it let's you know! When you next have a chance (which
might be immediately), you can make an accusation and WIN THE GAME!
Congratulations, you! Congratulation, Dr. Clue!




****************************************************
FEATURES OF DR. CLUE: WHAT ARE ITS LIMITATIONS?
****************************************************

Well, there are lots of things that Dr. Clue doesn't do -- including making me
an omlette, regrettably -- but here are some of the more interesting ones:

1.  Handling unsportspersonlike users
Dr. Clue does not work if you LIE to it, or you CHEAT. In general, it's
smart enough to know when you've simply made a typo and let's you try again
(reminding you, as always, what the valid answers would be), but it will get
confused, for example, if you tell it that a rival player asked to see a card
that you have, but you are then too lazy to tell Dr. Clue that you actually
showed that card, telling it instead that you passed.

2.  Recovery from a mistake.
If you accidentally tell Dr. Clue the wrong thing -- such as that you were shown
Green when you were really shown White, or that Player 2 passed, when in fact,
Player 2 showed something -- then its database might be permanently confused.
Unfortunatley, the only way to get Dr. Clue into a usable state after something
like that is to quit the program then reload and run it. There is no Undo; there
is no Ctrl+Z.

3.  Guesses
Besides updating its database with card has and card has-nots, Dr. Clue doesn't
keep track of what suggestions players are making. Consequently, it can't guess
what cards or strategies might be lurking within opposing hands and minds.




****************************************************
THANK YOU THANK YOU THANK YOU!
****************************************************
We are honoured that you chose Dr. Clue as your Clue helper. Please Clue
responsibly and we look forward to playing a game with you sometime. If you
really enjoy the program, consider sending a donation of Canadian pennies to

    Dr. Clue, Inc.
    100 Worthington Lane
    Whestlyham, Berkenshire
    V8382B320

See you in the Conservatory!

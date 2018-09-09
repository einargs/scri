# What is Scri?
Scri is an extensible markup language designed to automate the bookkeeping
involved in complex forum quests.

Scri is written in Haskell, and uses Haskell files to write the buisness logic
specific to each quest's gameplay system.

## Wait wait. Backup. What's a "forum quest"?
In the simplest terms possible, it's a form of roleplaying where participants
on a forum vote on actions that should be taken in response to longform textual
updates written out on a forum by the "Quest Master".

Many quests involve complex systems resembling those of both tabletop and
computer RPGs which the Quest Master has to keep track of. Scri seeks

For more details, please check out the  [Intro to Questing](https://forums.sufficientvelocity.com/threads/intro-to-questing.12494/)
thread on the Sufficient Velocity forums.

## Why is Scri useful?
Many quests involve complex systems resembling those of both tabletop and
computer RPGs which the Quest Master has to keep track of, often on a massive
scale. Further, these systems often involve random chance, which can necessitate
a somewhat silly amount of dice rolls. Scri seeks to allow Quest Masters to
simply write markup statements alongside the actual text of an update which Scri
will then read in order to generate information like character sheets in a
reproducible way.

There are several benefits to this beyond just time savings. Because all
calculations are declarative, the Quest Master doesn't need to worry about
making mistakes--and if they do make a mistake, it's easy for them to correct
it no matter where in an update (or even story) that mistake might be.

Further, because all character sheets and similar apochrypha are re-generated
upon each build, it's easy for a Quest Master to change formats without having
to worry about manually re-writing something from scratch each time.

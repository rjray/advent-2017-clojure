# Breakdown of Files

Jump to day: [1](#day01clj)&nbsp;|&nbsp;[2](#day02clj)&nbsp;|&nbsp;[3](#day03clj)&nbsp;|&nbsp;[4](#day04clj)&nbsp;|&nbsp;[5](#day05clj)&nbsp;|&nbsp;[6](#day06clj)&nbsp;|&nbsp;[7](#day07clj)&nbsp;|&nbsp;[8](#day08clj)&nbsp;|&nbsp;[9](#day09clj)&nbsp;|&nbsp;[10](#day10clj)&nbsp;|&nbsp;[11](#day11clj)&nbsp;|&nbsp;[12](#day12clj)&nbsp;|&nbsp;[13](#day13clj)&nbsp;|&nbsp;[14](#day14clj)&nbsp;|&nbsp;[15](#day15clj)&nbsp;|&nbsp;[16](#day16clj)&nbsp;|&nbsp;[17](#day17clj)&nbsp;|&nbsp;[18](#day18clj)&nbsp;|&nbsp;[19](#day19clj)&nbsp;|&nbsp;[20](#day20clj)&nbsp;|&nbsp;[21](#day21clj)&nbsp;|&nbsp;[22](#day22clj)&nbsp;|&nbsp;[23](#day23clj)&nbsp;|&nbsp;[24](#day24clj)&nbsp;|&nbsp;[25](#day25clj)

Here is a breakdown of the various files in this directory. Files with names of
the form `dayNN.clj` represent the code actually used to solve the problems
(with some tweaking done using a static analysis plug-in for Leiningen). Files
with `bis` in the name are modified/tuned versions of the given original day.
(If you see comments in a file, I can usually promise you they were added after
the fact.)

The numbers in parentheses in the descriptions of the files represent the rank
I had for when my solutions were submitted and accepted. Time, if given, is a
rough estimate of how long it took to solve both halves.

A given day and part can be run via:

```
lein run DAY PART
```

where `DAY` is a number from 1-25 and `PART` is 1 or 2. If there is a "bis"
version of a day, that can be run via:

```
lein run -b DAY PART
```

## [day01.clj](day01.clj)

Day 1 (--/--).

Off to a good start: got part 1 wrong the first shot.

## [day02.clj](day02.clj)

Day 2 (--/--).

Took advantage of how the combinatorics' library's `combinations` function
maintains element order. This allowed me to assume the first number in the
pair was always the smaller, for part 2.

## [day03.clj](day03.clj)

Day 3 (--/--).

Part 1 took an inordinate amount of time, finding a usable algorithm for
calculating the position of an arbitrary number on the spiral. I made a point
of not iterating _n_ times to get the coordinates, because I was certain that
there was a mathematical way to do it. Plus, I worried that it wouldn't help
part 2.

Part 2 was much simpler, by comparison. The `spiral` function written for part
1 was helpful, but an iterative mapping would have also worked in this case.
Still, part 2 went much more quickly.

## [day04.clj](day04.clj)

Day 4 (--/--).

This was a pretty simple day. Part 1 was just identifying phrases with at
least one duplicate word. This was done with a running set instance and a
`loop` construct over the words in the phrase.

Part 2 added an addition constraint, that no two words could be anagrams of
each other. Anagrams had to use all letters in both words. This was done by
turning each word in the phrase into a `map` by use of `group-by` (with
`identity` as the grouping predicate). This produced structures that could be
definitively compared for equality.

## [day05.clj](day05.clj)

Day 5 (--/--).

Another fairly simple day; part 1 was just a matter of walking the "program"
and applying the jumps.

Part 2 upped things by changing the way the instructions in the "program"
updated each step. As I had used `update` on the vector of jump-values for
part 1, I just had to make the function I'd written take an updater as a
parameter, change part 1 to pass `inc` as this parameter, then write a
different one for part 2.

## [day06.clj](day06.clj)

Day 6 (--/--).

Part 1 was a familiar pattern: transform a data structure repeatedly until you
detect a loop. Nothing new here (given previous years done).

Part 2 was a little different twist than usual: when the loop is detected, find
the size of the cycle by returning the distance between the first and second
occurrences of the structure. Not hard for Clojure, just added a `map` that used
the structure as a key and the first-seen step as the value.

I probably could have re-coded the initial `find-loop-count` fn to solve both
halves of the problem, but it wasn't a priority.

## [day07.clj](day07.clj)

Day 7 (--/--).

Breaking my rules a little bit on this one, because part 2 nearly drove me mad.
This time, I've re-factored the code completely after solving part 2, such that
part 1 now uses code from part 2 instead of the initially solution to part 1.

Part 1 was straightforward: find the node in the input that has no parent, and
that will be your root node. The initial version of this worked fine, but the
structure that was built from the data to solve part 1 was essentially useless
in solving part 2. And the structure I came up with for part 2 made finding the
root-node much easier and more succinct.

But part 2 took many hours spread over about 3 days. (To be fair to myself, I'm
not laser-focused on this year like I am in the live events so I would only
work on it in little bits of time.) I started out trying to do it as a
depth-first search using an iterative algorithm. I couldn't get that to
actually run to completion, so I got frustrated and looked at some past
solutions from the reddit thread on 2017 day 7. I found a pretty tight Python
solution that was also using DFS, so I looked to see what I was doing
differently. The main difference seemed to be that the Python was using
recursion and printing *all* cases where a node was adjusted, including those
higher in the tree that don't actually change (because the lowest instance
solves the tree overall). Generally, I liked the approach. But I (stubbornly)
told myself I could still do it iteratively with a stack, and *my* version
would be able to terminate and return the answer when first found, rather than
traversing the entire tree.

I was wrong, and I got my first two answers to part 2 wrong.

After hours of debugging and comparing my output to that of the Python code, I
realized that (for some reason) my code was *never looking at the key set of
nodes*, at least not all together. I could not figure out why, and I never did.
Out of frustration, I simply re-tooled my solving function to be recursive and
to print each number as it was calculated, taking it on faith that (like the
Python solution) the first number displayed would be the correct value. It was,
and I got the star for part 2 and went to bed.

But it felt **wrong**. The function was not returning the answer in this case,
it was returning the total weight at the root, a value that was then ignored.
So I looked at it some more this morning, and the only way I could think of
that would carry things over the series of recursive calls was to use an
[atom](https://clojuredocs.org/clojure.core/atom). I slightly re-factored the
recursive function to take an atom as a second argument and in cases where an
adjustments was found it would `conj` the adjusted value onto the list that the
atom held. Then, at the end of the solving process, instead of returning the
total-weight value of the root it returned the first value placed into the atom
(via `(last @values)`).

It still feels wrong. I still feel like there's a way to do this with a stack
and an iterative algorithm. But I can't spend more time on it right now.

## [day08.clj](day08.clj)

Day 8 (--/--).

Another machine-simulation pair of problems. In this case, the "code" was made
up of lines of register operations, each gated on an `if` statement.

Part 1 was to just run all the lines and determine the highest value of all the
registers at the end. Writing the "runner" was pretty easy given that I've done
similar problems in previous years.

Part 2 wanted to know the highest value any register reached throughout the full
run. That only took a little tweaking to the original run-code, though I chose
to just replicate it out of expediency.

## [day09.clj](day09.clj)

Day 9 (--/--).

A parsing problem, generally. Part 1 was to count the number of groups at each
level (depth) and compute a score based on the counts multiplied by the
associated depths. For this, I wrote a small character-driven state-machine that
tracked the machine's state through an integer `depth` and two Boolean values:
`skip` which indicated that we just saw a `!` and the current character should
be skipped, and `garbage` which indicated that the stream was currently in a
"garbage" sequence.

In part 2, the task was to count the number of characters skipped within the
garbage sequences. The leading and trailing `<` and `>` didn't count, nor did
the `!` that cancels a character or the character that gets canceled. Note that
this _does_ mean that extraneous `<` characters should be counted. Because of
the state-machine nature of the part 1 solution, I only had to add an additional
value to the tracking state (`gc`, for "garbage count") and move one of the
rules one step sooner than it had been for part 1 (to ensure that the extra `<`
got counted).

## [day10.clj](day10.clj)

Day 10 (--/--).

Got my *third* wrong answer on part 2, simply because I forgot to trim the
newline character from the input string before feeding it to the puzzle code.

Took a little while to get the mechanics right for part 1, to apply the hashing
algorithm itself on a vector of the numbers. Once I got it, I had a bug that I
*almost* didn't catch because the test data yielded the correct answer. On a
hunch, I ran it again while dumping the vector at each step and saw that the
intermediate steps were wrong. Had I run with this, I would have gotten part 1
wrong on the first try, as well.

Part 2 was just to run the hashing multiple times cumulatively, while treating
the input as a series of ASCII bytes instead of integers. This was where I made
my third error, as I forgot to take the newline off of the string first. Thus,
my input sequence had an additional number (13) at the end. The rest of part 2
was pretty simple.

## [day11.clj](day11.clj)

Day 11 (--/--).

This day was another graph-distance type problem, with the twist being that the
field was a hex-grid instead of a typical square-grid.

Part 1 was just to find the distance of the final point reached. I used a
coordinate representation and distance calculation from the
[Red Blob Games](https://www.redblobgames.com/grids/hexagons/) website. Simple,
took longer to read it than it took to code it.

Part 2 asked for the furthest distance from the origin the walker got during
the walk. At this point, I knew that everything was relative to a (0, 0, 0)
origin, so I simplified a few things over part 1 (like the distance
calculation).

## [day12.clj](day12.clj)

Day 12 (--/--).

This should not have gone as it did. Part 1 was easily answered, part 2 took
three tries.

The problem was basically identification of disjoint graphs. In part 1, the
task was to find the size of the graph that contained the 0 node. In part 2,
the task was to count the total number of disjoint graphs. There was a subtle
bug in my part 1 code that did not affect part 1, but broke part 2. Basically,
the `seen` set was being initialized to the starting node, but should have been
initialized as an empty set. I don't really understand why that made the
difference in the outcome, so I'll have to study it a bit more.

## [day13.clj](day13.clj)

Day 13 (--/--).

This was actually a pretty easy day as well, save for me misunderstanding a key
part of the the part 2 conditions.

Part 1 is just to see what the impact would be for running through the firewall
immediately. With some simple modulo math and such, it was an easy function to
write and test.

For part 2, the goal was to find a time-delay that would prevent you from being
"caught" by any of the scanners. I mis-read this as being the same as having a
score of zero (based on the scoring metric of part 1). So I had a lot of time
lost trying to understand why my result for the test value was so much smaller
than the correct value. The actual requirement was to pass through the firewall
with *no* scanner hits. So, if the first (level 0) scanner hits, that counts
against the goal even though the "severity" from it is zero. Once I accounted
for that, I got the correct answer via brute-force in just under 80 seconds.

I'll probably revisit part 2, since I feel that there's a better way to get the
answer. I'm reminded of a previous year's puzzle that relied on the Chinese
Remainder Theorem, but I don't want to get into that right now.

## [day13.clj](day13.clj)

As usual, I couldn't really wait to try something that would hopefully be
faster.

I rewrote part 2 to use `not-any?` to see if a given time-offset produced *any*
hits at given scanner. I then passed the testing function to `filter` and used
`first` to get the first off the resulting iterator. The run time was just a
hair over 9 seconds, an almost 89% increase in speed.

## [day14.clj](day14.clj)

Day 14 (--/--).

The basis of this day's puzzles was a 128x128 representation of disk blocks in
use. The determination of each point was based on the "knot hash" algorithm of
day 10. One hash string would expand into a 128-bit binary number.

For part 1 the goal was just to count all the used blocks. The majority of the
running time was spent on calculating the 128 hashes. Counting the 1's from
there was basic.

Part 2 was a clever take on a search problem. The task was to find and count
all the "regions" on the disk, where a region is a group of contiguous blocks
based on up/down/left/right adjacency. Rather than "search" the whole 128x128
field, a `for` comprehension was used to create a set of the coordinate pairs
of the used blocks. With the set, a `loop` was executed that would take the
next point from the set and find all set-members that were in the region. Once
the region was identified, a counter was incremented and the members of the
region were removed from the main set. When the set was empty, the count of
regions was the answer.

## [day15.clj](day15.clj)

Day 15 (--/--).

This was a fairly unique puzzle, and a very educational one (in a good way).

For this puzzle, I had to learn how to create my own lazy-sequences of values
from a function. To be fair, I'm not even sure I did it in the "best" way, but
it worked the first time and (given the number of values generated) was pretty
performant.

The puzzle calls for simulating two generators that produce numerical values in
the 1-2147483647 range. The goal of part 1 is to look at the first 40,000,000
pairs of numbers to see how many of the pairs are identical in the bottom 16
bits. The generators are created as a pair of lazy sequences using an
intermediary function that holds the "factor" value that is unique to each
generator. The resulting two sequences were fed to `map` to produce a sequence
of pairs that were tested. Part 1 took about 33.19 seconds to run.

In part 2, you are to "refine" the two generators so that generator A only
gives back those numbers from its sequence that are multiples of 4, and
generator B only gives back those that are multiples of 8. Rather than add
these factors directly into the generator functions themselves, I took
advantage of the fact that they were already lazy sequences. Each generator was
wrapped in a call to `filter` with a corresponding predicate. This produced a
secondary lazy-seq for each of the two. These were passed to the same "judge"
function that did the map->pair->comparison pipeline. For part 2, you only had
to compare 5,000,000 such pairs, since the number of underlying numbers being
produced would be so much higher. This was a case of part 2 taking significantly
less time than part 1, as it was only about 5 minutes to write and test the
wrapping filter code. Part 2 took about 22.62 seconds to run. Just for fun, I
ran it a second time with 40,000,000 pairs just to see how much longer it would
take: 180.17 seconds, which tracks pretty close with the original run-time for
5,000,000 pairs.

## [day16.clj](day16.clj)

Day 16 (--/--).

One key area of growth for me, is how quickly I now recognize certain "types"
of puzzles. Part 1 of today was simple-enough, but part 2 *could* have been a
lot harder had I not faced similar problems in past years.

Part 1 has you take a string of 16 "dancers" in a line, and permute them in
various ways based on the input. There were three different "dance moves", and
converting them to operations on a vector was pretty easy. The requested answer
was a string of the 16 (represented by letters "a" through "p") in the final
order.

Part 2, however, was to return the ordering of the 16 after running the series
of dance moves 1,000,000,000 times. While part 1 finished in about 0.1 seconds,
it's obvious that a billion permutations would take far too long. For problems
like this, the basis of the solution is to save the configuration after each
permutation and note when you see a result for the second time. Get the length
of the cycle by comparing to the number of the permutation to the number when
it was first seen, then you can use modulo arithmetic to compute which of the
permutations in the cycle would fall on 1,000,000,000.

This recognition meant solving part 2 in about the same amount of time as part
1 was solved. Only a single bug (reversed arguments to a function) kept it from
working initially, but once fixed the correct answer came out the first time.

## [day17.clj](day17.clj)

Day 17 (--/--).

In a way, this was another "solve for N iterations, now solve for an ungodly
number of iterations" puzzle.

Part 1 was to insert the numbers 1-2,017 into a list according to given rules
and a "cycle" number. It went pretty quickly and the answer took about 2.4
seconds.

Part 2 was to insert numbers up to 50,000,000 and then return the number that
was immediately after the 0 in the resulting list. Besides the amount of time
it would have taken to run the original algorithm up to that number, the memory
use would probably also have been an issue. After some thought, it occurred to
me that I actually only had to keep track of which numbers were slated to be
inserted at position 1. The number 0 starts at position 0, and no new numbers
ever go into that slot. The lowest insertion point is 1, and since only indices
higher than 1 get shifted by an insertion I wouldn't have to track that. The
answer took about 10.5 seconds to produce.

## [day18.clj](day18.clj)

Day 18 (--/--).

Another machine-code-simulation pair. Part 1 went pretty quickly and got the
answer right on the first try. The techniques are well-established at this
point.

Part 2 *should* have been about as easy. It changes the definition of two of
the opcodes from part 1 and introduces async parallelism. The first run ran
long-enough to be clear that it was stuck in a loop. It took way too long to
figure out that one of the "instructions" in the input was using a number
literal for the first parameter to the opcode, and I had interpreted the
description to mean that the first argument was always a register. Once I
realized this, it was fixed. The fix was tricky, because there was more than
one place where I presumed an opcode's first parameter was going to be a
register.

## [day19.clj](day19.clj)

Day 19 (--/--).

This puzzle was of the type where you traverse a maze/field. For part 1, the
goal was to track in what order you encountered the letters in the field and
return them in order.

For part 2, you were just supposed to count the steps. Because of how I had
done part 1, this was trivial.

## [day20.clj](day20.clj)

Day 20 (--/--).

Got another wrong answer on part 1 of this.

The puzzle is: you are given a list of 1,000 particles. Each line gives you
the (starting) position, velocity, and acceleration vectors. In part 1, you are
to move the particles in 3D space until it becomes clear which particle will
always be the closest to the origin. My first answer was high, because my
approach needed to run a little longer than I initially did. My method was to
move the particles one tick then record the closest-to-origin index. I would
run this until the same index showed up in 100 consecutive steps. But that was
not enough. I got the answer wrong, so I bumped it up to 500 consecutive steps.
That gave the right answer, but there was probably a more mathematical approach
that wouldn't require so many calculations.

In part 2, you are told to remove any particles that collide after a time-step.
The requested answer is how many particles remain once there are no more
possible collisions. I started out thinking that I could do this with a system
of linear equations, but quickly (as in, before I wrote any code!) remembered
that the paths aren't linear (due to the acceleration). Poking around via
Google gave me an equation for calculating the position *P<sub>t</sub>* for any
*t* without having to do it step-wise for each particle. But again, I was
vexed by the question of how long I needed to see no further collisions before
I could safely assume the system was stable. Still, part 2 ran faster than part
1 had, and I got the correct answer on the first try.

## [day21.clj](day21.clj)

Day 21 (--/--).

Boy, did part 1 take longer than it should have. Besides struggling with
finding a correct algorithm, I didn't read the description quite well-enough.
So when I couldn't solve part 1, I spent an hour or more trying to debug why.
I finally searched for every rotation of the starting pattern in my puzzle data
only to not find it. It was then that I caught the bit about *flipping* the
patterns as well.

On the plus side, my code was able to brute-force part 2 in just over 37
seconds.

## [day22.clj](day22.clj)

Day 22 (--/--).

This day's puzzles were based on infinite grids. The simulation was that of a
virus infecting nodes in a compute grid.

Part 1 was simple, just move around the grid according to the given rules and
toggle the infected/clean state. For the answer, you were to report the number
of grid points that were explicitly toggled to infected (not counting any that
started out that way).

Part 2 introduced two new states for the grid points, and had you run the
simulation for 10,000,000 iterations (rather than the previous 10,000). In
this case, the technique used to represent the data and handle the move/turn
steps was easily extended to cover 4 states instead of 2.

This does show a need for some generalization of the grid/directions/movement
code into `utils.clj`.

## [day23.clj](day23.clj)

Day 23 (--/--).

Another assembly-simulation puzzle. Part 1 just wanted to count how many `mul`
instructions were executed. Part 2, however, calls for changing one register
from 0 to 1 at the start before running the program and reading the value of
a different register at the end. Needless to say, actually running the code
would have taken far far too long. Some reading of reddit threads and
examination showed that the code was looping between two values by a step-size
of 17 and counting all non-prime numbers in that range. Most answers on reddit
did a simple loop and focused on as optimal of a prime-tester as they could
manage. Since my `utils.clj` module has a primes lazy-sequence generator, I
took the opposite approach: I found all primes between the two boundary numbers
and converted them to a `set` structure. I then looped over the range and
counted numbers that were *not* in the set. Thus, primality wasn't really
tested since the generator function produces the primes without testing every
number in the range.

## [day24.clj](day24.clj)

Day 24 (--/--).

This puzzle was based on building sequences of "compoents" to create a bridge
over a gap. Laying out sequences was based on the "connector" values on each
end of a given component.

For both parts I did an interative BFS algorithm using the
`clojure.lang.PersistentQueue` class. I recorded the "strength" of each valid
combination (and for part 2, the length of the bridge as well). It worked for
both parts, with correct answers on the first try for each. But it wasn't very
speedy-- each run took between 4:50 and 5:00 minutes to run.

## [day25.clj](day25.clj)

Day 25 (--/--).

This day involved writing a simple Turing Machine simulator and running it an
inordinate number of steps. I *could* have finished this a lot faster if I had
just hard-coded the machine based on reading the input. But instead I chose to
have it actually parse out the parameters of the machine from the text.

Other than that, it wasn't that much different from any other virtual
machine-like puzzle.

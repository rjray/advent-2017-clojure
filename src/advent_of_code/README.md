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

## [day13.clj](day13.clj)

Day 13 (--/--).

## [day14.clj](day14.clj)

Day 14 (--/--).

## [day15.clj](day15.clj)

Day 15 (--/--).

## [day16.clj](day16.clj)

Day 16 (--/--).

## [day17.clj](day17.clj)

Day 17 (--/--).

## [day18.clj](day18.clj)

Day 18 (--/--).

## [day19.clj](day19.clj)

Day 19 (--/--).

## [day20.clj](day20.clj)

Day 20 (--/--).

## [day21.clj](day21.clj)

Day 21 (--/--).

## [day22.clj](day22.clj)

Day 22 (--/--).

## [day23.clj](day23.clj)

Day 23 (--/--).

## [day24.clj](day24.clj)

Day 24 (--/--).

## [day25.clj](day25.clj)

Day 25 (--/--).

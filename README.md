# scalac-chess-problem
This prue functional scala application found all unique chess board where no one piece don't threaten to any another peaces.

For run use `sbt -mem 4096 run`

Example output:
```
Solving the problem: 7×7 board with 2 Kings, 2 Queens, 2 Bishops and 1 Knight
Ok, done.
Elapsed time 0 min, 6 sec, 372 millis (total 6372 millis)
I have 3063828 solutions. No more 3 of them follow: 
♘**-***
--*--**
-*****♗
*-*****
*****♕*
**♕****
♗***♔*♔

-**♘***
-**--**
--****♗
*-*****
*****♕*
**♕****
♗***♔*♔

-**-***
♘-*--**
--****♗
*******
*****♕*
**♕****
♗***♔*♔
```

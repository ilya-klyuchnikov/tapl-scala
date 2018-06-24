# TAPL in Scala

This project is an attempt to port very nice companion code (written by Pierce in OCaml)
for the book "Types and Programming Languages" by Benjamin C. Pierce into Scala.

Live demo: http://ilya-klyuchnikov.github.io/tapl-scala/

## Roadmap

01. `tapl.arith` - (chapters 3, 4)
02. `tapl.untyped` (chapters 5 - 7)
03. `tapl.fulluntyped` (chapters 5 - 7)
04. `tapl.tyarith` (chapter 8)
05. `tapl.simplebool` (chapter 10)
06. `tapl.fullsimple` (chapter 9 and 11)
07. `tapl.fullref` (chapter 13, 18, based on `tapl.fullsimple`)
08. `tapl.fullerror` (chapter 14, based on `tapl.simplebool`)
09. `tapl.rcdsubbot` (chapter 15, rcd=record)
10. `tapl.fullsub` (chapters 15 - 17, based on `tapl.fullsimple`) (The only addition is Top type)
11. `tapl.joinsub` (chapter 16) - Not yet done. This is a subset of `tapl.bot`.
12. `tapl.bot` (chapter 16) - simplification of `tapl.rcdsubbot`.
13. `tapl.fullequirec` (chapter 20).
14. `tapl.fullisorec` (chapter 20).
15. `tapl.equirec` (chapter 21). The subtyping was not implemented in original code. But there is a code in the Book.
16. `tapl.recon` (chapter 22).
17. `tapl.fullrecon` (chapter 22).
18. `tapl.fullpoly` (chapters 23, 24).
19. `tapl.fullomega` (chapters 23, 29, 30).
20. `tapl.fullfsub` (chapters 26, 28) - based on `tapl.fullsimple` (additions: Top type and subtyping).
21. `tapl.fullfomsub` (chapters 26, 31).
22. `tapl.fullfsubref` (chapter 27) - original (a bit wrong) implementation. Not yet ported.
23. `tapl.fullfomsubref` (chapter 27) combination of all systems from the book.
24. `tapl.purefsub` (chapter 28) - a subset of `tapl.fullfsub`. Not yet ported.
25. `tapl.fomsub` (chapter  31) - simplification of `tapl.fullfomsub`. Not yet ported.
26. `tapl.fullupdate` (chapter 32) - simplification of `tapl.fullfomsub`. Not yet ported.
27. `tapl.fomega` (solution to 30.3.20) - not yet ported.
28. `tapl.letexercise` - not yet ported.
29. `tapl.joinexercise` - not yet ported.

## Code structure.

The code structure for each implementation follows original code structure and consists of 4 files:

* `syntax.scala` - AST, contexts, commands, pretty-printing.
* `parser.scala` - parsing.
* `core.scala` - evaluator and typer.
* `demo.scala` - the main application for processing input files.

## Start playing with me

	sbt
	> taplScalaJVM/run

	Multiple main classes detected, select one to run:

     [1] tapl.ArithDemo
     [2] tapl.BotDemo
     [3] tapl.EquirecDemo
     [4] tapl.FullEquirecDemo
     [5] tapl.FullErrorDemo
     [6] tapl.FullFSubDemo
     [7] tapl.FullFomSubDemo
     [8] tapl.FullFomSubRefDemo
     [9] tapl.FullIsorecDemo
     [10] tapl.FullOmegaDemo
     [11] tapl.FullPolyDemo
     [12] tapl.FullReconDemo
     [13] tapl.FullRefDemo
     [14] tapl.FullSimpleDemo
     [15] tapl.FullSubDemo
     [16] tapl.FullUntypedDemo
     [17] tapl.RcdSubBotDemo
     [18] tapl.ReconDemo
     [19] tapl.SimpleBoolDemo
     [20] tapl.TyArithDemo
     [21] tapl.UntypedDemo
	Enter number: 2

	[info] Running tapl.BotDemo
	====================
	(lambda x: Top. x): Top -> Top;
	||
	\/
	(lambda x: Top. x): Top -> Top;
	====================
	((lambda x: Top. x) (lambda x: Top. x)): Top;
	||
	\/
	(lambda x: Top. x): Top -> Top;
	====================
	((lambda x: Top -> Top. x) (lambda x: Top. x)): Top -> Top;
	||
	\/
	(lambda x: Top. x): Top -> Top;
	====================
	(lambda x: Bot. x): Bot -> Bot;
	||
	\/
	(lambda x: Bot. x): Bot -> Bot;
	====================
	(lambda x: Bot. x x): Bot -> Bot;
	||
	\/
	(lambda x: Bot. x x): Bot -> Bot;

	> taplScalaJVM/runMain tapl.FullUntypedDemo progs/fulluntyped.tapl
	[info] Running tapl.FullUntypedDemo progs/fulluntyped.tapl
	====================
	tru;
	||
	\/
	(lambda t. lambda f. t);
	====================
	fls;
	||
	\/
	(lambda t. lambda f. f);
	====================
	(tru tru);
	||
	\/
	(lambda f. lambda t. lambda f'. t);
	====================
	(tru fls);
	||
	\/
	(lambda f. lambda t. lambda f'. f');
	====================
	(test tru v w);
	||
	\/
	((lambda m. lambda n. (lambda t. lambda f. t) m n) v w);
	...


## Notes.

There was some incompleteness in the original OCaml code that *was* ported into Scala code:

1. `tapl.equirec` - subtyping was not implemented
2. For subtyping for References, Sources and Sinks (scattered across many implementations) there is a comment in the original code that implementation is incomplete.
See code for calculation of `meet` and `join` of two references.
3. It will be interesting to add implementation of dependent types from the sequel book "Advanced Topics in Types and Programming Languages".
The book mentions OCaml implementation `deptypes` but I did not find this implementation on the web.
4. I have noticed that some cases (in typers and evaluators) were omitted in the original code.

## Building

- JVM Scala: `sbt compile`
- JS: `sbt fastOptJS`

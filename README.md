# TAPL in Scala

This project is an attempt to port very nice companion code (written by Pierce in OCaml) 
for the book "Types and Programming Languages" by Benjamin C. Pierce into Scala.

Almost all implementations are ported. Not yet ported implementations will be ported soon. Here is the status:

01. `tapl.arith` - (chapter 3, 4)
02. `tapl.untyped` (5 - 7)
03. `tapl.fulluntyped` (Chapters 5 - 7)
04. `tapl.tyarith` (Chapter 8)
05. `tapl.simplebool` (Chapter 10)
06. `tapl.fullsimple` (Chapter 9 and 11)
07. `tapl.fullref` (Chapter 13, 18, based on `tapl.fullsimple`)
08. `tapl.fullerror` (Chapter 14, based on `tapl.simplebool`)
09. `tapl.rcdsubbot` (Chapter 15, rcd=record)
10. `tapl.fullsub` (Chapters 15 - 17, based on `tapl.fullsimple`) (The only addition is Top type)
11. `tapl.joinsub` (Chapter 16) - Not yet done. This is a subset of `tapl.bot`.
12. `tapl.bot` (Chapter 16) - simplification of `tapl.rcdsubbot`.
13. `tapl.fullequirec` (Chapter 20).
14. `tapl.fullisorec` (Chapter 20).
15. `tapl.equirec` (Chapter 21). The subtyping was not implemented in original code. But there is a code in the Book.
16. `tapl.recon` (Chapter 22).
17. `tapl.fullrecon` (Chapter 22).
18. `tapl.fullpoly` (Chapters 23, 24).
19. `tapl.fullomega` (Chapters 23, 29, 30).
20. `tapl.fullfsub` (Chapters 26, 28) - based on `tapl.fullsimple` (additions: Top type and subtyping).
21. `tapl.fullfomsub` (Chapters 26, 31).
22. `tapl.fullfsubref` (Chapter 27) - original (a bit wrong) implementation. Not yet ported.
23. `tapl.fullfomsubref` (Chapter 27) combination of all systems from the book.
24. `tapl.purefsub` (Chapter 28) - a subset of `tapl.fullfsub`. Not yet ported.
25. `tapl.fomsub` (Chapter  31) - simplification of `tapl.fullfomsub`. Not yet ported.
26. `tapl.fullupdate` (Chapter 32) - simplification of `tapl.fullfomsub`. Not yet ported.
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
	> run
	
	Multiple main classes detected, select one to run:
	
	 [1] tapl.fullisorec.FullIsorecDemo
	 [2] tapl.rcdsubbot.RcdSubBotDemo
	 [3] tapl.arith.ArithDemo
	 [4] tapl.simplebool.SimpleBoolDemo
	 [5] tapl.equirec.EquirecDemo
	 [6] tapl.tyarith.TyArithDemo
	 [7] tapl.untyped.UntypedDemo
	 [8] tapl.fullfsub.FullFSubDemo
	 [9] tapl.fullpoly.FullPolyDemo
	 [10] tapl.recon.ReconDemo
	 [11] tapl.fullfomsub.FullFomSubDemo
	 [12] tapl.fullerror.FullErrorDemo
	 [13] tapl.fulluntyped.UntypedDemo
	 [14] tapl.fullsub.FullSubDemo
	 [15] tapl.fullequirec.FullEquirecDemo
	 [16] tapl.fullrecon.FullReconDemo
	 [17] tapl.fullref.FullRefDemo
	 [18] tapl.fullomega.FullOmegaDemo
	 [19] tapl.fullsimple.FullSimpleDemo
	 [20] tapl.fullfomsubref.FullFomSubRefDemo
	 [21] tapl.bot.BotDemo
	 
	Enter number: 21
	
	[info] Running tapl.bot.BotDemo 
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
	
	> run-main tapl.fulluntyped.UntypedDemo progs/fulluntyped.tapl
	[info] Running tapl.fulluntyped.UntypedDemo progs/fulluntyped.tapl
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

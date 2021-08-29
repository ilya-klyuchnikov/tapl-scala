# TAPL in Scala

## Roadmap

1. `tapl.arith` - (chapters 3, 4).
2. `tapl.untyped` (chapters 5-7).
3. `tapl.fulluntyped` (chapters 5-7).
4. `tapl.tyarith` (chapter 8).
5. `tapl.simplebool` (chapter 10).
6. `tapl.fullsimple` (chapter 9 and 11).
7. `tapl.fullref` (chapter 13, 18)
8. `tapl.fullerror` (chapter 14)
9. `tapl.rcdsubbot` (chapter 15)
10. `tapl.fullsub` (chapters 15-17)
11. `tapl.bot` (chapter 16)
12. `tapl.fullequirec` (chapter 20).
13. `tapl.fullisorec` (chapter 20).
14. `tapl.equirec` (chapter 21).
15. `tapl.recon` (chapter 22).
16. `tapl.fullrecon` (chapter 22).
17. `tapl.fullpoly` (chapters 23, 24).
18. `tapl.fullomega` (chapters 23, 29, 30).
19. `tapl.fullfsub` (chapters 26, 28).
20. `tapl.fullfomsub` (chapters 26, 31).
21. `tapl.fullfomsubref` (chapter 27) combination of all systems from the book.

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

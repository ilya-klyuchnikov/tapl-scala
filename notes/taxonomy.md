# Taxonomy of interpreters

## Simple + Subtyping (01-12)

* arith (01)
	* tyarith (04)
* untyped (02)
	* fulluntyped (03)
* simplebool (05)
	* fullsimple (06)
		* fullsub (10) (+TyTop) + subtyping
			* fullref(07) (+ TyRef, TySource, TySource, TySink, TyTop, TyBot), subtyping, meet, join 
	* fullerror (08) (+ Try, Error, Bot) + subtype
	* rcdsubbot(09) (+ Record, + Top, + Bottom)
	* bot (12) (+Top, +Bot, +Subtype, -Bool, -If)
		* (11) joinsub (+ Boolean, + If, + Bool => join/meet?) - simplification of fullsub

## Recursive types	

* FullPoly (System F)
	* FullSub
		* ++ 131231
	* FullOmega (F<sub>ω</sub>)
		+ ++ Kind
		+ ++ Ref
		+ ++ TyApp, TyAbs
	* 
		


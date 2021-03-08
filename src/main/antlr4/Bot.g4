grammar Bot;

program
    : (cmds+=command ';')+ EOF
    ;

command
    : LCID ':' typ
    # CommandBind
    | term
    # CommandEval
    ;

typ
    : ty=arrowType
    # Ty1
    ;

aType
    : '(' ty=typ ')'
    # TyParens
    | 'Bot'
    # TyBot
    | 'Top'
    # TyTop
    ;

arrowType
    : t1=aType '->' t2=arrowType
    # TyArrow
    | aType
    # Ty2
    ;

term
    : t=appTerm
    # Tm1
    | 'lambda' v=(LCID | '_') ':' ty=typ '.' t=term
    # TmAbs
    ;

appTerm
    : t1=appTerm t2=aTerm
    # TmApp
    | t=aTerm
    # TmAtomic
    ;

aTerm
    : '(' t=term ')'
    # TmParens
    | id=LCID
    # TmVar
    ;

NUM
    : [0-9]+
    ;

LCID
    : [a-z][a-z,A-Z]*
    ;

UCID
    : [A-Z][a-z,A-Z]*
    ;

WS
    : [ \t\r\n]+ -> skip
    ;

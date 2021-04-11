grammar Arith;

program
    : (cmds+=command ';')+ EOF
    ;

command
    : t=term
    # CommandEval
    ;

term
    : t=aTerm
    # TmAtomic
    | 'succ' t=aTerm
    # TmSucc
    | 'pred' t=aTerm
    # TmPred
    | 'iszero' t=aTerm
    # TmIszero
    | 'if' cond=term 'then' t1=term 'else' t2=term
    # TmIf
    ;

aTerm
    : '(' inner=term ')'
    # TmParens
    | 'true'
    # TmTrue
    | 'false'
    # TmFalse
    | num=NUM
    # TmNum
    ;

NUM
    : [0-9]+
    ;

ID
    : [a-z]+
    ;

WS
    : ([ \t\r\n]+ | '//' ~[\r\n]*) -> skip
    ;

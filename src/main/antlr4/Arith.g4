grammar Arith;

program
    : (command ';')+ EOF
    ;

command
    : term
    ;

term
    : appTerm
    | 'if' term 'then' term 'else' term
    ;

appTerm
    : aTerm
    | 'succ' aTerm
    | 'pred' aTerm
    | 'iszero' aTerm
    ;

aTerm
    : '(' term ')'
    | 'true'
    | 'false'
    | NUM
    ;

NUM
    : [0-9]+
    ;

ID
    : [a-z]+
    ;

WS
    : [ \t\r\n]+ -> skip
    ;

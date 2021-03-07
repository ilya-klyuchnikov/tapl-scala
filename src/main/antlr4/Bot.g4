grammar Bot;

program
    : (command ';')+ EOF
    ;

command
    : LCID ':' typ
    | term
    ;

typ
    : arrowType
    ;

aType
    : '(' typ ')'
    | 'Bot'
    | 'Top'
    ;

arrowType
    : aType '->' arrowType
    | aType
    ;

term
    : appTerm
    | 'lambda' LCID ':' typ '.' term
    | 'lambda' '_' ':' typ '.' term
    ;

appTerm
    : appTerm aTerm
    | aTerm
    ;

aTerm
    : '(' term ')'
    | LCID
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

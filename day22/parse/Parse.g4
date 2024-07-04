

grammar Parse; // Define a grammar called Parse

// r is root node
// end

r : line (line)* end ;

line : nodex INT nodey INT size used avail percent;

nodex : '/dev/grid/node-x' ;
nodey : '-y' ;
size  : INT 'T' ;
used  : INT 'T' ;
avail  : INT 'T' ;
percent  : INT '%' ;
        
// end of input marker
end : 'end' ;

// an integer
INT : [0-9]+ ;

// a lowercase letter
LETTER : [a-z] ;

// skip whitespac tabs newlines returns
WS : [ \t\r\n]+ -> skip ; 	


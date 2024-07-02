
grammar Parse; // Define a grammar called Parse

// r is root node
// added keyword end to end of input to FORCE getting a complete parse
// incomplete parse will flag up as end keyword will not be parsed since stopped halfway through input
// clever !

r : statement (statement)* end ;
statement : (swapxyEx | swapletterEx | rotposEx | rotleftEx | rotrightEx | moveEx | reverseEx ) ;
swapxyEx : swap position INT with position INT ; 
swapletterEx :  swap letter LETTER with letter LETTER ;
rotposEx : rotate based on position of letter LETTER ; 
rotleftEx : rotate left INT step ; 
rotrightEx : rotate right INT step ;
reverseEx : reverse position INT through INT ;


// move more flexible if whitespace between words ?
moveEx : move position INT to position INT ;

// words themselves 
based : 'based';
rotate : 'rotate' ;
right : 'right' ;
left : 'left' ;
move : 'move' ;
position : 'positions' | 'position' ;
through: 'through' ;
to : 'to' ;
on : 'on' ;
of : 'of' ;
letter : 'letter' ;
step : 'steps' | 'step' ;
swap : 'swap' ;
with : 'with' ;
end : 'end' ;
reverse : 'reverse' ;


// an integer
INT : [0-9]+ ;

// a lowercase letter
LETTER : [a-z] ;

// skip whitespac tabs newlines returns
WS : [ \t\r\n]+ -> skip ; 	


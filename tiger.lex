type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
digits=[0-9]+;

%%
type => (Tokens.TYPE(yypos,yypos+4));
var => (Tokens.VAR(yypos,yypos+3));
function => (Tokens.FUNCTION(yypos,yypos+8));
break => (Tokens.BREAK(yypos,yypos+4));
of => (Tokens.OF(yypos,yypos+2));
end => (Tokens.OF(yypos,yypos+3));
in => (Tokens.IN(yypos,yypos+2));
nil => (Tokens.NIL(yypos,yypos+3));
let => (Tokens.LET(yypos,yypos+3));
end => (Tokens.END(yypos,yypos+3));
:= => (Tokens.ASSIGN(yypos,yypos+2));
= => (Tokens.EQ(yypos,yypos+1));
: => (Tokens.COLON(yypos,yypos+1));
,	=> (Tokens.COMMA(yypos,yypos+1));
\".*\" => (Tokens.STRING(String.substring(yytext,1,size yytext - 2),yypos,yypos+size yytext));
{digits} => (case Int.fromString(yytext) of
              SOME i => (Tokens.INT(i,yypos,yypos+size yytext))
            | NONE => (ErrorMsg.error yypos ("Expected int, found: " ^ yytext); continue()));
[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext,yypos,yypos+size yytext));

(" "|"\t") => (linePos := yypos :: !linePos; continue());
\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());


type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val stringstart = ref 0
val charlist = ref (nil: char list)
val comLevel = ref 0

fun inc x = x := !x + 1
fun dec x = x := !x - 1

fun addString (s: char) = charlist := s :: (!charlist)
fun makeString () = (implode(rev(!charlist)) before charlist := nil)

fun eof () = let val pos = hd(!linePos)
              in 
                if !comLevel>0 
                  then ErrorMsg.error pos ("unclosed comment")
                else ();
               Tokens.EOF(pos,pos) 
            end

fun nl yypos = let in lineNum := !lineNum+1; linePos := yypos :: !linePos end

fun makeInt s = foldl (fn (c,a) => a*10 + ord c - ord #"0") 0 (explode s)

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s STRING COMMENT;
digits=[0-9]+;

%%
<INITIAL> \" => (charlist := nil; stringstart := yypos; YYBEGIN STRING; continue());
<STRING> \\n => (addString(#"\n"); continue());
<STRING> \\t => (addString(#"\t"); continue());
<STRING> \\\^c => (addString(#"\^C"); continue());
<STRING> \\\" => (addString(#"\""); continue());
<STRING> \\\ => (addString(#"\\"); continue());
<STRING> \\[\n\t ]*\\ => (continue());
<STRING> \\[0-9]{3} => (case Char.fromString(yytext) of
                          SOME c => addString(c)
                        | NONE => (ErrorMsg.error yypos ("illegal ASCII code " ^ yytext));
                        continue());
<STRING> \\. => (ErrorMsg.error yypos ("illegal escape character" ^ yytext);
                  continue());
<STRING> \" => (YYBEGIN INITIAL;
                  let val str = makeString()
                  in Tokens.STRING(str,!stringstart,!stringstart+size str)
                  end);
<STRING> . => (case Char.fromString(yytext) of
                SOME c => addString(c)
              | NONE => (ErrorMsg.error yypos ("illegal character " ^ yytext));
              continue());
<STRING> \n => (ErrorMsg.error yypos ("illegal newline, must be escaped"); continue());

<INITIAL> "/*" => (inc comLevel; YYBEGIN COMMENT; continue());
<COMMENT> "/*" => (inc comLevel; continue());
<COMMENT> "*/" => (dec comLevel; if !comLevel = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());
<COMMENT> \n => (nl yypos; continue());

<INITIAL> type => (Tokens.TYPE(yypos,yypos+4));
<INITIAL> var => (Tokens.VAR(yypos,yypos+3));
<INITIAL> function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> break => (Tokens.BREAK(yypos,yypos+5));
<INITIAL> of => (Tokens.OF(yypos,yypos+2));
<INITIAL> end => (Tokens.END(yypos,yypos+3));
<INITIAL> in => (Tokens.IN(yypos,yypos+2));
<INITIAL> nil => (Tokens.NIL(yypos,yypos+3));
<INITIAL> let => (Tokens.LET(yypos,yypos+3));
<INITIAL> do => (Tokens.DO(yypos,yypos+2));
<INITIAL> to => (Tokens.TO(yypos,yypos+2));
<INITIAL> for => (Tokens.FOR(yypos,yypos+3));
<INITIAL> while => (Tokens.WHILE(yypos,yypos+5));
<INITIAL> else => (Tokens.ELSE(yypos,yypos+4));
<INITIAL> then => (Tokens.THEN(yypos,yypos+4));
<INITIAL> if => (Tokens.IF(yypos,yypos+2));
<INITIAL> array => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL> ":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL> \| => (Tokens.OR(yypos,yypos+1));
<INITIAL> \& => (Tokens.AND(yypos,yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL> \> => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL> \< => (Tokens.LT(yypos,yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL> \= => (Tokens.EQ(yypos,yypos+1));
<INITIAL> \/ => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> \* => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> \- => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> \+ => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> \. => (Tokens.DOT(yypos,yypos+1));
<INITIAL> \] => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> \{ => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> \} => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> \[ => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> \) => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> \( => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> \; => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> \: => (Tokens.COLON(yypos,yypos+1));
<INITIAL> \, => (Tokens.COMMA(yypos,yypos+1));

<INITIAL> {digits} => (Tokens.INT(makeInt yytext
              handle Overflow => (ErrorMsg.error yypos ("Integer overflow " ^ yytext); 1),
              yypos,yypos+size yytext));

<INITIAL> [a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext,yypos,yypos+size yytext));

<INITIAL> (" "|"\t") => (continue());
<INITIAL> \n => (nl yypos; continue());

<INITIAL> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());


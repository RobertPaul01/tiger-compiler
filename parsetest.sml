structure Parse : sig val parse : string -> unit end =
struct
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
	   absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

  fun invoke lexstream =
    let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut,
                          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in TigerP.parse(0,lexstream,print_error,())
    end

  (*fun calc filename =
    let
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      val lexer = TigerP.makeLexer get
      val dummyEOF = TigerLrVals.Tokens.EOF(0,0)
      val dummySEMI = TigerLrVals.Tokens.SEMICOLON(0,0)
      fun loop lexer =
          let val (result,lexer) = invoke lexer
              val (nextToken,lexer) = TigerP.Stream.get lexer
           in case result
                of SOME r =>
                    TextIO.output(TextIO.stdOut,
                           "result = " ^ (Int.toString r) ^ "\n")
                 | NONE => ();
              if TigerP.sameToken(nextToken,dummyEOF) then ()
              else loop lexer
          end
     in loop lexer
    end*)
end


Clear
Cd c:\desarrollo\mygarbage\vfp\mprlexer\src\
lexer = Createobject("MPRLexer")
lexer.lDumpTokens = .t.
lcTokenFile = "c:\desarrollo\mygarbage\vfp\mprlexer\LEXEMES.log"
If File(lcTokenFile)
	Delete File &lcTokenFile
EndIf
lexer.cTokenFile = lcTokenFile
lexer.ScanFile("c:\desarrollo\mygarbage\vfp\mprlexer\MENU1.mpr")

lnSec = Seconds()
lexer.NextToken()
Do While lexer.Token.Code != 0
	lexer.ToString()
	lexer.NextToken()
EndDo
?Seconds() - lnSec

lexer = .Null.
Release lexer
If File(lcTokenFile)
	Modify File (lcTokenFile)
EndIf
&& ======================================================================== &&
&& MPRLexer class
&& ======================================================================== &&
Define Class MPRLexer As Session
	#Define CR 				Chr(13)
	#Define LF 				Chr(10)
	#Define EOF_CHAR 		Chr(255)
	#Define _SPACE			Space(1)
	#Define _Tab			Chr(9)
	#Define _SIGLE_COMMENT	'&'
	#Define _STAR_COMMENT	'*'
	#Define _SINGLE_QUOTE	"'"
	#Define _DOUBLE_QUOTE	'"'
	#Define _LEFT_BRACKET	"["
	#Define _RIGHT_BRACKET	"]"

	Hidden Reader
	Hidden Keyword
	Token = 0
	Hidden TokenCode
	Hidden clManager
	Hidden nColumnNumber
	Hidden nLineNumber
	Hidden cLook
	lDumpTokens = .F.
	cTokenFile = ""
	FoxLib = .Null.
	DataSession = 2
&& ======================================================================== &&
&& Function Init
&& ======================================================================== &&
	Function Init
		Set Procedure To "FoxLibManager" Additive
		With This
			.FoxLib = Createobject("FoxLibManager")
			With .FoxLib
				.AddClass("StreamReader")
				.AddClass("Keyword")

				.AddProcedure("StreamReader")
				.AddProcedure("KeyWord")
				.AddProcedure("EnumType")

				.LoadProcedures()
			Endwith
			.Reader  = Createobject("StreamReader")
			.Keyword = Createobject("Keyword")
			lcEnum = "tEndOfStream = 0,"
			lcEnum = lcEnum + .Keyword.GetTokenList()
			lcEnum = lcEnum + ",tLeftCurleyBracket"
			lcEnum = lcEnum + ",tRightCurleyBracket"
			lcEnum = lcEnum + ",tLeftParenthesis"
			lcEnum = lcEnum + ",tRightParenthesis"
			lcEnum = lcEnum + ",tTrue"
			lcEnum = lcEnum + ",tFalse"
			lcEnum = lcEnum + ",tIdentifier"
			lcEnum = lcEnum + ",tPoint"
			lcEnum = lcEnum + ",tEqual"
			lcEnum = lcEnum + ",tExactlyEqual"
			lcEnum = lcEnum + ",tNotEqual"
			lcEnum = lcEnum + ",tMoreThan"
			lcEnum = lcEnum + ",tMoreThanOrEqualTo"
			lcEnum = lcEnum + ",tLessThan"
			lcEnum = lcEnum + ",tLessThanOrEqualTo"
			lcEnum = lcEnum + ",tPlus"
			lcEnum = lcEnum + ",tMinus"
			lcEnum = lcEnum + ",tMult"
			lcEnum = lcEnum + ",tDivide"
			lcEnum = lcEnum + ",tPercent"
			lcEnum = lcEnum + ",tExponent"
			lcEnum = lcEnum + ",tComparison"
			.TokenCode = Enum(lcEnum)
			.Token = Createobject("Empty")
			=AddProperty(.Token, "LineNumber", 0)
			=AddProperty(.Token, "ColumnNumber", 0)
			=AddProperty(.Token, "Code", 0)
			=AddProperty(.Token, "Value", 0)			
		Endwith
	EndFunc
&& ======================================================================== &&
&& Function StartScanner
&& ======================================================================== &&
	Function StartScanner
		With This
			.nColumnNumber = 0
			.nLineNumber = 0
			.cLook = .NextChar()
		Endwith
	Endfunc
&& ======================================================================== &&
&& Function NextToken
&& Obtiene el siguiente Token válido desde el buffer de entrada.
&& ======================================================================== &&
	Function NextToken
		This.SkipBlanksAndComments()
		Do Case
		Case This.IsAlphaNum(This.cLook)
			This.Getword()
		Case Inlist(This.cLook, _SINGLE_QUOTE, _DOUBLE_QUOTE, _LEFT_BRACKET)
			This.GetString()
		Case Isdigit(This.cLook) or InList(This.cLook, "-", ".")
			This.GetNum()
		Case This.cLook = EOF_CHAR
			This.Token.Code = 0
			This.Token.Value = 0
		Otherwise
			This.GetSpecial()
		Endcase
	Endfunc
&& ======================================================================== &&
&& Function GetWord
&& ======================================================================== &&
	Function Getword As Void
		With This
			Local lnTokenCode As Integer
			lnTokenCode = 0
			.Token.Value = ""
			Do While .IsAlphaNum(.cLook)
				.Token.Value = .Token.Value + .cLook
				.cLook = .NextChar()
			Enddo
			.Keyword.Iskeyword(.Token.Value, @lnTokenCode)
			.Token.Code = Iif(!Empty(lnTokenCode), lnTokenCode, .TokenCode.tIdentifier)
		Endwith
	Endfunc
&& ======================================================================== &&
&& Function GetString
&& ======================================================================== &&
	Function GetString As Void
		Local lcSkipChar As Character
		With This
			lcSkipChar = .cLook
			If .cLook = _LEFT_BRACKET
				lcSkipChar = _RIGHT_BRACKET
			Endif
			.Match(This.cLook)
			.Token.Code = .TokenCode.tString
			.Token.Value = ""
			Do While .cLook != lcSkipChar
				.Token.Value = .Token.Value + .cLook
				.cLook = .NextChar()
			Enddo
			.Match(lcSkipChar)
		Endwith
	Endfunc
&& ======================================================================== &&
&& Function GetNum
&& ======================================================================== &&
	Function GetNum
		Local lnScale As Number, lnDigit As Integer, lnSign As Integer, lnDecAct As Integer
		This.Token.Code = This.TokenCode.tInteger
		lnDecAct = Set("Decimals")
		lnSign   = 1
		Set Decimals To 0
		If This.cLook = "-"
			This.Match("-")
			lnSign = -1
		EndIf
		This.Token.Value = 0
		If This.cLook != "."
			Do While Isdigit(This.cLook)
				lnDigit = Val(This.cLook)
				This.Token.Value = This.Token.Value * 10 + lnDigit
				This.cLook = This.NextChar()
			Enddo
		Endif
		If This.cLook = "."
			This.Token.Code = This.TokenCode.tFloat
			This.Match(".")
			lnScale = 1
			Do While Isdigit(This.cLook)
				lnScale = lnScale * 0.1
				lnDigit = Val(This.cLook)
				This.Token.Value = This.Token.Value + lnScale * lnDigit
				This.cLook = This.NextChar()
			Enddo
		EndIf
		This.Token.Value = This.Token.Value * lnSign
		Set Decimals To &lnDecAct
	Endfunc
&& ======================================================================== &&
&& Function GetSpecial
&& ======================================================================== &&
	Function GetSpecial As Void
		Do Case
		Case This.cLook = "{"
			This.Token.Code = This.TokenCode.tLeftCurleyBracket
		Case This.cLook = "}"
			This.Token.Code = This.TokenCode.tRightCurleyBracket
		Case This.cLook = "("
			This.Token.Code = This.TokenCode.tLeftParenthesis
		Case This.cLook = ")"
			This.Token.Code = This.TokenCode.tRightParenthesis
		Case This.cLook = ","
			This.Token.Code = This.TokenCode.tComma
		Case This.cLook = "."
			This.Token.Code = This.TokenCode.tPoint			
		Case This.cLook = ";"
			If This.Reader.Peek() = CR
				This.cLook = This.NextChar()
				This.NextToken()
				Return
			Endif
		Case This.cLook = "="
			If This.Reader.Peek() = "="
				This.cLook = This.NextChar() && eat '='
				This.Token.Code = This.TokenCode.tExactlyEqual
			Else
				This.Token.Code = This.TokenCode.tEqual
			EndIf
		Case This.cLook = "<"
			Do case
			case This.Reader.Peek() = ">"
				This.cLook = This.NextChar() && eat '<'
				This.Token.Code = This.TokenCode.tNotEqual
			Case This.Reader.Peek() = "="
				This.cLook = This.NextChar() && eat '='
				This.Token.Code = This.TokenCode.tLessThanOrEqualTo
			Otherwise
				This.Token.Code = This.TokenCode.tLessThan
			endcase
		Case This.cLook = ">"
			If This.Reader.Peek() = "="
				This.cLook = This.NextChar() && eat '='
				This.Token.Code = This.TokenCode.tMoreThanOrEqualTo
			else
				This.Token.Code = This.TokenCode.tMoreThan
			EndIf
		Case This.cLook = "!"
			If This.Reader.Peek() = "="
				This.cLook = This.NextChar() && eat '='
				This.Token.Code = This.TokenCode.tNotEqual
			Else
				This.Token.Code = This.TokenCode.tNot
			EndIf
		Case This.cLook = "#"
			This.Token.Code = This.TokenCode.tNotEqual
		Case This.cLook = "+"
			This.Token.Code = This.TokenCode.tPlus
		Case This.cLook = "-"
			This.Token.Code = This.TokenCode.tMinus
		Case This.cLook = "^"
			This.Token.Code = This.TokenCode.tExponent
		Case This.cLook = "*"
			If This.Reader.Peek() = "*"
				This.cLook = This.NextChar() && eat '*'
				This.Token.Code = This.TokenCode.tExponent
			else
				This.Token.Code = This.TokenCode.tMult
			EndIf
		Case This.cLook = "/"
			This.Token.Code = This.TokenCode.tDivide
		Case This.cLook = "%"
			This.Token.Code = This.TokenCode.tPercent
		Case This.cLook = "$"
			This.Token.Code = This.TokenCode.tComparison
		Otherwise
			Error "unknown character '" + This.cLook + "' ASCII code '" + Transform(Asc(This.cLook)) + "'"
		Endcase
		This.cLook = This.NextChar()
	Endfunc
&& ======================================================================== &&
&& Function Match
&& ======================================================================== &&
	Function Match As Void
		Lparameters tcMatched As Character
		If This.cLook == tcMatched
			This.cLook = This.NextChar()
		Else
			This.Expected(tcMatched)
		Endif
	Endfunc
&& ======================================================================== &&
&& Function Expected
&& ======================================================================== &&
	Function Expected As Void
		Lparameters tcMatched As Character
		Error tcMatched + ' expected.'
	Endfunc
&& ======================================================================== &&
&& Function IsAlphaNum
&& ======================================================================== &&
	Function IsAlphaNum As Boolean
		Lparameters tcChar As Character
		Return Between(Asc(tcChar), Asc("A"), Asc("Z")) Or ;
			Between(Asc(tcChar), Asc("a"), Asc("z")) Or ;
			Between(Asc(tcChar), Asc("0"), Asc("9")) Or tcChar = "_"
	Endfunc
&& ======================================================================== &&
&& Function SkipBlanksAndComments
&& ======================================================================== &&
	Function SkipBlanksAndComments As Void
		Do While Inlist(This.cLook, _SPACE, _Tab, _SIGLE_COMMENT, _STAR_COMMENT) and !this.Reader.EndOfStream
			If Inlist(This.cLook, _SPACE, _Tab)
				This.cLook = This.NextChar()
			Else
				If This.Reader.Peek() = _SIGLE_COMMENT
					This.SkipSingleComment()
				Else
					nBackPos = 1
					lcChar = This.Reader.LookBehind(nBackPos)
					Do while InList(lcChar, _STAR_COMMENT, _SPACE, _TAB) and lcChar != EOF_CHAR
						nBackPos = nBackPos + 1
						lcChar = This.Reader.LookBehind(nBackPos)
					EndDo
					If InList(lcChar, LF) or lcChar = EOF_CHAR
						This.SkipSingleComment()
					EndIf
					nPos = This.Reader.GetPosition()
				Endif
			Endif
		EndDo
	Endfunc
&& ======================================================================== &&
&& Function SkipSingleComment
&& ======================================================================== &&
	Function SkipSingleComment
		Do While This.cLook != LF And !This.Reader.EndOfStream
			This.cLook = This.GetChar()
		EndDo
		If This.cLook = LF
			This.nLineNumber = This.nLineNumber + 1
			this.cLook = This.NextChar()
		EndIf
	Endfunc
&& ======================================================================== &&
&& Function NextChar
&& Devuelve el siguiente caracter válido excluyendo CR y LF
&& ======================================================================== &&
	Function NextChar As Character
		Local lcChar
		lcChar = This.GetChar()
		If lcChar = LF
			lcChar = _SPACE
			This.nLineNumber = This.nLineNumber + 1
		Endif
		Return lcChar
	Endfunc
&& ======================================================================== &&
&& Function GetChar
&& Devuelve el siguiente caracter válido excluyendo CR pero incluyendo LF.
&& ======================================================================== &&
	Function GetChar As Character
		Local lcChar As Character
		lcChar = This.ReadFromInput()
		If Inlist(lcChar, CR, LF)
			If lcChar = CR
				lcChar = This.ReadFromInput()
				If lcChar != LF
					Error "Expecting Line Feed character"
				Endif
			Endif
		Endif
		Return lcChar
	Endfunc
&& ======================================================================== &&
&& Function ReadFromInput
&& Lee el siguiente caracter (válido o inválido) desde el buffer de entrada.
&& ======================================================================== &&
	Function ReadFromInput As Character
		Local lcChar As Character
		lcChar = EOF_CHAR
		If !This.Reader.EndOfStream
			lcChar = This.Reader.Read()
			This.nColumnNumber = This.nColumnNumber + 1
		Endif
		Return lcChar
	Endfunc
&& ======================================================================== &&
&& Function ScanString
&& ======================================================================== &&
	Function ScanString(tcString As Memo)
		This.Reader.SetString(tcString)
		This.StartScanner()
	Endfunc
&& ======================================================================== &&
&& Function ScanFile
&& ======================================================================== &&
	Function ScanFile(tcFileName As String)
		This.ScanString(Filetostr(tcFileName))
	Endfunc
&& ======================================================================== &&
&& Function ToString
&& ======================================================================== &&
	Function ToString As String	
		Local lcTokenStr As String
		lcTokenStr = ""
		Do Case
		Case This.Token.Code = This.TokenCode.tString
			lcTokenStr = "string: <'" + This.Token.Value + "'>"
		Case This.Token.Code = This.TokenCode.tInteger
			lcTokenStr = "integer: <'" + Transform(This.Token.Value) + "'>"
		Case This.Token.Code = This.TokenCode.tFloat
			lcTokenStr = "float: <'" + Transform(This.Token.Value) + "'>"
		Case This.Token.Code = This.TokenCode.tTrue
			lcTokenStr = "boolean: <'.T.'>"
		Case This.Token.Code = This.TokenCode.tFalse
			lcTokenStr = "boolean: <'.F.'>"
		Case This.Token.Code = This.TokenCode.tIdentifier
			lcTokenStr = "identifier: <'" + This.Token.Value + "'>"
		Case This.Token.Code = This.TokenCode.tPoint
			lcTokenStr = "special: <'.'>"
		Case This.Token.Code = This.TokenCode.tComma
			lcTokenStr = "special: <','>"
		Case This.Token.Code = This.TokenCode.tLeftParenthesis
			lcTokenStr = "special: <'('>"
		Case This.Token.Code = This.TokenCode.tRightParenthesis
			lcTokenStr = "special: <')'>"
		Case This.Token.Code = This.TokenCode.tEqual
			lcTokenStr = "special: <'='>"
		Case This.Token.Code = This.TokenCode.tExactlyEqual
			lcTokenStr = "special: <'=='>"
		Case This.Token.Code = This.TokenCode.tNotEqual
			lcTokenStr = "special: <'" + This.Token.Value + "'>"
		Case This.Token.Code = This.TokenCode.tLessThan
			lcTokenStr = "special: <'<'>"
		Case This.Token.Code = This.TokenCode.tLessThanOrEqualTo
			lcTokenStr = "special: <'<='>"
		Case This.Token.Code = This.TokenCode.tMoreThan
			lcTokenStr = "special: <'>'>"
		Case This.Token.Code = This.TokenCode.tMoreThanOrEqualTo
			lcTokenStr = "special: <'>='>"
		Case This.Token.Code = This.TokenCode.tNot
			lcTokenStr = "special: <'!'>"
		Case This.Token.Code = This.TokenCode.tEndOfStream
			lcTokenStr = "special: <'EOF'>"
		Case This.Token.Code = This.TokenCode.tPlus
			lcTokenStr = "special: <'+'>"
		Case This.Token.Code = This.TokenCode.tMinus
			lcTokenStr = "special: <'-'>"
		Case This.Token.Code = This.TokenCode.tExponent
			lcTokenStr = "special: <'^'>"
		Case This.Token.Code = This.TokenCode.tMult
			lcTokenStr = "special: <'*'>"
		Case This.Token.Code = This.TokenCode.tDivide
			lcTokenStr = "special: <'/'>"
		Case This.Token.Code = This.TokenCode.tPercent
			lcTokenStr = "special: <'%'>"
		Case This.Token.Code = This.TokenCode.tComparison
			lcTokenStr = "special: <'$'>"
		Otherwise
			lcTokenStr = This.Keyword.ToString(This.Token.Code)
		Endcase
		If Empty(lcTokenStr)
			lcTokenStr = "unknown token " + Alltrim(Str(This.Token.Code))
		EndIf
		If This.lDumpTokens And !Empty(This.cTokenFile)
			=StrToFile(lcTokenStr + CR + LF, This.cTokenFile, 1)
		EndIf
		Return lcTokenStr
	Endfunc
&& ======================================================================== &&
&& Function Destroy
&& ======================================================================== &&
	Function Destroy
		With This
			.Reader  = .Null.
			.Keyword = .Null.
			Try
				.clManager.ReleaseAll()
			Catch
			Endtry
		Endwith
	Endfunc
Enddefine
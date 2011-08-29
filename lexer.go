package main

import (
	"io"
	"os"
	"fmt"
	"bufio"
	"bytes"
	"unicode"
	"strconv"
)

//-------------------------------------------------------------------------------
// Token
//-------------------------------------------------------------------------------

type Token int

const (
	EOF = iota

	IDENT      // identifier
	INT        // integer literal
	FLOAT      // floating point literal
	STRING     // string literal
	RAW_STRING // raw string literal (e.g. `hello`)

	COMMENT_L  // line comment (C++)
	COMMENT_ML // multiline comment (C)

	// OPERATORS
	ADD        // +
	SUB        // -
	MUL        // *
	DIV        // /
	REM        // %

	ASSIGN     // =
	GT         // >
	LT         // <

	LEN        // #
	LSB        // [
	RSB        // ]

	// big group of operators with the second '=' rune
	ADD_A      // +=
	SUB_A      // -=
	MUL_A      // *=
	DIV_A      // /=
	REM_A      // %=
	EQ         // ==
	NEQ        // !=
	GE         // >=
	LE         // <=

	SHIFT_R    // >>
	SHIFT_L    // <<
	SHIFT_R_A  // >>=
	SHIFT_L_A  // <<=

	DOT        // .
	DOTDOT     // ..
	DOTDOTDOT  // ...

)

var tokenStrings = [...]string{
	EOF: "EOF",

	IDENT: "IDENT",
	INT: "INT",
	FLOAT: "FLOAT",
	STRING: "STRING",
	RAW_STRING: "RAW_STRING",
	COMMENT_L: "COMMENT_L",
	COMMENT_ML: "COMMENT_ML",

	ADD: "ADD",
	SUB: "SUB",
	MUL: "MUL",
	DIV: "DIV",
	REM: "REM",

	ASSIGN: "ASSIGN",
	GT: "GT",
	LT: "LT",

	LEN: "LEN",
	LSB: "LSB",
	RSB: "RSB",

	ADD_A: "ADD_A",
	SUB_A: "SUB_A",
	MUL_A: "MUL_A",
	DIV_A: "DIV_A",
	REM_A: "REM_A",
	EQ: "EQ",
	NEQ: "NEQ",
	GE: "GE",
	LE: "LE",

	SHIFT_R: "SHIFT_R",
	SHIFT_L: "SHIFT_L",
	SHIFT_R_A: "SHIFT_R_A",
	SHIFT_L_A: "SHIFT_L_A",

	DOT: "DOT",
	DOTDOT: "DOTDOT",
	DOTDOTDOT: "DOTDOTDOT",
}

func (t Token) String() (s string) {
	if 0 <= t && t < Token(len(tokenStrings)) {
		s = tokenStrings[t]
	}
	if s == "" {
		s = fmt.Sprintf("token(%d)", int(t))
	}
	return
}

type Error string
func (e Error) String() string { return string(e) }

//-------------------------------------------------------------------------------
// Tokenizer
//-------------------------------------------------------------------------------

type tokenInfo struct {
	tok Token
	line int
	col int
	lit string
}

type Tokenizer struct {
	r *bufio.Reader
	b bytes.Buffer
	line int
	col int
	prevCol int
	lastRune int

	deferToken tokenInfo
}

func NewTokenizer(r io.Reader) *Tokenizer {
	t := new(Tokenizer)
	t.r = bufio.NewReader(r)
	t.line = 1
	t.col = 0
	t.prevCol = -1
	t.lastRune = -1
	return t
}

// Little helper, panics on error
func panicIfFailed(error os.Error) {
	if error != nil {
		panic(error)
	}
}

// Another helper, if error is not an EOF, panic!
func panicOnNonEOF(error os.Error) {
	if error != os.EOF {
		panic(error)
	}
}

// Check if 'lit' is a keyword, return it as an identifier otherwise
func identOrKeyword(line, col int, lit string) (Token, int, int, string) {
	return IDENT, line, col, lit
}

// Beginning of an identifier
func isIdentifier1st(rune int) bool {
	return unicode.IsLetter(rune) || rune == '_'
}

// Body of an identifier
func isIdentifier(rune int) bool {
	return unicode.IsLetter(rune) || unicode.IsDigit(rune) || rune == '_'
}

// Hex digit
func isHexDigit(rune int) bool {
	return unicode.IsDigit(rune) ||
		(rune >= 'a' && rune <= 'f') ||
		(rune >= 'A' && rune <= 'F')
}

// Set a deferred token
func (t *Tokenizer) setDeferToken(tok Token, line, col int, lit string) {
	t.deferToken = tokenInfo{tok, line, col, lit}
}

// Read the next rune and automatically increment column and line if necessary
func (t *Tokenizer) readRune() (int, os.Error) {
	rune, _, err := t.r.ReadRune()
	if err != nil {
		return rune, err
	}

	t.lastRune = rune
	t.prevCol = t.col

	t.col++
	if rune == '\n' {
		t.line++
		t.col = 0
	}

	return rune, nil
}

// Unread rune, manage line and column as well
func (t *Tokenizer) unreadRune() {
	if t.prevCol == -1 || t.lastRune == -1 {
		// DEBUG, disable for speed
		panic("more than one unreadRune, without corresponding readRune")
	}

	// previous restore column
	t.col = t.prevCol

	// restore line if necessary
	if t.lastRune == '\n' {
		t.line--
	}

	t.prevCol, t.lastRune = -1, -1
	panicIfFailed(t.r.UnreadRune())
}

// Return temporary buffer contents and reset the buffer
func (t *Tokenizer) flushBuffer() string {
	s := t.b.String()
	t.b.Reset()
	return s
}

// Checks if buffer contains '0' and only '0' (for hex numbers)
func (t *Tokenizer) bufHasOnly0() bool {
	return t.b.Len() == 1 && t.b.Bytes()[0] == '0'
}

// Matches two possible variants: '1' or '12'
func (t *Tokenizer) match2(tok1 Token, lit1 string, rune2 int, tok2 Token, lit2 string) (Token, int, int, string) {
	line, col := t.line, t.col

	rune, err := t.readRune()
	if err != nil {
		panicOnNonEOF(err)
		return tok1, line, col, lit1
	}

	if rune != rune2 {
		t.unreadRune()
		return tok1, line, col, lit1
	}

	return tok2, line, col, lit2
}

// Shortcut for '+=', '-=', etc. tokens (second rune is '=')
func (t *Tokenizer) match2eq(tok1 Token, lit1 string, tok2 Token, lit2 string) (Token, int, int, string) {
	return t.match2(tok1, lit1, '=', tok2, lit2)
}

// Matches three possible variants: '1' or '12' or '123'
func (t *Tokenizer) match3(tok1 Token, lit1 string,
			   rune2 int, tok2 Token, lit2 string,
			   rune3 int, tok3 Token, lit3 string) (Token, int, int, string) {
	line, col := t.line, t.col

	// try second
	rune, err := t.readRune()
	if err != nil {
		panicOnNonEOF(err)
		return tok1, line, col, lit1
	}

	if rune != rune2 {
		t.unreadRune()
		return tok1, line, col, lit1
	}

	// try third
	rune, err = t.readRune()
	if err != nil {
		panicOnNonEOF(err)
		return tok2, line, col, lit2
	}

	if rune != rune3 {
		t.unreadRune()
		return tok2, line, col, lit2
	}

	return tok3, line, col, lit3
}

func hexPairToByte(rune1 int, rune2 int) byte {
	// TODO: this one is slowpoke probably
	s := string([]byte{byte(rune1), byte(rune2)})
	ui, err := strconv.Btoui64(s, 16)
	if err != nil {
		panic(err)
	}
	return byte(ui)
}

func (t *Tokenizer) readHexRuneInString(line, col int) int {
	rune, err := t.readRune()
	if err != nil {
		panicOnNonEOF(err)
		s := fmt.Sprintf("Bad hex escape sequence in string at: %d:%d", line, col)
		panic(Error(s))
	}

	if !isHexDigit(rune) {
		s := fmt.Sprintf("Bad hex escape sequence in string at: %d:%d", line, col)
		panic(Error(s))
	}

	return rune
}

// Process escape characters and put a processed token into the temporary
// buffer. Panics on error
func (t *Tokenizer) escape(line, col int) {
	rune, err := t.readRune()
	if err != nil {
		panicOnNonEOF(err)
		s := fmt.Sprintf("Incomplete string at: %d:%d", line, col)
		panic(Error(s))
	}

	switch rune {
	case 'a':
		t.b.WriteRune('\a')
	case 'b':
		t.b.WriteRune('\b')
	case 'f':
		t.b.WriteRune('\f')
	case 'n':
		t.b.WriteRune('\n')
	case 'r':
		t.b.WriteRune('\r')
	case 't':
		t.b.WriteRune('\t')
	case 'v':
		t.b.WriteRune('\v')
	case '\\':
		t.b.WriteRune('\\')
	case '"':
		t.b.WriteRune('"')
	case '\'':
		t.b.WriteRune('\'')
	case 'x':
		// yuck, but whatever
		rune1 := t.readHexRuneInString(line, col)
		rune2 := t.readHexRuneInString(line, col)
		t.b.WriteByte(hexPairToByte(rune1, rune2))
	}
}

// Scan wrapper, we catch all the panics here
func (t *Tokenizer) Next() (tok Token, line, col int, lit string, err os.Error) {
	defer func() {
		if e := recover(); e != nil {
			err = e.(os.Error)
		}
	}()
	tok, line, col, lit = t.next()
	return
}

func (t *Tokenizer) lastRuneIsStar() bool {
	return t.b.Len() > 2 && t.b.Bytes()[t.b.Len()-1] == '*'
}

// Scans the stream for the next token and returns:
// - Token kind
// - Line where the beginning of the token is located
// - Column where the token begins
// - Corresponding literal (if any)
func (t *Tokenizer) next() (Token, int, int, string) {
	var rune, line, col, dline, dcol, strrune int
	var err os.Error

	if t.deferToken.line != 0 {
		dt := t.deferToken
		t.deferToken = tokenInfo{}
		return dt.tok, dt.line, dt.col, dt.lit
	}

read_more:
	rune, err = t.readRune()
	if err != nil { goto error_check }

	// big switch, starting point for every token
	switch {
	case isIdentifier1st(rune):
		goto scan_ident
	case unicode.IsDigit(rune):
		goto scan_number
	case rune == '\'':
		fallthrough
	case rune == '"':
		goto scan_string
	case rune == '`':
		goto scan_raw_string
	case rune == '+':
		return t.match2eq(ADD, "+", ADD_A, "+=")
	case rune == '-':
		return t.match2eq(SUB, "-", SUB_A, "-=")
	case rune == '*':
		return t.match2eq(MUL, "*", MUL_A, "*=")
	case rune == '/':
		// '/' or '/='
		tok, l, c, lit := t.match2eq(DIV, "/", DIV_A, "/=")
		if tok == DIV_A {
			return tok, l, c, lit
		}

		// '/' or '//'
		tok, l, c, lit = t.match2(DIV, "/",
					  '/', COMMENT_L, "//")
		if tok == COMMENT_L {
			line, col = l, c
			goto scan_comment_line
		}

		// '/' or '/*'
		tok, l, c, lit = t.match2(DIV, "/",
					  '*', COMMENT_ML, "/*")
		if tok == COMMENT_ML {
			line, col = l, c
			goto scan_comment_multiline
		}
	case rune == '%':
		return t.match2eq(REM, "%", REM_A, "%=")
	case rune == '=':
		return t.match2eq(ASSIGN, "=", EQ, "==")
	case rune == '>':
		tok, l, c, lit := t.match2eq(GT, ">", GE, ">=")
		if tok == GE {
			return tok, l, c, lit
		}
		return t.match3(GT, ">",
				'>', SHIFT_R, ">>",
				'=', SHIFT_R_A, ">>=")
	case rune == '<':
		tok, l, c, lit := t.match2eq(LT, "<", LE, "<=")
		if tok == LE {
			return tok, l, c, lit
		}
		return t.match3(LT, "<",
				'<', SHIFT_L, "<<",
				'=', SHIFT_L_A, "<<=")
	case rune == '#':
		return LEN, t.line, t.col, "#"
	case rune == '[':
		return LSB, t.line, t.col, "["
	case rune == ']':
		return RSB, t.line, t.col, "]"
	case rune == '.':
		return t.match3(DOT, ".",
				'.', DOTDOT, "..",
				'.', DOTDOTDOT, "...")
	default:
		goto read_more
	}

scan_comment_line:
	t.b.WriteString("//")
	for {
		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			return COMMENT_L, line, col, t.flushBuffer()
		}

		if rune == '\n' {
			return COMMENT_L, line, col, t.flushBuffer()
		}

		t.b.WriteRune(rune)
	}

scan_comment_multiline:
	t.b.WriteString("/*")
	for {
		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			s := fmt.Sprintf("Incomplete multiline comment at: %d:%d", line, col)
			panic(Error(s))
		}

		if rune == '/' && t.lastRuneIsStar() {
			t.b.WriteByte('/')
			return COMMENT_ML, line, col, t.flushBuffer()
		}

		t.b.WriteRune(rune)
	}

scan_raw_string:
	line, col = t.line, t.col
	for {
		t.b.WriteRune(rune)

		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			s := fmt.Sprintf("Incomplete string at: %d:%d", line, col)
			panic(Error(s))
		}

		if rune == '`' {
			t.b.WriteRune(rune)
			return RAW_STRING, line, col, t.flushBuffer()
		}
	}

scan_string:
	line, col = t.line, t.col
	strrune = rune
	t.b.WriteRune(rune)
	for {
		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			s := fmt.Sprintf("Incomplete string at: %d:%d", line, col)
			panic(Error(s))
		}

		switch rune {
		case '\\':
			t.escape(line, col)
			continue
		case '\n':
			s := fmt.Sprintf("New line in string at: %d:%d", line, col)
			panic(Error(s))
		case strrune:
			t.b.WriteRune(rune)
			return STRING, line, col, t.flushBuffer()
		}

		t.b.WriteRune(rune)
	}

scan_number:
	line, col = t.line, t.col
	for {
		t.b.WriteRune(rune)

		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			return INT, line, col, t.flushBuffer()
		}

		switch {
		case unicode.IsDigit(rune):
			continue
		case rune == '.':
			goto scan_number_fraction
		case (rune == 'x' || rune == 'X') && t.bufHasOnly0():
			goto scan_number_hex
		default:
			t.unreadRune()
			return INT, line, col, t.flushBuffer()
		}
	}

scan_number_hex:
	// write [xX]
	t.b.WriteRune(rune)

	rune, err = t.readRune()
	// '0x<EOF>' case, panic
	if err != nil {
		panicOnNonEOF(err)
		s := fmt.Sprintf("Bad hex number literal at: %d:%d", line, col)
		panic(Error(s))
	}

	if !isHexDigit(rune) {
		s := fmt.Sprintf("Bad hex number literal at: %d:%d", line, col)
		panic(Error(s))
	}

	for {
		t.b.WriteRune(rune)

		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			return INT, line, col, t.flushBuffer()
		}

		if !isHexDigit(rune) {
			t.unreadRune()
			return INT, line, col, t.flushBuffer()
		}
	}

scan_number_fraction:
	// we need to save '.' position separately, in case if it's not a
	// fraction actually, but '.' or '..' token
	dline, dcol = t.line, t.col

	// put the '.' into the buffer
	t.b.WriteRune(rune)

	rune, err = t.readRune()
	// '1231.<EOF>' case, defer DOT token and return INT
	if err != nil {
		panicOnNonEOF(err)
		t.setDeferToken(DOT, dline, dcol, ".")
		s := t.flushBuffer()
		return INT, line, col, s[:len(s)-1]
	}

	// '1231..' case, defer DOTDOT token and return INT
	if rune == '.' {
		t.setDeferToken(DOTDOT, dline, dcol, "..")
		s := t.flushBuffer()
		return INT, line, col, s[:len(s)-1]
	}

	// '1231.something' case, not a floating point number: unread rune,
	// defer DOT and return INT
	if !unicode.IsDigit(rune) {
		t.unreadRune()
		t.setDeferToken(DOT, dline, dcol, ".")
		s := t.flushBuffer()
		return INT, line, col, s[:len(s)-1]
	}

	// at this point it's a floating point number, let's parse the fraction part
	for {
		t.b.WriteRune(rune)

		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			return FLOAT, line, col, t.flushBuffer()
		}

		switch {
		case unicode.IsDigit(rune):
			continue
		case rune == 'e' || rune == 'E':
			goto scan_number_exponent
		default:
			t.unreadRune()
			return FLOAT, line, col, t.flushBuffer()
		}
	}

scan_number_exponent:
	t.b.WriteRune(rune)
	rune, err = t.readRune()
	// '123.123e<EOF>' case, panic
	if err != nil {
		panicOnNonEOF(err)
		s := fmt.Sprintf("Bad floating point literal at: %d:%d", line, col)
		panic(Error(s))
	}

	// if it's not a number, '+' or '-' after [eE], panic
	if !unicode.IsDigit(rune) && rune != '+' && rune != '-' {
		s := fmt.Sprintf("Bad floating point literal at: %d:%d", line, col)
		panic(Error(s))
	}

	if rune == '+' || rune == '-' {
		t.b.WriteRune(rune)
		rune, err = t.readRune()
		// '123.123e[-+]<EOF>' case, panic
		if err != nil {
			panicOnNonEOF(err)
			s := fmt.Sprintf("Bad floating point literal at: %d:%d", line, col)
			panic(Error(s))
		}
	}

	if !unicode.IsDigit(rune) {
		s := fmt.Sprintf("Bad floating point literal at: %d:%d", line, col)
		panic(Error(s))
	}

	// ok, we got a correct exponent part, parse it
	for {
		t.b.WriteRune(rune)

		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			return FLOAT, line, col, t.flushBuffer()
		}

		switch {
		case unicode.IsDigit(rune):
			continue
		default:
			t.unreadRune()
			return FLOAT, line, col, t.flushBuffer()
		}
	}

scan_ident:
	line, col = t.line, t.col
	for {
		t.b.WriteRune(rune)

		rune, err = t.readRune()
		if err != nil {
			panicOnNonEOF(err)
			return identOrKeyword(line, col, t.flushBuffer())
		}

		if !isIdentifier(rune) {
			t.unreadRune()
			return identOrKeyword(line, col, t.flushBuffer())
		}
	}

error_check:
	panicOnNonEOF(err)
	return EOF, 0, 0, ""
}

func main() {
	t := NewTokenizer(os.Stdin)
	for {
		tok, line, col, lit, err := t.Next()
		if err != nil {
			fmt.Println(err)
			return
		}
		if tok == EOF {
			fmt.Println(tokenStrings[tok])
			return
		}
		fmt.Printf("%s: (%d:%d) %s\n", tokenStrings[tok], line, col, lit)
	}
}

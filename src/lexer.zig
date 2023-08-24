const std = @import("std");
const token = @import("./token.zig");
const utils = @import("./utils.zig");

const testing = std.testing;
const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    const Self = @This();

    input: []const u8,
    // Position of the current character under examination.
    currentPosition: usize,
    // Next character to be examined (use to peek ahead).
    nextPosition: usize,
    // Current character under examination.
    currentChar: u8,

    pub fn new(input: []const u8) Self {
        var lexer = Self{
            .input = input,
            .currentPosition = 0,
            .nextPosition = 0,
            .currentChar = 0,
        };
        lexer.readChar();
        return lexer;
    }

    pub fn nextToken(self: *Self) Token {
        self.skipWhitespace();

        var tok: Token = switch (self.currentChar) {
            '=' => blk: {
                if (self.peekChar() == '=') {
                    const pos = self.currentPosition;
                    self.readChar();
                    break :blk Token.new(TokenType.eq, self.currentTokenLiteral(pos, 2));
                } else {
                    break :blk Token.new(TokenType.assign, self.currentCharTokenLiteral());
                }
            },
            '+' => Token.new(TokenType.plus, self.currentCharTokenLiteral()),
            '-' => Token.new(TokenType.minus, self.currentCharTokenLiteral()),
            '!' => blk: {
                if (self.peekChar() == '=') {
                    const pos = self.currentPosition;
                    self.readChar();
                    break :blk Token.new(TokenType.noteq, self.currentTokenLiteral(pos, 2));
                } else {
                    break :blk Token.new(TokenType.bang, self.currentCharTokenLiteral());
                }
            },
            '/' => Token.new(TokenType.slash, self.currentCharTokenLiteral()),
            '*' => Token.new(TokenType.asterisk, self.currentCharTokenLiteral()),
            '<' => Token.new(TokenType.lt, self.currentCharTokenLiteral()),
            '>' => Token.new(TokenType.gt, self.currentCharTokenLiteral()),
            ';' => Token.new(TokenType.semicolon, self.currentCharTokenLiteral()),
            '(' => Token.new(TokenType.lparen, self.currentCharTokenLiteral()),
            ')' => Token.new(TokenType.rparen, self.currentCharTokenLiteral()),
            ',' => Token.new(TokenType.comma, self.currentCharTokenLiteral()),
            '{' => Token.new(TokenType.lbrace, self.currentCharTokenLiteral()),
            '}' => Token.new(TokenType.rbrace, self.currentCharTokenLiteral()),
            0 => Token.new(TokenType.eof, ""),
            else => blk: {
                if (utils.isLetter(self.currentChar)) {
                    const literal = self.readIdentifier();
                    const tokenType = token.lookupIndent(literal);
                    return Token.new(tokenType, literal);
                } else if (utils.isDigit(self.currentChar)) {
                    const number = self.readNumber();
                    return Token.new(TokenType.int, number);
                }
                break :blk Token.illegal();
            },
        };

        self.readChar();
        return tok;
    }

    // A.K.A consumeChar
    fn readChar(self: *Self) void {
        if (self.nextPosition >= self.input.len) {
            self.currentChar = 0;
        } else {
            self.currentChar = self.input[self.nextPosition];
        }
        self.currentPosition = self.nextPosition;
        self.nextPosition += 1;
    }

    fn currentTokenLiteral(self: *const Self, start: usize, length: usize) []const u8 {
        return self.input[start .. start + length];
    }

    fn currentCharTokenLiteral(self: *const Self) []const u8 {
        return self.currentTokenLiteral(self.currentPosition, 1);
    }

    fn readIdentifier(self: *Self) []const u8 {
        const position = self.currentPosition;
        while (utils.isLetter(self.currentChar)) {
            self.readChar();
        }
        return self.input[position..self.currentPosition];
    }

    fn readNumber(self: *Self) []const u8 {
        const position = self.currentPosition;
        while (utils.isDigit(self.currentChar)) {
            self.readChar();
        }
        return self.input[position..self.currentPosition];
    }

    fn skipWhitespace(self: *Self) void {
        while (self.currentChar == ' ' or
            self.currentChar == '\t' or
            self.currentChar == '\n' or
            self.currentChar == '\r')
        {
            self.readChar();
        }
    }

    fn peekChar(self: *Self) u8 {
        if (self.nextPosition >= self.input.len) {
            return 0;
        } else {
            return self.input[self.nextPosition];
        }
    }
};

test "TestNextToken" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;

    const TestCase = struct { TokenType, []const u8 };
    const tests = [_]TestCase{
        .{ TokenType.let, "let" },
        .{ TokenType.ident, "five" },
        .{ TokenType.assign, "=" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.ident, "ten" },
        .{ TokenType.assign, "=" },
        .{ TokenType.int, "10" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.ident, "add" },
        .{ TokenType.assign, "=" },
        .{ TokenType.function, "fn" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.ident, "x" },
        .{ TokenType.comma, "," },
        .{ TokenType.ident, "y" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.ident, "x" },
        .{ TokenType.plus, "+" },
        .{ TokenType.ident, "y" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.ident, "result" },
        .{ TokenType.assign, "=" },
        .{ TokenType.ident, "add" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.ident, "five" },
        .{ TokenType.comma, "," },
        .{ TokenType.ident, "ten" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.bang, "!" },
        .{ TokenType.minus, "-" },
        .{ TokenType.slash, "/" },
        .{ TokenType.asterisk, "*" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.int, "5" },
        .{ TokenType.lt, "<" },
        .{ TokenType.int, "10" },
        .{ TokenType.gt, ">" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.if_, "if" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.int, "5" },
        .{ TokenType.lt, "<" },
        .{ TokenType.int, "10" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.return_, "return" },
        .{ TokenType.true_, "true" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.else_, "else" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.return_, "return" },
        .{ TokenType.false_, "false" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.int, "10" },
        .{ TokenType.eq, "==" },
        .{ TokenType.int, "10" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.int, "10" },
        .{ TokenType.noteq, "!=" },
        .{ TokenType.int, "9" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.eof, "" },
    };

    var lexer = Lexer.new(input);
    for (tests) |testCase| {
        const tok = lexer.nextToken();

        //std.debug.print("{s} {s}\n", .{testCase[1], tok.literal});
        try testing.expectEqual(testCase[0], tok.tokenType);
        try testing.expectEqualStrings(testCase[1], tok.literal);
    }
}

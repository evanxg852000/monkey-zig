const std = @import("std");
const ComptimeStringMap = std.ComptimeStringMap;

pub const TokenType = enum(u32) {
    illegal = 0,
    eof,

    // Indentifiers + literals
    ident,
    int,

    // Operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    eq,
    noteq,
    lt, // <
    gt, // >

    // Delimiters
    comma,
    semicolon,

    lparen,
    rparen,
    lbrace,
    rbrace,

    // Keywords
    function,
    let,
    true_,
    false_,
    if_,
    else_,
    return_,
};

pub const Token = struct {
    const Self = @This();

    tokenType: TokenType,
    literal: []const u8,

    pub fn new(tokenType: TokenType, tokenLiteral: []const u8) Self {
        return Self{
            .tokenType = tokenType,
            .literal = tokenLiteral,
        };
    }

    pub fn illegal() Self {
        return Self{
            .tokenType = TokenType.illegal,
            .literal = "",
        };
    }
};

const KV = struct { []const u8, TokenType };
const keywordsMap = ComptimeStringMap(TokenType, [_]KV{
    .{ "fn", TokenType.function },
    .{ "let", TokenType.let },
    .{ "true", TokenType.true_ },
    .{ "false", TokenType.false_ },
    .{ "if", TokenType.if_ },
    .{ "else", TokenType.else_ },
    .{ "return", TokenType.return_ },
});

pub fn lookupIndent(keyword: []const u8) TokenType {
    if (keywordsMap.get(keyword)) |tokenType| {
        return tokenType;
    }
    return TokenType.ident;
}

################
#####IMPORTS####
################

from strings_with_arrows import string_with_arrows

import string

from datetime import datetime as __dt

from sys import stdout, stderr

#################
####CONSTANTS####
#################

DIGITS = "0123456789"
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS
VER_INFO = {
    "version": "v0.0.3",
    "stage": "PreAlpha",
    "release_date": __dt(2024, 7, 10)
}


#################
#####ERRORS######
#################


class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.error_name = error_name
        self.details = details
        self.pos_start = pos_start
        self.pos_end = pos_end

    def as_string(self):
        result = f"{self.error_name}: {self.details}"
        result += (
            f"\nFile {self.pos_start.fn}, line "
            f"{self.pos_start.ln + 1}, col "
            f"{self.pos_end.col}"
        )
        result += '\n' + string_with_arrows(
            self.pos_start.ftxt, self.pos_start, self.pos_end
        )
        return result


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(
            pos_start,
            pos_end,
            "Illegal Character",
            details
        )


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(
            pos_start,
            pos_end,
            "Illegal Syntax",
            details
        )


class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(
            pos_start,
            pos_end,
            "Expected character",
            details
        )


class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(
            pos_start,
            pos_end,
            "Runtime Error",
            details
        )
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f"{self.error_name}: {self.details}"
        result += '\n' + string_with_arrows(
            self.pos_start.ftxt, self.pos_start, self.pos_end
        )

        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result += (
                    f"\nFile {pos.fn}, line "
                    f"{pos.ln + 1} in "
                    f"{ctx.display_name}\n" +
                    result
            )
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return f"Traceback (most recent call last):{result}"


#################
#####POSITION####
#################


class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == "\n":
            self.ln += 1
            self.col = 1

        return self

    def copy(self):
        return Position(
            self.idx, self.ln, self.col,
            self.fn, self.ftxt
        )

    def __repr__(self):
        return f"{self.idx}:({self.ln},{self.col})"


#################
#####TOKENS######
#################

TT_INT        = "INT"
TT_FLOAT      = "FLOAT"
TT_STRING     = "STRING"
TT_IDENTIFIER = "IDENTIFIER"
TT_KEYWORD    = "KEYWORD"
TT_PLUS       = "PLUS"
TT_MINUS      = "MINUS"
TT_ASTERISK   = "ASTERISK"
TT_SLASH      = "SLASH"
TT_POW        = "POW"
TT_TILDE      = "TILDE"
TT_AMPER      = "AMPER"
TT_BAR        = "BAR"
TT_CARET      = "CARET"
TT_EQ         = "EQ"
TT_ATTR       = "ATTR"
TT_LPAREN     = "LPAREN"
TT_RPAREN     = "RPAREN"
TT_LSQUARE    = "LSQUARE"
TT_RSQUARE    = "RSQUARE"
TT_LBRACE     = "LBRACE"
TT_RBRACE     = "RBRACE"
TT_EE         = "EE"
TT_NE         = "NE"
TT_LT         = "LT"
TT_GT         = "GT"
TT_LTE        = "LTE"
TT_GTE        = "GTE"
TT_COMMA      = "COMMA"
TT_ARROW      = "ARROW"
TT_NEWLINE    = "NEWLINE"
TT_EOF        = "EOF"

KW_TABLE = {
    "var_declare": "var",
    "logic_and": "and",
    "logic_or": "or",
    "logic_not": "not",
    "cond_once": "if",
    "then": "then",
    "cond_sec": "elif",
    "cond_last": "else",
    "loops_for": "for",
    "ranges_to": "to",
    "ranges_step": "step",
    "cond_loop": "while",
    "func_def": "func",
    "block_end": "end",
}
KEYWORDS = KW_TABLE.values()


class Token:
    def __init__(
            self, type_, value=None,
            pos_start=None, pos_end=None
    ):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_start.advance()

        if pos_end:
            self.pos_end = pos_end.copy()

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value is not None:
            return f"{self.type}:{self.value}"
        return f"{self.type}"


#################
######LEXER######
#################


class Lexer:
    def __init__(self, fn, text):
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = (
            self.text[self.pos.idx]
            if self.pos.idx < len(self.text)
            else None
        )

    def make_tokens(self):
        tokens = []

        while self.current_char is not None:
            if self.current_char in " \t":
                self.advance()
            elif self.current_char in ";\n":
                tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
                self.advance()
            elif self.current_char in DIGITS + ".":
                tokens.append(self.make_number())
            elif self.current_char in LETTERS + "_":
                tokens.append(self.make_identifier())
            elif self.current_char == "\"" or self.current_char == "'":
                tokens.append(self.make_string(quotes=self.current_char))
            elif self.current_char == "+":
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == "-":
                tokens.append(self.make_minus_or_arrow())
            elif self.current_char == "*":
                tokens.append(self.make_multiply_or_exponent())
            elif self.current_char == "/":
                tokens.append(Token(TT_SLASH, pos_start=self.pos))
                self.advance()
            elif self.current_char == "(":
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ")":
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == "[":
                tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == "]":
                tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == "!":
                tok, error = self.make_not_equals()
                if error:
                    return [], error
                tokens.append(tok)
            elif self.current_char == "=":
                tokens.append(self.make_equals())
            elif self.current_char == "<":
                tokens.append(self.make_less_than())
            elif self.current_char == ">":
                tokens.append(self.make_greater_than())
            elif self.current_char == ",":
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()
            elif self.current_char == ":":
                self.advance()
                tokens.append(self.make_identifier(attr=True))
            elif self.current_char == "{":
                tokens.append(Token(TT_LBRACE, pos_start=self.pos))
                self.advance()
            elif self.current_char == "}":
                tokens.append(Token(TT_RBRACE, pos_start=self.pos))
                self.advance()
            elif self.current_char == "|":
                tokens.append(Token(TT_BAR, pos_start=self.pos))
                self.advance()
            elif self.current_char == "&":
                tokens.append(Token(TT_AMPER, pos_start=self.pos))
                self.advance()
            elif self.current_char == "^":
                tokens.append(Token(TT_CARET, pos_start=self.pos))
                self.advance()
            elif self.current_char == "~":
                tokens.append(Token(TT_TILDE, pos_start=self.pos))
                self.advance()

            else:
                char = self.current_char
                self.advance()
                pos_start = self.pos.copy()

                return [], IllegalCharError(
                    pos_start,
                    self.pos,
                    f"'{char}'"
                )

        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None

    def make_number(self):
        pos_start = self.pos.copy()
        num_str = ""
        dot_count = 0

        while (
                self.current_char is not None and
                self.current_char in DIGITS + "."
        ):
            if self.current_char == ".":
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += "."
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            if num_str == ".":
                num_str = "0"
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

    def make_string(self, quotes="\""):
        string = ""
        pos_start = self.pos.copy()
        escape_character = False
        tok_type = TT_STRING
        self.advance()

        escape_characters = {
            "n": "\n",
            "t": "\t",
            "r": "\r",
        }

        while self.current_char is not None and (self.current_char != quotes or escape_character):
            if escape_character:
                string += escape_characters.get(self.current_char, self.current_char)
                escape_character = False
            else:
                if self.current_char == '\\':
                    escape_character = True
                else:
                    string += self.current_char
            self.advance()
        escape_character = False

        self.advance()
        return Token(tok_type, string, pos_start, self.pos)

    def make_identifier(self, attr=False):
        id_str = ""
        pos_start = self.pos.copy()

        while (
                self.current_char is not None and
                self.current_char in LETTERS_DIGITS + "_"
        ):
            id_str += self.current_char
            self.advance()

        tok_type = (
            TT_KEYWORD if id_str in KEYWORDS
            else TT_ATTR if attr
            else TT_IDENTIFIER
        )

        return Token(tok_type, id_str, pos_start, self.pos)

    def make_minus_or_arrow(self):
        tok_type = TT_MINUS
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == ">":
            self.advance()
            tok_type = TT_ARROW

        return Token(
            tok_type,
            pos_start=pos_start,
            pos_end=self.pos
        )

    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == "=":
            self.advance()
            return Token(
                TT_NE,
                pos_start=pos_start,
                pos_end=self.pos,
            ), None

        self.advance()
        return None, ExpectedCharError(
            pos_start,
            self.pos,
            "'=' after '!'",

        )

    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == "=":
            self.advance()
            tok_type = TT_EE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == "=":
            self.advance()
            tok_type = TT_LTE

        return Token(
            tok_type,
            pos_start=pos_start,
            pos_end=self.pos
        )

    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == "=":
            self.advance()
            tok_type = TT_GTE

        return Token(
            tok_type,
            pos_start=pos_start,
            pos_end=self.pos
        )

    def make_multiply_or_exponent(self):
        tok_type = TT_ASTERISK
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == "*":
            self.advance()
            tok_type = TT_POW

        return Token(
            tok_type,
            pos_start=pos_start,
            pos_end=self.pos
        )


#################
######NODES######
#################


class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = tok.pos_start
        self.pos_end = tok.pos_end

    def __repr__(self):
        return f"{self.tok}"


class StringNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = tok.pos_start
        self.pos_end = tok.pos_end

    def __repr__(self):
        return f"{self.tok.type}:{self.tok.value!r}"


class ListNode:
    def __init__(self, element_nodes, pos_start, pos_end):
        self.element_nodes = element_nodes
        self.pos_start = pos_start
        self.pos_end = pos_end


class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = var_name_tok.pos_start
        self.pos_end = var_name_tok.pos_end

    def __repr__(self):
        return f"{self.var_name_tok}"


class AttrNode:
    def __init__(self, parent_tok, attr_name_tok):
        self.parent_node = parent_tok
        self.pos_start = parent_tok.pos_start
        self.pos_end = attr_name_tok.pos_end
        self.attr_name_tok = attr_name_tok

    def __repr__(self):
        return f"({self.parent_node}).{self.attr_name_tok.value}"


class VarAssignNode:
    def __init__(self, var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.value_node.pos_end


class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = left_node.pos_start
        self.pos_end = right_node.pos_end

    def __repr__(self):
        return (
            f"({self.left_node}, {self.op_tok},"
            f"{self.right_node})"
        )


class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = op_tok.pos_start
        self.pos_end = node.pos_end

    def __repr__(self):
        return f"({self.op_tok} {self.node})"


class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (
            self.else_case or self.cases[-1]
        )[0].pos_end



class ForNode:
    def __init__(
            self,
            var_name_tok,
            start_value_node,
            end_value_node,
            step_value_node,
            body_node,
            should_return_null,
    ):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node
        self.should_return_null = should_return_null

        self.pos_start = var_name_tok.pos_start
        self.pos_end = body_node.pos_start


class WhileNode:
    def __init__(self, cond_node, body_node, should_return_null):
        self.condition_node = cond_node
        self.body_node = body_node
        self.should_return_null = should_return_null

        self.pos_start = cond_node.pos_start
        self.pos_end = body_node.pos_end


class FuncDefNode:
    def __init__(
        self, var_name_tok, arg_name_toks, body_node,
        should_return_null
    ):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_node
        self.should_return_null = should_return_null

        if self.var_name_tok:
            self.pos_start = self.var_name_tok.pos_start
        elif len(self.arg_name_toks) > 0:
            self.pos_start = self.arg_name_toks[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start

        self.pos_end = self.body_node.pos_end


class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.pos_start = self.node_to_call.pos_start

        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end

    def __repr__(self):
        arg_nodes = [str(i) for i in self.arg_nodes]
        return f"{self.node_to_call}({", ".join(arg_nodes)})"


class SubscriptNode:
    def __init__(self, node_to_subscript, subscript_node):
        self.pos_start = node_to_subscript.pos_start
        self.pos_end = subscript_node.pos_end
        self.node_to_subscript = node_to_subscript
        self.subscript_node = subscript_node


class SuperscriptNode:
    def __init__(self, parent_node, superscript_node):
        self.pos_start = parent_node.pos_start
        self.pos_end = superscript_node.pos_end
        self.parent_node = parent_node
        self.superscript_node = superscript_node

    def __repr__(self):
        return f"{self.parent_node}" + "{" + f"{self.superscript_node}" + "}"


#################
##PARSE RESULT###
#################


class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.last_registered_advance_count = 0
        self.advance_count = 0
        self.to_reverse_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.last_registered_advance_count = res.advance_count
        self.advance_count += res.advance_count
        if res.error:
            self.error = res.error
        return res.node

    def try_register(self, res):
        if res.error:
            self.to_reverse_count = res.advance_count
            return None
        return res.register(res)

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self


#################
#####PARSER######
#################


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.current_tok = None
        self.advance()

    def advance(self):
        self.tok_idx += 1
        self.update_current_tok()
        return self.current_tok

    def reverse(self, amount=1):
        self.tok_idx -= amount
        self.update_current_tok()
        return self.current_tok

    def update_current_tok(self):
        if 0 <= self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]

    def parse(self):
        result = self.statements()
        if not result.error and self.current_tok.type != TT_EOF:
            pos_start = self.current_tok.pos_start.copy()

            return result.failure(InvalidSyntaxError(
                pos_start, self.current_tok.pos_end,
                "Expected int, float, '+', '-', '*', '/', '**', '==', '>'"
                ", '<', '<=', '>=', 'and' or 'or'"
            ))
        return result

    ###################################

    def power(self):
        return self.bin_op(self.call, (TT_POW,), self.factor)

    def call(self):
        res = ParseResult()
        subscript = res.register(self.subscript())
        if res.error:
            return res

        if self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []

            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ')', 'var', 'if', 'for', 'while ', 'func', "
                        "int, float, identifier, '+', '-', '(', '[' or 'not'"
                    ))

                while self.current_tok.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()

                    arg_nodes.append(res.register(self.expr()))
                    if res.error:
                        return res

                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        f"Expected ',' or ')'"
                    ))

                res.register_advancement()
                self.advance()

            return res.success(CallNode(subscript, arg_nodes))
        return res.success(subscript)

    def subscript(self):
        res = ParseResult()
        superscript = res.register(self.superscript())
        if res.error:
            return res

        if self.current_tok.type == TT_LSQUARE:
            res.register_advancement()
            self.advance()
            subscript_node = None

            subscript_node = (res.register(self.expr()))
            if res.error:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')', 'var', 'if', 'for', 'while ', 'func', "
                    "int, float, identifier, '+', '-', '(' or 'not'"
                ))

            if self.current_tok.type != TT_RSQUARE:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected ']'"
                ))

            res.register_advancement()
            self.advance()

            return res.success(SubscriptNode(superscript, subscript_node))
        return res.success(superscript)

    def superscript(self):
        res = ParseResult()
        attribute = res.register(self.attribute())
        if res.error:
            return res

        if self.current_tok.type == TT_LBRACE:
            res.register_advancement()
            self.advance()
            subscript_node = None

            subscript_node = (res.register(self.expr()))
            if res.error:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')', 'var', 'if', 'for', 'while ', 'func', "
                    "int, float, identifier, '+', '-', '(', '{', '[' or 'not'"
                ))

            if self.current_tok.type != TT_RBRACE:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected '" + "}'"
                ))

            res.register_advancement()
            self.advance()

            return res.success(SuperscriptNode(attribute, subscript_node))
        return res.success(attribute)

    def attribute(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error: return res

        if self.current_tok.type == TT_ATTR:
            attr = self.current_tok
            self.advance()
            return res.success(AttrNode(atom, attr))
        return res.success(atom)

    def if_expr(self):
        res = ParseResult()
        all_cases = res.register(self.if_expr_cases(KW_TABLE["cond_once"]))
        if res.error:
            return res
        cases, else_case = all_cases

        return res.success(IfNode(cases, else_case))

    def if_expr_b(self):
        return self.if_expr_cases(KW_TABLE["cond_sec"])

    def if_expr_c(self):
        res = ParseResult()
        else_case = None

        if self.current_tok.matches(TT_KEYWORD, KW_TABLE["cond_last"]):
            res.register_advancement()
            self.advance()

            if self.current_tok.type == TT_NEWLINE:
                res.register_advancement()
                self.advance()

                statements = res.register(self.statements())
                if res.error: return res
                else_case = (statements, True)

                if self.current_tok.matches(TT_KEYWORD, KW_TABLE["block_end"]):
                    res.register_advancement()
                    self.advance()
                else:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected 'end'"
                    ))
            else:
                expr = res.register(self.expr())
                if res.error: return res
                else_case = (expr, False)

        return res.success(else_case)

    def if_expr_b_or_c(self):
        res = ParseResult()
        cases, else_case = [], None

        if self.current_tok.matches(TT_KEYWORD, KW_TABLE["cond_sec"]):
            all_cases = res.register(self.if_expr_b())
            if res.error: return res
            cases, else_case = all_cases
        else:
            else_case = res.register(self.if_expr_c())
            if res.error: return res
    
        return res.success((cases, else_case))

    def if_expr_cases(self, case_keyword):
        res = ParseResult()
        cases = []
        else_case = None

        if not self.current_tok.matches(TT_KEYWORD, case_keyword):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected '{case_keyword}'"
            ))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["then"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'then'"
            ))

        self.advance()
        res.register_advancement()

        if self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

            statements = res.register(self.statements())
            if res.error:
                return res
            cases.append((condition, statements, True))

            if self.current_tok.matches(TT_KEYWORD, KW_TABLE["block_end"]):
                res.register_advancement()
                self.advance()
            else:
                all_cases = res.register(self.if_expr_b_or_c())
                if res.error: return res
                new_cases, else_case = all_cases
                cases.extend(new_cases)
        else:
            expr = res.register(self.expr())
            if res.error: return res
            cases.append((condition, expr, False))
            all_cases = res.register(self.if_expr_b_or_c())
            if res.error: return res
            new_cases, else_case = all_cases
        return res.success((cases, else_case))

    def for_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["loops_for"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'for'"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected identifier"
            ))

        var_name = self.current_tok
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_EQ:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected '='"
            ))

        res.register_advancement()
        self.advance()

        start_value = res.register(self.expr())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["ranges_to"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'to'"
            ))

        res.register_advancement()
        self.advance()

        end_value = res.register(self.expr())
        if res.error:
            return res

        if self.current_tok.matches(TT_KEYWORD, KW_TABLE["ranges_step"]):
            res.register_advancement()
            self.advance()

            step_value = res.register(self.expr())
            if res.error:
                return res
        else:
            step_value = None

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["then"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'then'"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

            body = res.register(self.statements())
            if res.error: return res

            if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["block_end"]):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected 'end'"
                ))

            res.register_advancement()
            self.advance()

            return res.success(ForNode(var_name, start_value, end_value, step_value, body, True))
    

        body = res.register(self.expr())
        if res.error:
            return res

        return res.success(ForNode(var_name, start_value, end_value, step_value, body, False))

    def while_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["cond_loop"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'while'"
            ))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["then"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'then'"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

            body = res.register(self.statements())
            if res.error: return res

            if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["block_end"]):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected 'end'"
                ))

            res.register_advancement()
            self.advance()

            return res.success(WhileNode(condition, body, True))

        body = res.register(self.expr())
        if res.error:
            return res

        return res.success(WhileNode(condition, body, False))

    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_INT, TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))
        elif tok.type == TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(StringNode(tok))

        elif tok.type == TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                pos_start = self.current_tok.pos_start.copy()
                pos_start.col -= 1

                return res.failure(InvalidSyntaxError(
                    pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                ))
        elif tok.type == TT_LSQUARE:
            list_expr = res.register(self.list_expr())
            if res.error:
                return res
            return res.success(list_expr)

        elif tok.matches(TT_KEYWORD, KW_TABLE["cond_once"]):
            if_expr = res.register(self.if_expr())
            if res.error:
                return res
            return res.success(if_expr)

        elif tok.matches(TT_KEYWORD, KW_TABLE["loops_for"]):
            for_expr = res.register(self.for_expr())
            if res.error:
                return res
            return res.success(for_expr)

        elif tok.matches(TT_KEYWORD, KW_TABLE["cond_loop"]):
            while_expr = res.register(self.while_expr())
            if res.error:
                return res
            return res.success(while_expr)

        elif tok.matches(TT_KEYWORD, KW_TABLE["func_def"]):
            func_def = res.register(self.func_def())
            if res.error:
                return res
            return res.success(func_def)

        pos_start = tok.pos_start.copy()
        pos_start.col -= 1

        pos_end = tok.pos_end.copy()

        return res.failure(InvalidSyntaxError(
            pos_start, pos_end,
            "Expected int, float, indentifier, '+',"
            " '-', '(', '[', 'for', 'while', or 'func'"
        ))

    def list_expr(self):
        res = ParseResult()
        element_nodes = []
        pos_start = self.current_tok.pos_start.copy()

        if self.current_tok.type != TT_LSQUARE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected '['"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_RSQUARE:
            res.register_advancement()
            self.advance()
        else:
            element_nodes.append(res.register(self.expr()))
            if res.error:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ']', 'var', 'if', 'for',"
                    "'while', 'func', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
                ))

            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                element_nodes.append(res.register(self.expr()))
                if res.error: return res

            if self.current_tok.type != TT_RSQUARE:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected ',' or ']'"
                ))

            res.register_advancement()
            self.advance()

        return res.success(ListNode(
            element_nodes,
            pos_start,
            self.current_tok.pos_end.copy()
        ))

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS, TT_MINUS, TT_TILDE):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        return self.power()

    def term(self):
        return self.bin_op(self.factor, (TT_ASTERISK, TT_SLASH))

    def arith_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def comp_expr(self):
        res = ParseResult()

        if self.current_tok.matches(TT_KEYWORD, KW_TABLE["logic_not"]):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())

            if res.error:
                return res

            return res.success(UnaryOpNode(op_tok, node))

        node = res.register(self.bin_op(
            self.bitwise_expr,
            (TT_EE, TT_NE, TT_GT, TT_LT, TT_GTE, TT_LTE)
        ))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start,
                self.current_tok.pos_end,
                "Expected int, float, indentifier, '+',"
                " '-', '(', ']' or 'not'"
            ))

        return res.success(node)

    def bitwise_expr(self):
        res = ParseResult()

        node = res.register(self.bin_op(
            self.arith_expr,
            (TT_AMPER, TT_BAR, TT_CARET)
        ))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start,
                self.current_tok.pos_end,
                "Expected int, float, indentifier, '+',"
                " '-', '(', ']' or 'not'"
            ))

        return res.success(node)

    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_tok.pos_start.copy()

        while self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

        statement = res.register(self.expr())
        if res.error: return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_tok.type == TT_NEWLINE:
                res.register_advancement()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if not more_statements: break
            statement = res.try_register(self.expr())
            if not statement:
                self.reverse(res.to_reverse_count)
                more_statements = False
                continue
            statements.append(statement)

        return res.success(ListNode(
            statements,
            pos_start,
            self.current_tok.pos_end.copy()
        ))

    def expr(self):
        res = ParseResult()

        if self.current_tok.matches(TT_KEYWORD, KW_TABLE["var_declare"]):
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start,
                    self.current_tok.pos_end,
                    "Expected identifier",
                ))

            var_name = self.current_tok
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start,
                    self.current_tok.pos_end,
                    "Expected '='",
                ))

            res.register_advancement()
            self.advance()

            expr = res.register(self.expr())

            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(
            self.bin_op(
                self.comp_expr,
                (
                    (TT_KEYWORD, KW_TABLE["logic_and"]),
                    (TT_KEYWORD, KW_TABLE["logic_or"])
                )
            )
        )

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start,
                self.current_tok.pos_end,
                "Expected 'var', int, float, "
                "identifier, '+', '-', '(', or '['"
            ))

        return res.success(node)

    def func_def(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["func_def"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'func'"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_IDENTIFIER:
            var_name_tok = self.current_tok
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected '('"
                ))
        else:
            var_name_tok = None
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected identifier or '('"
                ))

        res.register_advancement()
        self.advance()
        arg_name_toks = []

        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()

            while self.current_tok.type == TT_COMMA:

                res.register_advancement()
                self.advance()

                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        f"Expected identifier"
                    ))

                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()

            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected ',' or ')'"
                ))
        else:
            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected identifier or ')'"
                ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_ARROW:
            res.register_advancement()
            self.advance()

            body = res.register(self.expr())
            if res.error: return res

            return res.success(FuncDefNode(
                var_name_tok,
                arg_name_toks,
                body,
                False
            ))
        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected '->' or NEWLINE"
            ))

        res.register_advancement()
        self.advance()

        body = res.register(self.statements())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD, KW_TABLE["block_end"]):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'end'"
            ))

        res.register_advancement()
        self.advance()

        return res.success(FuncDefNode(
            var_name_tok,
            arg_name_toks,
            body,
            True
        ))

    ###################################

    def bin_op(self, func_a, ops, func_b=None):
        if func_b is None:
            func_b = func_a

        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res

        while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)


#################
##RUNTIME RESULT#
#################


class RTResult:
    def __init__(self):
        self.value = self.error = None

    def register(self, res):
        if res.error:
            self.error = res.error

        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self


#################
##SYMBOL TABLE###
#################


class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    def get(self, name):
        value = self.symbols.get(name)

        if value is None and self.parent:
            return self.parent.get(name)

        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self, name):
        del self.symbols[name]

    def __repr__(self):
        from copy import deepcopy
        parent = deepcopy(self.parent)
        if parent is None:
            parent_symbols = {}
        else:
            parent_symbols = parent.symbols

        return (
            f"{ {**parent_symbols, **self.symbols} }"
        )


#################
#####VALUES######
#################


class Value:
    def __init__(self):
        self.pos_end = None
        self.pos_start = None
        self.context = None
        self.attr_table = SymbolTable()

        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        return None, self.illegal_operation(other)

    def subbed_by(self, other):
        return None, self.illegal_operation(other)

    def multed_by(self, other):
        return None, self.illegal_operation(other)

    def dived_by(self, other):
        return None, self.illegal_operation(other)

    def powed_by(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_eq(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_ne(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lte(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gte(self, other):
        return None, self.illegal_operation(other)

    def anded_by(self, other):
        return None, self.illegal_operation(other)

    def ored_by(self, other):
        return None, self.illegal_operation(other)

    def notted(self):
        return None, self.illegal_operation()

    def execute(self, args):
        args
        return RTResult().failure(self.illegal_operation())

    def copy(self):
        from copy import copy
        return copy(self)

    def is_true(self):
        return True

    def subscript(self, subscript):
        return RTResult().failure(self.illegal_operation())

    def bitwise_anded_by(self, other):
        return None, self.illegal_operation(other)

    def bitwise_ored_by(self, other):
        return None, self.illegal_operation(other)

    def bitwise_xored_by(self, other):
        return None, self.illegal_operation(other)

    def bitwise_not(self):
        return None, self.illegal_operation()

    def illegal_operation(self, other=None):
        if not other:
            other = self
        return RTError(
            self.pos_start, other.pos_end,
            'Illegal operation',
            self.context
        )


class Number(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Division by zero',
                    self.context
                )

            return Number(self.value / other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(float(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(float(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def notted(self):
        return Boolean(1 if self.value == 0 else 0).set_context(self.context), None

    def bitwise_anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(int(self.value) & int(other.value))).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def bitwise_ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(int(self.value) | int(other.value))).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def bitwise_xored_by(self, other):
        if isinstance(other, Number):
            return Number(int(int(self.value) ^ int(other.value))).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def bitwise_not(self):
        return Number(int(~int(self.value))).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def is_true(self):
        return self.value != 0

    def __repr__(self):
        return str(self.value)


class Boolean(Value):
    def __init__(self, value):
        super().__init__()
        self.value = bool(value)

    def get_comparison_eq(self, other):
        if isinstance(other, Boolean):
            return Boolean(self.value == other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Boolean):
            return Boolean(self.value != other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, Boolean):
            return Boolean(float(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored_by(self, other):
        if isinstance(other, Boolean):
            return Boolean(float(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def notted(self):
        return Boolean(1 if self.value == 0 else 0).set_context(self.context), None

    def is_true(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class Null(Value):
    def __init__(self):
        super().__init__()

    def get_comparison_eq(self, other):
        if isinstance(other, Boolean):
            return Boolean(True).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Boolean):
            return Boolean(False).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def is_true(self):
        return False

    def __repr__(self):
        return "NULL"


class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
        self.attr_table.set("split", List(value.split()))

    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def is_true(self):
        return len(self.value) > 0

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f"'{self.value.replace("'", "\\'")}'"


class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    def added_to(self, other):
        new_list = self.copy()
        new_list.elements.append(other)
        return new_list, None

    def subbed_by(self, other):

        if isinstance(other, Number):
            new_list = self.copy()
            try:
                new_list.elements.pop(other.value)
                return new_list, None
            except Exception:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    "Element at this index could not be removed from list because index is out of bounds",
                    self.context
                )

        else:
            return Value.illegal_operation(self, other)

    def multed_by(self, other):
        new_list = self.copy()

        if isinstance(other, List):
            new_list.elements.extend(other.elements)
            return new_list, None
        elif isinstance(other, Number):
            new_list.elements *= int(other.value)
            return new_list, None
        else:
            return None, Value.illegal_operation(self, other)

    def subscript(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except Exception:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    "Element at this index could not be retrieved "
                    "from list because index is out of bounds",
                    self.context
                )

        else:
            return Value.illegal_operation(self, other)

    def __repr__(self):
        return f'[{", ".join([str(x) for x in self.elements])}]'


class File(Value):
    def __init__(self, name):
        super().__init__()
        try:
            self.file = open(str(name.value))
            self.success = True
        except Exception:
            self.file = open("__CB_NULLFILE.txt")
            self.success = False

        self.attr_table.set("name", name)
        self.attr_table.set("contents", String(self.file.read()))
        self.name = name

    def __repr__(self):
        return f"<file {self.name}>"


class BaseFunction(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name or "<anonymous>"

    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        return new_context

    def check_args(self, arg_names, args):
        res = RTResult()

        if len(args) > len(arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(args) - len(arg_names)} too many args passed into {self}",
                self.context
            ))

        if len(args) < len(arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(arg_names) - len(args)} too few args passed into {self}",
                self.context
            ))

        return res.success(None)

    def populate_args(self, arg_names, args, exec_ctx):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(exec_ctx)
            exec_ctx.symbol_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names, args, exec_ctx):
        res = RTResult()
        res.register(self.check_args(arg_names, args))
        if res.error: return res
        self.populate_args(arg_names, args, exec_ctx)
        return res.success(None)


class Function(BaseFunction):
    def __init__(
        self, name, body_node, arg_names, should_return_null
    ):
        super().__init__(name)
        self.body_node = body_node
        self.arg_names = arg_names
        self.should_return_null = should_return_null

    def execute(self, args, pos_start, pos_end):
        res = RTResult()
        interpreter = Interpreter()
        exec_context = self.generate_new_context()

        res.register(self.check_and_populate_args(self.arg_names, args, exec_context))
        if res.error: return res

        value = res.register(interpreter.visit(self.body_node, exec_context))
        if res.error: return res
        return res.success(value)

    def copy(self):
        from copy import copy
        return copy(self)

    def __repr__(self):
        return f"<function {self.name}>"


class BuiltInFunction(BaseFunction):
    def __init__(self, name):
        super().__init__(name)

    def execute(self, args, pos_start:Position=None, pos_end:Position=None):
        res = RTResult()
        exec_ctx = self.generate_new_context()

        method_name = f"execute_{self.name}"
        method = getattr(self, method_name, self.no_visit_method)

        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        if res.error: return res

        return_value = res.register(method(exec_ctx, pos_start, pos_end))
        if res.error: return res
        return res.success(return_value)

    def no_visit_method(self, node, context):
        raise NotImplementedError(f'No execute_{self.name} method defined')

    def __repr__(self):
        return f"<built-in function {self.name}>"

    ##################################

    def execute_print(self, exec_ctx, pos_start, pos_end):
        print(exec_ctx.symbol_table.get("value"), file=cb_out)
        return RTResult().success(Null())

    execute_print.arg_names = ["value"]

    def execute_print_ret(self, exec_ctx, pos_start, pos_end):
        val = String(str(exec_ctx.symbol_table.get("value")))
        return RTResult().success(val)

    execute_print_ret.arg_names = ["value"]

    def execute_input(self, exec_ctx, pos_start, pos_end):
        text = input()
        return RTResult().success(String(text))

    execute_input.arg_names = []

    def execute_input_int(self, exec_ctx, pos_start, pos_end):
        while 1:
            try:
                text = input()
                number = int(text)
                break
            except ValueError:
                print("Input is not an integer.", file=cb_err)
        return RTResult().success(Number(text))

    execute_input_int.arg_names = []

    def execute_clear(self, exec_ctx, pos_start, pos_end):
        from os import system, name
        if name == "nt":
            system("cls")
        else:
            system("clear")
        return RTResult().success(Null())

    execute_clear.arg_names = []

    def execute_dir(self, exec_ctx, pos_start, pos_end):
        result = str((
                        global_symbol_table
                    ).symbols)
        return RTResult().success(String(result))

    execute_dir.arg_names = []

    def execute_open(self, exec_ctx, pos_start, pos_end):
        name = exec_ctx.symbol_table.get("name")
        file = File(name)
        if file.success:
            return RTResult().success(file)
        else:
            print(dir(exec_ctx))
            return RTResult().failure(
                RTError(
                    pos_start=pos_start, pos_end=pos_end,
                    details= f"Unknown file ({name!r})",
                    context=exec_ctx
                )
            )

    execute_open.arg_names = ["name"]

    def execute_exit(self, exec_ctx, pos_start, pos_end):
        exit()

    execute_exit.arg_names = []


BuiltInFunction.print = BuiltInFunction("print")
BuiltInFunction.print_ret = BuiltInFunction("print_ret")
BuiltInFunction.input = BuiltInFunction("input")
BuiltInFunction.input_int = BuiltInFunction("input_int")
BuiltInFunction.clear = BuiltInFunction("clear")
BuiltInFunction.dir = BuiltInFunction("dir")
BuiltInFunction.open = BuiltInFunction("open")
BuiltInFunction.exit = BuiltInFunction("exit")


#################
#####CONTEXT#####
#################


class Context:
    def __init__(
            self, display_name, parent=None, parent_entry_pos=None
    ):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None


#################
###INTERPRETER###
#################


# noinspection PyPep8Naming
class Interpreter:
    def visit(self, node, context):
        method_name = f"visit_{type(node).__name__}"
        method = getattr(self, method_name, self.no_visit_method)
        # noinspection PyArgumentList
        return method(node, context)

    def no_visit_method(self, node, context):
        raise NotImplementedError(
            f"No visit_{type(node).__name__} method defined"
        )

    ################

    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value)
            .set_context(context)
            .set_pos(node.pos_start, node.pos_end)
        )

    def visit_StringNode(self, node, context):
        return RTResult().success(
            String(node.tok.value)
            .set_context(context)
            .set_pos(node.pos_start, node.pos_end)
        )

    def visit_ListNode(self, node, context):
        res = RTResult()
        elements = []

        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node, context)))
            if res.error: return res

        return res.success(
            List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"'{var_name}' is not defined",
                context
            ))

        value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)

    def visit_AttrNode(self, node: AttrNode, context):
        res = RTResult()
        var_name = node.attr_name_tok.value
        parent = res.register(self.visit(node.parent_node, context))
        if res.error: return res
        value = parent.attr_table.get(var_name)

        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"{parent} has no attribute '{var_name}'",
                context
            ))

        value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error:
            return res

        context.symbol_table.set(var_name, value)
        return res.success(value)

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left_node, context))
        if res.error:
            return res
        right = res.register(self.visit(node.right_node, context))
        if res.error:
            return res

        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_ASTERISK:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_SLASH:
            result, error = left.dived_by(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powed_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, KW_TABLE["logic_and"]):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, KW_TABLE["logic_or"]):
            result, error = left.ored_by(right)
        elif node.op_tok.type == TT_AMPER:
            result, error = left.bitwise_anded_by(right)
        elif node.op_tok.type == TT_BAR:
            result, error = left.bitwise_ored_by(right)
        elif node.op_tok.type == TT_CARET:
            result, error = left.bitwise_xored_by(right)
        else:
            raise NotImplementedError(f"Non-implemented binary operator type: {node.op_tok.type}")

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        val = res.register(self.visit(node.node, context))

        if res.error:
            return res

        if node.op_tok.type == TT_MINUS:
            val, error = val.multed_by(Number(-1))
        elif node.op_tok.type == TT_PLUS:
            val, error = val.multed_by(Number(1))
        elif node.op_tok.matches(TT_KEYWORD, KW_TABLE["logic_not"]):
            val, error = val.notted()
        elif node.op_tok.type == TT_TILDE:
            val, error = val.bitwise_not()

        if error:
            return res.failure(error)
        else:
            return res.success(val.set_pos(node.pos_start, node.pos_end))

    def visit_IfNode(self, node, context):
        res = RTResult()

        for condition, expr, should_return_null in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error:
                return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error:
                    return res
                return res.success(
                    Null() if should_return_null else expr_value
                )

        if node.else_case:
            expr, should_return_null = node.else_case
            expr_value = res.register(self.visit(expr, context))
            if res.error:
                return res
            return res.success(
                Null() if should_return_null else expr_value
            )

        return res.success(Null())

    def visit_ForNode(self, node, context):
        res = RTResult()
        elements = []

        start_value = res.register(self.visit(node.start_value_node, context))
        if res.error:
            return res

        end_value = res.register(self.visit(node.end_value_node, context))
        if res.error:
            return res

        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.error:
                return res
        else:
            step_value = Number(1)

        i = start_value.value

        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value

        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value

            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error:
                return res

        return res.success(
            Null() if node.should_return_null else
            List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_WhileNode(self, node, context):
        res = RTResult()
        elements = []

        while 1:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error:
                return res

            if not condition.is_true():
                break

            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error:
                return res

        return res.success(
            Null() if node.should_return_null else
            List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_FuncDefNode(self, node, context):
        res = RTResult()

        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names, node.should_return_null)\
            .set_context(context) \
            .set_pos(node.pos_start, node.pos_end)

        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)

        return res.success(func_value)

    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []

        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error:
            return res
        value_to_call = value_to_call.copy().set_pos(
            node.pos_start, node.pos_end
        )
        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error:
                return res

        return_value = res.register(value_to_call.execute(args, node.pos_start, node.pos_end))
        if res.error:
            return res
        return_value = return_value.set_context(context).set_pos(node.pos_start, node.pos_end)

        return res.success(return_value)

    def visit_SubscriptNode(self, node: SubscriptNode, context):
        res = RTResult()
        left = res.register(self.visit(node.node_to_subscript, context))
        if res.error:
            return res
        right = res.register(self.visit(node.subscript_node, context))
        if res.error:
            return res

        subscript_result = left.subscript(right)

        if type(subscript_result) is RTResult:
            result, error = subscript_result.value, subscript_result.error
        else:
            result, error = subscript_result[0], subscript_result[1]

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_SuperscriptNode(self, node: SuperscriptNode, context):
        res = RTResult()
        var_name = str(res.register(self.visit(node.superscript_node, context)))
        if res.error: return res
        parent = res.register(self.visit(node.parent_node, context))
        if res.error: return res
        value = parent.attr_table.get(var_name)

        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"{parent} has no attribute '{var_name}'",
                context
            ))

        value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)


#################
#######RUN#######
#################


def lex(fn, text):
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error:
        return None, error
    return tokens, error


def parse(tokens):
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    return ast.node, ast.error


builtins_symbol_table = SymbolTable()
builtins_symbol_table.set("NULL", Null())
builtins_symbol_table.set("True", Boolean(1))
builtins_symbol_table.set("False", Boolean(0))
builtins_symbol_table.set("print", BuiltInFunction.print)
builtins_symbol_table.set("print_ret", BuiltInFunction.print_ret)
builtins_symbol_table.set("input", BuiltInFunction.input)
builtins_symbol_table.set("input_int", BuiltInFunction.input_int)
builtins_symbol_table.set("clear", BuiltInFunction.clear)
builtins_symbol_table.set("dir", BuiltInFunction.dir)
builtins_symbol_table.set("open", BuiltInFunction.open)
builtins_symbol_table.set("exit", BuiltInFunction.exit)

global_symbol_table = SymbolTable()
global_symbol_table.parent = builtins_symbol_table

cb_out = stdout
cb_err = stderr


def run(fn, text, max_stage=3):
    tokens, error = lex(fn, text)
    if error:
        return None, error

    if max_stage == 1:
        return tokens, error

    # Generate AST
    node, error = parse(tokens)

    if max_stage == 2:
        return node, error
    if error:
        return None, error

    # Run program
    interpreter = Interpreter()
    context = Context("<program>")
    context.symbol_table = global_symbol_table

    result = interpreter.visit(node, context)

    if max_stage == 3:
        if result is not None:
            return result.value, result.error
        else:
            return "", None

    if max_stage == 4:
        return result.value.elements, result.error

(* token.sml *)

structure Token =
struct

  datatype token
    = ID of string
    | NAT of int
    | NEG
    | PLUS | MINUS       (* additive operators (+, -) *)
    | TIMES | DIV | MOD  (* multiplicative operators ( *, /, %) *)
    | EQUAL | NOTEQUAL | LESS | GREATER | LESSEQ | GREATEREQ
        (* relational operators *)
    | TRUE | FALSE       (* boolean constants *)
    | AND | OR | NOT    (* boolean operators *)
    | XOR | NOR | NAND (* fancy boolean operators *)
    | FN | DARROW        (* function expression keywords *)
    | LET | IN | EQ | VAL | FUN  (* declaration keywords and punctuation *)
    | IF | THEN | ELSE   (* conditional expr keywords *)
    | CASE | OF | BAR | INL | INR  (* case expressions *)
    | LPAR | RPAR        (* left and right parentheses *)
    | LBRACKET | RBRACKET     (* left and right brackets *)
    | COMMA | COLON | SEMI
    | QUIT
    | NULL

  fun toString tok =
      case tok
       of ID s => concat["ID(", s, ")"]
        | NAT i => concat["INT(", Int.toString i, ")"]
        | NEG => "NEG"
        | PLUS => "PLUS"
        | MINUS => "MINUS"
        | TIMES => "TIMES"
        | DIV => "DIV"
        | MOD => "MOD"
        | EQUAL => "EQUAL"
        | NOTEQUAL => "NOTEQUAL"
        | LESS => "LESS"
        | GREATER => "GREATER"
        | LESSEQ => "LESSEQ"
        | GREATEREQ => "GREATEREQ"
        | TRUE => "TRUE"
        | FALSE => "FALSE"
        | AND => "AND"
        | OR => "OR"
        | NOT => "NOT"
        | FN => "FN"
        | DARROW => "DARROW"
        | LET => "LET"
        | IN => "IN"
        | EQ => "EQ"
        | VAL => "VAL"
        | FUN => "FUN"
        | IF => "IF"
        | THEN => "THEN"
        | ELSE => "ELSE"
        | CASE => "CASE"
        | OF => "OF"
        | BAR => "BAR"
        | INL => "INL"
        | INR => "INR"
        | LPAR => "LPAR"
        | RPAR => "RPAR"
        | LBRACKET => "LBRACKET"
        | RBRACKET => "RBRACKET"
        | COMMA => "COMMA"
        | COLON => "COLON"
        | SEMI => "SEMI"
        | QUIT => "QUIT"

end

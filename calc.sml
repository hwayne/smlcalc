
open StringStream
open Token

(* List of all expression types. SMLcalc does not compile expressions into a code. It just calculates the expression recursively. *)
datatype Exp =
Null

| Num of int 
| Var of string
| Bool of bool
| Quit of string

| Fun of string * Exp

| Neg of Exp (* tight bind *)
| If of Exp * Exp * Exp
| Plus of Exp * Exp
| Mult of Exp * Exp
| Subt of Exp * Exp
| Div of Exp * Exp
| Mod of Exp * Exp

| Not of Exp
| And of Exp * Exp
| Or of Exp * Exp

| Eq of Exp * Exp
| Leq of Exp * Exp
| Less of Exp * Exp

| Bind of Exp (* tight bind *)

type varmap = (string * Exp) list (*name, value*)
type funmap = (string * string * Exp) list (*name, name of argumeent, value*)

(*till finds the next point the list which has the appropriate value, and returns everything between them. *)
fun till (_, []) = []
|till(x, y::l) = if x = y then [] else y::till(x,l)

(*tillcount is used to deal with nestable operations, like () and if.
 *It counts the level of nesting and only returns the list when the last
 *Element is found. x is the token that starts a new scope (such as ( for ())
 *And y is a token that ends on (like )) *)

fun tillcount(l, x, y) = let 
fun count'([], _) = []
|count'(u::l,  n) = if u = x then u::count'(l, n+1)
                 else if u = y then if n = 0 then []
                                             else u::count'(l, n-1) 
                      else u::count'(l, n)
in count'(l, 0) end

(*cut takes a token and removes everything from the start of the list ot the next occurance*)
fun cut (_, []) = []
|cut(x, y::l) = if x = y then l else cut(x, l)

(*nextp finds the priority operator in the token stream. This lets us get order
 *of operations right. *)
fun nextp [] = (~1,NULL)
|nextp(x::L) = (case x of
(EQUAL | NOTEQUAL | LESSEQ | LESS | GREATER | GREATEREQ) => (0, x)
|(AND | OR | XOR | NAND | NOR) => (0, x)
|(PLUS | MINUS | MOD) => (1,x)
|(TIMES | DIV | NEG | NOT) => (2,x)
|(LPAR |  IF) => (3,x)
|_ => nextp L)

exception Badfn


(*Parser! The parser converts a token list into a giant expression. It takes 
 *tokens off the list and evaluates them. The major concern is getting order
 *of operations right, so that it doesn't evaluate 2+2*2 as 8. To handle this,
 *parse' stores the previous expression generated. This means that binary
 *operators can compare to the next operator with priority, and if higher
 *just operate on the last expression and the next one. If a lower priority,
 *it can throw the next expression to the next operator. The first argument
 *become the last expression, the second becomes the parser acting on the rest o *f the stream.*)

fun parser ([]:Token.token list) : Exp = Null
| parser (x::stream) =  (* Exp list * token * token list *)
let fun parse' (queue : Exp, Token.NULL, []:Token.token list) = queue
|parse' (queue : Exp, x : token, tail as t::t1) =
let val (valop,nextop) = nextp(tail) in (*get next operator for later comps*)
(case x of 
NULL => parse' (queue, t, t1)

(*If parse hits a value, reserve in case an operator follows. *)
|NAT a => parse' ((Num a), t, t1)
|TRUE => parse' ((Bool true), t, t1)
|FALSE => parse' ((Bool false), t, t1)


(*Operators. If it has a higher precedence then we cut off everything to the 
 *next operator and continue parsing. Note that we use a < comparison, not a 
 *<=. This means that if the next operator is the same, the first one will have
 *a lower priority. This ensures that operators are left-associative; 1-1-1 will
 *be -1, not 0.*)

|PLUS => if((#1 (nextp(PLUS::[]))) < valop) 
       then Plus(queue, parser(tail)) 
        else parse'(Plus(queue,parser(till(nextop,tail))), nextop, cut(nextop,t1))

|TIMES => if((#1 (nextp(TIMES::[]))) < valop) 
       then Mult(queue, parser(tail)) 
        else parse'(Mult(queue,parser(till(nextop,tail))), nextop, cut(nextop,t1))

|MINUS => if((#1 (nextp(MINUS::[]))) < valop) 
       then Subt(queue, parser(tail)) 
        else parse'(Subt(queue,parser(till(nextop,tail))), nextop, cut(nextop,t1))


|DIV => if((#1 (nextp(DIV::[]))) < valop) 
       then Div(queue, parser(tail)) 
        else parse'(Div(queue,parser(till(nextop,tail))), nextop, cut(nextop,t1))

|MOD => if((#1 (nextp(MOD::[]))) < valop) 
       then Mod(queue, parser(tail)) 
        else parse'(Mod(queue,parser(till(nextop,tail))), nextop, cut(nextop,t1))

(* data flippers. These also compare priorities so we can use parenthesis
 * after them. Otherwise we could not do ~(1 + 3). *)
|NOT  => if((#1 (nextp(NOT::[]))) < valop) 
       then Not(parser(tail)) 
        else parse'(Not(parser(till(nextop,tail))), nextop, cut(nextop,t1))

|NEG  => if((#1 (nextp(NEG::[]))) < valop) 
       then Neg(parser(tail)) 
        else parse'(Neg(parser(till(nextop,tail))), nextop, cut(nextop,t1))

(*Logical values are considered to have no priority. NAND, NOR, and XOR are 
 *Defined solely in terms of NOTs, ANDs, and ORs. If I was trying to be more
 *space efficient I'd rewrite everything as NANDs and NORs, but this is a good
 *balance between time and space.*)

|AND => And(queue, parser(tail))
|OR => Or(queue, parser(tail))
|NAND => Not(And(queue, parser(tail)))
|NOR => Not(Or(queue, parser(tail)))
|XOR => And(Not(And(queue, parser(tail))),Or(queue,parser(tail)))

(* Same applies here. Equal and Notequal are special- since they're independent  * of the type of expression, they work on both integers and booleans.*)

|EQUAL => Eq(queue, parser(tail))
|NOTEQUAL => Not( Eq(queue, parser(tail)))

|LESSEQ =>Leq(queue, parser(tail)) 
|LESS => Less(queue, parser(tail))
|GREATEREQ => Not( Less(queue, parser(tail)))
|GREATER => Not( Leq(queue, parser(tail)))

(*Quit lets us escape from our expressions early, which would primarily be 
 *useful as a means of breaking from nested if statements. We can also give it
 *a one word statement. *)
|QUIT => (case t of
         ID a => (Quit a) 
	 |_ => (Quit "Calculator encountered a quit statement"))

(* Parsers that create their own scope, and therefore can be nested. We have to 
 * use tillcount here to avoid nesting problems. Otherwise something like
 * (2+(3+4)+5) would be parsed as (2+(3+4).*)
|IF => let val x = tillcount(tail, IF, THEN)
           val sx = List.length x +1
           val y = tillcount(tail, IF, ELSE)
           val sy = List.length y
       in  If(parser(x), parser(List.drop(y, sx)), parser(List.drop(t1, sy)))
       end

(* LPAR starts a () statement. Therefore it has highest priority. *)
|LPAR => let val x = tillcount(tail, LPAR, RPAR) in
             parse'(Bind(parser(x)), NULL, List.drop(t1, (List.length x)))
           end

(* Whenever we see a name we first check to see if it's a function or not by
 * seeing if the next character is a (. If not, it's a variable and we can drop
 * Var as an expression. If so, we have to use a tillcount to track nesting. 
 * This lets us parse things like f((3+5*(7+9))). *)

|ID a => if t = LPAR then let val x = tillcount(t1, LPAR, RPAR) in
         parse'(Fun (a, parser(x)), NULL, List.drop(t1, (List.length x)+1)) end
         else parse' (Var a, t, t1)
|EQ => parse'(queue, t, t1) (* Hello! How goes? Nice weather we're having.*)
)
end
(*By throwing a NULL at the end we can have
 *a single termination condition: parse' (_, NULL, []).*)

in parse' (Null, x, stream@[NULL]) 
end


(* fnbuild *)

exception Badfn
exception Nullfn

(* Oy, this is a mess. fnbuild takes a list of functions and *drumroll* builds
 * it. It takes two tokens at a time: if the first is a string and the second a
 * =, it throws everything till the next comma into the list of variables.
 * If it's string string, it first checks to see if the second string is 
 * already a variable. If this is the case, then error. This should prevent
 * free variable problems, unless you're sneaky and define the variables after
 * the functions. But that's just mean. *)

(* Functions and variables are delimited by commas. *)

fun fnbuild([]:token list, varlist, fnlist) = (varlist, fnlist)
|fnbuild(x::y::l, varlist, fnlist) = case (x,y) of
 (ID a,EQ) => fnbuild(cut(COMMA, l), (a, parser(till(COMMA, l)))::varlist, fnlist)
| (ID a, ID b) => if (getval(varlist,b) = Null) andalso hd(l) = EQ 
               then let val z = tillcount(tl(l), EQ, COMMA) (* handle anonvars*)
               in fnbuild(List.drop(l, List.length z + 1), varlist,
                                      (a, b, parser(z))::fnlist)
               end
               else raise Badfn
| (COMMA, _) => fnbuild(y::l, varlist, fnlist)          
| (_, _) => raise Nullfn (*badly formatted func declaration. *)

(* getval searches for the variable of the appropriate name. *)
and getval ([]:varmap, _) : Exp = Null
|getval((name,expr)::l, a) = if name = a then expr else getval(l, a) 

(* ditto functions.*)
and getfn ([]:funmap, _) = ("", Null)
|getfn((name,arg,exp)::l, a) = if name = a then (arg,exp) else getfn(l, a)

exception IfBranch
exception QuitStatement

(* Time for the big one. eval does a recursive processing on the big expression.
 * If it ever hits an expression that isn't a value, it just evaluates it too.
 * Most of the code is self explaining, but tricky bits are commented. *)
fun evaluate (expr, varmap, fnmap) = let

fun eval (Neg(Num a)) = Num (~a)
    |eval (Neg(exp)) = eval (Neg(eval(exp)))

(*operators.*)

|eval (Plus(Num a, Num b)) = Num (a + b)
|eval (Plus(exp1, exp2)) = eval (Plus(eval exp1, eval exp2))

|eval (Mult(Num a, Num b)) = Num (a * b)
|eval (Mult(exp1, exp2)) = eval (Mult(eval exp1, eval exp2))

|eval (Subt(Num a, Num b)) = Num (a - b)
|eval (Subt(exp1, exp2)) = eval (Subt(eval exp1, eval exp2))

|eval (Div(Num a, Num b)) = Num (a div b)
|eval (Div(exp1, exp2)) = eval (Div(eval exp1, eval exp2))

|eval (Mod(Num a, Num b)) = Num (a mod b)
|eval (Mod(exp1, exp2)) = eval (Mod(eval exp1, eval exp2))

|eval (Quit a) = (print a; raise QuitStatement)

(*if branch. Flips out if the branch is not a boolean. *)
|eval (If(branch, exp1, exp2)) = (case branch of 
Bool true => eval exp1
|Bool false => eval exp2
|Num _ => raise IfBranch
|exp => eval (If (eval(exp), exp1, exp2)))

(* logical operators. *)

|eval (And(Bool true, Bool true)) = Bool true
|eval (And(Bool false, _)) = Bool false
|eval (And(_, Bool false)) = Bool false
|eval (And(exp1, exp2)) = eval (And(eval exp1, eval exp2))

|eval (Or(Bool false, Bool false)) = Bool false
|eval (Or(Bool true, _)) = Bool true
|eval (Or(_, Bool true)) = Bool true
|eval (Or(exp1, exp2)) = eval (Or(eval exp1, eval exp2))


|eval (Not(Bool true)) = Bool false
|eval (Not(Bool false)) = Bool true
|eval (Not(exp1)) = eval (Not(eval exp1))

|eval (Less(Num a, Num b)) = if a < b then Bool true else Bool false
|eval (Less(exp1, exp2)) = eval (Less(eval exp1, eval exp2))

|eval (Leq(Num a, Num b)) = if a <= b then Bool true else Bool false
|eval (Leq(exp1, exp2)) = eval (Leq(eval exp1, eval exp2))

|eval (Eq(Num a, Num b)) = if a = b then Bool true else Bool false
|eval (Eq(Bool a, Bool b)) = if a = b then Bool true else Bool false
|eval (Eq(exp1, exp2)) = eval (Eq(eval exp1, eval exp2))

(*Bind is just a dummy expression. It can probably be removed without impacting
 *The function of the code. I included it because it made it easier for me to
 *Visualize how parenthesis would parse.*) 

|eval (Bind(exp)) = eval(exp)

(* values *)

|eval (Num a) = Num a
|eval (Bool a) = Bool a

(* Variables. If it hits a variable, it finds the corresponding expression and
 * evaluates that instead. *)

|eval (Var a) = eval (getval(varmap, a))

(* Functions. This is the fun part. If it hits a function, it finds
 * the corresponding expression and evalutes it. The trick is that it also
 * binds a new variable to the map with the corresponding expression equal
 * to the argument. So if you defined "f x = x*x" and called "f(1+1)", it would
 * evaluate x*x but add ("x", Plus(Num 1, Num 1)) to the varmap. *)
 
|eval (Fun (a, arg)) = let val (vname, exp) = getfn(fnmap, a)
                       in evaluate (exp, (vname,arg)::varmap, fnmap) end

in eval (expr)
end

structure CalcTokenizer = LexerFn(StringStream);

(* Two separate solve functions. solve does a regular thing. fsolve is for
 * when you need to define variables and functions. *)
fun solve(a: string) : Exp = 
    let   val  x = parser(#1 (valOf
           (CalcTokenizer.tokenize (StringStream.mkInstream a))))
    in evaluate(x, [], []) end

fun fsolve(a: string, b: string) : Exp =
    let val (v,f) = fnbuild(#1 (valOf
           (CalcTokenizer.tokenize (StringStream.mkInstream b))), [], [])
        val  x = parser(#1 (valOf
           (CalcTokenizer.tokenize (StringStream.mkInstream a))))
    in evaluate(x, v, f)
end

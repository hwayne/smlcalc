SMLcalc is my current indepth calculator. 

-

{structure}

The calculator is built off Simple's lexer function. I used the definition of stream, tokenize, and lexerfn. Everything else is my own.

Most of the hard code is in the parser, which doesn't use any states or monads. in order to get around priority issues (like 2+2*2), the parser has to know what else is in the token stream, so it can react accordingly. I get around this by always keeping track of what the next operator is and how it relates to the current one. I also keep a list of priorities that the computer checks. This lets it take a token string and output a single expression that has proper order of operations.

The expression reader is just a natural extension of a previous homework assignment. Instead it recursively evaluates the entire thing, eventually returning a number or boolean. It's main problem is that it can't handle expressions with the wrong type of input: if you have Plus(num 5, bool true), it will evaluate num 5 as num 5 and bool true as bool true. So it will never be in a form that the evaluator can evaluate, and the program will hang. 

In order to handle variables, I have a separate variable mapping and function mapping that's applied to eval. both of them are constructed by passing in a list of variables and functions to the solver. Variables work by mapping expressions onto strings. Functions work by mapping {expressions that contain strings} onto strings, and then when called adding a new variable that maps {function argument} onto {string contained in function expression.} This function works, but it cannot handle recursive functions. I might be able to manage that by replacing the two string format with a special let command that signifies a variable or function.

The program does not use any state dependence, meaning that it theoretically be very easy to port into Haskell. I'd have to make the function builder monadic to enforce strict evaluation, though.

-

{using the calculator}

You load it by entering

CM.make "smlcalc.cm"
use "calc.sml"

The calculator takes input the form fsolve(string1, string2) where string1 is the expression to evaluate and string2 is your list of variables and functions. An example varlist would look like

"x = 3, f y = y + x"

Note that functions are declared without parenthesis. In order to use them in your code you need parenthesis. For example, putting "f 7" in string1 is invalid. "f(7)" is the proper syntax. The following will print "Num 11"

fsolve("f(3+x)+x", "x = 4, f y = y");

To avoid bound variable problems the function cannot accept a dummy variable that has already been assigned to a free variable. In the previous case, "f x = x" will throw an error. Note you can get around this by putting variable declarations after the function declarations, but this shouldn't actually cause a free varaible / bound variable conflict (the bound variable will come first in the varlist).

If you don't want to define any variables or functions, you can use solve instead. Solve only takes one string, which is the equation. For example:

solve("3*(2-1)");

You can test it with <use "test.sml";>.
-

{Logical Operators}

SMLcalc has a boolean type designed to handle logical operators. It lexes the following symbols:

true -> TRUE
false -> FALSE
not -> NOT
&& -> AND
/& -> NAND
|| -> OR
/| -> NOR
^^ -> XOR

NXOR is not implemented but can be simulated with a NOT statement. SMLcalc also has the following comparison operators:

< -> LESS
<= -> LESSEQ
> -> GREATER
>= -> GREATEREQ
== -> EQUAL
/= -> NOTEQUAL

= is reserved for defining variables and functions.



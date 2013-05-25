(* Basic test. Evaluates to 1.*)
print("3*(2-1)\n");
solve("3*(2-1)");

(* Priority test. Ten random numbers. Should evaluate to -106*)

print("5-9*10/8*9+10-1*3+4+~9\n");
solve("5-9*10/8*9+10-1*3+4+~9");

(*Variable test. Should evaluate to 5*)

print("2*x - 1, x = 3\n");
fsolve("2*x - 1", "x = 3");

(*Function test. Should evaluate to 15.*)

print("f(4), f x = x*x - 1\n");
fsolve("f(4)", "f x = x*x - 1");

(*Variable and function test. Should evaluate to 7155*)

print("5*x + f(3+y), x = 45, y = 11, f z = x*y*z\n");
fsolve("5*x + f(3+y)", "x = 45, y = 11, f z = x*y*z");

(*Logic test. Should evaluate to FALSE TRUE FALSE FALSE TRUE FALSE TRUE *)

print("2 > 4, 3 >= 1, true && false, true ^^ true, (2 > 4) /& (3 >= 1), not true, not true || not (true && ((2+3) > 5))\n\n");

solve("2 > 4"); 
solve("3 >= 1");
solve(" true && false");
solve("true ^^ true");
solve("(2 > 4) /& (3 >= 1)");
solve("not true"); 
solve("not true || not (true && ((2+3) > 5))");


(*If statement control. Should evaluate to false due to the second part of hte AND statement being false. This means it should quit.*)

print("if (f(3) == 11) && (2 < g(3)) then f(3) + g(3) else quit bounds, f x = x*x + 2, g y = y - 1\n");
fsolve("if (f(3) == 11) && (2 < g(3)) then f(3) + g(3) else quit bounds", "f x = x*x + 2, g y = y - 1");



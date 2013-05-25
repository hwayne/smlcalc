(* stream.sml *)

structure Stream =
struct

datatype 'a susp
  = SUSP of unit -> 'a
  | VALUE of 'a

datatype 'a stream
  = Stream of 'a stream susp ref
  | SCons of 'a * 'a stream
  | SNil

(* this should never be raised *)
exception StreamError

(* strict cons, for use when we already have a stream *)
fun scons (x, s) = SCons (x,s)

(* lazy cons, for when we are defining a stream whose tail has not been computed yet *)
fun lcons f = Stream(ref(SUSP f))

fun sforce (Stream r) =
    (case !r
       of VALUE(s) => s
        | SUSP f => let val s = f() in r := VALUE s; s end)   
  | sforce s = s

(* mkStream : (unit -> 'a stream) -> 'a stream *)
fun mkStream f = Stream(ref(SUSP f))

fun snull s =
    case sforce s
      of SNil => true
       | SCons _ => false
       | Stream _ => raise StreamError

fun shd s =
    case sforce s
      of SNil => raise Empty
       | SCons(x,_) => x
       | Stream _ => raise StreamError

fun stl s = 
    case sforce s
      of SNil => raise Empty
       | SCons(_,xs) => xs
       | Stream _ => raise StreamError

(* destruct a stream into its head and tail elements *)
fun sdest s =
    case sforce s
      of SNil => raise Empty
       | SCons(x,xs) => (x,xs)
       | Stream _ => raise StreamError

(* stake : 'a stream * int -> 'a stream *)
fun stake (s,0) = []
  | stake (s,n) = 
      if snull s then raise Subscript
      else shd s :: stake (stl s, n-1)

(* sdrop : 'a stream * int -> 'a stream *)
fun sdrop (s,0) = s
  | sdrop (s,n) = 
    if snull s then raise Subscript
    else sdrop (stl s, n-1)

end (* structure Stream *)


structure StreamExamples =
struct

open Stream

(* generate the infinite stream of successive numbers starting with n *)
fun nums n = lcons(fn() => SCons(n, nums(n+1)));

(* stream of all positive ints:  1,2,3,... *)
val posints = nums 1;

(* Eratosthenes Sieve *)

fun sfilter f s =
    let val (x,s') = sdest s
     in if f x then lcons(fn () => SCons(x, sfilter f s'))
        else sfilter f s'
    end;

fun notdivisibleby n m = m mod n <> 0;

fun sieve s = 
    let val (x,s') = sdest s 
     in lcons(fn () => SCons(x, sieve(sfilter (notdivisibleby x) s')))
    end;

(* stream of prime numbers *)
val primes = sieve (nums 2);

fun first_n_primes n = stake(primes,n);

end (* structure Primes *)

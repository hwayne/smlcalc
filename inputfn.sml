(* Purely Functional Input, functor version *)

signature INSTREAM =
sig
  type instream
  val fromString : string -> instream
  val inputc : instream -> (char * instream) option
end

functor InputFn(X : INSTREAM) =
struct

  open X

  (* an 'a reader is a function that consumes some prefix
   * of its stream argument and produces an 'a and 
   * the remainder of the stream if successful. As a
   * function, a reader action is performed by applying it
   * to an instream. *)
  type 'a reader = instream -> ('a * instream) option

  (* return :  'a -> 'a reader *)
  fun return v = fn instream => SOME(v, instream)

  (* failure : 'a reader *)
  val fail = fn instream => NONE

  (* chain: (p: 'a reader) -> ('a -> 'b reader) -> 'b reader
   * this chains together a reader, and another reader computed
   * from the value produced by the first reader (if successful). *)
  fun chain (p: 'a reader) (f: 'a -> 'b reader) instream =
      case p instream
	of NONE => NONE
	 | SOME(v, instream') => (f v) instream'

  (* choice: 'a reader * 'a reader -> 'a reader  *)
  (* perform p, but if it fails, perform q on same instream *)
  fun choice (p, q) instream =
      case p instream
	of NONE => q instream
	 | res => res

  (* return next char if it satisfies prop, otherwise fail immediately *)
  fun sat (prop : char -> bool) : char reader =
      chain inputc (fn c => if prop c then return c else fail)

  (* checkChar : char -> char reader 
   * check that the next character available is c, then return c *)
  fun checkChar c = sat (fn c' => c = c')

  (* checkChars: char list -> char list reader
   * check that the instream starts with s, then return s *) 
  fun checkChars ([] : char list) = return []
    | checkChars (s as (c::cs)) = 
      chain (checkChar c) (fn _ => chain (checkChars cs) (fn _ => (return s)))

end; (* functor InputFn *)


structure ListInstream :> INSTREAM =
struct
   type instream = char list

   fun fromString string = explode string

   fun inputc [] = NONE
     | inputc (c::cs) = SOME(c, cs)
end;

structure StringInstream :> INSTREAM =
struct
   type instream = {base: string, pos: int, limit: int}

   fun fromString string = {base=string, pos=0, limit=size string}

   fun inputc {base,pos,limit} = 
       if pos >= limit then NONE
       else SOME(String.sub(base,pos), {base=base,pos=pos+1,limit=limit})

end;

structure ListInput = InputFn(ListInstream);

structure StringInput = InputFn(StringInstream);

structure StreamInStream :> INSTREAM where type instream = char Stream.stream =
struct
  open Stream

  type instream = char stream
  
  fun fromString string = raise Fail "unimplemented"

  fun inputc s =
      if snull s then NONE
      else let val (c,s') = sdest s in SOME(c,s) end

end; (* structure StreamInstream *)

structure StreamInput = InputFn(StreamInStream);

structure TextFileStream =
struct

  fun mkFileStream filename = 
      let val ins = TextIO.openIn filename
          fun f () = Stream.scons(TextIO.input1 ins, Stream.mkStream f)
       in Stream.mkStream f
      end

end;


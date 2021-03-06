(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * Runs of Turing Machines
 *
 *)


open Symbol
open Alphabet
open Band
open Transition
open Turing_Machine
open Configuration
open Execution
open Emulator

 
let (incr_decr: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
	let band1 = Band.make alphabet [U;U;Z;U] in
	  let tm = Turing_Machine.sequence [ Run Turing_Machine.incr ; Run Turing_Machine.left_most ; Run Turing_Machine.decr ] in
	    let cfg = Configuration.make tm [ band1 ] in
	      Execution.log_run cfg 


let (incr: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
	let band1 = Band.make alphabet [U;U;Z;U] in
	  let cfg = Configuration.make Turing_Machine.incr [ band1 ] in
	    Execution.log_run cfg 
	      

let (decr1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z;D] in
	let band1 = Band.make alphabet [Z;Z;Z;U] in
	  let cfg = Configuration.make Turing_Machine.decr [ band1 ] in
	    Execution.log_run cfg 

let (decr2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z;D] in
	let band1 = Band.make alphabet [Z;Z;Z;U;U] in
	  let cfg = Configuration.make Turing_Machine.decr [ band1 ] in
	    Execution.log_run cfg 
	      

let (gen_dup: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
	let dup = Turing_Machine.generic_dup alphabet.symbols in
	  let band1 = Band.make alphabet [U;Z;Z;U] in
	    let cfg = Configuration.make dup [ band1 ] in
	      Execution.log_run cfg 

let (gen_copy: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
	let copy = Turing_Machine.generic_copy alphabet.symbols in
	  let band1 = Band.make alphabet [Z;U;U;Z] 
	  and band2 = Band.make alphabet [] in
	    let cfg = Configuration.make copy [ band1 ; band2 ] in
	      Execution.log_run cfg 

	      
let (gen_reverse: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
	let reverse = Turing_Machine.generic_reverse alphabet.symbols in	
	  let band1 = Band.make alphabet [U;Z;U;U;Z;Z] 
	  and band2 = Band.make alphabet [] in
	    let cfg = Configuration.make reverse [ band1 ; band2 ] in
	      Execution.log_run cfg 


let (gen_swap: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
	let swap = Turing_Machine.generic_swap alphabet.symbols in	
	  let band1 = Band.make alphabet [U;Z;U;U;Z;Z] in
	    let cfg = Configuration.make swap [ band1 ] in
	      Execution.log_run cfg 

let (xor: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U] in
	let band1 = Band.make alphabet [U;Z;U;U;Z;Z] 
	and band2 = Band.make alphabet [U;Z;U;U;Z;Z] in
	  let cfg = Configuration.make Turing_Machine.xor [ band1 ; band2 ] in
	    Execution.log_run cfg
		

let (busy_beaver: Turing_Machine.t -> Configuration.t) = fun bb ->
      let alphabet = Alphabet.binary in
	let band = Band.make alphabet [] in
	  let cfg = Configuration.make bb [ band ] in
	    Execution.log_run cfg 

(** Test Partie2 projet : B-Reduction *)

(* MT à utiliser
 *
 * let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.good_parenthesis ] in
 *
 * let question2 = Turing_Machine.sequence [ Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.good_remplacement ] in
 *
 * let question3 = Turing_Machine.sequence [ Run Turing_Machine. ; Run Turing_Machine. ] in
 *
 *
 * Demo.q1t11();;
 *
 *)	
(*-------------------------------------------------------------------------------------------------------------------------*)	
(*Recopier terme parenthese *)

(*Cas de base *)
let (q1t11: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;Z;U;X;U;Z;X;U;X;Z;X;Z;X;U;Z;X;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg	

let (q1t12: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;Z;U;X;U;Z;X;U;X;Z;X;Z;X;U;Z;X;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Parentheses imbriquées *)
let (q1t21: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;Z;O;X;Z;U;C;X;U;O;X;U;O;X;Z;C;C;X;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

let (q1t22: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;Z;O;X;Z;U;C;X;U;O;X;U;O;X;Z;C;C;X;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*parenthese pas au debut *) 
let (q1t31: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [X;U;Z;X;U;Z;O;X;Z;Z;X;U;O;X;U;Z;X;Z;C;X;Z;C;] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

let (q1t32: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [X;U;Z;X;U;Z;O;X;Z;Z;X;U;O;X;U;Z;X;Z;C;X;Z;C;] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Test error *)	    
let (q1t41: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [X;U;Z;U;Z;O;X;Z;U;Z;U;O;X;U;Z;X;U;Z;C;X;Z;B;B;B] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

let (q1t42: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [X;U;Z;U;Z;O;X;Z;U;Z;U;O;X;U;Z;X;U;Z;C;X;Z;B;B;B] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg
	    	
(*-------------------------------------------------------------------------------------------------------------------------*)
(*Substitution des variables *)

(*Presente *)
let (q2t1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;X;Z;U;X;U;C;B;B;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.sub ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Variable plus longue presente *)
let (q2t2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;X;Z;U;X;U;Z;U;C;B;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.sub ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Seul variable presente *)
let (q2t3: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;U;X;U;X;U;X;U;C;B;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.sub ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Var plus courte presente *)
let (q2t4: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;X;Z;U;X;U;Z;C;B;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;Z;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.sub ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Variable non présente *)
let (q2t5: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;Z;X;Z;X;Z;U;X;U;Z;C;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.sub ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg
	
(*------------------------------------------------------------------------------------------------------------------------*)
(*Substitution des variables Sans masquage *)

(*Pas de masquage *)
let (q3t1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;X;Z;U;X;U;C;B;B;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg	

(*Masquage *)
let (q3t2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;O;L;X;U;O;X;Z;C;C;C;B;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Pas de masquage *)
let (q3t3: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;O;L;X;Z;U;O;X;U;C;C;C;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Un avec et un sans masquage *)
let (q3t4: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;X;U;X;Z;O;L;X;Z;U;O;X;U;C;C;O;L;X;U;O;X;U;C;C;C;B;B;B] 
	and band2 = Band.make alphabet [O;X;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;X;U;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*-------------------------------------------------------------------------------------------------------------------------*)
(*Beta reduction **)	
(*B reduc normal avec 1 var *)
let (q5t1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;L;X;U;O;X;Z;U;X;U;C;C;O;X;U;X;U;C;B;B;B;B;B;B;B;B;B;B;B] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ; Run Turing_Machine.reduc ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Var a reduire non présent *)
let (q5t2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;L;X;U;O;X;Z;U;Z;X;Z;X;Z;Z;X;U;Z;C;C;O;X;U;U;U;U;C;B;B;B] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ; Run Turing_Machine.reduc ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*Var presente plusieurs fois *)
let (q5t3: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;L;X;U;O;X;U;X;Z;X;Z;Z;X;U;X;U;C;C;O;X;U;U;U;U;C;B;B;B;B] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ; Run Turing_Machine.reduc ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg

(*B reduc avec et sans masquage *)
let (q5t4: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
	let band1 = Band.make alphabet [O;L;X;U;O;L;X;Z;U;O;X;U;C;C;O;L;X;U;O;X;U;C;C;C;O;X;Z;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ; Run Turing_Machine.reduc ; Run Turing_Machine.subL ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg


(*-------------------------------------------------------------------------------------------------------------------------*)
(*Simulation de mt traduite en binaire*)
(*let (q5Sim: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S;U;Z] in
			let code = Binary.build_encoding alphabet in
	let band1 = encode_with code (Band.make alphabet [O;L;X;U;O;X;U;X;U;X;U;X;U;X;U;C;C;O;X;Z;C;B;B;B;B;B;B;B;B] )
	and band2 = encode_with code (Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] )
	and band3 = encode_with code (Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] ) 
	and band4 = encode_with code (Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] ) in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.go_first_O ; Run Turing_Machine.cpy_Bone_Btwo ; Run Turing_Machine.reduc ; Run Turing_Machine.sub ] in
	  (*let question3_trans = Binary.turingSimulator question3 code in
	*)	let cfg = Configuration.make question3_trans (Binary.decode_with code [ band1 ; band2 ; band3 ; band4 ] )in
	    Execution.log_run cfg
*)
(* DEMO *)
	        
let (demo: unit -> unit) = fun () ->
      begin
	print_string "\n\n* DEMO * Demo.ml:\n\n" ;
	List.iter (fun _ -> ())
	  [ 
	  (* incr () ;
	   * decr1 () ;
	   * decr2 () ;    
	   * incr_decr () ;
	   * gen_dup () ;
	   * gen_copy () ;    
	   * gen_reverse () ;
	   * gen_swap () ;
	   * xor () ;
	   * gr2 ()
	   * gp Turing_Machine.cpy_Bone_Bthree
	   *)


           (* busy_beaver Turing_Machine.bb4
	    * /!\  TERMINATING BUT EXTREMLY LONG COMPUTATIONS ... The sun will be dead before the end of BB6.
	    *
	      busy_beaver Turing_Machine.bb5 ;    
	      busy_beaver Turing_Machine.bb6 ;    
	    *)
	  ]
      end


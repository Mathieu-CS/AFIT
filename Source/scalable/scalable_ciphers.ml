(** Ciphers
    bitarrays based ciphers.
*)

open Scalable;;
open Scalable_basic_arithmetics;;
open Scalable_power;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)

(* ------------------------------- *)

let find_cipher_exponant phi =
  let rec exp phi a = if (compare_b (gcd_b phi a) [0; 1]) = 0 then
      a
    else
      exp phi (add_b a [0; 1])
  in exp phi [0; 0; 1] ;;

let find_decipher_exponant a phi =
  let b = bezout_b a phi in
  match b with
    |(x, y, z) -> x ;;
      
(* ------------------------------- *)

let generate_keys_rsa p q = 

  let n = mult_b p q in
  let phi = mult_b (diff_b p [0; 1]) (diff_b q [0; 1]) in
  let a = find_cipher_exponant phi in
  ((n, a), (n, find_decipher_exponant a phi));;
  
(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
*)
  
let encrypt_rsa m (n, e) = mod_power m e n ;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
*)
  
let decrypt_rsa m (n , d) = mod_power m d n ;;

(********** ElGamal Cipher **********)


let rec random x = match x with
  | _ when x > 0 -> (Random.int 2) :: random (x - 1)
  | _ -> 1 :: [];;


(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
*)

let rec length list = match list with
  | e :: l -> 1 + length l
  | [] -> 0;;

let rec find_g p =
  let g =  random ((length p) - 2) in
  if compare_b (mod_power g [0; 0; 1] p) [0; 1] = 0 then
    find_g p
  else
    g ;;
  
let rec public_data_g p = (find_g p, p) ;;

(* gives (g, p) public data *)

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
*)
  
let generate_keys_g (g, p) =
  let a = random ((length p) - 2) in
  (mod_power g a p, a);;

(* (public_key, private_key) *)


(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
*)
  
let encrypt_g msg (g, p) kA =
  let k = random ((length p) - 2) in
  (mod_power g k p, mod_b (mult_b msg (mod_power kA k p)) p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
*)
  
let decrypt_g (msgA, msgB) a (g, p) =
let f = (mod_power msgA a p) in
  let (u, v, z) = bezout_b f p in
  mod_b (mult_b u msgB) p;;


(*

(from_int 100000007, from_int 42)
let (g, p) = public_data_g p 
let (pub, priv) = generate_keys_g (g, p) 
let (g_k, xA_k) = encrypt_g msg (g, p) pub 

let rec find_g p =
  let g = Random.int (500) in
  if (mod_power g 2 p) = 1 then
    find_g p
  else
    g ;;

let rec public_data_g p = (find_g p, p);;

let generate_keys_g (g, p) =
  let a = Random.int (500) in
  (mod_power g a p, a);;

  
let encrypt_g msg (g, p) kA =
  let k = (500) in
(mod_power g k p, modulo (msg * (mod_power kA k p)) p);;

let decrypt_g (msgA, msgB) a (g, p) =
  let f = (mod_power msgA a p) in
  let (u, v, z) = bezout f p in
  modulo (u * msgB) p;;

*)

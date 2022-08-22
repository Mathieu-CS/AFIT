(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
*)

let rec encrypt_cesar k m b = match m with
  | e :: l -> if e + k > b then
      (e + k - b) :: encrypt_cesar k l b
    else
      (e + k) :: encrypt_cesar k l b
  | [] -> [] ;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
*)

let rec decrypt_cesar k m b = match m with
  | e :: l -> if e - k < 0 then
      (e - k + b) :: decrypt_cesar k l b
    else
      (e - k) :: decrypt_cesar k l b
  | [] -> [] ;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)

(* ------------------------------- *)

let find_cipher_exponant phi =
  let rec exp phi a = if (gcd a phi) = 1 then
      a
    else
      exp phi (a + 1)
  in exp phi 2 ;;

let find_decipher_exponant a phi =
  let b = bezout a phi in
  match b with
    |(x, y, z) -> x ;;
      
(* ------------------------------- *)

let generate_keys_rsa p q =
  let n = p * q in
  let phi = (p - 1) * (q - 1) in
  let a = find_cipher_exponant phi in
  ((n, a), (n, find_decipher_exponant a phi));;


(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
*)
  
let encrypt_rsa m (n, e) = mod_power m e n ;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
*)
  
let decrypt_rsa m (n , d) = mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
*)

let rec find_g p =
  let g = Random.int ((p - 1) + 1) in
  if (prime_mod_power g 2 p) = 1 then
    find_g p
  else
    g ;;

let rec public_data_g p = (find_g p, p);;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
*)

let generate_keys_g (g, p) =
  let a = Random.int ((p - 1) + 1) in
  (prime_mod_power g a p, a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
*)
  
let encrypt_g msg (g, p) kA =
  let k = Random.int ((p - 1) + 1) in
(prime_mod_power g k p, modulo (msg * (prime_mod_power kA k p)) p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

let decrypt_g (msgA, msgB) a (g, p) =
  let f = (prime_mod_power msgA a p) in
  let (u, v, z) = bezout f p in
  modulo (u * msgB) p;;

(*

let p = 10000007;;
let msg = 42;;

let c = public_data_g p ;;
let (pub, priv) = generate_keys_g c;; 
let u = encrypt_g msg c pub;;
let x = decrypt_g u priv c;;


*)
(*

let p = 100000007;;

let a = public_data_g p;;

generate_keys_g (a);;

*)

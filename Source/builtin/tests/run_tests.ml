open Alcotest
open Test_builtin
open Test_builtin_basic_arithmetics
open Test_builtin_power
open Test_builtin_test_primes
open Test_builtin_generate_primes
open Test_builtin_encoding_msg
open Test_builtin_ciphers
open Test_builtin_break_ciphers

let () =
    run "Builtin functions"
    [("builtin",            builtin_set);
     ("basic_arithmetics",  basic_arithmetics_set);
     ("power",              power_set);
     ("test_primes",        test_primes_set);
     ("generate_primes",    generate_primes_set);
     ("encoding_msg",       encoding_msg_set);
     ("ciphers",            ciphers_set);
     ("break_ciphers",      break_ciphers_set)]

let sec_of_time (a,b,c) = 
  let sec_heure = a * 60 * 60 in
  let sec_min = b * 60 in
  let seconde = sec_heure + sec_min + c in
  seconde

let valeur = (23,13,59)  
let result = sec_of_time valeur 

let () = Printf.printf "le resultat pour 23h 13min 59sec est : %d\n" result

let time_of_sec x =

  let fp_heure = float_of_int x /. 3600. in
  let heure = x / 3600 in
  let fp_minute = (fp_heure -. float_of_int heure) *. 60. in
  let minute = int_of_float fp_minute in 
  let fp_seconde = (fp_minute -. float_of_int minute) *. 60. in
  let seconde = int_of_float fp_seconde in
  (heure , minute, seconde)

let test = 83639
let (result1 , result2, result3) = time_of_sec  test

let () = Printf.printf "la convertion de %d est %dh, %dmin, %dsec\n"test result1 result2 result3

let add_time (a,b,c) (x,y,z) =
  let v1 = sec_of_time (a,b,c) in
  let v2 = sec_of_time (x,y,z) in 
  let v3 = v1 + v2 in
  let (h,m,s) = time_of_sec v3 in
  (h,m,s)

let (h,m,s) = add_time (12,22,15) (15,13,10)

let () = Printf.printf "l'addition de 12h22mn15sec et 15h13mn10sec est %dh, %dmin, %dsec\n" h m s


(*3.1*)

(* -: int = 7 *)

let addv2 a b =
  if a >= b then
    (a - b) + (a / b)
  else 
    (b - a) + (b / a)

let f a b c = 
  (if a > b then
    if b > c then a + b else c + a
  else 
    if a > c then a + b else b + c) *
  (if a > b then
    if b > c then a - b else a - c
  else
    if a > c then a - b else b - c)



let fv2 a b c = 
  (if a > b then
    if b > c then (a + b) * (a - b) else (c + a) * (a - c)
  else 
    if a > c then (a + b) * (a - b) else (b + c) * (b - c))

let all_even a b c d =
  if a mod 2 = 0 then
    if b mod 2 = 0 then
      if c mod 2 = 0 then
        if d mod 2 = 0 then
          true
        else 
          false
      else 
        false
    else 
      false
  else 
    false 

let all_evenv2 a b c d =
  if a mod 2 = 0 && b mod 2 = 0 && c mod 2 = 0 && d mod 2 = 0 then
    true
  else
    false 


let et_logique a b =
  if a then
    if b then
      true
    else
      false
  else 
    false

let ou_logique a b =
  if a then
    true
  else
    if b then
      true
    else
      false 


let non_a_ou_b a b =
  if a then
    if b then 
      true
    else 
      false
  else
    true 

let xor x y = 
  let ou_logique a b =
    if a then
      true
    else
      if b then
        true
      else
        false 
  in
  let a_et_non_b a b =
    if b then
      false
    else
      if a then
        true
      else 
        false
  in
  
  let resultat = ou_logique (a_et_non_b x y) (a_et_non_b y x) in
  resultat

let r1 = xor true true
let () = Printf.printf "xor true,true = %b\n" r1

let r2 = xor true false
let () = Printf.printf "xor true,false = %b\n" r2

let r3 = xor false true
let () = Printf.printf "xor false,true = %b\n" r3

let r4 = xor false false
let () = Printf.printf "xor false,false= %b\n" r4


let congru x y =
  let congruence = et_logique (non_a_ou_b x y) (non_a_ou_b y x) in
  congruence

let c1 = congru true true
let () = Printf.printf "congru true,true = %b\n" c1

let c2 = congru true false
let () = Printf.printf "congru true,false = %b\n" c2

let c3 = congru false true
let () = Printf.printf "congru false,true = %b\n" c3

let c4 = congru false false
let () = Printf.printf "congru false,false= %b\n" c4
  
  
  







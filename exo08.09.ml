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
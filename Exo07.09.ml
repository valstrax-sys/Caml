let mirror x =
  (x mod 10) * 10 + (x / 10)


let val1 = 16
let result = mirror val1

let () = Printf.printf "result mirror de %d est : %d\n" val1 result 


let abba x = 
  x * 100 + mirror x


let result2 = abba val1

let () = Printf.printf "result abba de %d est : %d\n" val1 result2 

let stammer x =
  ((abba x * 100) + mirror x ) * 100 + x  

let result3 = stammer val1

let () = Printf.printf "result stammer de %d est : %d\n" val1 result3 

let rec power31 x n =
  if n = 0 then
    x
  else
    x * power31 x (n - 1)


let val2 = 2 
let result4 = power31 val2 31

let () = Printf.printf "result power31 de %d est : %d\n" val2 result4
    

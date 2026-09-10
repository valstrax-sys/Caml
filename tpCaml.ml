(*Soit un entier positif à 3 chiffres myst ayant les propriétés suivantes :
 myst est un carré parfait 1 ;
 la somme des carrés des deux plus grands chiffres de myst est également un carré parfait.
Écrire la fonction Caml mystery : int -> bool qui détermine si un entier donné est le nombre mystère myst.
La fonction devra déclencher une exception si le paramètre n'est pas un nombre positif à trois chiffres.*)

let myst nombre_ut =
  let est_carree_parfait nb_verifier =
    let sqrt_nombre = int_of_float(sqrt(float_of_int(nb_verifier))) in 
    if (sqrt_nombre * sqrt_nombre) = nb_verifier then
      true
    else 
      false 
  in 

  let decompose nb_a_decomposer = 
    let unite = nb_a_decomposer mod 10 in
    let dizaine = (nb_a_decomposer mod 100) / 10 in 
    let centaine = nb_a_decomposer / 100 in 
    (unite, dizaine, centaine)
  in

  let plus_grand (a, b , c) =
    if a > b then
      if b > c then 
        (a, b)
      else 
        (a, c)
    else 
      if a > c then
        (a, b)
      else 
        (b, c)
  in  
  
  let verification x = 
    if est_carree_parfait x then 
      let (a, b) = plus_grand(decompose(x)) in
      if  est_carree_parfait ((a * a)+(b * b)) then
        true
      else 
        false
    else 
      false
  in 
  
  verification nombre_ut

let test = 324
let result = myst test
let () = Printf.printf "le nombre %d est un carrée parfait ? %b\n" test result



(*Dans l'école, il existe un coffre fort qui contient tous les sujets d'examens. Le code C est un constitué de 4
chiffre tels que :
 les chiffre de C sont pairs ;
 la somme des 4 chiffre de C est 10 ;
 le dernier chiffre de C est strictement supérieur à 5 ;
 en ajoutant à C son "miroir" (4321 est le miroir de 1234), on obtient un nombre palindrome à 4 chiffre :
un entier identique à son "miroir" (par exemple 1991) ;
 C − 1 est un carré parfait
Écrire une fonction Caml qui détermine si un entier donné en paramètre satisfait toutes ces conditions.
La fonction devra déclencher une exception si le paramètre n'est pas valide (n'est pas positif et/ou ne contient
pas quatre chiffre !) *)

let crack_the_Code nb_utilisateur =
  if nb_utilisateur < 1000 || nb_utilisateur > 9999 then
  invalid_arg "Out of range"
  else
    let est_paire nombre =
      if nombre mod 2 = 0 then
        true
      else 
        false
    in

    let decompose nombre_decompose =
      let unite = nombre_decompose mod 10 in
      let dizaine = (nombre_decompose mod 100) / 10 in 
      let centaine = (nombre_decompose mod 1000) / 100 in 
      let milier = nombre_decompose / 1000 in
      (unite, dizaine, centaine, milier)
    in

    let mirror (unite, dizaine, centaine, milier) =
      let inverse = (unite * 1000) + (dizaine * 100) + (centaine * 10) + (milier) in
      inverse
    in

    let est_carree_parfait nb_verifier =
      let sqrt_nombre = int_of_float(sqrt(float_of_int(nb_verifier))) in 
      if (sqrt_nombre * sqrt_nombre) = nb_verifier then
        true
      else 
        false 
    in 

    let verificationCode nombre_code =
      let (unite, dizaine, centaine, milier) = decompose nombre_code in
      if unite <= 5 then
        false
      else
        if ((unite + dizaine + centaine + milier) = 10) && est_paire nombre_code then
          let mirror_utilisateur = mirror (unite, dizaine, centaine, milier) in
          let pré_palindrome = mirror_utilisateur + nombre_code in
          if pré_palindrome = mirror(decompose(pré_palindrome)) then 
            if est_carree_parfait (nombre_code - 1) then
              true
            else 
              false
          else
            false
        else
          false
    in
  
  verificationCode nb_utilisateur

let test_code = 2026
let result_code = crack_the_Code test_code
let () = Printf.printf "le nombre %d est le code ? %b\n" test_code result_code


let surface nombre_ut =
  if nombre_ut < 0 then
    invalid_arg "surface : negative"
  else

    let calcul nombre =
      let reste7 = nombre mod 7 in
      let resultat7 = (nombre / 7) - (7 * reste7) in

      let reste11 = resultat7 mod 11 in
      let resultat11 = (resultat7 / 11) - (11 * reste11) in

      let reste13 = resultat11 mod 13 in
      let resultat13 = (resultat11 / 13) - (13 * reste13) in

      resultat13
    in

    let verification nombre_surface =
      let nombre_restant = nombre_surface - 5000 in

      if nombre_restant < 100 || nombre_restant > 999 then
        false
      else
        let nombre_double = (nombre_restant * 1000) + nombre_restant in

        if calcul nombre_double = 555 then
          true
        else
          false
    in

    verification nombre_ut

let test_surface = 5555
let result_surface = surface test_surface
let () = Printf.printf "la surface %d correspond-elle ? %b\n" test_surface result_surface



let rms a b =
  ((a**2. +. b**2.)/. 2.)**1./.2.;;

let _=assert(rms 2. 2. = 2.)
let _=assert(rms 6. 8. = 25.)
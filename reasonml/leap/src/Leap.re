let isLeapYear = fun
  | (year) when year mod 400 === 0 => true
  | (year) when year mod 100 === 0 => false
  | (year) when year mod 4   === 0 => true
  | (_)                            => false
  ;

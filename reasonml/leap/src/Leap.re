let isLeapYear = fun
  | (year) when year mod 4 === 0 && (year mod 100 !== 0 || year mod 400 === 0) => true
  | (_) => false
  ;

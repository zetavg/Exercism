type planet =
  | Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Neptune
  | Uranus;

let ageOn = (planet, seconds) =>
  switch (planet, seconds) {
  | (Mercury, seconds) => seconds /. (31557600.0 *. 0.2408467)
  | (Venus,   seconds) => seconds /. (31557600.0 *. 0.61519726)
  | (Earth,   seconds) => seconds /. (31557600.0 *. 1.0)
  | (Mars,    seconds) => seconds /. (31557600.0 *. 1.8808158)
  | (Jupiter, seconds) => seconds /. (31557600.0 *. 11.862615)
  | (Saturn,  seconds) => seconds /. (31557600.0 *. 29.447498)
  | (Neptune, seconds) => seconds /. (31557600.0 *. 164.79132)
  | (Uranus,  seconds) => seconds /. (31557600.0 *. 84.016846)
  };

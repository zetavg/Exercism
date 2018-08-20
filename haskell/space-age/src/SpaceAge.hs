module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds / (31557600.0 * 0.2408467)
ageOn Venus   seconds = seconds / (31557600.0 * 0.61519726)
ageOn Earth   seconds = seconds / (31557600.0 * 1.0)
ageOn Mars    seconds = seconds / (31557600.0 * 1.8808158)
ageOn Jupiter seconds = seconds / (31557600.0 * 11.862615)
ageOn Saturn  seconds = seconds / (31557600.0 * 29.447498)
ageOn Uranus  seconds = seconds / (31557600.0 * 84.016846)
ageOn Neptune seconds = seconds / (31557600.0 * 164.79132)

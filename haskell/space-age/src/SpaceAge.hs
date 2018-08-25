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
ageOn Mercury seconds = yearsOfSecondsOnEarth seconds / 0.2408467
ageOn Venus   seconds = yearsOfSecondsOnEarth seconds / 0.61519726
ageOn Earth   seconds = yearsOfSecondsOnEarth seconds
ageOn Mars    seconds = yearsOfSecondsOnEarth seconds / 1.8808158
ageOn Jupiter seconds = yearsOfSecondsOnEarth seconds / 11.862615
ageOn Saturn  seconds = yearsOfSecondsOnEarth seconds / 29.447498
ageOn Uranus  seconds = yearsOfSecondsOnEarth seconds / 84.016846
ageOn Neptune seconds = yearsOfSecondsOnEarth seconds / 164.79132

yearsOfSecondsOnEarth :: Float -> Float
yearsOfSecondsOnEarth seconds = seconds / 31557600.0

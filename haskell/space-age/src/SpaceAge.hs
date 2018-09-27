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
ageOn planet seconds = yearsOfSecondsOnEarth seconds / orbitalPeriodOfEarthYears planet

yearsOfSecondsOnEarth :: Float -> Float
yearsOfSecondsOnEarth seconds = seconds / 31557600.0

orbitalPeriodOfEarthYears :: Planet -> Float
orbitalPeriodOfEarthYears Earth   = 1
orbitalPeriodOfEarthYears Mercury = 0.2408467
orbitalPeriodOfEarthYears Venus   = 0.61519726
orbitalPeriodOfEarthYears Mars    = 1.8808158
orbitalPeriodOfEarthYears Jupiter = 11.862615
orbitalPeriodOfEarthYears Saturn  = 29.447498
orbitalPeriodOfEarthYears Uranus  = 84.016846
orbitalPeriodOfEarthYears Neptune = 164.79132

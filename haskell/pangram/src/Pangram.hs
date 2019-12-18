module Pangram (isPangram) where
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram = isPangram' initialCharTable

isPangram' :: CharTable -> String -> Bool
isPangram' _ [] = False
isPangram' charTable (x:xs) =
  let charTable' = setCharTable (toLower x) charTable
  in charTableFull charTable' || isPangram' charTable' xs

type CharTable = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

initialCharTable :: CharTable
initialCharTable = (False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False)

charTableFull :: CharTable -> Bool
charTableFull (True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True) = True
charTableFull _ = False

setCharTable :: Char -> CharTable -> CharTable
setCharTable 'a' (False, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (True, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'b' (a, False, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, True, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'c' (a, b, False, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, True, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'd' (a, b, c, False, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, True, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'e' (a, b, c, d, False, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, True, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'f' (a, b, c, d, e, False, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, True, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'g' (a, b, c, d, e, f, False, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, True, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'h' (a, b, c, d, e, f, g, False, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, True, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'i' (a, b, c, d, e, f, g, h, False, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, True, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'j' (a, b, c, d, e, f, g, h, i, False, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, True, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'k' (a, b, c, d, e, f, g, h, i, j, False, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, True, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'l' (a, b, c, d, e, f, g, h, i, j, k, False, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, True, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'm' (a, b, c, d, e, f, g, h, i, j, k, l, False, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, True, n, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'n' (a, b, c, d, e, f, g, h, i, j, k, l, m, False, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, True, o, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'o' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, False, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, True, p, q, r, s, t, u, v, w, x, y, z)
setCharTable 'p' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, False, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, True, q, r, s, t, u, v, w, x, y, z)
setCharTable 'q' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, False, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, True, r, s, t, u, v, w, x, y, z)
setCharTable 'r' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, False, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, True, s, t, u, v, w, x, y, z)
setCharTable 's' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, False, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, True, t, u, v, w, x, y, z)
setCharTable 't' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, False, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, True, u, v, w, x, y, z)
setCharTable 'u' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, False, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, True, v, w, x, y, z)
setCharTable 'v' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, False, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, True, w, x, y, z)
setCharTable 'w' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, False, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, True, x, y, z)
setCharTable 'x' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, False, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, True, y, z)
setCharTable 'y' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, False, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, True, z)
setCharTable 'z' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, False) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, True)
setCharTable _ table = table


doubleMe x = x + x


doubleUs x y = doubleMe x + doubleMe y


doubleSmallNumber x = if x > 100
    then x
    else x*2


length' xs = sum [1 | _ <- xs]


removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]


test xxs = [
    [ x | x <- xs, even x]
    | xs <- xxs]


rightTriangles x = [ (a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
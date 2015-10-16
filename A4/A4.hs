-- Name : Jinpeiqi
-- ID : 12207549
----------------------EXERCISE 1----------------

-------a)---------
time ::Int->(Int,Int,Int)
time s =(s`div`3600,(s`mod`3600)`div`60,s-((s`mod`3600)`div`60)*60-(s`div`3600)*3600)

-------b)---------
type Point = (Int, Int)
type Line = (Int, Int, Int)
onLine :: Point -> Line -> Bool
onLine p l =  0 == a * x + b * y + c
  where a = item0 l
        b = item1 l
        c = item2 l
        x = fst p
        y = snd p
        item0 (a, _, _) = a
        item1 (_, a, _) = a
        item2 (_, _, a) = a


----------------------EXERCISE 2----------------

-------a)---------
type Fraction=(Int,Int)
powFr ::Fraction->Int->String
powFr (a,b) c
	|c==0 =(show 1)
	|a==0 =(show 0)
	|b==0 =(show "Enter valid fraction number")
	|otherwise =(show (a^c`div`(gcd (a^c) (b^c))))++"/"++(show (b^c`div`(gcd (a^c) (b^c))))
	
-------b)---------
(%%) :: Fraction->Fraction->Bool
(a,b) %% (c,d) = ((a^3`div`(gcd (a^3) (b^3))),(b^3`div`(gcd (a^3) (b^3))))==((d^3`div`(gcd (c^3) (d^3))),(c^3`div`(gcd (c^3) (d^3))))


----------------------EXERCISE 3----------------

-------a)---------
prod ::[Int]->[Int]->Int
prod [] [] = 1
prod (head1:tail1) (head2:tail2)
	|length (head1:tail1) /= length (head2:tail2) = error"List have different sizes"
	|head1 `mod` head2 ==0 =head1*prod tail1 tail2
	|otherwise =prod tail1 tail2

-------b)---------
removeItem ::Int->[Int]->[Int]
removeItem _ [] = []
removeItem x (head1:tail1) 
	| x == head1    = removeItem x tail1
	| otherwise = head1 : removeItem x tail1

magic ::[Int]->(Int,[Int])
magic a =((minimum a),removeItem (minimum a) a)
-------c)---------	
total ::[[Int]]->Int
total a = sum (concat a)


----------------------EXERCISE 4----------------
-------a)---------
sOdd1 :: [Int] -> [Int]
sOdd1 []      = []
sOdd1 [_]     = []
sOdd1 (x:xs)  = a ++ sOdd1 xs
  where a = if odd x then [head xs] else []
  
-------b)---------
--sOdd1 ::[Int]->[Int]
sOdd2 []=[]
sOdd2 (head1:tail1)
	|even head1 == True =sOdd2 tail1
	|otherwise = head1+1: sOdd2 tail1
-------c)---------
sOdd3 ::[Int]->[Int]
sOdd3 a = [x+1 | x<-a,odd x ==True]



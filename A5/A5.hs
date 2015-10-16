-- Name : Jinpeiqi
-- ID : 12207549
import DVDdata
----------------------EXERCISE 1----------------
-------------a-----------
hasD :: Int -> Int -> Bool
hasD n d
  | n<0 = error "n is non-negative"
  | d<0||d>9   =   error "d is between 0 to 9"
  | (n-d)`mod`10==0||n-d==0 = True
  | n<1 = False
  |otherwise = hasD(n`div`10) d

-------------b-----------
count ::[Int]->Int->Int
count [] _ = 0
count(x:xs) d 
	|hasD x d==True = 1 + count xs d
	|hasD x d==False = count xs d

-------------c-----------
countC :: [Int] -> Int -> Int
countC xs d=length[x|x<-xs,hasD x d]


----------------------EXERCISE 2----------------
-------------a-----------
takeOut::Int->[Int]->[Int]
takeOut _ [] = []
takeOut m (x:xs)
	|m==x = takeOut m xs
	|otherwise = x:takeOut m xs
-------------b-----------
takeOutB ::Int->[Int]->[Int]
takeOutB m xs=[x|x<-xs,x/=m]
-------------c-----------
triples::(Int,Int,Int)->Bool
triples (a,b,c)
	|a+b==c = True
	|otherwise = False
	
count3r::[(Int,Int,Int)]->Int
count3r []=0
count3r (x:xs)
	|triples x = 1 + count3r xs
	|otherwise = count3r xs
-------------d-----------	
get1 (x,_,_)=x
get2 (_,x,_)=x
get3 (_,_,x)=x

count3L::[(Int,Int,Int)]->Int
count3L xs = length[get1 x+get2 x==get3 x|x<-xs]


----------------------EXERCISE 3----------------

take1 (x,_,_,_) = x
take2 (_,x,_,_) = x
take3 (_,_,x,_) = x
take4 (_,_,_,x) = x
getTotal:: [DVD]->Int
getTotal []=0
getTotal (x:xs)
	|take3 x == "YES" =1+getTotal xs
	|otherwise = getTotal xs

showb::[DVD]->IO()
showb [] = return()
showb (x:xs) = do 
			print ((take1 x)++"        "++(take2 x)++"        "++(take3 x)++"        "++show ((take4 x)*5))
			showb xs

prinTable::[DVD]->IO()
prinTable x = do
		putStrLn("             Welecome to the DVD hoppy Doo Shop")
		putStrLn("DVD Id               Title            Available    payment")
		showb x
		print("              DVD in stock:                 "++show(getTotal x))
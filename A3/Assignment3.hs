-- Name : Jinpeiqi
-- ID : 12207549
----------------------EXERCISE 1----------------


-------a)---------
--59702569

-------b)---------
--1690

-------c)---------
--2
--FUNTHOMAS
--False

----------------------EXERCISE 2----------------

-------a)---------
sum2a :: Int -> Int
sum2a n = (n*(n+1)`div`2)^2

-------b)---------
sum2b :: Int -> Int
sum2b n
	|n==1      =1
	|otherwise =sum2b(n-1)+n^3

-------c)---------
sum2c :: Int -> Int
sum2c n = if(n==1)  then 1
			else sum2c(n-1)+n^3

--------d)--------
--The sum2a will give us the best run-time.
--Because sum2a only need to run one times,sum2b and sum2c doing recursion,they may call itself more than one times. 


----------------------EXERCISE 3----------------

-------a)---------
f :: Char -> Int
f n  
	|n=='0'  =0
	|n=='1'  =1
	|n=='2'  =2
	|n=='3'  =3
	|n=='4'  =4
	|n=='5'  =5
	|n=='6'  =6
	|n=='7'  =7
	|n=='8'  =8
	|n=='9'  =9
	|n=='a'||n=='A'  =10
	|n=='b'||n=='B'  =11
	|n=='c'||n=='C'  =12
	|n=='d'||n=='D'  =13
	|n=='e'||n=='E'  =14
	|n=='f'||n=='F'  =15
	|otherwise = error "input is not hexadecimal"
	  
	  
-------b)---------
netEarning :: Float -> Float
netEarning n
	|n<=14000 =n-(n*0.105)
	|n>14000&&n<=48000 = n - (1470+(n-14000)*0.175)
	|n>48000&&n<=70000 = n - (1470+5950+(n-48000)*0.3)
	|otherwise = n-(1470+5950+6600+(n-70000)*0.33)
	
	
----------------------EXERCISE 4----------------
-------a)---------
gcdD ::Int -> Int -> Int
gcdD x y
	|x==y =x
	|x>y = gcdD (x-y) y
	|otherwise = gcdD x (y-x)

-------b)---------

pow :: Integer -> Integer -> Integer
pow n k
  | 0 == k        = 1
  | 0 == mod k 2  = pow (n * n) (div k 2)
  | 1 == mod k 2  = n * (pow n (k - 1))

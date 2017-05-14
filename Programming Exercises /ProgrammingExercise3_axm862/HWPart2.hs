-- Aditya Malik
-- HW3 PLC 

--First definte the list type for elements and nested sublists
data NestList a = Element a | SubList [NestList a] deriving (Show)

--To flatten the list, first make the function definition
flatten :: [NestList a] -> [NestList a] 
--Then we return an empty list if empty
flatten [] = []
--Then if there is a single element, return that element 
flatten [Element a] = [Element a] 
--Then if there is a sublist then recursively flatten sublist
flatten [SubList a] = (flatten a) 
--And append the last element to the front 
flatten l = (flatten [(head l)]) ++ (flatten (tail l)) 

--Now to reserve, first make the function definition
myReverse :: [NestList a] -> [NestList a] 
--Then we return an empty list if empty 
myReverse [] = []
--Then if there is a single element, return that element
myReverse [Element a] = [Element a] 
--Then if there is a sublist, resively call on it 
myReverse [SubList a] = [SubList (myReverse a)] 
--And lastly append tail before head 
myReverse ls = (myReverse (tail ls)) ++ (myReverse [(head ls)])

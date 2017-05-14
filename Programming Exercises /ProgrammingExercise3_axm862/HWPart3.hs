-- Aditya Malik
-- HW3 PLC 

--First definte the function
myfunc :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]

myfunc val list testfunc = do
  ls <- list 
  if (testfunc val) then return (val : ls) 
    else Nothing
  
--Now for checklist, with base case returning if not given 
checklist [] testfunc = Just []
checklist ls testfunc = (myfunc (head ls) (checklist (tail ls) testfunc) testfunc)

checkappend ls1 ls2 testfunc = do
  list1 <- ls1 
  list2 <- ls2 
  --if "Nothing" then simply do nothing, else append
  if (checklist list1 testfunc) == Nothing then Nothing
    else return (list1 ++ list2) 
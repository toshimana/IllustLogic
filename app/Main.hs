import Lib

inputdata :: [String] -> ([[Int]],[[Int]])
inputdata cs = (rc, cc)
    where
      rc = Prelude.map g rowStr 
      cc = Prelude.map g colStr
      g = Prelude.map read . words
      (rowStr,colStr) = let (a,b) = break (\s -> s == "") cs in (a,tail b)

main = do
    cs <- getContents
    let (rc, cc) = inputdata $ lines cs
    let f = sum.concat
    if f rc /= f cc then print "Input Data Failed."
    else do
        results <- solveIllustLogic rc cc
        print "Success!"
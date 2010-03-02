cornerSplit,rightSplit,upSplit,squareLimit:: Painter -> Int -> Painter
cornerSplit _ 0 = blank
cornerSplit p (n+1) = (topLeft </> p) <-> (corner </> bottomRight)
   where up = upSplit p n
         right = rightSplit p n
         topLeft = up <-> up
         bottomRight = right </> right
         corner = cornerSplit p n

rightSplit _ 0 = blank
rightSplit p (n+1) = p <-> (smaller </> smaller)
    where smaller = rightSplit p n

upSplit _ 0 = blank
upSplit p (n+1) = (smaller <-> smaller) </> p
    where smaller = upSplit p n

squareLimit p n = half </> (flipVert half)
   where half = (flipHoriz quarter) <-> quarter
         quarter = cornerSplit p n
main = squareLimit wave 3 unitSquare

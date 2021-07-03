========================
SUDOKU SOLVER IN HASKELL
========================

This sudoku solver is my first attempt at implementing a simple Haskell
program. It follows Graham Hutton's tutorial, [available at
YouTube](https://www.youtube.com/watch?v=glog9DZh8G0). For those interested in
reproducing Graham's solution, I recommend looking at the video first and then
trying to implement the solution on their own.

Good luck!


=====
TYPES
=====

Let's start by defining the types we'll use throughout the program.

> type Grid = Matrix Value
>
> type Matrix a = [Row a]
>
> type Row a = [a]
>
> type Value = Char


===============
SUDOKU EXAMPLES
===============

This first puzzle is super easy. It can be solved by the first solver we'll be
implementing.

> grid :: Grid
> grid = ["1234","5678","9abc","defg"]

> easy :: Grid
> easy = ["2....1.38",
>         "........5",
>         ".7...6...",
>         ".......13",
>         ".981..257",
>         "31....8..",
>         "9..8...2.",
>         ".5..69784",
>         "4..25...."]


================
HELPER FUNCTIONS
================

We'll start by implementing some helper functions, and then we'll move to the
solver(s).

The original tutorial uses some built-in functions like `Data.List.transpose`.
Since this is an exercise to get used to Haskell, I imported none. Instead, I
decided I'd be implementing them myself. You'll find my implementations at the
end of this file.


Part 1. Checking if a sudoku is valid
-------------------------------------

This function returns a list of rows. Each element corresponds to a row in the
original matrix. Given the type definitions we're using, the function is
equivalent to the identity function.

> rows :: Matrix a -> [Row a]
> rows = id


This function returns a list of columns. Each element in the result is a column
from the original matrix. In other words, it's equivalent to transposing a
matrix.

> cols :: Matrix a -> [Row a]
> cols = transpose


This function returns a list of boxes. Each element in the result is a box from
the original matrix. This means that, for this function to work correctly, it's
expected that the matrix is a square whose side is `x^2`. For example, a
regular Sudoku is a 9 by 9 matrix, which means its side is `3^2 = 9`.

> boxs :: Matrix a -> [Row a]
> boxs m = unpack . map cols . pack $ m
>          where
>              pack   = split . map split
>              unpack = map concat . concat
>              split  = chunksOf (boxSize m)


This function returns the box size of a given Sudoku matrix. For instance, in a
regular 9x9 Sudoku matrix, its box size is 3.

> boxSize :: Matrix a -> Int
> boxSize = floor . sqrt . fromIntegral . length . head


Notice that all these functions have an interesting property: composing each
function with itself results in identity:

`rows . rows = id`
`cols . cols = id`
`boxs . boxs = id`


Finally, we'll implement a helper function that determines if a Sudoku grid is
valid or not. A Sudoku grid is valid if it doesn't contain duplicates on any of
its rows, columns, or boxes:

> valid :: Grid -> Bool
> valid g = all nodups (rows g) &&
>           all nodups (cols g) &&
>           all nodups (boxs g)

where `nodups` is defined as:

> nodups :: Eq a => [a] -> Bool
> nodups []     = True
> nodups (x:xs) = not (x `elem` xs) && nodups xs



===========================================
CUSTOM IMPLEMENTATION OF BUILT-IN FUNCTIONS
===========================================

This function flips a matrix by its diagonal. Its implementation has three
possible cases:

1. If we're given an empty list, there's nothing to transpose so... let's
   return the empty list.

2. If there's at least one row, but the row has no items (i.e. it looks like
   there aren't any columns), there's nothing to transpose either.

3. Finally we get somewhere interesting: the first row has at least one item
   `x`. In this case, we have to do two things:

	 a) First, we create a list with the "first" column. To do so, we append to
      `x` the head of all the other rows `xss`.
   b) Second, we have to transpose all the other columns. To do so, we create
      a new matrix by removing the first element from the first row (i.e. we
      use `xs`) and we get the tails of all the other rows `xss`.

> transpose :: [[a]] -> [[a]]
> transpose []              = []
> transpose ([] : _)        = []
> transpose ((x:xs) : xss)  = column : columns
>           where
>               column  = (x : [ h | (h:_) <- xss ])
>               columns = transpose (xs : [ t | (_:t) <- xss ])


This function splits an array into chunks of the given size:

> chunksOf :: Int -> [a] -> [[a]]
> chunksOf _ [] = []
> chunksOf s x = take s x : chunksOf s (drop s x)


This function intersperses the given element in a list of elements:

> intercalate :: [a] -> [[a]] -> [a]
> intercalate _ []       = []
> intercalate _ (x:[])   = x
> intercalate i (x:y:xs) = x ++ i ++ intercalate i (y:xs)


======================
OTHER HELPER FUNCTIONS
======================

The following function will beautifully print a Sudoku on the terminal.

> sudoku :: Grid -> IO ()
> sudoku g = mapM_ putStrLn lines
>        where
>           bs     = boxSize g
>           rowSep = intercalate "┼" $ replicate bs (replicate bs '─')
>           rows   = map (intercalate "│" . chunksOf bs) g
>           lines  = intercalate [rowSep] . chunksOf bs $ rows

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


=================
BASIC DEFINITIONS
=================

> boxsize :: Int
> boxsize = 3
>
> values :: [Value]
> values = ['1'..'9']
>
> single :: [a] -> Bool
> single [_] = True
> single _   = False


===============
SUDOKU EXAMPLES
===============

Solvable only using the basic rules:

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

First gentle example from sudoku.org.uk:

> gentle :: Grid
> gentle = [".1.42...5",
>           "..2.71.39",
>           ".......4.",
>           "2.71....6",
>           "....4....",
>           "6....74.3",
>           ".7.......",
>           "12.73.5..",
>           "3...82.7."]

> diabolical :: Grid
> diabolical = [".9.7..86.",
>               ".31..5.2.",
>               "8.6......",
>               "..7.5...6",
>               "...3.7...",
>               "5...1.7..",
>               "......1.9",
>               ".2.6..35.",
>               ".54..8.7."]

First "unsolvable" (requires backtracking) example:

> unsolvable :: Grid
> unsolvable = ["1..9.7..3",
>               ".8.....7.",
>               "..9...6..",
>               "..72.94..",
>               "41.....95",
>               "..85.43..",
>               "..3...7..",
>               ".5.....4.",
>               "2..8.6..9"]

Minimal sized grid (17 values) with a unique solution:

> minimal :: Grid
> minimal = [".98......",
>            "....7....",
>            "....15...",
>            "1........",
>            "...2....9",
>            "...9.6.82",
>            ".......3.",
>            "5.1......",
>            "...4...2."]

Empty grid:

> blank :: Grid
> blank = replicate n (replicate n '.')
>         where n = boxsize ^ 2

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
>              split  = chunksOf boxsize


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



=======
SOLVERS
=======

Let's now implement the different Sudoku solvers presented by Graham in his
course.


1. Simplest Sudoku solver possible
----------------------------------

The first solver is extremely simple and naive:

> solve1 :: Grid -> [Grid]
> solve1 = filter valid . collapse . choices


Let's take a closer look at each helper function. `choices` simply "fills the
gaps" with a list of all possible numbers (1 to 9). Its result is a matrix
where each cell is either the list of numbers 1 to 9 (if the cell was empty) or
a list with a single number (i.e. the number that was already there
originally).

> type Choices = [Value]
>
> choices :: Grid -> Matrix Choices
> choices = map (map choice)
>           where
>               choice '.' = values
>               choice v   = [v]


`collapse` generates all the possible Grids by "flattening" the Choices into a
single value.

> collapse :: Matrix [a] -> [Matrix a]
> collapse = cp . map cp

where `cp` is a helper function that implements the cartesian product:

> cp :: [[a]] -> [[a]]
> cp []       = [[]]
> cp (xs:xss) = [ h : ts | h <- xs, ts <- cp xss ]


2. Pruning choices before collapsing
------------------------------------

The previous solver doesn't work because, well, the number of possible
combinations we're generating is huge. So let's tweak the solver in a
way such that it's able to _prune_ invalid Choices:

> solve2 :: Grid -> [Grid]
> solve2 = filter valid . collapse . fix prune . choices


The previous solver uses two new helper functions: `fix` and `prune`.
`fix` is quite simple: it repeats the same function over and over again
until the result it gets is equal to its input:

> fix :: Eq a => (a -> a) -> a -> a
> fix f x = if x == x' then x else fix f x'
>           where x' = f x


`prune`, on the other hand, gets rid of choices that are already set in
our matrix. The idea is also quite simple: we have to remove single
values from a list of multiple choices when said value appears in the
same row, column, or box as the cell with multiple choices:

> prune :: Matrix Choices -> Matrix Choices
> prune = pruneBy rows . pruneBy cols . pruneBy boxs
>         where
>             pruneBy f = f . map reduce . f
>
> reduce :: Row Choices -> Row Choices
> reduce rows = [ row `without` singles | row <- rows ]
>               where singles = concat (filter single rows)
>
> without :: Choices -> Choices -> Choices
> a `without` b = if single a then a else [ x | x <- a, not (x `elem` b) ]


3. Expanding choices one at a time
----------------------------------

The problem with the previous algorithm is that it generates the cartesian
product of all choices. This results in an explosion of possible grids, most
of which won't be valid. To overcome this issue, the idea is to expand the
multiple choices one at each step and disregard any (partial) grid that we
already know that will fail.

Instead of collapsing all the choices and filtering out the invalid ones,
this final solver only explores the search space of grids that look
promising:

> solve3 :: Grid -> [Grid]
> solve3 = search . prune . choices


Let's take a look at `search`:

> search :: Matrix Choices -> [Grid]
> search m | blocked m     = []
>          | collapsable m = collapse m
>          | otherwise     = [ g | m' <- expand m,
>                                  g  <- search (prune m') ]

As you can see, `search` defines three possible scenarios:

1. If the current (partially-expanded) matrix can't lead to a valid Sudoku
solution, we abort the search and return the empty list.

2. If the current matrix doesn't have any multiple choices, we've found a
solution and we can collapse it.

3. Otherwise, we expand the first cell and keep searching.


Let's now define the three helper functions `search` uses.

`collapsable` checks if a matrix of choices contains single-valued choices
only. If it does, the matrix can be converted into a final Sudok Grid:

> collapsable :: Matrix Choices -> Bool
> collapsable = all (all single)


`expand` assumes the matrix of choices has at least one multiple-choice
cell and it simply expands it into _n_ matrices of choices:

> expand :: Matrix Choices -> [Matrix Choices]
> expand m = [ brs ++ [ bcs ++ [c] : acs ] ++ ars | c <- choices ]
>            where
>                multi              = not . single
>                (brs, row:ars)     = break (any multi) m
>                (bcs, choices:acs) = break multi row


Finally, `blocked` checks if a matrix of choices can't produce a final Sudoku
Grid and, therefore, it doesn't deserve further expanding. A matrix of choices
won't result in a successful Sudoku Grid if it contains empty cells or if
single options aren't consistent (as defined in the Sudoku game):

> blocked :: Matrix Choices -> Bool
> blocked m = incomplete || invalid
>             where
>                 incomplete = any (any null) m
>                 invalid    = any inconsistent (rows m) ||
>                              any inconsistent (cols m) ||
>                              any inconsistent (boxs m)


where `inconsistent` looks for duplicate single choices on a row:

> inconsistent :: Row Choices -> Bool
> inconsistent = dups . concat . filter single
>                where dups = not . nodups


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
>           rowSep = intercalate "┼" $ replicate boxsize (replicate boxsize '─')
>           rows   = map (intercalate "│" . chunksOf boxsize) g
>           lines  = intercalate [rowSep] . chunksOf boxsize $ rows

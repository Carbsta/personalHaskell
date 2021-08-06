{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         PPUtilities                                          *
*       Purpose:        Pretty-printing utilities                            *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2012                  *
*                                                                            *
******************************************************************************
-}

-- | Pretty-printing utilities.

module PPUtilities (
    ppName,     -- :: Name -> ShowS
    ppSrcPos,   -- :: SrcPos -> ShowS
    ppOpt,      -- :: Int -> (Int -> a -> ShowS) -> Maybe a -> ShowS
    ppSeq,      -- :: Int -> (Int -> a -> ShowS) -> [a] -> ShowS
    ppTpl,      -- :: Int -> ((Int -> a -> ShowS, Int -> b -> ShowS)) -> (a,b) -> ShowS
    ppSeqPairs, -- :: Int -> (Int -> a -> ShowS, Int -> b -> ShowS) -> [(a,b)] -> ShowS
    indent,     -- :: Int -> ShowS
    nl,         -- :: ShowS
    spc         -- :: ShowS
) where

-- HMTC module imports
import Name (Name)
import SrcPos (SrcPos)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Pretty-prints a name.
ppName :: Name -> ShowS
ppName n = showChar '\"' . showString n . showChar '\"'


-- | Pretty-prints a source code position.
ppSrcPos :: SrcPos -> ShowS
ppSrcPos sp = showChar '<' . showString (show sp) . showChar '>'


-- | Pretty-prints an optional item. Arguments:
--
-- (1) Indentation level.
--
-- (2) Pretty-printing function for the item.
--
-- (3) The optional item to print.
ppOpt :: Int -> (Int -> a -> ShowS) -> Maybe a -> ShowS
ppOpt _ _  Nothing  = id
ppOpt n pp (Just x) = pp n x


-- | Pretty-prints a sequence of items. Arguments:
--
-- (1) Indentation level.
--
-- (2) Pretty-printing function for each item.
--
-- (3) Sequence of items to print.
ppSeq :: Int -> (Int -> a -> ShowS) -> [a] -> ShowS
ppSeq _ _  []     = id
ppSeq n pp (x:xs) = pp n x . ppSeq n pp xs

-- | Pretty-prints a tuple of items. Arguments:
--
-- (1) Indentation level.
--
-- (2) Pretty-printing function for each item.
--
-- (3) Tuple of items to print
ppTpl :: Int -> ((Int -> a -> ShowS, Int -> b -> ShowS)) -> (a,b) -> ShowS
ppTpl n (ppl,ppr) (l,r) = ppl n l . ppr n r

-- | Pretty-prints a sequence of pairs. Arguments:
--
-- (1) Indentation level.
--
-- (2) Pair of pretty printing functions.
--
-- (4) Sequence of pairs to print.
ppSeqPairs :: Int -> (Int -> a -> ShowS, Int -> b -> ShowS) -> [(a,b)] -> ShowS
ppSeqPairs _ _ []      = id
ppSeqPairs n pp (x:xs) = ppTpl n pp x . ppSeqPairs n pp xs


-- | Indent to specified level by printing spaces.
indent :: Int -> ShowS
indent n = showString (take (2 * n) (repeat ' '))


-- | Start a new line.
nl :: ShowS
nl  = showChar '\n'


-- | Print a space.
spc :: ShowS
spc = showChar ' '

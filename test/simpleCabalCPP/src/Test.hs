{-# LANGUAGE CPP #-}
module Test (exported) where

#ifdef INSIDE
-- | <<diagrams/blah.svg#diagram=blah>>
-- 
-- > blah = pad 1.1 $ square 1
--
#endif

-- | <<diagrams/d'.svg#diagram=d'>>
-- 
-- > d' = pad 1.1 $ circle 1
--

exported = putStrLn "blah"

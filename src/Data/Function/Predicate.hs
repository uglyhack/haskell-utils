module Data.Function.Predicate
  where

-- |The 'fromPredicates' function builds a predicate from a list of predicates
--  and uses the function 'f' to fold the results.
fromPredicates :: ([Bool] -> Bool) -- ^Function 'f' is used to fold the results.
               -> [a -> Bool]      -- ^List of predicates 'ps', that form the
                                   --  resulting predicate function
               -> (a -> Bool)
fromPredicates f ps = \x -> f $ map ($ x) ps


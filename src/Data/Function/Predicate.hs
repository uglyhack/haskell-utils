module Data.Function.Predicate
  where

-- |'fromMany' builds a predicate from a list of predicates.
fromMany :: ([Bool] -> Bool) -- ^Function 'f' is used to fold the results.
         -> [a -> Bool]      -- ^List of predicates 'ps', that form the
                             --  resulting predicate function
         -> (a -> Bool)
fromMany f ps = \x -> f $ map ($ x) ps


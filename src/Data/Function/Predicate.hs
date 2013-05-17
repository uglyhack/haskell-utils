module Data.Function.Predicate
  where

-- |'fromMany' builds a predicate from a list of predicates.
fromMany :: ([Bool] -> Bool) -- ^Function 'f' is used to fold the results.
         -> [a -> Bool]      -- ^List of predicates 'ps', that form the
                             --  resulting predicate function
         -> (a -> Bool)
fromMany f ps = \x -> f $ map ($ x) ps

-- |'fromValues' builds a predicate from a relation and a list of values.
fromValues :: ([Bool] -> Bool) -- ^'f' is used to fold the results.
           -> (a -> a -> Bool) -- ^'p' realtion
           -> [a]              -- ^'xs' values
           -> (a -> Bool)
fromValues f p = fromMany f . map (p $) 

module Metro (
               Candidate (..)
             , metropolisHastings
             ) where

import System.Random.MWC --This requires that the mwc-random
import Control.Monad
import Control.Monad.Primitive
import Debug.Trace

-- A function that takes two states and one parameter and
-- calculates the quotient between the scores of the two
-- states given the parameter.
type ScoreFunc s p = s -> s -> p -> Double

--  A function that generates a new random candidate
-- given the current state of a Markov chain.
type CandFunc s m = s -> m (Candidate s)

-- A structure containing the candidate state, the
-- probability of getting this state given the previous
-- state and the probability of getting the previous state
-- given this state.
data Candidate s = Candidate { candidate :: s
                             , pthere :: Double
                             , pback :: Double
                             }

metropolisHastings :: (PrimMonad m) =>
                   ScoreFunc s p ->
                   CandFunc s m ->
                   Gen (PrimState m) ->
                   s ->
                   [p] ->
                   m [s]
metropolisHastings scoref candf g init params = foldM f [init] params
    where
        f marx t = do
            let state = head marx
            cand <- candf state
            let candstate = candidate cand
            let a = min 1.0 ((pback cand / pthere cand) * scoref candstate state t)
            let b = traceShow a a
            if a>=1
                then return (candstate:[]) --(candstate:marx)
                else do
                    u <- uniformR (0.0, 1.0) g
                    if u <= a
                        then return (candstate:[]) -- (candstate:marx)
                        else return marx

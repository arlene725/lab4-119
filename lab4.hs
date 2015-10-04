--
-- lab4.hs
-- Implementing (deterministic) finite state machines in Haskell
--

import Data.List

-- Finite state machines where the states are of type a. For this lab, this
-- will be Int, but in the future we will want to construct (e.g.) the Union
-- machine, where the states will be pairs of states.
--             Q,  q0,  F,  delta
type FSM a = ([a], a,  [a], [(a,Char,a)])

-- Some helper functions for extracting the parts of a machine (although 
-- usually it will be better to use pattern matching for this).
states :: FSM a -> [a]
states (qs,_,_,_) = qs

start :: FSM a -> a
start (_,q0,_,_) = q0

finals :: FSM a -> [a]
finals (_,_,fs,_) = fs

delta :: FSM a -> [(a,Char,a)]
delta (_,_,_,ds) = ds

-------------------------------------------------------------------------------
-- Begin lab

-- lookup M q a
-- In a general finite state machine, the transition function can actually 
-- return a *set* of states. For a DFA, this set will always have size 1, but
-- we'll need the general version later, so we write it now. `lookup M q a`
-- returns the list of states that we would move to if we were in state q, and
-- we read a character `a`. Thus, lookup will need to search through the 
-- delta component of the machine for all triples that have `q` as its first 
-- element and `a` as its second, and then return the list of all their
-- third elements. 
lookupAll :: (Eq a) => (a,Char) -> [(a,Char,a)] -> [a]
lookupAll (q,a) dlt = [q1' |(q1, a, q1')<- dlt, q1==q, a==a]


-- Checks whether a finite state machine (qs, s, fs, ts) is correct/complete:
-- (1) States qs are unique (no duplicates, qs must be a set)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition relation is a function from qs and sigma to qs
checkFSM :: (Ord a, Eq a) => [Char] -> FSM a -> Bool
checkFSM sigma fsm = state_qs(states fsm) && start_state (start fsm) (states fsm) && state_subset (finals fsm) (states fsm)
 -- && transition fsm

proper:: Ord a=> [a] -> Bool
proper [] = True
proper (l:ls) = and[l `notElem` ls, proper ls]

state_qs::Ord a=> [a]-> Bool
state_qs as = proper as

start_state:: Eq a=> a-> [a]-> Bool
start_state a bs = (a `elem` bs)

subset :: Eq a=> [a]->[a]->Bool
subset [] bs= True
subset as []= False
subset (a:as) bs= a `elem` bs && subset as bs 

state_subset:: Eq a => [a]->[a]-> Bool
state_subset as bs = subset as bs 

--transition:: Eq a=> FSM a ->Bool
--transition fsm = and [(qs `elem` ts) && (qs' `elem` ts) && not(null qs) |(qs, a, qs') <- delta fsm , ts <- states fsm]

-- dlt M q a == q'
-- dlt implements the delta function of M: given state q and input symbol a, 
-- it returns the new state q'.
dlt :: (Eq a) => FSM a -> a -> Char -> a
dlt fsm q a = 

-- deltaStar M q s
-- deltaStar implements the delta* function, which returns the state the machine
-- will be in after processing an entire string. 
deltaStar :: (Eq a) => FSM a -> a -> String -> a
deltaStar fsm q s = undefined

-- accept1 M s
-- accept1 defines string acceptance using deltaStar.
accept1 :: (Eq a) => FSM a -> String -> Bool
accept1 fsm s = undefined

-- accept2 M s
-- accept2 defines string acceptance using the Lq(M) construct: starting the
-- machine in an arbitrary state q, instead of just its start state.
accept2 :: (Eq a) => FSM a -> String -> Bool
accept2 fsm@(qs,st,_,_) s = accept_q fsm st s
  where
    accept_q :: (Eq a) => FSM a -> a -> String -> Bool
    accept_q fsm q s = undefined

----------------------------------- Part 2 -------------------------------------

-- Implement the following machines, and test them using accept1 and accept2
-- using some acceptable strings (the `strings` function is again provided
-- for you, if you want to use it):
--
-- * The machine that accepts all strings in (ab)*
-- * The machine that accepts all strings in (ab)(ab)* (i.e., (ab)+)
-- * The machine over sigma=['0','1'] that accepts strings which, when 
--   interpreted as base-2 numbers, are divisible by 3. Assume that the 
--   empty string is equivalent to 0. 
--
-- Use Ints for your states.

machine_ab_star :: FSM Int
machine_ab_star = ([undefined],  -- List of states
                   undefined,    -- Start state
                   [undefined],  -- list of final states
                   [(undefined,undefined,undefined)]) -- delta function

machine_ab_plus :: FSM Int
machine_ab_plus = undefined

machine_div_3 :: FSM Int
machine_div_3 = undefined

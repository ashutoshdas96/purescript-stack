module Test.Main where

import Prelude

import Data.List
import Data.Maybe
import Data.Stack
import Data.Tuple

import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

main :: Effect Unit
main = do
  log "stackNew"
  assert $ (stackNew :: Stack Int) == (mempty :: Stack Int)

  log "stackPush"
  let stack1 = stackPush stackNew 1
  assert $ stack1 == Stack 1 (1 : Nil)
  let stack2 = stackPush stack1 2
  assert $ stack2  == Stack 2 (2 : 1 : Nil)
  let x = 3
  assert $ stackPop (stackPush stack2 x) == Just (Tuple stack2 x)
  assert $ stackPop (stackPush stack1 x) == Just (Tuple stack1 x)
  assert $ stackPop (stackPush stackNew x) == Just (Tuple stackNew x)

  log "stackPeek"
  assert $ (stackPeek stackNew :: Maybe Int) == (Nothing :: Maybe Int)
  assert $ stackPeek (stackPush stack2 x) == Just x
  assert $ stackPeek stack2 == map snd (stackPop stack2)

  log "stackPop"
  assert $ (stackPop stackNew :: Maybe (Tuple (Stack Int) Int)) == (Nothing :: Maybe (Tuple (Stack Int) Int))
  assert $ stackPop (stackPush stack2 x) == Just (Tuple stack2 x)

  log "stackIsEmpty"
  assert $ stackIsEmpty stackNew
  assert $ stackIsEmpty (stackPush stack2 x) == false
  assert $ if (stackSize stack2 == 0) then stackIsEmpty stack2 else true
  assert $ if (stackSize stackNew == 0) then stackIsEmpty stackNew else true

  log "stackSize"
  assert $ stackSize stackNew == 0
  let n = stackSize stack2
  assert $ stackSize (stackPush stack2 x) == n + 1

-- | Stack data structure and associated operations
-- |
-- | A stack is a basic data structure that can be logically thought as linear structure represented by a real physical stack or pile, a structure where insertion and deletion of items takes place at one end called top of the stack.
-- |
-- | In other words, a 'Stack' is an abstract data type that serves as a collection of elements, with two principal operations: 'stackPush', which adds an element to the collection, and 'stackPop', which removes the most recently added element that was not yet removed.
-- |
-- | <<https://upload.wikimedia.org/wikipedia/commons/b/b4/Lifo_stack.png>>
-- |
-- | See also <https://en.wikipedia.org/wiki/Stack_(abstract_data_type)>
module Data.Stack
  ( Stack(..)
  , stackNew
  , stackPush
  , stackPeek
  , stackPop
  , stackIsEmpty
  , stackSize
  ) where

import Prelude

import Data.Eq (class Eq1, eq1)
import Data.List (List(..), head, intercalate, reverse, uncons, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

-- | Abstract Stack data type
data Stack a = Stack Int (List a)


instance showStack :: Show a => Show (Stack a) where
  show (Stack _ Nil) = "Empty"
  show (Stack sz items) = "Size: " <> show sz <> "\n Stack: (Nil : " <> intercalate " : " (reverse $ show <$> items) <> ")"

instance eqStack :: Eq a => Eq (Stack a) where
  eq = eq1

instance eq1Stack :: Eq1 Stack where
  eq1 (Stack sz1 items1) (Stack sz2 items2) = sz1 == sz2 && items1 == items2


instance semigroupStack :: Semigroup (Stack a) where
  append (Stack sz1 items1) (Stack sz2 items2) = Stack (sz1 + sz2) (items1 <> items2)


instance monoidStack :: Monoid (Stack a) where
  mempty = Stack 0 Nil


-- | /O(1)/. Create new empty Stack
stackNew :: forall a. Stack a
stackNew = Stack 0 Nil

-- | /O(1)/. Push item onto Stack
-- |
-- | > (∀x)(∀s)(stackPop (stackPush s x) == Just (Tuple s x))
stackPush :: forall a. Stack a -> a -> Stack a
stackPush (Stack sz items) item = Stack (sz + 1) (item : items)

-- | /O(1)/. Pop most recently added item without removing from the Stack
-- |
-- | > stackPeek stackNew == Nothing
-- | > (∀x)(∀s)(stackPeek (stackPush s x) == Just x)
-- | > (∀s)(stackPeek s == fmap snd (stackPop s))
stackPeek :: forall a. Stack a -> Maybe a
stackPeek (Stack _ Nil) = Nothing
stackPeek (Stack _ items) = head items

-- | /O(1)/. Pop most recently added item from Stack
-- |
-- | > stackPop stackNew == Nothing
-- | > (∀x)(∀s)(stackPop (stackPush s x) == Just (Tuple s x))
stackPop :: forall a. Stack a -> Maybe (Tuple (Stack a) a)
stackPop (Stack sz items) =
  maybe
    Nothing
    (\a -> Just $ Tuple (Stack (sz - 1) (a.tail)) a.head)
    (uncons items)

-- | /O(1)/. Test if stack is empty
-- |
-- | > stackIsEmpty stackNew == true
-- | > (∀x)(∀s)(stackIsEmpty (stackPush s x) == false)
-- | > (∀s)((stackSize s == 0) ⇔ (stackIsEmpty s == true))
stackIsEmpty :: forall a. Stack a -> Boolean
stackIsEmpty (Stack _ Nil) = true
stackIsEmpty (Stack _ _)  = false

-- | /O(1)/. Compute number of elements contained in the Stack
-- |
-- | > stackSize stackNew == 0
-- | > (∀x)(∀s)((stackSize s == n) ⇒ (stackSize (stackPush s x) == n+1))
stackSize :: forall a. Stack a -> Int
stackSize (Stack sz _) = sz

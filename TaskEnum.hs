module TaskEnum(twoTasks) where

import Data.List as L

import Syntax

twoTasks =
  L.map (\(t1, t2) -> [t1, t2]) $ cartProd (taskOptions "task_A") (taskOptions "task_B")

taskOptions taskName =
  (task taskName []):(L.map (\rr -> task taskName [rr]) $ allRRs dummyLR "FIELD_A")

allRRs lr f =
  L.map (\(p, c) -> regionRequirement dummyLR [f] p c) accessSettings

dummyLR = logicalRegion "r1" (indexSpace "i1" 0 0) (fieldSpace "f1" ["FIELD_A"])

accessSettings = cartProd privileges coherences

privileges = [RW, RO]
coherences = [ATOMIC, SIMULTANEOUS, EXCLUSIVE]

cartProd xs ys = [(x, y) | x <- xs, y <- ys]

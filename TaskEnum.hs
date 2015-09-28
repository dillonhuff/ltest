module TaskEnum(basicTasks) where

import Data.List as L

import Imperative

basicTasks = [testCase "basicCase" ["FIELD_X"] [topLevelTask, ta, tb]]

topLevelTask = task "top_level_task" []

ta = task "task_A" []
tb = task "task_B" []

cartProd xs ys = [(x, y) | x <- xs, y <- ys]

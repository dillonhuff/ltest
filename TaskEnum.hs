module TaskEnum(basicTasks) where

import Data.List as L

import Imperative

basicTasks = [[topLevelTask, ta, tb]]

topLevelTask = task "top_level_task" []

ta = task "task_A" []
tb = task "task_B" []

cartProd xs ys = [(x, y) | x <- xs, y <- ys]

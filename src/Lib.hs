{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( tasks
    ) where
import Task1 (task1)
import Task3 (task3)

tasks :: IO ()
tasks = do
  task1
  task3

module Lib (
    tasks,
) where

import Task1 (task1)
import Task2 (task2)
import Task3 (task3)
import Task4 (task4)

tasks :: IO ()
tasks = do
    task1
    task2
    task3
    task4

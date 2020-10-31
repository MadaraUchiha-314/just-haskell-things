module Sorting.Quicksort (
    quicksort
) where

quicksort :: (Ord item) => [item] -> [item]
quicksort [] = []
quicksort [item] = [item]
quicksort list = quicksort(firstPartition) ++ [pivot] ++ quicksort(secondPartition) where 
    pivot = last list
    firstPartition = [item | item <- list, item < pivot]
    secondPartition = [item | item <- list, item > pivot]
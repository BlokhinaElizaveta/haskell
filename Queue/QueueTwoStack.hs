module QueueTwoStack where
    import Queue
    data QueueTwoStack a = Record [a] [a]
    instance Queue QueueTwoStack where
        push x (Record [] []) = Record [x] []
        push x (Record [] pushStack) = push x (Record (reverse pushStack) [])
        push x (Record popStack pushStack) = Record popStack (x:pushStack)

        pop (Record [] pushStack) = pop (Record (reverse pushStack) [])
        pop (Record popStack pushStack) = Record (tail popStack) pushStack

        top (Record popStack pushStack) = head popStack

        empty (Record popStack pushStack) = null pushStack && null popStack

        create x = Record [x] []

        getLen (Record popStack pushStack) = length (popStack ++ pushStack)

    instance Show a => Show (QueueTwoStack a) where
        show (Record popStack pushStack) = show (popStack ++ (reverse pushStack))
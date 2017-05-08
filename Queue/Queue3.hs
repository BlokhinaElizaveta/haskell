module Queue3 where
    import Queue
    data Queue3 a = Record [a] [a] Int Int
    instance Queue Queue3 where
        push x (Record [] [] 0 0) = Record [x] [] 1 0
        push x (Record popStack pushStack popLen pushLen) =  if (popLen + 1) < pushLen
                                                             then push x (Record (popStack ++ (reverse pushStack)) [] (popLen + pushLen) 0)
                                                             else Record popStack (x:pushStack) popLen (pushLen + 1)

        pop (Record popStack pushStack popLen pushLen) = if (popLen + 1) < pushLen
                                                         then pop (Record (popStack ++ (reverse pushStack)) [] (popLen + pushLen) 0)
                                                         else Record (tail popStack) pushStack (popLen - 1) pushLen

        top (Record popStack pushStack popLen pushLen) = head popStack

        empty (Record popStack pushStack popLen pushLen) = null pushStack && null popStack

        create x = Record [x] [] 1 0

        getLen (Record popStack pushStack popLen pushLen) = popLen + pushLen

    instance Show a => Show (Queue3 a) where
        show (Record popStack pushStack popLen pushLen) = show (popStack ++ (reverse pushStack))


module Main where
    import Queue
    import SimpleQueue
    import QueueTwoStack
    import Queue3
    import System.CPUTime

    test q = do
        start <- getCPUTime
        let t1 = foldl(\v x-> push x v) q [1..10000]
        print $ extr t1
        end <- getCPUTime
        print $ fromIntegral(end - start) / 10^12

    extr q = if empty q then 0
             else (1 + (extr (pop q)))

    main = do
        let first :: SimpleQueue Int; first = create 1
        test first
        let second :: QueueTwoStack Int; second = create 1
        test second
        let third :: Queue3 Int; third = create 1
        test third
module SimpleQueue where --реализуем интерфейс очереди
    import Queue
    data SimpleQueue a = Record [a]
    instance Queue SimpleQueue where
        push x (Record v) = Record (v ++ [x])
        pop (Record v) = Record (tail v)
        top (Record v) = head v
        empty (Record v) = null v --встроенная функция проверяет пустой ли список
        create x = Record [x]
        getLen (Record v) = length v

    instance Show a => Show (SimpleQueue a) where
        show (Record v) = show v

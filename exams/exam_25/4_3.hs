import Log

insert :: LogEntry -> MessageTree -> MessageTree
insert (Unknown message) tree = tree
insert entry Empty = Node entry Empty Empty
insert (LogMessage er time message) (Node (LogMessage er1 time1 message1) left right)
    | time >= time1 = Node (LogMessage er1 time1 message1) left (insert (LogMessage er time message) right)
    | time < time1 = Node (LogMessage er1 time1 message1) (insert (LogMessage er time message) left) right

build :: [LogEntry] -> MessageTree    -- construir uma árvore ordenada
build = foldr insert Empty

inOrder :: MessageTree -> [LogEntry]
inOrder Empty = []
inOrder (Node entry Empty Empty) = [entry]
inOrder (Node entry Empty right) = entry : inOrder right
inOrder (Node entry left right) = inOrder left ++ [entry] ++ inOrder right

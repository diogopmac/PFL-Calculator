import Log

insert :: LogEntry -> MessageTree -> MessageTree
insert (Unknown message) tree = tree 
insert entry Empty = Node entry Empty Empty
insert (LogMessage er time message) (Node (LogMessage er1 time1 message1) left right)
    | time >= time1 = Node (LogMessage er1 time1 message1) left (insert (LogMessage er time message) right)
    | time < time1 = Node (LogMessage er1 time1 message1) (insert (LogMessage er time message) left) right
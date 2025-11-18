import Log

parseMessage :: String -> LogEntry
parseMessage (x : x1 : xs)
  | x == 'E' && x1 == ' ' = parseMessageError xs
  | x == 'I' && x1 == ' ' = LogMessage Info (read time) (unwords message)
  | x == 'W' && x1 == ' ' = LogMessage Warning (read time) (unwords message)
  | otherwise = Unknown (x:x1:xs) 
  where
      (time : message) = words xs

parseMessageError :: String -> LogEntry
parseMessageError log = LogMessage (Error (read typ)) (read num) (unwords message)
    where (typ:num:message) = words log
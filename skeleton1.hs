type Command = String
type Message = String
data State = ...

state0 :: State
state0 = ...

step :: State -> Command -> (Message, Maybe State)
step ... ... = ...

main :: IO ()
main = loop $ Just state0
  where loop Nothing = return ()
        loop (Just s) = do c <- getLine
                           let (m, ms) = step s c
                           putStrLn m
                           loop ms
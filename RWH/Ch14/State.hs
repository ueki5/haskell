module Ch14.State where
newtype State s a = State {
      runState :: s -> (a, s)
    }
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)
-- bindState :: State s a -> (a -> State s b) -> State s b
-- bindState step makeStep =  \oldState -> 
--     let (result, newState) = runState step $ oldState
--     in State (runState (makeStep result) $ newState)
bindState :: State s a -> (a -> State s b) -> State s b
bindState step makeStep = State $ \oldState ->
    let (result, newState) = runState step $ oldState
    in runState (makeStep result) newState
get :: State s s
get = State $ \s -> (s, s)
put :: s -> State s ()
put s = State $ \_ -> ((), s)

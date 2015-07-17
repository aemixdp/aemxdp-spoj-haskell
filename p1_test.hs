{-# LANGUAGE LambdaCase #-}

main = readLn >>= \case
    42 -> return ()
    x  -> print x >> main
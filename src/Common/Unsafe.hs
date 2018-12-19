module Common.Unsafe where


right :: Either a b -> b
right (Right b) = b

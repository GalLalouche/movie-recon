{-
  Copyright © 2014 Christian Marie
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

      1. Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.

      2. Redistributions in binary form must reproduce the above
         copyright notice, this list of conditions and the following
         disclaimer in the documentation and/or other materials provided
         with the distribution.

      3. Neither the name of the project nor the names of its contributors
         may be used to endorse or promote products derived from this
         software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

-- |
-- Module      : Flippers
-- Copyright   : (c) Christian Marie 2014
-- License     : BSD3
--
-- Maintainer  : christian@ponies.io
-- Stability   : experimental
-- Portability : portable
--
-- This module provides functions to rearrange arguments, such as rotate4 and
-- flip4. Compose them to achive whatever ordering you like.
--
module Common.Flippers
(
    -- * Rotation
    -- | Rotate all arguments right one position
    rotate1,
    rotate2,
    rotate3,
    rotate4,
    rotate5,
    rotate6,
    rotate7,
    rotate8,
    rotate9,
    -- * Lotation = Left rotation
    -- | Rotate all arguments left one position
    lotate1,
    lotate2,
    lotate3,
--    lotate4,
--    lotate5,
--    lotate6,
--    lotate7,
--    lotate8,
--    lotate9,
    -- * Flipping
    -- | Reverse the order of all arguments
    flip1,
    flip2,
    flip3,
    flip4,
    flip5,
    flip6,
    flip7,
    flip8,
    flip9,
) where


-- | Does nothing, 'id'
rotate1 :: (a -> b) -> (a -> b)
rotate1 = id

-- | Does nothing, 'id'
lotate1 :: (a -> b) -> (a -> b)
lotate1 = id

-- | Does nothing, 'id'
flip1 :: (a -> b) -> (a -> b)
flip1 = id

-- | Move the second argument to the first place
rotate2 :: (a -> b -> c) -> (b -> a -> c)
rotate2 = flip

-- | Move the second argument to the first place
lotate2 :: (a -> b -> c) -> (b -> a -> c)
lotate2 = flip

-- | Reverse both arguments, same as 'rotate2'
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 = flip

-- | Move the third argument to the first place
rotate3 :: (a -> b -> c -> d) -> (c -> a -> b -> d)
rotate3 = flip . (flip .)

-- | Move the third argument to the first place
lotate3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
lotate3 = flip . flip3

-- | Reverse three arguments
flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 = rotate3 . flip2

-- | Move the fourth argument to the first place
rotate4 :: (a -> b -> c -> d -> e) -> (d -> a -> b -> c -> e)
rotate4 = flip . (rotate3 .)

-- | Reverse four arguments
flip4 :: (a -> b -> c -> d -> e) -> (d -> c -> b -> a -> e)
flip4 = rotate4 . flip3

-- | Move the fifth argument to the first place
rotate5 :: (a -> b -> c -> d -> e -> f) -> (e -> a -> b -> c -> d -> f)
rotate5 = flip . (rotate4 .)

-- | Reverse five arguments
flip5 :: (a -> b -> c -> d -> e -> f) -> (e -> d -> c -> b -> a -> f)
flip5 = rotate5 . flip4

-- | Move the sixth argument to the first place
rotate6 :: (a -> b -> c -> d -> e -> f -> g) ->
         (f -> a -> b -> c -> d -> e -> g)
rotate6 = flip . (rotate5 .)

-- | Reverse six arguments
flip6 :: (a -> b -> c -> d -> e -> f -> g) ->
           (f -> e -> d -> c -> b -> a -> g)
flip6 = rotate6 . flip5

-- | Move the seventh argument to the first place
rotate7 :: (a -> b -> c -> d -> e -> f -> g -> h) ->
         (g -> a -> b -> c -> d -> e -> f -> h)
rotate7 = flip . (rotate6 .)

-- | Reverse seven arguments
flip7 :: (a -> b -> c -> d -> e -> f -> g -> h) ->
           (g -> f -> e -> d -> c -> b -> a -> h)
flip7 = rotate7 . flip6

-- | Move the eight argument to the first place
rotate8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) ->
         (h -> a -> b -> c -> d -> e -> f -> g -> i)
rotate8 = flip . (rotate7 .)

-- | Reverse eight arguments
flip8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) ->
           (h -> g -> f -> e -> d -> c -> b -> a -> i)
flip8 = rotate8 . flip7

-- | Move the ninth argument to the first place
rotate9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) ->
         (i -> a -> b -> c -> d -> e -> f -> g -> h -> j)
rotate9 = flip . (rotate8 .)

-- | Reverse nine arguments
flip9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) ->
           (i -> h -> g -> f -> e -> d -> c -> b -> a -> j)
flip9 = rotate9 . flip8
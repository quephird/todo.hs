{-# LANGUAGE OverloadedStrings #-}

module Todo.Views.Index where

import Text.Blaze.Html5

render = do
  html $ do
    body $ do
      h1 "My todo list"
      ul $ do
        li "learn haskell"
        li "make a website"
        li "PROFIT1!!"

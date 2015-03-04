## Description

The todo app seems to have become the canonical beginner exercise for building a Web/database application regardless of programming language or choice of framework(s). This implementation is based on a very helpful blog post here: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html. There are a few sigificant differences here, namely that I tried separating concerns instead of keeping everything in  Main.hs. Also, since the post was written there were a fair amount of changes necessary in the code due to flux in the underlying libraries.

## Getting it running

This project requires cabal; you can find instructions on how to install it here: https://www.haskell.org/cabal/download.html

Download the project to a local directory:

    git clone https://github.com/quephird/todo.hs

... move into that directory and run the following:

    cabal sandbox init
    cabal install

If all goes well, issue the following to start the Web server:

    ./.cabal-sandbox/bin/todo-hs

## License

Copyright (C) 2015, ⅅ₳ℕⅈⅇℒℒⅇ Ƙⅇℱℱoℜⅆ Distributed under the Eclipse Public License.
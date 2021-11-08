# fprog_test_suit

Here one can find student written tests for the lecture ''Functional Programming'' at Vienna University of Technology.

## What do i need to install?

For the tests `tasty` and `tasty-hunit` are used. According to the lecture one can either install [stack](https://docs.haskellstack.org/en/stable/README/) or [cabal](https://www.haskell.org/cabal/) to execute the tests.

## Where do I put the tests?

At first you need to clone this repository or just download the files. Then you need to put the files into the `./test` folder of the project template. If you don't use the template, have a look at (See ''Lies-mich-Dateien/Fprog.pdf'' (WS2021)).

In the project template you can insert the tests in the `./test` folder, so the folder would look like: 
```
./test
  testSuite1.hs
  testSuite2.hs
  ...
```

## Do the tests guarantee that I get the full points?

Nope - The tests are written by students for students. 

## Can I add my own tests?

Yes, you can fork the project and add your own tests and then create a pull request, that your tests are merged into this repository OR write me on mattermost (@thinklex).

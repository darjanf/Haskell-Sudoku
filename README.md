# Sudoku Game written in Haskell (created within the EMURGO Academy Haskell Course: Part I)

This repository contains a preconfigured Haskell development environment, allowing you to play the game Sudoku using [Gitpod](https://www.gitpod.io/) and a browser-based version of VS Code. Alternatively you can play the game in your local environment.

## Preview of the game
![Sudoku](https://user-images.githubusercontent.com/116302019/227886853-316fa935-7e09-4cfe-aca2-5a7104627326.jpg)

## Scope of the Sudoku game
1. The program generates a complete, random and valid Sudoku board, using the backtracking algorithm.
2. The program generates a quiz board by deleting randomly cells from the generated Board, based on the configured difficulty level. 
3. When e cell is deleted, the program checks (by using again the backtracking algorithm) following criteria
   1. if the game is still solveable
   2. if there is still only one solution available.
4. After creating the quizBoard, the starts the game by calling the function playGame.

## Play the game in your Gitpod environment

1. Fork this repository
2. Copy the link to your new repository and prefix it with "https://gitpod.io/#" in your browser
3. Click `Continue with GitHub` and `Authorize gitpod-io`
4. Wait for the environment to build. This can take a while the first time.
5. Gitpod runs automatically the command "cabal run", so the game should start in a Terminal-Session
6. Have fun playing the game!

## Play the game in your local environment
1. Clone this repository to into a folder on your local machine
2. Open a terminal session and navigate into the folder
3. Run the command "cabal run"
4. Have fun playing the game!

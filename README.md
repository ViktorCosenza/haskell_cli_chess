# haskell_cli_chess

### Cabal Deps:
  - matrix
  - rainbow

### Run:
  ```make && ./chessgame```
  
![InitialPosition](https://i.imgur.com/XqW2wP4.png)


#### Instructions:
A move is a 4 char string where the first two are the piece to move and the last 2 the destination.
For example
* a2a4
* b7b5

#### Simplifications:
1. You cannot castle.
2. You cannot promote to any piece other than the queen.
3. You cannot capture En passant.
4. There is no check or checkmate, the win condition is to actually capture the enemy King.

![Game](https://i.imgur.com/rtfyU3u.png)

### Clear:
  ```make clean```


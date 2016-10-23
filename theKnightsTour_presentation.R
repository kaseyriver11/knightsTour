
# Make a board
M=6; N=6
(chess <- matrix(0, nrow=M,ncol=N))


# At any given time, I'll need to update my board, and figure out if my board is occupied
getValue <- function(position){chess[position[1], position[2]]}
setValue <- function(position, x){chess[position[1], position[2]] <<- x}

# Lets start with a random position
pos <- c(1,2)
pos
getValue(pos)
setValue(pos,1)
chess
setValue(pos,0)
chess

# Which direction can a knight move?
knightMoves <- cbind(c(-2,-1), c(-1,-2), c(1,-2), c(2,-1),c(2,1), c(1,2), c(-1,2), c(-2,1))


# Where will this take my knight?
pos1 <- pos + knightMoves
pos1
# Obviously some of these are not valid moves

# Write a function to check validity
# 1: Is the board already occupied there?
# 2: Does the move remain on the board?
valid <- function (move) {all(1 <= move & move <= c(M, N)) && (getValue(move) == 0)}
apply(pos1,2,valid)

# What are the possible moves?
possibleMoves <- function (position) {
    moves <- position + knightMoves
    cbind(moves[, apply(moves, 2, valid)])
}
possibleMoves(pos)

# To solve this quickly, we need to use a heuristic 
# Possible moves sorted according to their Wornsdorff cost.
# If I run into a problem, I need to quick this process
candidates <- function (position) {
    moves <- posibleMoves(position)
    # Which moves has minimum number of next possible moves?
    wcosts <- apply(moves, 2, function (position) { ncol(posibleMoves(position)) })
    moves <- cbind(moves[, order(wcosts)])
    moves 
}
moves <- possibleMoves(pos)
moves
wcosts <- apply(moves, 2, function(cols){ncol(possibleMoves(cols))})
wcosts
order(wcosts)
moves <- cbind(moves[,order(wcosts)])
moves


knightTour = function (position, moveN) {
    # Tour Complete.
    if (moveN > (M * N)) {
        # Take the board and make it look like an actual chess board
        board <- chess
        board <<- board; 
        print(board)
        opt <- options(show.error.messages=FALSE) 
        on.exit(options(opt))
        stop() 
    }
    # Available moves.
    moves = candidates(position) 
    # None possible. Backtrack.
    if (ncol(moves) == 0) {return()}
    # Make a move, and continue the tour.
    apply(moves, 2, function (position) {
        setValue(position, moveN)
        knightTour(position, moveN + 1)
        setValue(position, 0)
    })
}
moves <- candidates(pos)
moves

# Did it work?
(chess <- matrix(0, nrow=M,ncol=N))
setValue(c(1,2), 1)
chess
knightTour(c(1,2), 2)

# THIS DOES NOT WORK
M=5; N=5; chess <- matrix(0, nrow=M,ncol=N)
setValue(c(1,2), 1)
knightTour(c(1,2),2)


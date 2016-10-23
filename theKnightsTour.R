
# Completing the Knights Tour in R
theKnightsTour <- function(startKnight = "a1", M=8,N=8){
    # Where am I starting?
    col <- as.numeric(charToRaw(tolower(substr(startKnight,1,1)))) - 96
    row <- as.numeric(substr(startKnight,2,nchar(startKnight)))
    if(row > M | col > N){stop("Your knight is not on the chess board =/")}
    startKnight <- c(row,col)
    
    # Create a blank chess board
    chess <- matrix(0, nrow = M, ncol = N)
    # Current value on board
    getValue <- function(position){chess[position[1], position[2]]}
    # Set Value on board
    setValue <- function(position, x){chess[position[1], position[2]] <<- x}

    # Matrix of possible moves a knight can make
    knightMoves <- cbind(c(-2,-1), c(-1,-2), c(1,-2), c(2,-1),c(2,1), c(1,2), c(-1,2), c(-2,1))
    # 1: The board has to be empty - (getboard(move) == 0)
    # 2: The move has to stay on the board
        # 1 <= move: is x and y greater than 1
        # move <= c(M,N): does the move stay within the board
    valid <- function (move) {all(1 <= move & move <= c(M, N)) && (getValue(move) == 0)}
    # Moves possible from a given position.
    possibleMoves <- function (position) {
        moves <- position + knightMoves
        cbind(moves[, apply(moves, 2, valid)])
    }
    # Possible moves sorted according to their Wornsdorff cost.
    candidates <- function (position) {
        moves <- possibleMoves(position)
        # No candidate moves available; #if (ncol(moves) == 0) {}
        # Which moves has minimum number of next possible moves?
        wcosts <- apply(moves, 2, function (position) { ncol(possibleMoves(position)) })
        moves <- cbind(moves[, order(wcosts)])
        moves 
    }
    
    # Recursive function for touring the chess board.
    knightTour = function (position, moveN) {
        # Tour Complete.
        if (moveN > (M * N)) {
            # Take the board and make it look like an actual chess board
            board <- chess
            board <- as.data.frame(board)
            sortBoard <- rev(order(as.numeric(rownames(board))))
            board <- board[sortBoard,]
            colnames(board) <- letters[1:ncol(board)]
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
    setValue(startKnight, 1); knightTour(startKnight, 2)
    print("Your board has no solution using this technique.")
}
n <- 1 
theKnightsTour("a8", 8, 8)







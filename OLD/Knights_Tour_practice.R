

# Convert Letters to numbers
pos <- c("b3")
col <- as.numeric(charToRaw(substr(pos,1,1))) - 96
row <- as.numeric(substr(pos,2,2))

# M x N Chess Board.
M = 5; N = 5; board = matrix(0, nrow = M, ncol = N)

# Get/Set value on a board position.
getboard = function (position)    { board[position[1], position[2]] }
setboard = function (position, x) { board[position[1], position[2]] <<- x }

# (Relative) Hops of a Knight.
hops = cbind(c(-2, -1), c(-1, -2), c(+1, -2), c(+2, -1),
             c(+2, +1), c(+1, +2), c(-1, +2), c(-2, +1))

# Validate a move.
valid = function (move) {
    all(1 <= move & move <= c(M, N)) && (getboard(move) == 0)
}

# Moves possible from a given position.
explore = function (position) {
    moves = position + hops
    cbind(moves[, apply(moves, 2, valid)])
}

# Possible moves sorted according to their Wornsdorff cost.
candidates = function (position) {
    moves = explore(position)
    
    # No candidate moves available.
    if (ncol(moves) == 0) { return(moves) }
    
    wcosts = apply(moves, 2, function (position) { ncol(explore(position)) })
    cbind(moves[, order(wcosts)])
}

# Recursive function for touring the chess board.
knightTour = function (position, moveN) {
    
    # Tour Complete.
    if (moveN > (M * N)) {
        print(board)
        stop()
    }

    # Available moves.
    moves = candidates(position) 
    
    # None possible. Backtrack.
    if (ncol(moves) == 0) { return() }
    
    # Make a move, and continue the tour.
    apply(moves, 2, function (position) {
        setboard(position, moveN)
        knightTour(position, moveN + 1)
        setboard(position, 0)
    })
}

# User Input: Starting position (in algebraic notation).
square = commandArgs(trailingOnly = TRUE)

# Convert into board co-ordinates.
row      = M + 1 - as.integer(substr(square, 2, 2))
ascii    = function (ch) { as.integer(charToRaw(ch)) }
col      = 1 + ascii(substr(square, 1, 1)) - ascii('a')
position = c(row, col)

position = c(4,4)
# Begin tour.
setboard(position, 1); knightTour(position, 2)

theKnightsTour <- function(position = "a1", M=8,N=8){
    
    # Where am I starting?
    col <- as.numeric(charToRaw(substr(position,1,1))) - 96
    row <- as.numeric(substr(position,2,2))
    position <- c(row,col)
    
    # Create the chess board
    chess = matrix(0, nrow = M, ncol = N)
    
    # Current Value on board
    getboard = function (position)    { chess[position[1], position[2]] }
    # Set the value on the board
    setboard = function (position, x) { chess[position[1], position[2]] <<- x }
    
    # (Relative) Hops of a Knight.
    hops = cbind(c(-2, -1), c(-1, -2), c(+1, -2), c(+2, -1),
                 c(+2, +1), c(+1, +2), c(-1, +2), c(-2, +1))
    
    # Validate a move.
    valid = function (move) {
        all(1 <= move & move <= c(M, N)) && (getboard(move) == 0)
    }
    
    # Moves possible from a given position.
    explore = function (position) {
        moves = position + hops
        cbind(moves[, apply(moves, 2, valid)])
    }
    
    # Possible moves sorted according to their Wornsdorff cost.
    candidates = function (position) {
        moves = explore(position)
        
        # No candidate moves available.
        if (ncol(moves) == 0) { return(moves) }
        
        wcosts = apply(moves, 2, function (position) { ncol(explore(position)) })
        cbind(moves[, order(wcosts)])
    }
    
    # Recursive function for touring the chess board.
    knightTour = function (position, moveN) {
        
        # Tour Complete.
        if (moveN > (M * N)) {
            board <- chess
            board <- as.data.frame(a)
            sortBoard <- rev(order(rownames(board)))
            board <- board[sortBoard,]
            colnames(board) <- letters[1:ncol(board)]
            board <<- board
            print(board)
                opt <- options(show.error.messages=FALSE) 
                on.exit(options(opt)) 
                stop() 
        }
        
        # Available moves.
        moves = candidates(position) 
        
        # None possible. Backtrack.
        if (ncol(moves) == 0) { return() }
        
        # Make a move, and continue the tour.
        apply(moves, 2, function (position) {
            setboard(position, moveN)
            knightTour(position, moveN + 1)
            setboard(position, 0)
        })
    }
    
    setboard(position, 1); knightTour(position, 2)
    
}
theKnightsTour()














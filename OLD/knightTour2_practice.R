

# Create a blank chess board
M=5; N=5
chess <- matrix(0, nrow = M, ncol = N)
chess2 <- matrix(1:(M*N), nrow=M, ncol=N)

pos1 <- c(4,3)
pos2 <- c(5,5)
setValue(pos1,1)
setValue(pos2,1)
chess
# What moves cannot alread be exsisting? 
r1 <- pos1[1]; c1 <- pos1[2]
r2 <- pos2[1]; c2 <- pos2[2]
# First X
pick <- ifelse(abs(r1-r2) == 1, 2, 1)
v <- xvalues(pick,r1,r2,c1,c2)
place1 <- v[1,]



options <- place1 + knightMoves
options <- cbind(options[, apply(options, 2, noCC,r1,r2,c1,c2)])
options <- cbind(options[, apply(options, 2, tooFar,r1,r2,c1,c2)])
options <- cbind(options[, apply(options, 2, lastOne,r1,r2,c1,c2,pick,pos1,pos2,place1)])
options <- cbind(options[, apply(options, 2, valid2)])




# Is move valid?
valid <- function (move) {all(1 <= move & move <= c(M, N)) && (getValue(move) == 0)}
valid2 <- function (move) {all(1 <= move & move <= c(M, N))}
# Set value of board
setValue <- function(position, x){chess[position[1], position[2]] <<- x}
# Get value of board
getValue <- function(position){chess[position[1], position[2]]}
# get Number of board
getNumber <- function(position){
    ifelse(valid2(position),chess2[position[1], position[2]], 0)}
# Possible knight moves
knightMoves <- cbind(c(-2,-1), c(-1,-2), c(1,-2), c(2,-1),c(2,1), c(1,2), c(-1,2), c(-2,1))

# Moves possible from a given position.
possibleMoves <- function (position) {
    moves <- position + knightMoves
    cbind(moves[, apply(moves, 2, valid)])
}


















# Moves possible from a given position.
possibleMoves <- function (position) {
    moves <- position + knightMoves
    cbind(moves[, apply(moves, 2, valid)])
}

noCC <- function(position,r1,r2,c1,c2){
    position[1] %in% c(r1,r2) || position[2] %in% c(c1,c2)
}
# Too Far Away
tooFar <- function(position,r1,r2,c1,c2){
    abs(position[1] - r1) + abs(position[1] - r2) + abs(position[2] - c1) + abs(position[2] - c2) < 6
}
# Last one. Find the one that matches a row (or column, depnding on x) and that is too close!)
lastOne <- function(position,r1,r2,c1,c2,pick,pos1,pos2,place1){
    if(pick==2){a <- !(position[1] %in% c(r1,r2) &&
                           ifelse(tdist(pos1,place1) == 1, tdist(position,pos1) != 2, 
                                  tdist(position,pos2) != 2))}
    if(pick==1){a <- !(position[2] %in% c(c1,c2) && 
                           ifelse(tdist(pos1,place1) == 1, tdist(position,pos1) != 2, 
                                  tdist(position,pos2) != 2))}
    a
}


xvalues <- function(pick,r1,r2,c1,c2){
    if(pick == 2){
        col <- mean(c(c1,c2))
        place1 <- c(r1, col)
        place2 <- c(r2,col)
        place3 <- c(r1,c2)
        place4 <- c(r2,c1)
    }
    if(pick == 1){
        row <- mean(c(r1,r2))
        place1 <- c(row, c1)
        place2 <- c(row, c2)
        place3 <- c(r1,c2)
        place4 <- c(r2,c1)
    }
    a <- rbind(place1,place2,place3,place4)
}

tdist <- function(v1,v2){
    dist <- abs(v1[1] - v2[1]) + abs(v1[2] - v2[2])
    dist
}



middleChecks <- function(place1,r1,r2,c1,c2,pick,pos1,pos2){
    options <- place1 + knightMoves
    options <- cbind(options[, apply(options, 2, noCC,r1,r2,c1,c2)])
    options <- cbind(options[, apply(options, 2, tooFar,r1,r2,c1,c2)])
    options <- cbind(options[, apply(options, 2, lastOne,r1,r2,c1,c2,pick,pos1,pos2,place1)])
    options <- cbind(options[, apply(options, 2, valid2)])
    if(dim(options)[2]==0){options <- matrix(0,nrow=2,ncol=1)}
    connections <- cbind(getNumber(place1), apply(options,2,getNumber))
}

makeConnections <- function(pos1, pos2){ 
    # What moves cannot alread be exsisting? 
    r1 <- pos1[1]; c1 <- pos1[2]
    r2 <- pos2[1]; c2 <- pos2[2]
    # First X
    pick <- ifelse(abs(r1-r2) == 1, 2, 1)
    #r <- ifelse(pick == 2, min(r1,r2), mean(c(r1,r2)))
    #c <- ifelse(pick == 2, mean(c(c1,c2)), min(c1,c2))
    values <- xvalues(pick,r1,r2,c1,c2)
    # Calculate the three connections for the middle "x's"
    place1 <<- values[1,]
    a <- middleChecks(values[1,],r1,r2,c1,c2,pick,pos1,pos2)
    place1 <<- values[2,] 
    a <- rbind(a,middleChecks(values[2,],r1,r2,c1,c2,pick, pos1,pos2)) 
    # Calculate the 3 last possible connections
    if(pick==1){
        one <- c(mean(c(r1,r2)), max(c1,c2) + 1)
        two <- c(mean(c(r1,r2)), min(c1,c2) - 1)
    }
    if(pick==2){
        one <- c(max(r1,r2) + 1, mean(c(c1,c2)))
        two <- c(min(r1,r2) - 1, mean(c(c1,c2)))
    }
    a <- rbind(a, rbind(c(getNumber(values[3,]), getNumber(values[4,])), 
                        c(getNumber(values[3,]), getNumber(one)),
                        c(getNumber(values[3,]), getNumber(two)), 
                        c(getNumber(values[4,]), getNumber(one)),
                        c(getNumber(values[4,]), getNumber(two))))

} 











xvalues <- function(pick){
    if(pick == 2){
        col <- mean(c(c1,c2))
        place1 <- c(r1, col)
        place2 <- c(r2,col)
        place3 <- c(r1,c2)
        place4 <- c(r2,c1)
    }
    if(pick == 1){
        row <- mean(c(r1,r2))
        place1 <- c(row, c1)
        place2 <- c(row, c2)
        place3 <- c(r1,c2)
        place4 <- c(r2,c1)
    }
}

tdist <- function(v1,v2){
    dist <- abs(v1[1] - v2[1]) + abs(v1[2] - v2[2])
    dist
}


# For place1 and place2, run through this:
# No Common Coordinates
noCC <- function(position,r1,r2,c1,c2){
    position[1] %in% c(r1,r2) || position[2] %in% c(c1,c2)
}
# Too Far Away
tooFar <- function(position){
    abs(position[1] - r1) + abs(position[1] - r2) + abs(position[2] - c1) + abs(position[2] - c2) < 6
}
# Last one. Find the one that matches a row (or column, depnding on x) and that is too close!)
lastOne <- function(position){
    ifelse(pick==2, !(position[1] %in% c(r1,r2) &
                          ifelse(tdist(pos1,place1) == 1, tdist(position,pos1) != 2, 
                                 tdist(position,pos2) != 2)), 
           !(position[2] %in% c(c1,c2) & 
                 ifelse(tdist(pos1,place1) == 1, tdist(position,pos1) != 2, 
                        tdist(position,pos2) != 2)))
}

options <- place1 + knightMoves
options <- cbind(options[, apply(options, 2, noCC)])
options <- cbind(options[, apply(options, 2, tooFar)])
(options <- cbind(options[, apply(options, 2, lastOne)]))
options <- cbind(options[, apply(options, 2, valid)])











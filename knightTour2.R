


knightTour2 = function (position, moveN,path, cont) {
    # If we have a new best, then update
    if(moveN > cMax){
        cMax <<- moveN
        bestChess <<- chess
    }
    pos1 <- position
    moves <- possibleMoves(position)
    if (ncol(moves) == 0 | cont == 2) { return() }
    
    path2 <- path
    apply(moves, 2, function (position) {
        pos2 <- position
        problems <- makeConnections(pos1,pos2)
        path <- c(getNumber(pos1),getNumber(position))
        path2 <- rbind(path2,path)
        cont <- 1
        if(any(apply(path2,1, function(x){any(x[1] == problems[,1] & x[2] == problems[,2])})) |
           any(apply(path2,1, function(x){any(x[1] == problems[,c(2:1)][,1] & 
                                              x[2] == problems[,c(2:1)][,2])}))== TRUE){cont <-2}

        setValue(position, moveN)
        #print(chess)
        knightTour2(position, moveN + 1, path2, cont)
        setValue(position, 0)
    })
}

runKnight <- function(M,N, start, cont){
    M <<- M
    N <<- N
    time <- Sys.time()
    cMax <<-1
    chess <<- matrix(0,nrow=M,ncol=N)
    chess2 <<- matrix(1:(M*N), nrow=M, ncol=N)
    bestChess <<- chess
    setValue(start,1)
    knightTour2(start,2,start, cont)
    bestChess[which(bestChess == max(bestChess))] = 0
    print(bestChess)
    print(max(bestChess)-1)
    (time <- Sys.time() - time)
}

## For testing
a <- runKnight(5,5, c(1,2), 1)

a <- replicate(10,runKnight(5,5, c(1,2), 1))














f3 <- function(){
    time <- Sys.time()
    runKnight(5,5, c(1,2), 1)
    time <- Sys.time() - time
}

f4 <- function(){
    time <- Sys.time()
    getNumber(c(3,2))
    time <-  Sys.time() - time
}


a <- replicate(10, f3())
mean(a)
a <- replicate(100000, f4())
mean(a)*100000







#### Two if's are just much faster (40% faster) than an ifelse
f1 <- function(){
    time <- Sys.time()
        a <- runif(10000,0,1)
        j <- 1
        for(i in 1:10000){
            if(a[i] > .5){ j <- j + 1}
            if(a[i] < .5){ j <- j + 2}
        }
        time <- Sys.time() - time
        time
}

f2 <- function(){
    time <- Sys.time()
    a <- runif(10000,0,1)
    j <- 1
    for(i in 1:10000){
        j <- ifelse(a[i] > .5, j + 1, j + 2)
        }
    time <- Sys.time() - time
    time
}

a <- replicate(100, f1())
mean(a)
a <- replicate(100, f2())
mean(a)






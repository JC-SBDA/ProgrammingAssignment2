## Both functions are related to calculating the inverse of a matrix, the first one 'makeCacheMatrix'
## Is tasked with calculating the inverse and then caching it in a list.
## The second 'cacheSolve' is set to check the cache first, and if the data isn't present, to calculate the matrix inversion.

## Creates Special Matrix, Caches the inverse. Sets x as a matrix. For function y set's value x and z accordingly.
## Then displays (as last part of function) the set inverse of the matrix. Then Caches.
makeCacheMatrix <- function(x = matrix()) {
        z <-NULL
        set <- function(y){
                x <<- y
                z <<-NULL
        }
        get <- function()x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Gets value from 'makeCacheMatrix', if the value is 
## not null i.e. if it is present, it presents the message "getting cached data" 
## and then returns z (the inverted matrix). If it is not already set, it inverts the matrix.
cacheSolve <- function(x, ...) {
        z <- x$getInverse()
        if(!is.null(z)){
                message("getting cached data")
                return(z)
        }
        mat <- x$get()
        z <- solve(mat,...)
        x$setInverse(z)
        z
}
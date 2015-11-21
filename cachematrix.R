## These function store a matrix along with functions that can store and 
## retrieve the inverse of that matrix (actually it can store anything,
## doesn't have to be remotely related to the first matrix), and then another
## function that either retrieves said inverse from cache or calculates a new
## inverse. I don't understand why this is useful, but it works. 

## sets matrix and (optionally) its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Returns cached inverse or calculates a new one

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

## testing
test <- matrix(c(3,4,1,2), nrow = 2, ncol = 2)
test.o <- makeCacheMatrix(test)
test.o$get()
test.i <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
test.o$setInverse(test.i)
cacheSolve(test.o)

##-------------------------------------------------------------
## name: makeCacheMatrix(x,...)
## description: create a matrix that can cache its inverse
##-------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <-function(y) {
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <-function(solve) m <<- solve
        getinverse <-function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

##-------------------------------------------------------------
## name: cacheSolve(x, ...)
## description: computes the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve retrieve the inverse 
## from the cache
##-------------------------------------------------------------

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("==> getting cached data")
                return(m)
        }
        ## Matrix is not as cached and/or inverse matrix is not cached
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##-------------------------------------------------------------
## name: testcase()
## description: this is function to test cached matrix creation
## usage:
##      > source ("cachematrix.R")
##      > testcase()
##-------------------------------------------------------------

mytestcase <- function () {
        #-----------------------------------------------
        a <- diag(5,5)
        CachedMatrix <- makeCacheMatrix(a)
        ## print(CachedMatrix)
        #----------------------------------------------
        message("==> no cached inverse")
        t1 <- cacheSolve(CachedMatrix)
        print(t1)
        #-----------------------------------------------
        t2 <- cacheSolve(CachedMatrix)     
        print(t2)
}

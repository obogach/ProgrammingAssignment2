## Set of two functions makeCacheMatrix() and cacheSolve() to make 
## repeated computations of inverted matrix more effective

## Creates special 'matrix' (list) object

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL;
        set <- function(y) {
                x <<- y;
                invM <<- NULL;
        }
        get <- function() x
        setInv <- function(inv) invM <<- inv
        getInv <- function() invM
        list (set = set, get = get, 
              setInv = setInv, getInv = getInv)
}


## Calculates and caches an inverted matrix

cacheSolve <- function(x, ...) {
        invM <- x$getInv()
        if (!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get();
        invM <- solve(data);
        x$setInv(invM);
        invM;
}

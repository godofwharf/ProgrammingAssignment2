## makeCacheMatrix function creates a special matrix object which is a list 
## of functions namely set, get, setInv, getInv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Function used to set the matrix 'x'
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Function that returns the matrix 'x'
        get <- function() x
        ## Function used to set the inverse for matrix 'x'
        setInv <- function(i) inv <<- i
        ## Function that returns the inverse for matrix 'x'
        getInv <- function() inv
        ## Returns a list of functions
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## CacheSolve function takes a special matrix object and a variable
## number of arguments as input and returns an inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Checks if inverse is computed already for 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                ## Inverse has been computed already
                print("Cache hit successful")
                ## Returns the stored inverse matrix
                return(inv)
        }
        ## Gets the matrix object associated with 'x' 
        mat <- x$get()
        ## Compute inverse for 'mat'
        inv <- solve(mat, ...)
        ## Store inverse for 'mat' in 'x'
        x$setInv(inv)
        ## Return the inverse of 'x'
        inv
}


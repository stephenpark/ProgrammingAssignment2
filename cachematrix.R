## Put comments here that give an overall description of what your
## functions do
## The functions work as a pair to calculate , store and retrieve the inverse matrix
## When makeCacheMatrix is called with a matrix as input, it returns a list of 
## four sub functions.
##When cacheSolve is called for the first time it checks to see if the inverse matrix 
##has already been stored.   If it hasn't, it calculates the inverse matrix using solve()
## and stores the result in the variable m
##The second time cacheSolve is called,it retrieves the store value of the inverse matrix
##without having to calculate it.


## The makeCacheMatrix function take a matrix as input and creates a list of
##subfunctions called set, get, setsolve and getsolve

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## The cacheSolve function uses the subfunctions defined in the makeCacheMatrix function
## to store and retrieve the inverse matrix as well as storing and retrieving an indicator 
##determining if the inverse matrix has been stored or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}


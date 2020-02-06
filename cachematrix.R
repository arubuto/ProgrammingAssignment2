## This function caches the inverse of a matrix.  Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    
        nv <- null                              ##makeCacheMatrix defines the Set function
        set <- function(y)     {       
                x <<- y                 
                nv  <<- null            
                }
                get <- function() x             ## R retrives x from makeCacheMatrix
                setinverse <- function(inverse) nv <<- inverse 
                getinverse <- function() nv
                list(set = set, get = get,      ## A list is created and returned to the parent enviroment
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is needed to populate or retrive the solvematrix from makeCacheMatrix. 
cacheSolve <- function(x, ...) {                
        ## Return a matrix that is the inverse of 'x' 
        nv <- x$getinverse()     
        if(!is.null(nv)) {                      ## if false, cacheSolve gets the matrix from the input and calculate the solve()
                message("getting cached data")
                return(nv)
        }
        matrix <- x$get()
        nv <- solve(matrix, ...)        ## The solve function is used to get the inverse of a matrix
        x$setinverse(nv)
        nv
        
}

## Programming Assignment 2: Lexical Scoping 
## Caching the Inverse of a Matrix


## Function to create a special 'matrix' object that can cache its inverse.
## Returns a list, containing a function to give the data properly to compute 
## the inverse.

makeCacheMatrix <- function(m = matrix()) {
        j <- NULL
        
        ## Setting the value of the matrix 
        set <- function(mat) {
                m <<- mat
                j <<- NULL
        }
        ## Getting the value of the matrix with anonymous function to return it
        get <- function() m
        
        ## Setting the value of the matrix as done before
        setInverse <- function(inverse) j <<- inverse
        
        ## Getting the value of the matrix returning the inverse property
        getInverse <- function() j
        
        ## Return a list of the methods/steps in correct order
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computation of the inverse of a special matrix object, returned by 
## 'makeCacheMatrix' function. Moreover, can search from the cache to 
## retrieve the inverse from it, only if it has been already calculated.

cacheSolve <- function(obj, ...) {
        
        ## Return a matrix that is the inverse of 'obj'
        inv <- obj$getInverse()
        
        ## Checking if the inverse has been already computed.
        if(!is.null(inv)) {
                
                ## It returns the message that has been already calculated and 
                ## its inverse correspondingly
                message("Getting Cached Data...Inverse Found!")
                return(inv)
        }
        
        ## Getting the matrix from the object
        data <- obj$get()
        
        ## Computing the inverse using matrix multiplication
        inv <- solve(data) %*% data
        
        ## Setting the inverse to the object
        obj$setInverse(inv)
        
        ## Returning the matrix inverse solution
        inv
}


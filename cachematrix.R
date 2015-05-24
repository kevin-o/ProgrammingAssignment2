## These two functions are written for Programming Assignment 2 in Coursera R Programming (Section 14)
## 

## the first function creates a special vector that is a list of functions 
makeCacheMatrix <- function(x = matrix()) {
        ## the cache is initialized         
        
        cacheInverseMatrix <- NULL
        cacheMatrix <- NULL
        
        
        ## set and get operate on the matrix 
        set<- function(y) {
                x <<- y
                cacheInverseMatrix <<- NULL
        }
        get <- function() x
        
        ## the setinverse and setmatrix functions set the values of those variable in the cache
        setinverse <- function(inverse) cacheInverseMatrix <<- inverse
        setmatrix <- function(matrixN) cacheMatrix <<- matrixN
        
        
        # the getinverse  function gets the values of those variables in the cache
        getinverse <- function() cacheInverseMatrix
        getmatrix <- function() cacheMatrix
        
        
        ## the set of six functions is return as a vector
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, setmatrix = setmatrix, getmatrix = getmatrix)

}


## 

cacheSolve <- function(x, ...) {
        ## This function accepts the special vector from makeCacheMatrix, checks if it is in the cache, and if not computes the inverse 
        ##first retrieve the inverse in cache and if it exists, return it
        
        inverseMatrix <- x$getinverse()
        matrix1<-x$getmatrix()
        
        if (is.matrix(matrix1) && is.matrix(x$get()) && dim(matrix1) == dim(x$get()) && (matrix1 == x$get())) {
        
                if(!is.null(inverseMatrix)) {
                message("getting cached inverse matrix")
                return(inverseMatrix)
                }
        }
        ## otherwise, get the matrix and invert it
        matrix1<-x$get()
        cacheInverseMatrix <- solve(matrix1)
        ## then set it into the cache, along with the matrix
        x$setinverse(cacheInverseMatrix)
        x$setmatrix(matrix1)
        ## and return it
        cacheInverseMatrix
        
}

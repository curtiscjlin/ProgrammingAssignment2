## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##=============================================================================================
##The makeCacheMatrix fuction is to generate a Matrix for cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  list(set = set, 
       get = get
       )
}



## Write a short comment describing this function
## The cacheSolve function will return returns its inverse if the matrix from makeCacheMatrix is a square invertible matrix.
## If not, the warnning message of ""not a square invertible matrix" will be printed out. 

cacheSolve <- function(x) {
  m <- x$get()
  if (nrow(m) == ncol(m)){
    m = m
  }
  else if (nrow(m) != ncol(m)) {
    m <- NULL
  }
  
  if(!is.null(m)) {
    message("getting cached data")
    return(solve(m))
  }
  else if (is.null(m)) {
    message("not a square invertible matrix")
  }
}

##-------------------------------------------------------------
##Example:

##Input:
##A <- makeCacheMatrix(matrix(1:4,2))
##B <- A$get()
##C <- cacheSolve(A)
##D <- solve(B)

##Result:
## > B
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> C <- cacheSolve(A)
##getting cached data
## > C
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## > D
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##===============================================================

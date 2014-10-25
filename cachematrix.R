## This little sript creates two functions to cache and create
## the inverse of a matrix. 
## This is my solution for the 'Programming Assignment 2: Lexical Scoping'
## in the Coursera 'R Programming' course in the 
## Johns Hopkins Data Science Specialization

## The 'makeCacheMatrix' function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve 
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The 'cacheSolve' function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will retrieve the inverse 
## from the cache
## See example for the use of the functions below the functions.
## NOTICE the 'getting cached data' message when 
## you run the 'im1 <- cacheSolve(cm1)' for the second time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m        
}

##################################
## Example to test the solution ##
##################################
        ### 1. Don't forget to source the 'cachematrix.R' file :)

        ### 2. create and check source matrix
        # > m1 <- matrix(c(4,7, 2,6), nrow = 2, ncol = 2, byrow = TRUE
        #             , dimnames = list(c("row1", "row2"), c("Col1", "Col2")))
        # > m1
        #### Expected output:
        ####      Col1 Col2
        #### row1    4    7
        #### row2    2    6
        
        ### 3. cache the matrix
        # > cm1 <- makeCacheMatrix(m1)

        ### 4. create and check inverse
        # > im1 <- cacheSolve(cm1)
        # > im1
        #### Expected output:
        ####      row1 row2
        #### Col1  o.6 -0.7
        #### Col2 -0.2  0.4

        ### 5. calculate inverse on the same marix again
        # > im1 <- cacheSolve(cm1)
        #### Expected message:
        #### 'getting cached data'

        ### 6. Test inverse matrix with multiplication
        # > testinverse <- m1 %*% im1
        # > testinverse
        #### Expected output:
        ####        row1 row2
        #### row1    1    0
        #### row2    0    1

#### END OF FILE ####
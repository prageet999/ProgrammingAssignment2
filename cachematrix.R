##These are a pair of functions that cache the inverse of a matrix
##which helps in saving computational time


## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize value of null matrix
        inv <- NULL
        
        # The set function sets the value of the matrix provided to the cache "x" and clears the inverse matrix value if it exists
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # The get function retrieves the value of the matrix stored in "x"
        get <- function() {x}
        
        # The setinv function stores the value of the inverse matrix provided in "inv"
        setinverse <- function(inverse) {inv <<- inverse}
        
        # The getinv function retrieves the value of the inverse matrix stored in "inv"
        getinverse <- function() {inv}
        
        # This is the list that is returned to makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created 
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the
## cache via the setinv function.

cacheSolve <- function(x, ...) {
        # Get value of inverse
        inv <- x$getinverse()
        # Check if inverse is already computed
        if(!is.null(inv)) {
                message("getting cached matrix data")
                return(inv)
        }
        
        # Else computes the inverse of the data
        data <- x$get()
        inv <- solve(data, ...)
        
        # To set the computed inverse to the cache
        x$setinverse(inv)
        
        # Returns the inverted matrix
        inv
        
}



## The code below can be used to test the working of cacheSolve() vs using the Solve() function directly

#Creating a 4000*4000 matrix of randomized normal numbers
nrows <- 4000
ncols <- 4000
x <- stats::rnorm(nrows*ncols)
dim(x) <- c(nrows, ncols)

#Using the first function to assign the matrix
test <- makeCacheMatrix(x)

#Using the second function to compute the inverse
Result1 <- cacheSolve(test)
#Comparing the results
Result2 <- solve(x)
identical(Result1, Result2)

#It can be seen that if the code is rerun from line 78 then the cached solver produces the result instantaneously
#as the value of the inverse is just retrieved from the cache instead of recomputing
#Thus the defined function is able to reduce computational time.

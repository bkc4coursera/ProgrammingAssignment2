## Author: Bharath Chandrasekhar


## The function makeCacheMatrix creates a special matrix object that can cache
## its inverse, which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of matrix
##   4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

		## Note: If the data matrix is supplied right when makeCacheMatrix is
		## called, it is automatically stored in the right environment
		## Just set the matrix inverse object to NULL here.

        mInv <- NULL
        
        ## Store the matrix itself, in a different environment
        ## 	This method is called either to update the data matrix or to set
        ##  it the first time if no data was provided when makeCacheMatrix was
        ##  called.
        ##
        ##  Typical usage:
        ##     mySpecialMat <- makeCacheMatrix()
        ##
        ##     mySpecialMat$set(matrix(c(1,2,3,0,4,5,1,0,6), 3,3))
        ##
        
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        
        ## Get the contents of the data matrix stored either via the set()
        ## function or the constructor
        ## 		Involved by cacheSolve() if the inverse was already not
        ##    calculated
        
        get <- function() x
        
        ## Function to compute the inverse of the matrix and store
        ## the result in the cache
        setInvMatrix <- function(solve) mInv <<- solve
        
        ## Function to retrieve the inverse of the matrix from the cache
        getInvMatrix <- function() mInv
        
        # Create the list that has all the functions. This is the object
        # that is returned by this function
        list(set = set, 
             get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## Function returns the matrix that is inverse of x. If the inverse
## is already calculated, it gets it from the cache, calculates
## and updates the cache if it does not

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    mInv <- x$getInvMatrix()
    
    if(!is.null(mInv)){
      	message("Matrix inverse already computed. Getting cached data")
      	return(mInv)
    }
    
    ## Nothing in the cache, we have compute the matrix and store it in cache
		message("Inverse not yet computed... Doing it now")
		    
    ## First, retrieve the matrix data
    message("Retrieving the matrix data...")
    matrix_data <- x$get()
    
    ## Compute the inverse
    message("Computing the inverse...")
    mInv<-solve(matrix_data, ...)
    
    ## Cache the value
    message("Updating the cache...")
    x$setInvMatrix(mInv)
    
    # Return the inverse    
    return (mInv)        
}


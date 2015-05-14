## MakeCacheMatrix caches a matrix and its inverse.
## cacheSolve checks if the inverse matrix is already cached. 
## if not, it calculates the inverse matrix
## ## ## ##
## MakeCacheMatrix uses the provided matrix to create 4 functions
## dependant on that matrix:
##      set() Caches the matrix to be solved
##      get() Returns the matrix to be solved
##      setinv() caches the inverse matrix (It does not calculate anything)
##      getinv() Returns the cached inverse matrix

## You can check makeCacheMatrix using the following matrix
## m=matrix(c(9,10,3,20,25,30,35,40,45,50,3,8,65,70,75,80), nrow=4, byrow=T)
## mm <- makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
        cm <- NULL                              # When the matrix is stored the first time, assigns NULL value to "cm" (cm=CachedMatrix)
        get <- function() x                     # get is a function to call the cachedMatrix
        setinv <- function(solve) cm <<- solve  # setinv caches the inv. matrix calculated by cacheSolve into the special CachedMatrix
        getinv <- function() cm                 # getinv calls for the InverseCachedMatrix
        set <- function(y) {                    # Set function will be called only to change the "CachedMatrix"
                x <<- y                         # To set a "NewMatrix", we call the function: ## CachedMatrix$set(NewMatrix) ##
                cm <<- NULL                     # When the new matrix is assigned with set(), "cm" is redefined as NULL for cacheSolve to work again
        }
        list(set = set, get = get,              # This special list stores the functions created above 
             getinv = getinv,                   # so that they can be called individually
             setinv = setinv)
}

## cacheSolve decides if it must calculate the Inverse Matrix 
## or if it just needs to read the cached Inverse Matrix
## from the special vector

cacheSolve <- function(x, ...) {
        cm <- x$getinv()                        # gets the value of "cm" from the special vector
        if(!is.null(cm)) {                      # if cm is not NULL, it just reads the cached value 
                message("Getting cached Data")  # notifying that the value was already cached
                return(cm)
        }                                       # if "cm" is NULL, then the function will calculate its value
        data <- x$get()                         # Retrieve the matrix to be calculated from the special vector
        cm <- solve(data, ...)                  # calculate the inverse matrix and store it as "cm"
        x$setinv(cm)                            # calls the setinv function to cache the inverse matrix
        cm
        ## Return a matrix that is the inverse of 'x'
}
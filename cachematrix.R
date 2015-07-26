#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse
#   get the value of the inverse

makeCacheMatrix <- function(x = array()) {
        inverse <- NULL                                 # initialize the inverse variable (matrix) with NULL
        set <- function(y) {                            # function to allocate values in the variables x (matrix) and inverse (matrix) in the cache
                x <<- y                                 # allocate the y variable (matrix) to the x variable (matrix) in the cache
                inverse <<- NULL                        # initialize the inverse varaible in the cache with NULL
        }
        get <- function() x                             # to define a function called get to get the value x (matrix)
        setinverse <- function(solve) inverse <<- solve # to define a function called setinverse to allocate the inverse of the matrix to the inverse varaible (matrix)
        getinverse <- function() inverse                # to get the value of the inverse (matrix) in the cache
        list(set = set, get = get,                      # return the list of the four previous functions
             setinverse = setinverse,
             getinverse = getinverse)
}



# The following function calculates the inverse of the special "vector" created with the above function. However, it first 
# checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# In this case a message is dsiplayed saying the cash has been used
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()                         # to allocate to the inverse varaible, the value of the getinverse fonction in the list of four functions
        if(!is.null(inverse)) {
                message("getting cached data")            # display a message if the previous condition is met
                return(inverse)                           # return the inverse (which was already calcualted) if the condition is met
        }
        data <- x$get()                                   # if the previous condition is not met, put in data the matrix
        inverse <- solve(data, ...)                       # then calculate the inverse of the matrix
        x$setinverse(inverse)                             # then allocate the previous result to the inverse in the cache
        inverse                                           # then return the invers of the matrix
}

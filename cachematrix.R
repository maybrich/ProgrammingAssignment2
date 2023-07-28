## These two functions allow the inversion of a matrix. Importantly, the use of the cache allows the value of the inverse to be saved.
## This saves on computational effort if the matrix inverse has already been calculated.

## The first function returns a list of four functions, which essentially create the cached matrix and the environment for the second function.
## Note that this function is required for the second. In itself it does not compute the inverse.
## To use, define an object using this function with the argument of the matrix you desire to invert.

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {      ## First object of the list. This part allows a new matrix to be defined without need to rerun entire function.
                  x <<- y
                  i <<- NULL             ## Clears cache
            }
            get <- function() x        ## Retrieves the matrix
            setinverse <- function(inverse) i <<- inverse      
            getinverse <- function() i
            list(set = set, get = get,      ## Defines the output of the function, a list of four functions
                 setinverse = setinverse,
                 getinverse = getinverse)
}
  
## The second matrix requires the first, and is where the inverse of the matrix x is calculated.
## To use, input the object created using the first function. To reset the matrix, you can use the $set() element of this object.

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if(!is.null(i)) {         ## This section saves on computation by checking if the inverse already exists in the cache
                  message("getting cached data")
                  return(i)
          }
          data <- x$get()           ## If not, it retrieve the matrix from the environment of the first function.
          i <- solve(data, ...)     ## Now calculates the inverse of the matrix x.
          x$setinverse(i)
          i                       ## Returns a matrix which is the inverse of X.
}

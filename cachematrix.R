## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix saves the value of a matrix and it's inverse x in a list
# and can read the values from the list
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {                      # writting x and xinv into the list
    x <<- y
    xinv <<- NULL
  }
  get <- function() x                       # getting x from the list
  setinv <- function(invx) xinv <<- invx    # writing value for inverse of x into list
  getinv <- function() xinv                 # getting inverse from list
  list(set = set, get = get,                 
       setinv = setinv,
       getinv = getinv)                     # formatting the actual list
}


## Write a short comment describing this function
# function to find an inverse of matrix 
# which was saved with makeCacheMatrix() into list x 
# either from cache or from scratch if it wasn't calculated yet
cacheSolve <- function(x, ...) {
  xinv <- x$getinv()                      # getting the inverse from list x; 
  if (!is.null(xinv)){                    # if there is an inverse inside the list, 
    print("Reading solution from cache!") 
    return(xinv)                          # it returns the inverse from the list
  }
  else {                                  # otherwise
    data <- x$get()                       # getting matrix from list
    xinv <- solve(data, ...)              # solving for the inverse of matrix
    x$setinv(xinv)                        # writing the solution into the list
  }
  #         ## Return a matrix that is the inverse of 'x'
  }

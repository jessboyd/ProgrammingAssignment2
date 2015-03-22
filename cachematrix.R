## These functions are used to create an object that stores a matrix and cache's its inverse

## The 'makeCacheMatrix' function creates a list which contains functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(data_matrix = matrix()) {
  
  stored_inverse <- NULL                            # Initialise the inverse variable to NULL
  
  set <- function(y) {                              # 'y' is the matrix argument passed into 'makeCacheMatrix'
    data_matrix <<- y                               # Set 'data_matrix' for the function environment to 'y'
    stored_inverse <<- NULL                         # Set 'stored_inverse' for the 'makeCacheMatrix' environment to NULL
  }
  
  get <- function() {                               # Create a function 'get' in the 'makeCacheMatrix' parent environment 
    data_matrix                                     # and assign a matrix to it
    } 
  
  setinverse <- function(inverse) {                 # Takes a value ('inverse') and sets it to the value of 'stored_inverse' 
    stored_inverse <<- inverse                      # in the 'makeCachedMatrix' frame
  }
  
  getinverse <- function() {                        # Returns the value of 'stored_inverse' from the 'makeCachedMatrix' frame
    stored_inverse
  }
  
  list(set = set, get = get,                        # Lists out the values of the functions in the 'makeCachedMatrix' frame
       setinverse = setinverse,
       getinverse = getinverse)
  }
}


## The 'cacheSolve' function calculates the inverse of the object created with the above function. 
## It checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation
## If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function. 

cacheSolve <- function(made_matrix, ...) {
  
  local_inverse <- made_matrix$getinverse()         # Goes to the 'made_matrix' environment and assigns the 
                                                    # 'stored_inverse' value to the 'local_inverse' in this environment
  
  if(!is.null(local_inverse)) {                     # If the 'made_matrix' environment has been evaluated before,
    message("getting cached data")                  # the function prints the message and the cached inverse ('local_inverse)
    return(local_inverse)
  }
  
  local_data <- made_matrix$get()                   # If this 'made_matrix' has not been evaluated before, the 'made_matrix'
                                                    # gets put into a local variable called 'local_data'
  local_inverse <- solve(local_data, ...)           # Calculate the inverse by calling 'solve' on the local variable 
  
  made_matrix$setinverse(local_inverse)             # Assign the calculated inverse to the 'made_matrix' environment 
                                                    # using the 'setinverse' function
  
  local_inverse                                     # Display the calculated inverse
}
makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL #result of inversion is stored
  set <- function(y) {
    x <<- y
    xinv <<- NULL #Initialises xinv to null
  }
   get <- function() x # return input matrix
  setInv <- function(inv) xinv <<- inv # set  inversed matrix
  getInv <- function() xinv # return  inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() # get inversed matrix from object x
  if(!is.null(m)) 
    { 
    #if the inversion result is there
    message("cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # solve
  x$setInv(m) # set it to the object
  m # return result
}

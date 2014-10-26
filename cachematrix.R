## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function:makeCacheMatrix,creates a special list containing functions to
## set the value of the vector:set()
## get the value of the vector:get()
## set the value of the inversematrix:setinversematrix()
## get the value of the inversematrix:getinversematrix()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the vector from the cache
  get <- function() {x}
  ## set the value of the inversematrix
  setinversematrix <- function(inverse) {m <<- inverse}
  ## get the value of the inversematrix
  getinversematrix <- function() {m}
  ## return the final list of functions
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)

}


## Write a short comment describing this function
## The function cacheSolve calculates the inversematrix with the help of the special vector function:makeCacheMatrix created above. 
## It first checks if the inversematrix has already been calculated. If so, it gets the inversematrix from the cache. 
## Otherwise, it calculates the inversematrix of the data and sets the value in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
  ## get inversematrix from cache by assignning it to m and check whether it exists. If it is so, return m from cache.
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##If inversematrix hasn't been stored in cache, caculating it with solve fuction and store its value in cache by using setinversematrix function.
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}

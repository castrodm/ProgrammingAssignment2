#makeCacheMatrix: generate a list that will be used by the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
      
      #start variable to store the inverse 
      m <- NULL 
      
      #set: set matrix to be = to the input of makeCacheMatrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      #get: retrieve value of the matrix (setted by the set function)
      get <- function() x
      
      #setinv: set the inverse of matrix x to the variable m
      setinv <- function(solve) m <<- solve
      
      #getinv: retrieve value for the inverse
      getinv <- function() m

      #output: a list containing the inside functions set, get, setinv, getinv
      #should be used as input for cacheSolve function
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

#cacheSolve: compute the inverse of a "special" matrix returned by the above function
cacheSolve <- function(x, ...) {
      
      #m = inverse of x, if it was computed before
      #x$getinv = NULL, x inverse not computed before
      m <- x$getinv()
      
      #return inverse of x (m) stored by makeCacheMatrix if m not NULL
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      #if m is NULL, return value of the matrix x, using $get()
      data <- x$get()
      
      #...and compute m (inverse of x!)
      m <- solve(data, ...)
      
      #finally, use set inverse of x to the variable m
      x$setinv(m)
      
      #return inverse of x(m)
      m
}

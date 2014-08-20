makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}



# Part 1: makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        # set x to empty matrix, and  Inverse to null - initially

        set <- function(y){
                x <<- y
                I <<- NULL
        }
        get <- function() x
        #  return the matrix
       
	   setInverse <- function(solve) I <<- solve
        #  override the previous value of I, and assign it to Inverse 
        
		getInverse <- function() I
        #  return the Inverse
        
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        #  list of  functions      
}


# Part 2: cacheSolve

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("Take cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
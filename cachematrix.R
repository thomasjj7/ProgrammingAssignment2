## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the value (inv) of the vector
## get the value (inv)of the vector
## set the value (inv)of the inversion
## get the value (inv)of the inversion


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <- NULL
    }
    get <- function() x
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
      if(!is.null(inv)){
          message("getting cached data")
          return(inv)
      }

  ## Return a matrix that is the inverse of 'x'    
    data <- x$get()
      inv <- solve(data,...)
      x$setInv(inv)
      inv
}
## test ##

test.m <- matrix(3:6,2,2)
test.m
cached.test <- makeCacheMatrix(test.m)
cached.test$get()
cached.test$getInv

cacheSolve(cached.test)

test.m2 <- matrix(c(6,2,1,3),2,2)
test.m2
cached.test2 <- makeCacheMatrix(test.m2)
cached.test2$get()
cached.test2$getInv

cacheSolve(cached.test2)

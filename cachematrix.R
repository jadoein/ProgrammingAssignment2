##This script defines two functions:-makeCacheMatrix and cacheSolve. The
##first function creates a matrix which is capable of storing its own inverse
##second function clacultes the inverse and saves the inverse in the first functions
##environment as cached value to avoid recalculations.

##Function makeCacheMatrix
#Interal variables:-
#-----------------------------------
#x :- Input Matrix
#inv :- Inverse of Input matrix
#-----------------------------------
#Internal Funtions:-
#-----------------------------------
#set(y) :- set value of matrix x to y
#get() :- return value of x
#setinv(y) :- set value of inverse matrix inv to y
#getinv() :- return value of inverse inv
#-----------------------------------
makeCacheMatrix <- function(x = matrix()) {
#stop the process if matrix is not square
  if (nrow(x) != ncol(x)) {
    return(print ("Dimensions of input matrix are NOK"))
  }
	inv <- NULL
	set <-function(y) {
#stop the process if matrix is not square
	  if (nrow(y) != ncol(y)) {
	    return(print ("Dimensions of input matrix are NOK"))
	  }
		x   <<- y
		inv <<- NULL
	}
	get <-function() { x }
	setinv <-function(y) { inv <<- y}
	getinv <-function() {inv }
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##cacheSolve returns the cached inverted value of x created by makeCacheMatrix
##If cached value doesn't exist, it calculates the value and saves the value 
##in environment of x

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
  	print("Inverse of matrix found in cache")
  	return(inv)
  }
  inp <- x$get()
  inv <- solve(inp)
  x$setinv(inv)
  inv
}

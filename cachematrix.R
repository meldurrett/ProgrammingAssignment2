## These functions cache a vector and compute and cache its inverse, 
## then easily retrieve them in lieu of time-consuming re-computation.

## makeCacheMatrix() creates several useful functions to cache matrix data
## 		It uses lexical scoping to keep objects available outside of the functions
##		in order to use them later

makeCacheMatrix <- function(X = matrix()) {  ##argument is a matrix
	I <- NULL
	set <- function(Y) {					#cache this matrix for retrieval later
		X <<- Y			
		I <<- NULL
	}
	get <- function() X						#retrieve this matrix for cache
	setinv <- function(Inv) I <<- Inv		#cache the inverse of this matrix
	getinv <- function() I					#retrieve the inverse of this matrix
	list(set=set,
		get=get,
		setinv=setinv,
		getinv=getinv)						#print this list when function is called
}

## cacheSolve() returns a matrix that is the inverse of 'X'
##		and caches it for use by the functions above

cacheSolve <- function(mCM, ...) {		#argument is of type 'makeCacheMatrix'
	I <- mCM$getinv()
	if(!is.null(I)) {
		message("getting cached data")
		return(I)
	}
	X <- mCM$get()
	I <- solve(X)
	mCM$setinv(I)
	I
}


##------var for testing-------

iris.mat <- as.matrix(iris[1:4, 1:4])
class(iris.mat)
X <- iris.mat
mCM <- makeCacheMatrix(X)
mCM
mCM$get()
cacheSolve(mCM)
mCM$getinv()




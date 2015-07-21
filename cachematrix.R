## These functions can be used to create a special matrix that can cached its inverse `makeCacheMatrix`
## and to compute and cached the inverse of this special matrix `cacheSolve`.

## A function that creates a list that is used as a special "matrix" that can stored a matrix and cached its inverse
## It offers four methods:
## set(x) to set an new matrix x.
## get() to get the currently saved matrix or NULL if no matrix is saved.
## setInvMat(x) to cached the inverse matrix x.
## getInvMat() to get the inverse matrix if it has already been computed and NULL otherwise.

## Use example
## >> aMat <- matrix(1:9, nrow=3, ncol=3)
## >> aCacheMat <- makeCacheMatrix(aMat)
## >> cacheSolve(aCacheMat) ## compute, cache and return the inverse
## >> cacheSolve(aCacheMat) ## return the cache inverse

makeCacheMatrix <- function(x = matrix()) {
	myInvMat <- NULL

	## Set an new matrix x.
	set <- function(y) {
		x <<- y
		myInvMat <<- NULL
	}

	## Return the currently saved matrix or NULL if no matrix is saved
	get <- function() x

	## Cache the inverse matrix x.
	setInvMat <- function(invMat) myInvMat <<- invMat

	## Return the inverse matrix if it has already been computed and NULL otherwise
	getInvMat <- function() myInvMat
	list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}


## This function returns the inverse of a matrix (x).
## If the inverse was not computed, it is computed and cached.
## It assumes that x is invertible.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInvMat()
	if (!is.null(m)) {
		return(m)
	}
	## So the matrix has not been computed yet
	aMat <- x$get()
	invMat <- solve(aMat)
	x$setInvMat(invMat)
	invMat
}

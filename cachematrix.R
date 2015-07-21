## These functions can be used to create a special matrix that can cached its inverse `makeCacheMatrix`
## and to compute and cached the inverse of this special matrix `cacheSolve`.

## A function that creates an R object that can stored a matrix and its inverse
## It offers four methods:
## set(x) to set an new matrix x.
## get() to get the currently saved matrix or NULL if no matrix is saved.
## setInvMat(x) to cached the inverse matrix x.
## getInvMat() to get the inverse matrix if computer and NULL otherwise.
makeCacheMatrix <- function(x = matrix()) {
	myInvMat <- NULL
	set <- function(y) {
		x <<- y
		myInvMat <<- NULL
	}
	get <- function() x
	setInvMat <- function(invMat) myInvMat <<- invMat
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
	aMat <- x$get()
	invMat <- solve(aMat)
	x$setInvMat(invMat)
	invMat
}

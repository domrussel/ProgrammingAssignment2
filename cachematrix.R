# These functions allow us to more efficiently retreive the
# inverse of a matrix if it has to be calculated more than once.
# Information about the matrix can be stored and retrived using the list
# of functions that make up makeCacheMatrix.
# Then the cacheSolve function determines whether or not the inverse
# has already been calculated. It simply returns the already stored
# and calculated inverse function if it has; otherwise it calculates
# the inverse and returns it. If the inverse is needed more than once
# for the same large matrix, these two functions together help
# us prevent wasting time and RAM recalcuating that matrix.



# The makeCacheMatrix function takes a square matrix as an input.
# It then defines four more functions: setmat, getmat, setinv, and getinv.
# setmat can be used for resetting a matrix's values. Importantly,
# when it is called it also resets the inverse of the matrix to NULL.
# getmat simply returns the already set matrix. setinv sets the
# inv value (which reprsents the inverse of the matrix, 
# and is orginally set to NULL) to the value of the.
# function's input. getinv simply returns this inverse matrix.
# the makeCacheMatrix function simply returns a list of all of these functions
# with the input matrix.
# An example use of the function would be:
# a = makeCacheMatrix(matrix(rnorm(16), 4, 4))
# a$getmat()
# Returns something like:
#             [,1]        [,2]       [,3]       [,4]
# [1,]  0.86837243 -0.15161679  0.2752089  0.1975174
# [2,] -0.02783448 -0.79034830 -0.1587747  0.6875630
# [3,]  1.26084931 -0.04108948  0.1881300  0.5239410
# [4,] -0.19892255 -2.96833389  1.8337763 -0.4528985

makeCacheMatrix <- function(x = numeric()){
	inv <- NULL
	setmat <- function(y){
		x <<- y
		inv <<- NULL
	}
	getmat <- function(){
		x
	}
	setinv <- function(inverse){
		inv <<- inverse
	}
	getinv <- function(){
		inv
	}
	list(setmat = setmat, getmat = getmat,
		setinv = setinv, getinv = getinv)
}

# The cacheSolve function takes an input "cache matrix" the has been
# created using the makeCacheMatrix function. It first uses the
# object's getinv function to find the inverse of the matrix. If
# it hasn't previously been definied it returns NULL. If it does return
# NULL the function calls the getmat function to first get the matrix,
# then uses the setinv function to store the inverse of the function
# (which is calculated using the built-in solve function), and this
# newly calculated inverse is returned. If the inverse is not NULL
# at first, then the function simply retreves this cached data
# and returns it without recalculating the inverse.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	a = x$getinv()
	if(is.null(a)){
		mat = x$getmat()
		x$setinv(solve(mat))
		return(x$getinv())
	}
	print("getting cached data")
	a
}

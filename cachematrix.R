# # These functions work together to create a special object
# # filled with functions that manage other objects in memory
# # The cacheSolve will use this special object to return
# # the desired result by checking if it's already in memory

# ----- EXPLANATION FOR makeCacheMatrix -----
# This function creates an object related to the input Matrix and derives
# 4 functions that manage that as x, together with other non declared objects
# This takes advantage of Lexical Scoping since the inside functions will
# Reference the original input matrix named x as this is where the functions
# were originally created (Lexical vs Dynamic Scope) The original matrix is
# referenced by the name 'x' and remains so for the created object
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(value) inverse <<- value
	getInverse <- function() inverse
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

# ----- IMPORTANT NOTE FOR CODE EXPLANATION -----
# NOTE: The 'set' function inside the special vector is never actually used 
# in the example provided by the instructors or in this implementation. 
# BUT it can be used to modify the cached values of the original matrix and 
# "reset" the object itself, even if substituting with the original matrix.
# It will automatically modify the objects inside its own namespace. This 
# means that if you have a sepparate object resulting from makeCacheMatrix 
# neither will interfere with each other

# # # Please Refer to the attached picture for an example # # #

# ----- EXPLANATION FOR cacheSolve -----
# This function receives the special cache object from makeCacheMatrix
# that will be used to obtain the original matrix object. The special object
# does not have the matrix itself, as it is referenced in its creation 
# space which they search for. If within the creation environment of the 
# special object the Inverse has not been calculated it will do so and save 
# it using the defined functions. Else it will return the saved value
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
			message("getting cached data")
			return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}



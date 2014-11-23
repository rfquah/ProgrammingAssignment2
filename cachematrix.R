
## makeCacheMatrix is a function that takes a matrix as its argument,
##	and sets im (the inverse matrix) to NULL in the function environment.
##	Contained within the makeCacheMatrix function is the function set,
##	which sets im to NULL in the global environment.  But the function set 
##	is only defined within makeCacheMatrix.  it is actually executed within
##	the cacheSolve function.  

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL   # inverse matrix, set to NULL every time the makeCacheMatrix function is called

	## NOTE: functions defined below (set, get, setinv, and getinv) are only defined here
	##		they run when called from the cacheSolve function
	
	set <- function(y) {
		x <<- y
		im <<- NULL
		}
	get <- function() x                    # this function returns the value of the original matrix 
 
	setinv <- function(inv) im <<- inv     # this function is called by cacheSolve() during the first cacheSolve() access,
							   # and will store the value using the superassignment <<-	

	getinv <- function() im                # this function will return the cached inverse matrix to cacheSolve() on
                                             # subsequent accesses

	list(set = set, get = get, setinv = setinv, getinv = getinv)
 	                                       # this list is created each time makeCacheMatrix() is called.  it is a list
							   # of the internal functions ('methods') so a calling function knows how to
							   # access those methods.
	}


## cacheSolve is a function that takes a matrix (x) as its argument and
##	returns the matrix inverse from the cache if found in cache,
##    otherwise calculates the inverse and returns the newly calculated
##  	result.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$getinv()
	if (!is.null(im)) {
		message("getting cached data")
		return(im)
		}
	data <- x$get()
	im <- solve(data)
	x$setinv(im)
	im
	}








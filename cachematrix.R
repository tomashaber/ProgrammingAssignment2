## Returns a list of functions - set/get/setInverse/getInverse
makeCacheMatrix <- function(x = matrix()) {
	 cache <- NULL
		## set x and clears the cache
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
		## retrieves x
        get <- function() x
		## updates cache
        setInverse <- function(inverse) cache <<- inverse
		## retrieves cached value
        getInverse <- function() {
				solve(x) 
		}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes and inverse of a matrix. If the inverse is already computed,
## then it's returned from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInverse()
		
		#if the cache is bit there, then compute the inverse and store it 
        if (is.null(m)) {
			data <- x$get()
			m <- getInverse(data, ...)
			x$setInverse(m)
		}
		## return the inverse 
        return m
}

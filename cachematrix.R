## This function creates a Matrix
## cacheSolve gets de inverse of the Matrix
## If the inverse has been previously calculated
## it will return the value in the cache instead of 
## calculating the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
		mat_inv_x <- NULL
            set <- function(y) {
                    x <<- y
                    mat_inv_x <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) mat_inv_x <<- inverse
            getinv <- function() mat_inv_x
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## cacheSolve returns the inverse of a matrix created with makeCacheMatrix
## But if the inverse is in the cache the function returns it, if it does
## not exist in the cache the function computes de inverse, return
## it and stores it in the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mat_inv_x <- x$getinv()
            if(!is.null(mat_inv)) {
                    message("getting cached inverse matrix")
                    return(mat_inv_x)
            }
    else {
            mat_inv_x <- solve(x$get())
            x$setinv(x)
			return(mat_inv_x)
            
		}
}
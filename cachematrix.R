## Function 1

makeCacheMatrix <- function(x = matrix()) 
{
        invrs <- NULL
        set <- function(y) 
	{
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Function 2

cacheSolve <- function(x, ...) 
{
        invrs <- x$getInverse()
        if (!is.null(invrs)) 
	{
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setInverse(invrs)
        invrs
}
# Below are two functions that are used to create a special object that can 
# cache the inverse of a matrix


# makeCacheMatrix returns a list with a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix and cache the result

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}

# Test
# x <- matrix(4:7, nrow=2, ncol=2)
# x
# mc <- makeCacheMatrix(x)
# cs <- cacheSolve(mc)
# cs

# Returns a "getting cached data" message
# cacheSolve(mc)

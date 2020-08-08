## The functions take in a matrix and store it in memory along with its inverse. The inverse is calulated only once, and then it is retrieved from cache



# This functions takes in an matrix as input. It returns a list of functions that can set the matrix, get the matrix, set the matrix's inverse and get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) 
{
    matrix_inverse <- NULL
    set <- function(input) 
    {
        x <<- input
        matrix_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix_inverse <<- inverse
    getinverse <- function() matrix_inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function takes in the list of functions returned by makeCacheMatrix function as input. It returns the inverse of the matrix if it exists, else it computes the 
# inverse, stores it and then returns it

cacheSolve <- function(x, ...) 
{
    inverse <- x$getinverse()
    if(!is.null(inverse)) 
    {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

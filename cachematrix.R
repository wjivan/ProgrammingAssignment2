## cachematrix caches the matrix inverse solution

##makeCacheMatrix makes a list of functions that can be called on
##set sets the matrix
##get returns the matrix set
##setanswer sets the inverse to the matrix manually
##getanswer shows the inverse 

makeCacheMatrix <- function(x = matrix()) {
        ans <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setanswer <- function(answer) ans <<- answer
        getanswer <- function() ans
        list(set = set, get = get, setanswer = setanswer,getanswer = getanswer)
}



## cacheSolve solves the inverse of matrix. It caches the solution and 
## use it when you are suppose to solve the same matrix again.

cacheSolve <- function(x, ...) {
        inv <- x$getanswer()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ans <- solve(data, ...)
        x$setanswer(ans)
        ans
}

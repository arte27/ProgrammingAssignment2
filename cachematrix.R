# Matrix inversion function in R  costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inverse) inv <<- inverse
        getinversematrix <- function() inv
        list(set=set, get=get, setinversematrix=setinversematrix, getinversematrix=getinversematrix)
}


#cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed(in makeCacheMatrix function). If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinversematrix function.

cacheSolve <- function(x, ...) {
        inv <- x$getinversematrix()
        if(!is.null(inv)) {
                message("getting cached data.Inverse matrix is retrieved from the cash.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinversematrix(inv)
        inv
        
}

## Please take a look at the following example 

#Define and insert the below Matrix to the cash.
x = rbind(c(2,1), c(1,2))
m= makeCacheMatrix(x)
m$get() #look at the cashed matrix
cacheSolve(m) #calculat inverse matrix 1st time
cacheSolve(m) #calculate inverse matrix 2nd time(do not compute ,retrieve from the cash)

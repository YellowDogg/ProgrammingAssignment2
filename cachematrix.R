##  The methods below are designed to speed up calculations involving 
## calculating the inverse of a matrix by ensuring that the inverse of
## a matrix is only determined once.  The method makeCacheMatrix is used
## to create a special object that holds the matrix and a cached value
## if the matrix inverse.  The method cacheSolve takes this object and
## returns the cached value if available, or otherwise calculates the
## inverse and saves it for later use

## makeCacheMatrix creates a list functions including:
## 1. A function that sets the value of a matrix
## 2. A function that gets the value of a matrix
## 3. A function that sets the value of the matrix inverse
## 4. A function that gets the value of the matrix inverse
## Lexical scoping is use to maintain the values of the matrix (x) and
## inverse (inv) within the function closure

makeCacheMatrix <- function(x = matrix()) {
    
    # Inverse is not yet calculated: set initial value to NULL
    inv <- NULL
    
    # Create function that sets the value of matrix
    set <- function(y) {
        x <<- y # Use <<- to assign value to x in parent function environment
        inv <<- NULL # Need to reset inv, since matrix is new
    }
    
    # Create function that returns the value of matrix from parent environment
    get <- function() x
    
    # Create function that sets the value of inv in parent environment
    setinv <- function(inverse) inv <<- inverse
    
    # Create function that returns the value of inv from the parent environment
    getinv <- function() inv
    
    # Create list of the created functions and return list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve takes a cached matrix object made by makeCacheMatrix and returns
## the inverse.  The cached inverse is returned if available, otherwise the 
## the inverse is calculated, cached in the matrix object and returned

cacheSolve <- function(x_cm, ...) {
    ## Return a matrix that is the inverse of the matrix in 'x_cm'
    ## where x_cm is a cache matrix object created by makeCacheMatrix
    
    # Get cached value of matrix inverse
    inv <- x_cm$getinv()
    
    # If cached value is valid (not NULL) return it
    if(!is.null(inv)) {
        message("using cached data")
        return(inv)
    }
    
    # Otherwise, calculate inverse and return
    x <- x_cm$get() # get matrix from x_cm object
    inv <- solve(x) # calculate inverse (assuming x is always invertible)
    x_cm$setinv(inv) # cache value of inv
    inv # return calculated value
}

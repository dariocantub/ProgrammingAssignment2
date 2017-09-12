## Functions that cache the inverse of a matrix for computational efficiency

## Creates a list-class Matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {     
        # Define "m" as null for code convenience 
        m <- NULL
        
        ## Functions to define and get matrix in/from cache
        # Method to set up the matrix in cache
        set <- function(y) {
                x <<- y      
                m <<- NULL   
        }
        # Method to get the matrix from the cache
        get <- function() 
                x            
        
        # Method to set up the inverse
        setinv <- function(inv) {
                m <<- inv    # defines 
        }
        # Method to get (calls) the inverse from the cache
        getinv <- function() {
                m
        }
        # Return a list of our previously defined values
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Calculate the inverse of the special list-matrix created by "makeCacheMatrix" (above).
## If the inverse has been previously been calculated the code retrieves the inverse
## from the cache; if not, it will calculate its inverse with the function "solve()"

cacheSolve <- function(x, ...) {
        # Calls inverse from function getinv() 
        m <- x$getinv()
        ## Checks if "m" is equal to null, i.e. if inverse doesn't exist
        ## If it exists it returns the inverse
        if(!is.null(m)) {
                message("getting cached data")
                # returns the inverse value if exists and ends the function
                return(m)  
        }
        ## If it doesn't exist then
        # Gets matrix from object 'x'
        data <- x$get()
        # Calculates inverse
        m <- solve(data, ...)
        ## Sets inverse to the object
        x$setinv(m)
        # returns calculated inverse
        m
}

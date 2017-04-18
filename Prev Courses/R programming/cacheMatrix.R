# This function creates a special "matrix" object that can cache its inverse.
# m: an empty matrix with 0 col and 0 row
makeCacheMatrix <- function(m = matrix(numeric(),0,0) {
        
        #cached matrix
        m.cached <- NULL
        
        #Set function : store the new matrix and reset the cached matrix
        set <- function(y) {
                m <<- y
                m.cached <<- NULL
        }
        
        #Get function : return the matrix
        get <- function() m
        
        #Calculate the inverse matrix and cache it to m.cached
        cachematrix <- function(m.inverse) m.cached <<- m.inverse
        
        #Return the last cached inverse matrix
        getcache <- function() m.cached
        
        #list the available functions
        list(set = set, get = get, cachematrix = cachematrix, getcache = getcache)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
        
        #get the last cached inverse matrix
        m.cached <- m$getcache()
        
        #the cached inverse matrix is available then send a message and return it
        if(!is.null(m.cached)) {
                message("getting cached inverse matrix")
                return(m.cached)
        }
        
        #not cached? then get the matrix
        data <- m$get()
        
        #calculate the inverse matrix with its parameters
        m.inverse <- solve(data, ...)
        
        #cache this inverse matrix
        m$cachematrix(m.inverse)
        
        #return the cached inverse matrix
        m.cached
}
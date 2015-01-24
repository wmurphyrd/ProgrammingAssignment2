## These functions create a list object that holds a matrix and
## caches a copy of its inverse so that the calculation doesn't need
## to be repeated each time the inverse is needed. 

## makeCacheMatrix creates the list object from any invertible matrix x
## the value is a list containing functions to get/set the matrix itself
## and get/set the inverse solution. Note that the inverse getter and setter
## should not be called directly - see cacheSolve below

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL;
    set <- function(y) {
        x<<-y;
        solution<<-NULL; # reset the solution when the matrix is modified
    }
    # x is hidden from direct access to ensure the setter
    # can reset the solution when it is changed, use the
    # get function to view the value of x
    get <- function() x; 
    setSolution <- function(sol) {solution <<- sol;}
    getSolution <- function() solution; 
    list(set=set,get=get,setSolution=setSolution,getSolution=getSolution)
    
}


## Returns the inverse of the matrix, using the last calculated result
## if the matrix has not been changed. Always use this function rather than 
## getSolution from the cacheMatrix list object to ensure a valid
## solution is returned even when the solution hasn't been calculated 

cacheSolve <- function(x, ...) {
    #if the solution is not already cached, calculate it and cache it    
    if(is.null(x$getSolution())) x$setSolution(solve(x$get()));
    #now the solution is either already available or just calculated
    x$getSolution();
}

## These functions create a list object that holds a matrix and
## caches a copy of its inverse so that the calculation doesn't need
## to be repeated each time the inverse is needed. 

## makeCacheMatrix creates the list object from any invertible matrix x
## the value is a list containing functions to get/set the matrix itself
## and get/set the inverse solution. Note that the inverse getter and setter
## should not be called directly - see cacheSolve below

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL;
    # need to keep track of optional arguments used in the solve calculation
    # otherwise calling cacheSolve a second time after changing the 
    # arguments but not the matrix will return an incorrect chached result
    argsList <- list(); 
    set <- function(y) {
        x<<-y;
        solution<<-NULL; # reset the solution when the matrix is modified
    }
    # x is hidden from direct access to ensure the setter
    # can reset the solution when it is changed, use the
    # get function to view the value of x
    get <- function() x; 
    setSolution <- function(sol,...) {
        solution <<- sol; 
        # save any extra parameters that were sent to solve() 
        argsList<<-list(...);
    }
    getSolution <- function(...) {
        # force cacheSolve to recalculate the solution if
        # the ... arguments are different from when it was calculated
        if(!identical(list(...),argsList)) return(NULL);
        #otherwise just return the cached solution
        solution;
    } 
    list(set=set,get=get,setSolution=setSolution,getSolution=getSolution)
    
}


## Returns the inverse of the matrix, using the last calculated result
## if the matrix has not been changed and the solve parameters are the same.
## Always use this function rather than getSolution from the cacheMatrix 
## list object to ensure a valid solution is returned even when the 
## solution hasn't been calculated 

cacheSolve <- function(x, ...) {
    #if the appropriate solution is not already cached,
    # calculate the inverse and cache it    
    if(is.null(x$getSolution(...))){
        # in addition to providing them to the solve function,
        # the ... args are also passed to setSolution so that the
        # cache matrix can track if they have changed
        x$setSolution(solve(x$get(),...),...);
        print("Recalculating solution.");
    }
    #now the solution is either already available or was just updated
    x$getSolution(...);
}

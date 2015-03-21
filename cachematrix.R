
##First Function: makeCacheMatrix, this function creates a special matrix object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
##The function takes an argument, x which is set to an empty matrix
        I<-NULL
        ## Sets the variable I equal to NULL
        
        set<-function(y){
        ## set, the first element of the list is used to assign the matrix, as entered by the user, to x
                x<<-y
                ## The value of the argument is assigned to x
                I<<-NULL
                ## The variable I is reassigned to NULL after the set function is called
        }
  
        get<-function() x
        ## get, the second element of the list is used to return the matrix
        
        setInverse<-function(solve) I<<-solve
        ## setInverse, the third element is used to override the previous value of Inverse and assign the argument to I
        
        getInverse <-function() I
        ## This function is used to return the inverse
  
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        ## A list is returned with above defined functions as its elements

}

## Second Function: cacheSolve, this function is used to compute the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'

        I<-x$getInverse()
        ## Gets the value of inverse, if already calculated
        
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        ## The value of inverse, if not NULL, is returned. If NULL, it is calculated in steps below
        
        data<-x$get()
        ## The get function, also an element of the list defined in the makeCacheMatrix, is used to assign the matrix to the variable (data)
        
        I<-solve(data,...)
        ## solve function is used to calculate the inverse of the matrix
        
        x$setInverse(I) ## The newly calculated value is passed as an argument to the set function
        I ## The value of inverse is returned
}

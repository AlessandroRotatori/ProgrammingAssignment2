## The goal of these functions is to define a process to speed the computation
## of the inverse of a matrix. A special 'matrix' object will be defined, in which will be stored
## both the original matrix and the computated the inverse.
## After the computation the inverse matrix will be cached in the same object.
## In such a way, whenever will be required to compute again the inverse of original matrix,
## it will be immediately loaded from the cache.

## This first function only creates the special 'matrix' object in which the original matrix and, 
## subsequently, its inverse will be stored.
## x is the original matrix and inv is the inverse matrix. get (set) defines (loads)
## the original matrix. setinverse (getinverse) is the variable in which the inverse will 
## be stored (on which the control for a previous computation will be made)

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the object used to store the inverse matrix
        inv <- NULL
        ## set function re-sets the original matrix and re-initializes the inverse matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## get fuction retrives the orginal matrix
        get <- function() x
        ## setinverse function stores in the cache the computed inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        ## getinverse function retrives the inverse matrix
        getinverse <- function() inv
        ## result of makeCacheMatrix is a list object made by the 4 functions defined previously
        ## and the stored values.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is the function where the actual computation is made.
## First, a check IF the inverse matrix was already computed (if it is present in the 'special' object)
## If it is not, then the actual computation is made, and the inverse matrix is stored in the 
## 'special' object.
## Last but not least the value of the inverse matrix is returned as result of the function

cacheSolve <- function(x, ...) {
        ## retrive the value stored in the special 'object' for the inverse matrix
        inv <- x$getinverse()
        ## IF clause, check if the inverse function is already computed and if positive,
        ## retrive its value
        if(!is.null(inv)){
                message("Inverse matrix was already computed. Loading result from cache")
        }
        ## ELSE, retrive the orginal matrix, compute the inverse matrix with solve() function
        ## and store its value in the cache
        else{
                data <- x$get()
                inv <- solve(data)
                x$setinverse(inv)
}
        ## returning the inverse of the matrix
        return(inv)
}
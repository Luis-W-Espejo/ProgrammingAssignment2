################################## Coursera ##################################
################################ R Programming ###############################

#Luis W. Espejo


## First, I create the list of functions
## This list contains the main functions to get the inverse of a matrix
## In this case we passed the value to s

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y = matrix()){
        x <<- y
        s <<- NULL
    }
    get <- function(){
        x   
    } 
    
    setsolve <- function(solve) {
        s <<- solve
    }
    
    getsolve <- function() {
        s
    }
    
    list(set=set,
         get=get,
         setsolve=setsolve,
         getsolve=setsolve)
}

## And with this function we obtained the cached data
## First, by making sure that we've already calculated the inverse of the matrix
## If not, then it calculates the value

#It's important to note that we call the elements of the list
#That I created in the first part

cacheSolve <- function(x, ...) {
    s <-makeCacheMatrix$getsolve()    
    if(!is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    data <- makeCacheMatrix$get()
    s.local.calculated <- solve(data, ...)
    makeCacheMatrix$setsolve(s.local.calculated)
    s.local.calculated # Return of the inverse of the matrix
}

################# Test the function #################
#Inverse of a matrix in a "normal" process

#The matrix
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
#Inverse of the matrix
n1 <- solve(m1)
#By multiplying the arrays we get the identity matrix
i1 <- m1 %*% n1
i1

#Using the new function
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)

myMatrix_object$get()
myMatrix_object$getsolve()

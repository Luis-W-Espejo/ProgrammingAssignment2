################################## Coursera ##################################
################################ R Programming ###############################

#Luis W. Espejo


## First of all, I create a list of functions
## This list contains the main functions to get the inverse of a matrix

## In this case we defined a matrix called x
makeCacheMatrix <- function(x = matrix()) {
    #Defining the local variable
    s <- NULL
    #First function
    #To be used whenever we need to set a new matrix
    set <- function(y = matrix()){
        x <<- y
        s <<- NULL
    }
    #Second function
    #For calling the values of the matrix
    get <- function(){
        x } 
    #Third function
    #For setting the function to use (inverse of a matrix)
    setsolve <- function(solve) {
        s <<- solve }
    #Fourth function
    #For calling the function to use
    getsolve <- function() {
        s }
    #The list of functions
    list(set=set,
         get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}

## Second of all, I create the function for obtained the cached data
## If it is not already calculated, the function will do it

#It's important to note that we call the elements of the list in the x object
cacheSolve <- function(x, ...) {
    #Checking if there is a result already
    s <- x$getsolve()    
    if(!is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    data <- x$get()
    s.local.calculated <- solve(data, ...)
    x$setsolve(s.local.calculated)
    s.local.calculated # Return of the inverse of the matrix
}

################# Test the function #################

#Inverse of a matrix in a "normal" process

#The matrix
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
#Inverse of the matrix
n1 <- solve(m1)
n1
#By multiplying the arrays we get the identity matrix
i1 <- m1 %*% n1
i1

#Using the new function
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)

#Setting a new matrix
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
 makeCacheMatrix <- function(x = matrix()) {
+     
+     j <- NULL                         ##initialize inverse as NULL
+     set <- function(y){
+         x <<- y
+         j <<- NULL
+     }
+     get <- function()x                #function to obtain matrix x
+     setInverse <- function(inverse) {j <<- inverse}
+     getInverse <- function() (j)
+     list(set = set, get = get, 
+          setInverse = setInverse, 
+          getInverse = getInverse)     ##function to obtain inverse of matrix
+ }
> 
> # this is used for cached data
> 
> cacheSolve <- function(x, ...) 
+     ## Return a matrix that is the inverse of 'x'
+     cacheSolve <- function(x, ...) {     ##gets cache data
+         j <- x$getInverse()
+         if(!is.null(inv)){                   ##checking whether inverse is NULL 
+             message("getting cached data")
+             return(j)                        ##returns inverse value
+         }
+         mat <- x$get()
+         j <- solve(mat,...)                ##calculate inverse value
+         x$setInverse(j)
+         j                               ## returns a matrix that is inverse of 'x'
+     }
> source("cachematrix.R")
> pmatrix <- makeCacheMatrix(matrix(1:8, nrow=2, ncol=2))
> pmatrix$.get()
Error: attempt to apply non-function
> pmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> pmatrix$getInverse()
NULL
> 

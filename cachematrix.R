## Here I design two function to cache the Inverse of a Matrix.
## The Reason why I design these two functions:
## Matrix inversion is usually a costly computation. 
## It might be beneficial to cache the inverse of a matrix rather than compute it repeatedly.


## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
+         set <- function(a) {
+                 x <<- a
+                 inv <<- NULL
+         }
+         get <- function() x
+         setInverse <- function(inverse) inv <<- inverse
+         getInverse <- function() inv
+         list(set = set,
+              get = get,
+              setInverse = setInverse,
+              getInverse = getInverse)

}


## The second function computes the inverse of the special "matrix" created by the first fountion makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
+         if (!is.null(inv)) {
+                 message("getting cached data")
+                 return(inv)
+         }
+         b <- x$get()
+         inv <- solve(b, ...)
+         x$setInverse(inv)
+         inv

}

## Testing those two functions
> test_matrix <- makeCacheMatrix(matrix(4:7,2,2))
> test_matrix$get()
     [,1] [,2]
[1,]    4    6
[2,]    5    7
> test_matrix$getInverse()
NULL
> cacheSolve(test_matrix)
     [,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
> test_matrix$getInverse()
     [,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
> 

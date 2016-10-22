## This assignment is to create the inverse of a matrix and cache it.
## If the matrix changes, the script should refresh the inverse.
## If the matrix has not changed, it should be retrieved from the cache.

## This function creates the inverse of the matrix and caches it in 
## the parent environment.
makeCacheMatrix <- function(x = matrix()) {

  mat_inv <- NULL
  set_mat <- function(mat2) {
    x <<- mat2
    mat_inv <<- NULL
  }
  get_mat <- function() x
  create_inv <- function(solve) mat_inv <<- solve
  get_inv <- function() mat_inv
  list(set_mat = set_mat, get_mat = get_mat, create_inv = create_inv, get_inv = get_inv)
  }

## This function calls the matrix inverse caching function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$get_inv()
  if(!is.null(mat_inv)) {
    message("Getting cached matrix inverse data")
    return(mat_inv)
  }
  mat_data <- x$get_mat()
  mat_inv <- solve(mat_data, ...)
  x$create_inv(mat_inv)
  mat_inv
}

## Run script with a 3 X 3 matrix 
mat_3 <- matrix(c(2, 5, 3, 2, 6, 1, 4, 3, 3), nrow = 3, ncol = 3, byrow = TRUE)
m3_inv <- makeCacheMatrix(mat_3)
cacheSolve(m3_inv)
cacheSolve(m3_inv)
cacheSolve(m3_inv)

## Run script with a 4 X 4 matrix 
mat_4 <- matrix(c(2, 5, 3, 2, 6, 1, 4, 3, 3, 4, 2, 4, 5, 3, 2, 6), nrow = 4, ncol = 4, byrow = TRUE)
m4_inv <- makeCacheMatrix(mat_4)
cacheSolve(m4_inv)
cacheSolve(m4_inv)
cacheSolve(m4_inv)

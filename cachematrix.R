cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Use solve() to compute the matrix inverse
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Inicializa o inverso como NULL
  
  # Define o valor da matriz e reseta o inverso armazenado
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reseta o cache quando uma nova matriz é definida
  }
  
  # Retorna o valor da matriz
  get <- function() x
  
  # Define o valor do inverso no cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Retorna o valor armazenado do inverso
  getInverse <- function() inv
  
  # Retorna uma lista com as funções
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

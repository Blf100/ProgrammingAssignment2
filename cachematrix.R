## makeCacheMatrix: Esta fun��o cria um objeto "matriz" especial que pode armazenar em cache seu inverso.
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solvematrix) inv <<- solvematrix
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Esta fun��o calcula o inverso da "matriz" especial retornada por makeCacheMatrix acima. Se o inverso j� tiver sido calculado (e a matriz n�o tiver sido alterada), o cachesolve dever� recuperar o inverso do cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}
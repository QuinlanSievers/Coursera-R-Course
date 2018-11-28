makeCacheMatrix <- function(x = matrix()) { #define makeCacheMatrix function, define x as empty matrix
    i <- NULL #set i as null
    set <- function(y) { #define set function
        x <<- y #set x as y in global environment
        i <<- NULL #set i as null in global environment
    }
    get <- function() x #define get function (returns input matrix)
    setinverse <- function(inverse) i <<- inverse #define setinverse function (sets the inverse)
    getinverse <- function() i #define getinverse function (returns inverse of matrix)
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse) #Generates output list of four functions
}

cacheinverse <- function(x, ...) { #define cacheinverse function
    i <- x$getinverse() #define i as output of getinverse
    if(!is.null(i)) { #conditional stating that if getinverse is not NULL, then to return the inverse of the matrix
        message("getting cached data")
        return(i)
    }
    data <- x$get() #define data as the input matrix
    i <- solve(data, ...) #define i as inverse of input matrix
    x$setinverse(i) #set inverse value as i in the global environment?
    i #return i
}

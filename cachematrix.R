#MakeCacheMatrix is a function that defines four functions: set, get,
#setinverse, and getinverse.
#You may call these daughter functions from the parent function using $.

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

#You can run cachesolve on the output of MakeCacheMatrix.
#It will return the inverse of the input matrix.
#It contains a conditional such that if you've already defined i with the
#inverse (i.e. if you just run cachesolve again) it will pull from i rather
#than recalculating the inverse of the input matrix.

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

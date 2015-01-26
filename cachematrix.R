# INPUT : a 2D matrix x whose inverse exists
# RETURN: a list of get/set API for matrix data
#         and it's inverse
# NOTE  : This function actually stores a
#         cache of the inverse. The inverse
#         itself has to be computed outside
#
makeCacheMatrix <- function(x = matrix())
{
    matInv <- NULL
    
    # Set matrix data
    setMat <- function(a_mat) {
        message("CALLED setMat")
        x <<- a_mat
        matInv <<- NULL
    }
    
    # Get matrix data
    getMat <- function() { 
        message("CALLED getMat") 
        x
    }
    
    # Set matrix inverse
    setMatInv <- function(a_matInv) { 
        message("CALLED setMatInv") 
        matInv <<- a_matInv
    }
    
    # Get matrix inverse
    getMatInv <- function() { 
        message("CALLED getMatInv") 
        matInv
    }
    
    # Return set/get API
    list(setMat = setMat, 
         getMat = getMat,
         setInv = setMatInv,
         getInv = getMatInv)
}

# INPUT: an API list x that provides marix
#        caching feature as described 
#        in the first function above
#        'instantiated' with a specific
#        matrix
# RETURN: inverse of the specific matix
# NOTE:   The function either retrieves
#         the inverse from the cache or
#         computes it the first time
#
cacheSolve <- function(x, ...) {
    
    # Try getting inverse from cache
    matInv <- x$getInv()    
    if(!is.null(matInv)) {
        message("FOUND CACHE!")
        return(matInv)
    }
    
    # Since inverse was null, 
    # compute it
    mat <- x$getMat()
    message("COMPUTING INVERSE")
    matInv <- solve(mat)
    
    # Cache the computed inverse
    x$setInv(matInv)
    
    # Return the inverse
    matInv
}
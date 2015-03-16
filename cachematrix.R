## This function will create a special matrix object 
## The object returned by this function
## will have methods to get the matrix data,inverse matrix data and 
## the methods to modify them

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL     ## create a place for inverse matrix
    
    ## set matrix value
    set <- function(newmat){ 
        x <<- newmat    ## use newmat matrix for this special matrix object
                        ## '<<-' operator is used for assigning the matrix
                        ## for this object from the parent environment
                        ## This way the matrix value can be added after creating
                        ## the object or values can be changed for an
                        ## existing object
        
        invMat <- NULL ## reset invMat (inverse matrix) 
    }
    
    ## get matrix value 
    get <- function(){ x }
    
    ## set inverted matrix value
    setInvMat <- function(mat){
        message(".....  ")
        invMat <<- mat      ## cache the latest inverse matrix
                            ## '<<-' operator used for assigning the inverse
                            ## matrix from the parent(Calling) environment
        message("cache set..")
    }
    
    ## get inverted matrix value
    getInvMat <- function(){ invMat }
    
    list(get = get, set = set, setInvMat = setInvMat, getInvMat = getInvMat)
}

## This function will generate the inverse matrix using solve function
##  
## First it checks if the inverse matrix exists
## If it does then that value is returned
##
## If it doesnt, a new one is generated and is saved/cached
## to be retrieved later

cacheSolve <- function(x, ...) {
       
    ## Access inverse matrix of x from cache
    invMat <- x$getInvMat()
    
    ## If inverse matrix exists it comes from cache and exit function   
    if (!is.null(invMat) ) {
        message("getting from cache....")        
        return(invMat )   ## exit function
    }
    ## If inverse object does not exist in cache, generate one    
    
    matr <- x$get()        ## get the matrix to be inv. from special matrix 'x'
    invMat <- solve(matr)  ## inverse the matrix
    message("setting cache....")  
    x$setInvMat(invMat)    ## save/cache Inverse Matrix 
    invMat                 ## return the inverse matrix of 'x'
}

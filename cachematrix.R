##There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
##Here write a pair of functions that cache the inverse of a matrix.
##The function will create a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(mtx = matrix()) {
   inverse<- NULL
   set<-function(x){
      mtx<<-x
      inverse<<-NULL
      }
      get<-function() return(mtx)
      setinv<-function(inv) inverse<<-inv
      getinv<-function() return(inverse)
      return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## And computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        inverse<- mtx$getinv()
        if(!is.null(inverse)){
        message("Getting cached data...")
        return(inverse)
        }
        data<-mtx$get()
        inverse<-solve(data,...)
        mtx$setinv(inverse)
        return(inverse)
}

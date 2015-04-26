##There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
##Here write a pair of functions that cache the inverse of a matrix.
##The function will create a special "matrix" object that can cache its inverse. 

## mtx is the matrix to be processed
## inverse is the result after calculation

makeCacheMatrix <- function(mtx = matrix()) {
   inverse<- NULL

   ##x is value assigned to mtx

      set<-function(x){
      mtx<<-x
      inverse<<-NULL
      }
   
   ##get the value, return to mtx
      
      get<-function() return(mtx)

   ##assign value of inv to inverse

      setinv<-function(inv) inverse<<-inv
      getinv<-function() return(inverse)

   ##return the list of the previous set functions

      return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## This function omputes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## Then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        inverse<- mtx$getinv()
        
## if inverse is not null, it means inverse has already been calculated previously
## so return the rusult directly

        if(!is.null(inverse)){
        message("Getting cached data...")
        return(inverse)
        }

## if not calculated, get the data from original mtx, calculate inverse.

        data<-mtx$get()
        inverse<-solve(data,...)
        mtx$setinv(inverse)
        return(inverse)
}

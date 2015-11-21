## by creating a sptecial "matrix"object,it can cache its inverse.And I can computes the inverse of the special "matrix" returned by 
## the first function.If the inverse has already been calculated,then my function should retrive the result for me.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
+ set<-function(y){
+ x<<-y
+ m<<-NULL
+ }
+ get<-function()
+ setinverse<-function(inverse)m<<-inverse
+ getinverse<-function()m
+ list(set=set,
+ get=get,
+ setinverse=setinverse,
+ getinverse=getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated,
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
m<-x$getinverse()
+ if(!is.null(m)){
+ message("getting cached data")
+ return(m)
+ }
+ data<-x$get()
+ m<-inverse(data,...)
+ x$setinverse(m)
+ m}## Return a matrix that is the inverse of 'x'

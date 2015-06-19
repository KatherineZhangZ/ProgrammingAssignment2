## Caching the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## makeCacheMatrix creates a list contains four functions: set,get, setinverse, getinverse
## set is the function that sets the value of the matrix
## get is the function that gets the vlaue of the matrix
## setinverse is the function that sets the value of the inverse 
## getinverse is the function that gets the value of the inverse 

makeCacheMatrix<-function(x=matrix()){
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setinverse=function(inverse) i<<-inverse
	getinverse=function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
}


## cacheSolve first verifies value of the inverse i stored in the input list x
## if it exists and not NULL, it returns the value of the inverse along with a message 
## if it doesnot exist, it caculates the value of the inverse and stores it in the list x

cacheSolve<-function (x,...){
	i<-x$getinverse()
	if(!is.null(i)){
		message ("getting cached data")
		return(i)
	}
	data<-x$get()
	i<-solve(data, ...)
	x$setinverse(i)
	i
}

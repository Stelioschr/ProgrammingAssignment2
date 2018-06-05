## Create a matrix, compute the inverse and cache it. If inverse already calculated, retrieve from cache (i.e skip computation); otherwise, calculate inverse and cache it.  



## makeCacheMatrix: Create a matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        a<-NULL
        set<-function(y){
                x<<-y
                a<<-NULL
                
        }
        get<-function()x
        setinverse<-function(inverse) a <<- inverse
        getinverse<-function() a
        list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)

}





## cacheSolve: Compute the inverse of matrix returned from 'makeCacheMatrix'; If inverse already calculated, retrieve from cache (i.e skip computation)  


cacheSolve <- function(x, ...) {
        a<-x$getinverse()
        if(!is.null(a)){
                message("getting cashed data")
                return(a)
        }
        data<-x$get()
        a<-solve(data,...)
        x$setinverse(a)
        a
}

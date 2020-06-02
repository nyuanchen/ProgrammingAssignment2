## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(m1){
                x<<-m1
                inv<<-NULL
        }
        get <- function(){
                x
        }
        setInv <- function(inverse){
                inv <<- inverse
        }
        getInv <- function(){
                inv
        }
        list(set=set,get=get,setInv=setInv,getInv=getInv)
}

cacheSolve<- function(x,...){
        inv<-x$getInv()
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInv(inv)
        inv
}
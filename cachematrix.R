
##makecachematrix creates the model that puts the value of the inverted matrix to the cache

makeCacheMatrix <- function(x = matrix()) {
  #sets a variable named m to null
  m <- NULL
  set <- function(y) {
    #assigns the value y to an object x
    x <<- y
    #assigns the value NULL to an object m
    m <<- NULL
  }
  #the function get will return the value of x
  get <- function() x
  
  #The function setinv  assigns the value of inv to the object m
  setInv <- function(inv) m <<- inv
  
  #the function getinv returns the value of m if it exists
  getInv<- function() m
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function creates the inverted matrix
cacheSolve <- function(x) {
  m <- x$getInv()
  
  #searching for the inverted matrix value in the cache and returns the value is it exists
 if(!is.null(m)) {
    Message("Getting the inverted matrix from cache")
    return(m)
  }
 ## If the m value is null the following occurs
 #stores the value of the matrix into a variable named data
  data <- x$get()
 #stores and returns the value of the inverted value into m 
  m <- solve(data)
  x$setInv(m)
  return(m)
}
matr<-2*diag(3)
m<-makeCacheMatrix(matr)
res<-cacheSolve(m)
print(res)


##makeCacheMatrix will take a numeric matrix of any dimensions and return a list of function.

makeCacheMatrix <- function(x = matrix()) { ##the function will take an arguement "x" which must be a matrix
  m<-NULL                                   ## within the environment of makeCacheMatrix, m will be created as a null variable.
  set<-function(y){                         ## creates a function set which stores the arguement y in the parent environemnt as
    x<<-y                                   ## the variable x. It also creates a null m in the parent environment
    m<<-NULL
  }
  get<-function() x                         ##The second part of the list will store the original matrix as x$get
  setmatrix<-function(solve) m<<- solve     ## setmatrix is a function that will store the inverse of x as m in the global environemnt.
  getmatrix<-function() m                   ## getmatrix will return m from the global environment
  list(set=set, get=get,                    ## the function returns a list of function. this is usually stored in a new variable with
       setmatrix=setmatrix,                 
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {  ## cacheSolve can be called on a list that is created by makeCacheMatrix()
  m<-x$getmatrix()                         ## if there is already a matrix stored in x$getmatrix as a result of a previous call to setmatrix, it will be stored locally in m
  if(!is.null(m)){                         ## if m is not null (i.e. the matrix already has an inverse in the cache) this if statement will activate.
    message("getting cached data")         ## it will then return a message that it is using cached data and return the cached inverse.
    return(m)
  }
  matrix <- x$get()                        ##if there is no cached inverse, x$get() (the original matrix) will be stored as matrix
  m<-solve(matrix, ...)                    ## the inverse of matrix is them stored as m
  x$setmatrix(m)                           ## x$setmatrix will now hod the newly inverted matrix
  m
} # ProgrammingAssignment

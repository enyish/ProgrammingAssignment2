################################################################################
## There are two functions defined here to calculate
##and return the inverse of an inversible matrix given by the first function. 
## For a given matrix, when the makeCacheMatrix() is assigined to a variable
## and the CacheSolve() function is called on this variable, it checkes the  
##inverse of that matrix within the functions inside the makeCacheMatrix().
##if found will return the value. if vale is NULL will calculate the inverse using the 
## setinverse() function and reassign the value of the original matrix.
## anytime the CacheSolve() function is run after the first run, the value will be accessed from the saved value  
## from makeCacheMatrix() environment along with a message that the value was 
## previously calculated. Every time an inverse of a new matrix needs to be 
## calculated, the entire process is repeated.  

makeCacheMatrix <- function(x=matrix()) {       
        M <- NULL 
        set <-  function(y){
                #given the value y when the function is called,
                #it substitutes the value of x in the main function by the value of y                                                  
                x<<-y
                M <<- NULL
        }
        
        getinverse <- function() M  # getinverse returns the inverse of the matrix.                         
        get <- function() x # get is a function that returns the value of x stored in the main function, 
        #it doesnt require any input.                         
        setinverse <- function(inverse){                         
                M <<- inverse # reassigning M in the main function environment(makeCacheMatrix)            
        }
        list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}


## the CacheSolve function below accesses 
##the functions defined in makeCacheMatrix()
##and tried to get the inverse of M.
##If M is already calculated it returns that value,
##if not it calculates M,saves it using setinverse() & return M. 

CacheSolve <- function(x,...){                
        M <-x$getinverse()                        
        if(!is.null(M)){                        
                message("Getting cached data")                               
                return(M)                        
        }                        
        data <- x$get()                     
        M <- solve(data) # calculating the inverse of the matrix                          
        x$setinverse(M)  #saving the inverse M in makeCacheMatrix environment
        M
}  



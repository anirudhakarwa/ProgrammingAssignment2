# The first function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to
#
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
	#set variable i (inverse of matrix) to NULL
	i <- NULL

	#sets x to the argument y and set i to null
	set <- function(y)
	{
		x <<- y
		i <<- NULL
	}

	#returns the value of x
	get <- function()
	{
		x
	}

	#sets i to inverse 
	setInverse <- function(inv)
	{
		i <<- inv
	}

	#returns the value of i (from makeVector)
	getInverse <- function()
	{
		i
	}

	#returns a labeled vector of functions set, get, setInverse and getInverse
	list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) 
{
    #attempts to get the inverse from x (if it was calculated previously)
	
	i <- x$getInverse()
	
	#if not null, a valued was cached, so return i 
	if(!is.null(i))
	{
		print("cached data...")
		return (i)
	}
	
	#since its null, we need to calculate the new Inverse
	data <- x$get()
	
	#Calculate the inverse
	i <- solve(data)
	
	#set i in x to calculated inverse
	x$setInverse(i)
	
	#return i
	print("new calculated data")
	i		
		
}

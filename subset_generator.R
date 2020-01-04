#U is universal set (usually 1:n)
#n - number of desired subsets
#minSize = minimum subset size
#maxSize = maximum subset size (last set can be an exception)
generateSubsets <- function(U, n, minSize = 1, maxSize = sqrt(length(U))){
  Subsets <- list()
  leftovers <- U
  for(i in 1:(n-1)){
    items <- sample(1:length(U), sample(minSize:maxSize,1))
    Subsets[[i]] <- U[items]
    indexes <- which(leftovers %in% U[items])
    if(length(indexes) != 0){
      leftovers <- leftovers[-which(leftovers %in% U[items])]
    }
  }
  if(length(leftovers) == 0){
    items <- sample(1:length(U), sample(minSize:maxSize,1))
    Subsets[[n]] <- U[items]
  }else{
    #Add all remaning elements if needed
    Subsets[[n]] <- leftovers 
    if(length(Subsets[[n]]) < maxSize) {
      candidates <- U[-which(Subsets[[n]] %in% U)]
      items <- sample(1:length(candidates), sample(1:(maxSize-length(Subsets[[n]])), 1))
      Subsets[[n]] <- c(Subsets[[n]], candidates[items])
    }
  }
  Subsets
}

#Test
#a <- generateSubsets(1:10, 3)
#a
#typeof(a[1])
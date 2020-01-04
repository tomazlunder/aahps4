#Exercise 1: Greedy solution (5 points)
# Write a greedy algorithm SCPgreedy(U, S) that iteratively picks the most cost effective set Si.
# Cost effectiveness αi is calculated as αi = ci / |Si−C|,
# where C is a set of already covered elements of U.

scpGreedy <- function(U,S){
  orgS <- S
  pickedSubsets <- vector()
  finalCost <- 0
  
  #Set of elements that are already covered (of U)
  C <- vector()
  
  while(1){
    cost <- vector()
    
    #Calculate the cost effectiveness of all remaining subsets
    for(Si in S){
      Siv <- as.vector(Si)
      
      ci = length(Siv)
      diff = length(setdiff(Siv,C))
      if(diff == 0){
        cost <- c(cost, 0)
      }
      else{
        cost <- c(cost, ci/diff)
      }
    }
    
    #Pick the subset with the biggest ca, if there are more than one, pick first
    max_index <- which(cost==max(cost))
    maxSet <- S[max_index[1]]
    S[max_index[1]] <- NULL
    pickedSubsets <- c(pickedSubsets, (match(maxSet,orgS)))
    finalCost <- finalCost + length(unlist(maxSet[1]))
    
    #Update the covered set
    C <- union(C,as.vector(maxSet[1]))
    C <- unlist(C)
    
    #If the whole universe is covered, break
    if(setequal(C,U)){break}
  }
  
  cat("ScpGreedy finished!\n")
  cat("Solution contains ",length(pickedSubsets),"subsets and has a total cost of ",finalCost,"\n")
  cat(pickedSubsets)
}

#Test
a <- generateSubsets(1:5, 20)
a

scpGreedy((1:5), a)
#-------------------------------------------------
#--------------- use of list in R ----------------
#-------------------------------------------------
rm(list = ls())

w <- list()                        
w[[1]] <- c(1:12)                   # insert vectors or matrices into a list (cell)
w[[2]] <- matrix(0,2,3)
w

a <- runif(5)
b <- c(1, 2, 4, 7, 9, 11)
c <- matrix(1,2,3)
z <- matrix(rnorm(12), 3, 4)

x <- list(a,b,c,z)
x

x[2]
x[[2]]
x[[2]][3]
x[[4]][9]

##-----------------------------------------------

J <- 30                          # number of alternatives
T <- 100                        # number of times to choose a choice
N <- J*T                        # number observations for each user
I <- 50

beta <- c(1,1,0.1,0.05)

friend_threshold <- 0.2         # likelihood of connected friends

# variable initialization
set.seed(401716)
feature <- rnorm(T*J*I)         # create variable s ?
dim(feature) <- c(T, J, I)      # set dimensions of T*J matrices in I cells

total_choices <- list()
total_friends_choices <- list()
for (i in 1:I) {
  total_choices[[i]] <- matrix(0,1,J)
  total_friends_choices[[i]] <- matrix(0,1,J)
}

# create friendship matrix
temfriend <- matrix(runif(I^2), I)
friends <- matrix(0, I, I)

cutoff <- which(temfriend<friend_threshold)             # 0.2 cut off point
friends[cutoff] <- 1                               
friends[upper.tri(friends,diag=TRUE)] <- 0
friends <- friends + t(friends)

for (t in 1:T) {
  for (i in 1:I) {
    utility <- exp(beta[1]+beta[2]*feature[t, ,i]+beta[3]*total_choices[[i]]     # 1*J
                   +beta[4]*total_friends_choices[[i]])
    utility_allsum <- apply(utility,1,sum)         # utility of all alternatives
    utility_allsum <- kronecker(matrix(1,1,J),utility_allsum)            # 1*J
    prob <- utility/utility_allsum                                       # 1*J
    prob <- cumsum(prob)
    draw_for_choice <- runif(1)
    draw_for_choice <- kronecker(matrix(1,1,J),draw_for_choice)          # 1*J
    choice_short <- sum(prob<draw_for_choice)+1                          # 1*1
    choice <- kronecker(matrix(1,1,J),choice_short)
    test <- c(1:J)
    choice <- (test==choice)                                             # 1*J
    total_choices[[i]] <- total_choices[[i]] + choice
  }
  
  #    # update friends total choices
  #    choicemat <- matrix(unlist(total_choices), ncol = J, byrow = TRUE)  # I*J
  #    friendi <- which(friends[,i]==1)
  #    if (length(friendi)>=1){
  #      for (j in 1:length(friendi)) {
  #        friprep <- kronecker(matrix(1,1,J),friends[,friendi[j]])        # I*J
  #        choicefriends <- choicemat*friprep
  #        total_friends_choices[[friendi[j]]] <- apply(choicefriends,2,sum)
  #      }
  #    }    
  
  # update friends total choices (friends influence up to t-1)
  choicemat <- matrix(unlist(total_choices), ncol = J, byrow = TRUE)
  for (i in 1:I) {
    friendindex <- kronecker(matrix(1,1,J),friends[,i])
    choicefriend <- choicemat*friendindex
    total_friends_choices[[i]] <- apply(choicefriend,2,sum)
  }
}
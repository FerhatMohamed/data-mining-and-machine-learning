                             ################################
                             # ferhat mohamed 2 Master SIEC #
                             ################################

                                      ##########
                                      # TP SVM #
                                      ##########

####################
# 1 - SVM Linéaire #
####################
    ####################################
    # 1.1 Génération de jeu de données #
    ####################################

         n <- 150                        #number of data points
         p <- 2                          # dimension
         sigma <- 1                      # variance of the distribution
         meanpos <- 0                    # centre of the distribution of positive examples
         meanneg <- 3                    # centre of the distribution of negative examples
         npos <- round(n / 2)            # number of positive examples
         nneg <- n - npos                # number of negative examples

         # Generate the positive and negative examples
         xpos <- matrix(rnorm(npos * p, mean = meanpos, sd = sigma), npos, p)
         xneg <- matrix(rnorm(nneg * p, mean = meanneg, sd = sigma), npos, p)
         x <- rbind(xpos, xneg)

         # Generate the labels
         y <- matrix(c(rep(1, npos), rep(-1, nneg)))

         # Visualize the data
         plot(x, col = ifelse(y > 0, 1, 2))
         legend("topleft", c("Positive", "Negative"), col = seq(2), pch = 1, text.col = seq(2))
 

         # Maintenant, nous divisons les données en un ensemble d'entraînement (80%) et un ensemble de tests (20%)
         # Prepare a training and a test set
         ntrain <- round(n * 0.8)        # number of training examples
         tindex <- sample(n, ntrain)     # indices of training samples
         xtrain <- x[tindex, ]
         xtest <- x[-tindex, ]
         ytrain <- y[tindex]
         ytest <- y[-tindex]
         istrain <- rep(0, n)
         istrain[tindex] <- 1

         # Visualize
         plot(x, col = ifelse(y > 0, 1, 2), pch = ifelse(istrain == 1,1,2))
         legend("topleft", c("Positive Train", "Positive Test", "Negative Train", "Negative Test"), 
	            col = c(1, 1, 2, 2), pch = c(1, 2, 1, 2), text.col=c(1,1,2,2))



    ########################
    # 1.2 Entrainer le SVM #
    ########################

         # load the kernlab package
         # install.packages("kernlab")
         library(kernlab)

         # train the SVM
         svp <- ksvm(xtrain, ytrain, type = "C-svc", kernel = "vanilladot", C=100, scaled=c())

         #Look and understand what svp contains
         # General summary
         svp

         # Attributes that you can access
         attributes(svp)

         # For example, the support vectors
         alpha(svp)
         alphaindex(svp)
         b(svp)

         # Use the built-in function to pretty-plot the classifier
         plot(svp, data = xtrain)



##############
# Question 1 #
#######################################################################################################
plotlinearsvm <- function(svp, xtrain){                                                               #
  plot(xtrain, pch = ifelse(ytrain > 0, 1, 2), xlim = c(-2, 6), ylim = c(-3, 6))                      #
  legend("topleft", c("Positive", "Negative"), pch = seq(2))                                          #
  w = colSums(unlist(alpha(svp)) * ytrain[unlist(alphaindex(svp))] * xtrain[unlist(alphaindex(svp)),])#
  b = - b(svp)                                                                                        #
  abline(a= -b / w[2], b = -w[1]/w[2])                                                                #
  abline(a= (-b+1)/ w[2], b = -w[1]/w[2], lty = 2)                                                    #
  abline(a= (-b-1)/ w[2], b = -w[1]/w[2], lty = 2)                                                    #         
}                                                                                                     #
plotlinearsvm(svp, xtrain)                                                                            #
#######################################################################################################


    ###########################
    # 1.3 Prédire avec un SVM #
    ###########################


         # Predict labels on test
         ypred <- predict(svp, xtest)
         table(ytest, ypred) 

         # Compute accuracy
         sum(ypred == ytest) / length(ytest)

         # Compute at the prediction scores
         ypredscore <- predict(svp, xtest, type = "decision")

         # Check that the predicted labels are the signs of the scores
         table(ypredscore > 0, ypred)

         # Package to compute ROC curve, precision-recall etc...
         # install.packages("ROCR")
         library(ROCR)
         pred <- prediction(ypredscore, ytest)

         # Plot ROC curve
         perf <- performance(pred, measure = "tpr", x.measure = "fpr")
         plot(perf)

         # Plot precision/recall curve
         perf <- performance(pred, measure = "prec", x.measure = "rec")
         plot(perf)

         # Plot accuracy as function of threshold
         perf <- performance(pred, measure = "acc")
         plot(perf)



    ########################
    # 1.4 Cross-validation #
    ########################

         cv.folds <- function(y, folds = 3){
         ## randomly split the n samples into folds
         split(sample(length(y)), rep(1:folds, length = length(y)))
         }


##############
# Question 2 #
####################################################################################
cv.folds(ytrain, folds = 3)                                                        #
                                                                                   #
cv.ksvm <- function(x, y, folds = 3,...){                                          #
  index = cv.folds(y, folds = folds)                                               #
  predScore = rep(NA, length(y))                                                   #
  for (i in 1:folds){                                                              #
    toTrain = unname(unlist(index[-i]))                                            #
    testSet = index[[i]]                                                           #
    svp = ksvm(x[toTrain, ], y[toTrain], type = "C-svc",                           #
         kernel = "vanilladot", C=100, scaled=c())                                 #
    predScore[testSet] = predict(svp, x[unlist(index[[i]]), ], type = "decision")  #
  }                                                                                #
 predScore                                                                         #
}                                                                                  #
####################################################################################

         svp <- ksvm(x, y, type = "C-svc", kernel = "vanilladot", C = 100, scaled=c(), cross = 5)
         print(cross(svp))
         print(error(svp))

##############
# Question 3 #
#############################################################
ypredscore = cv.ksvm(x, y, folds=5)                         #
pred = prediction(ypredscore, y)                            #
perf = performance(pred, measure = "tpr", x.measure = "fpr")#
plot(perf)                                                  #
                                                            #
perf = performance(pred, measure = "acc")                   #
plot(perf)                                                  #
#############################################################


##############
# Question 4 #
#######################################################################################################
                                                                                                      #
#######################################################################################################



        #################
        # 1.5 Effet d C #
        #################


##############
# Question 5 #
#####################################################################################
cost = 2^(seq(-10, 14, by=3))                                                       #
par(mfrow = c(3,3))                                                                 #
for (c in cost){                                                                    # 
  svp = ksvm(xtrain, ytrain, type = "C-svc", kernel = "vanilladot", C=c, scaled=c())#
  plotlinearsvm(svp, xtrain)                                                        #
}                                                                                   #
par(mfrow=c(1,1))                                                                   #                             #
#####################################################################################


##############
# Question 6 #
#########################################################################################
cost = 2^(seq(-10, 15))                                                                 #
crossError = rep(NA, length(cost))                                                      #
error = sapply(cost, function(c){                                                       #
  cross(ksvm(x, y, type = "C-svc", kernel = "vanilladot", C = c, scaled=c(), cross = 5))#
})                                                                                      #
plot(cost, error, type='b')                                                             #
plot(log2(cost), error, type='b')                                                       #
#########################################################################################


##############
# Question 7 #
#######################################################################################
n <- 150                      # number of data points                                 #
p <- 2                        # dimension                                             #
sigma <- 1                    # variance of the distribution                          #
meanpos <- 0                  # centre of the distribution of positive examples       #
meanneg <- 1                  # centre of the distribution of negative examples       #
npos <- round(n / 2)          # number of positive examples                           #
nneg <- n - npos              # number of negative examples                           #
                                                                                      #
# Generate the positive and negative examples                                         #
xpos <- matrix(rnorm(npos * p, mean = meanpos, sd = sigma), npos, p)                  #
xneg <- matrix(rnorm(nneg * p, mean = meanneg, sd = sigma), npos, p)                  #
x <- rbind(xpos, xneg)                                                                #
                                                                                      #
# Generate the labels                                                                 #
y <- matrix(c(rep(1, npos), rep(-1, nneg)))                                           #
                                                                                      #
# Visualize the data                                                                  #
plot(x, col = ifelse(y > 0, 1, 2))                                                    #
legend("topleft", c("Positive", "Negative"), col = seq(2), pch = 1, text.col = seq(2))#
                                                                                      #
# generate training/testing                                                           #
ntrain <- round(n * 0.8)                   # number of training examples              #
tindex <- sample(n, ntrain)                # indices of training samples              #
xtrain <- x[tindex, ]                                                                 #
xtest <- x[-tindex, ]                                                                 #
ytrain <- y[tindex]                                                                   #
ytest <- y[-tindex]                                                                   #
istrain <- rep(0, n)                                                                  #
istrain[tindex] <- 1                                                                  #
                                                                                      #
                                                                                      #
# cost cross validation                                                               #
cost = 2^(seq(-10, 15))                                                               #
error = sapply(cost, function(c){                                                     #
  cross(ksvm(x, y, type = "C-svc", kernel = "vanilladot", C = c, scaled=c(),          #
             cross = 5))                                                              #
})                                                                                    #
plot(cost, error, type='b')                                                           #
plot(log2(cost), error, type='b')                                                     #
#######################################################################################




########################
# 2 - SVM Non Linéaire #
########################

         # Train a nonlinear SVM
         svp <- ksvm(x, y, type = "C-svc", kernel="rbf", kpar = list(sigma = 1), C = 1)

         # Visualize it
         plot(svp, data = x)


##############
# Question 8 #
###################################################################################################
RandomMatrix <- function( dist, n, p, ... ) {                                                     #
  rs <- dist( n*p, ... )                                                                          #
  matrix( rs, n, p )                                                                              # 
}                                                                                                 #
                                                                                                  #
GenerateDatasetNonlinear <- function( n, p ) {                                                    #
  bottom.left <- RandomMatrix( rnorm, n, p, mean=0, sd=1 )                                        #
  upper.right <- RandomMatrix( rnorm, n, p, mean=4, sd=1 )                                        #
  tmp1 <- RandomMatrix( rnorm, n, p, mean=0, sd=1 )                                               #
  tmp2 <- RandomMatrix( rnorm, n, p, mean=4, sd=1 )                                               #
  upper.left <- cbind( tmp1[,1], tmp2[,2] )                                                       # 
  bottom.right <- cbind( tmp2[,1], tmp1[,2] )                                                     #   
  y <- c( rep( 1, 2 * n ), rep( -1, 2 * n ) )                                                     #
  idx.train <- sample( 4 * n, floor( 3.5 * n ) )                                                  #
  is.train <- rep( 0, 4 * n )                                                                     #
  is.train[idx.train] <- 1                                                                        #
  data.frame( x=rbind( bottom.left, upper.right, upper.left, bottom.right ), y=y, train=is.train )#
}                                                                                                 #
                                                                                                  #
data = GenerateDatasetNonlinear(150, 2)                                                           #
plot(data[,1:2], col = data[,3] + 2)                                                              #
x = as.matrix(data[,1:2])                                                                         #
y = matrix(data[,3])                                                                              #  
                                                                                                  # 
                                                                                                  #
###################################################################################################


##############
# Question 9 #
##################################################################################
library(ggplot2)                                                                 #
cost = 2^(seq(-10, 15, by=2))                                                    #
sigma = 0:5                                                                      #
error = sapply(cost, function(c){                                                #
  sapply(sigma, function(s){                                                     #
    cross(ksvm(x, y, type = "C-svc", kernel="rbf", kpar = list(sigma = s), C = c,#
               scaled=c(), cross = 5))                                           #
  })                                                                             #
})                                                                               #
toPlotError = data.frame(sigma = rep(sigma, length(cost)),                       #
                         cost = rep(cost, each = length(sigma)),                 #
                         error = as.vector(error))                               #
                                                                                 #
ggplot(data = toPlotError, aes(x=cost, y=error)) + geom_point() + geom_line() +  #
  facet_grid(.~sigma)                                                            #
                                                                                 #
##################################################################################


         # Train a nonlinear SVM with automatic selection of sigma by heuristic
         svp <- ksvm(x, y, type = "C-svc", kernel = "rbf", C = 1)

         # Visualize it
         plot(svp, data = x)

###############
# Question 10 #
#############################################################################
library(ggplot2)                                                            #
cost = 2^(seq(-10, 15, by=2))                                               #
error = sapply(1:length(cost), function(i){                                 #
    svp = ksvm(x, y, type = "C-svc", kernel = "rbf", C = cost[i], cross = 5)#
    cross(svp)                                                              #
})                                                                          #  
plot(cost, error, type="o")                                                 #
#############################################################################


###############
# Question 11 #
###########################################################################
myKernels = c("polydot", "tanhdot", "laplacedot", "besseldot", "anovadot")#
for (kernel in myKernels){                                                #
  plot(ksvm(x, y, type = "C-svc", kernel = kernel, C = 1), data=x)        #
}                                                                         #
###########################################################################
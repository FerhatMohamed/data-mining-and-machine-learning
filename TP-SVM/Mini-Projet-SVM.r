                             ################################
                             # ferhat mohamed 2 Master SIEC #
                             ################################

                                    ###################
                                    # Mini-Projet SVM #
                                    ###################


######################################################################################
# 3- Mini-Projet : le diagnostic du cancer à partir des données d'expression génique #
######################################################################################

         # Load the ALL dataset
         library(ALL)
         data(ALL)

         # Inspect them
         ?ALL
         show(ALL)
         print(summary(pData(ALL)))

         x <- t(exprs(ALL))
         y <- substr(ALL$BT,1,1)


###############
# Question 12 #
##################################################################################
# very quick response. Your turn to play with the parameters (kernels, cost, ...)#
                                                                                 #
x <- t(exprs(ALL))                                                               #
y <- substr(ALL$BT,1,1)                                                          #
                                                                                 #
# train and test sets                                                            #
n = length(y)                                                                    #
ntrain <- round(n * 0.8)          # number of training examples                  #                    
tindex <- sample(n, ntrain)       # indices of training samples                  #
xtrain <- x[tindex, ]                                                            # 
xtest <- x[-tindex, ]                                                            #  
ytrain <- y[tindex]                                                              #
ytest <- y[-tindex]                                                              #
                                                                                 #
# train svm on train set                                                         #
svp = ksvm(xtrain, ytrain, type = "C-svc", kernel = "rbf", C = 10)               #
                                                                                 #
# predict on test set                                                            #
pred = predict(svp, xtest)                                                       #
acc = sum(as.vector(pred) == ytest) / length(ytest)                              #
##################################################################################


         y <- ALL$BT
         print(y)

###############
# Question 13 #
#####################################################################
# now y is multiclass                                               #                    
x <- t(exprs(ALL))                                                  #
y <- ALL$BT                                                         #
                                                                    #
# train and test sets (same as previous question)                   #
n = length(y)                                                       #
ntrain <- round(n * 0.8)      # number of training examples         #
tindex <- sample(n, ntrain)   # indices of training samples         #
xtrain <- x[tindex, ]                                               #
xtest <- x[-tindex, ]                                               #
ytrain <- y[tindex]                                                 #
ytest <- y[-tindex]                                                 #
                                                                    #
# train svm on train set                                            #
# type of svm is now able to handle multiclass                      #
svp = ksvm(xtrain, ytrain, type = "kbb-svc", kernel = "rbf", C = 10)#
                                                                    #
# predict on test set                                               #
pred = predict(svp, xtest)                                          #
acc = sum(as.vector(pred) == ytest) / length(ytest)                 #
#####################################################################
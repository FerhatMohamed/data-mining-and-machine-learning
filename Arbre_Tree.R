data(iris)
head(iris)

library(tree)
arbre1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(arbre1)

plot(arbre1)
text(arbre1)


plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(arbre1,label="Species",add=TRUE)
legend(cex = 1,1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)




set.seed(98286)
xvar <- sample(1:10, 100, replace=TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, 
                                           replace=TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, 
                                           replace=TRUE)
my.vars <- cbind(xvar, yvar, zvar)

summary(my.vars)

plot(yvar ~ xvar, data=jitter(my.vars))
plot(zvar ~ xvar, data=jitter(my.vars))
plot(yvar ~ zvar, data=jitter(my.vars))
cor(my.vars)

my.pca <- prcomp(my.vars)
summary(my.pca)

my.pca

brand.ratings <- read.csv("http://goo.gl/IQl8nc")
# check it out
head(brand.ratings)
tail(brand.ratings)
summary(brand.ratings)
str(brand.ratings)

brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
summary(brand.sc)

library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order="hclust")

brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)

plot(brand.pc, type="l")

biplot(brand.pc)

brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
brand.mean
rownames(brand.mean) <- brand.mean[, 1] # use brand for the row names
brand.mean <- brand.mean[, -1]          # remove brand name column
brand.mean

brand.mu.pc <- prcomp(brand.mean, scale=TRUE) 
summary(brand.mu.pc)
biplot(brand.mu.pc, main="Brand positioning", cex=c(1, 1))


library(ISLR2)

head(Boston)
class(Boston)
str(Boston)
dim(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit

confint(lm.fit)

plot(medv ~ lstat, data = Boston, pch = 16)
abline(lm.fit, lwd = 2, col = "red")

summary(lm.fit)

plot(residuals(lm.fit) ~fitted(lm.fit))
abline(h = 0, lty = 2, col = "blue")

hatvalues(lm.fit)
plot(hatvalues(lm.fit))
max(hatvalues(lm.fit))

which.max(hatvalues(lm.fit))

predict(lm.fit, newdata = data.frame(lstat=c(5, 10, 15)),
interval = "prediction")

### Outro exemplo
rm(list = ls())


Carseats
head(Carseats)
dim(Carseats)
str(Carseats)

lm.fit <- lm(Sales ~ Price, data = Carseats)
summary(lm.fit)

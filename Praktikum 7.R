##### Praktikum 7 Analisis Regresi Terapan ######
# Kamis, 8 April 2022, 10:30-12:10 WIB

##### Pendekatan Matriks #####

Y <- c(23, 7, 15, 17, 23, 22, 10, 14, 20, 19)
X1 <- c(10, 2, 4, 6, 8, 7, 4, 6, 7, 6)
X2 <- c(7, 3, 2, 4, 6, 5, 3, 3, 4, 3)


model <- lm(Y ~ X1 + X2)
model$coefficients
names(summary(model))
summary(model)$r.squared

cor(cbind(Y,X1,X2))

Y_ <- c(3, 4, 5, 6, 6, 7, 8, 9, 10, 10)
X_1 <- c(8, 7, 7, 7, 6, 6, 6, 6, 5, 5)
X_2 <- c(10, 10, 8, 5, 4, 3, 2, 2, 1, 1)

model2 <- lm(Y_ ~ X_1 + X_2)
model2
summary(model2)
cor(cbind(Y_,X_1,X_2))
sqrt(sum((model2$residuals)^2)/9)
sd(model2$residuals)

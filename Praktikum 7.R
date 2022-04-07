##### Praktikum 7 Analisis Regresi Terapan ######
# Kamis, 8 April 2022, 10:30-12:10 WIB


# Regresi Linear Berganda ------------------------

Y <- c(23, 7, 15, 17, 23, 22, 10, 14, 20, 19)
X1 <- c(10, 2, 4, 6, 8, 7, 4, 6, 7, 6)
X2 <- c(7, 3, 2, 4, 6, 5, 3, 3, 4, 3)

# Menghitung koefisien regresi dengan fungsi `lm()`
model <- lm(Y ~ X1 + X2)
model$coefficients

# Manual
X <- matrix(c(rep(1,10),X1,X2), ncol=3)
solve(t(X)%*%X) %*% (t(X)%*%Y)

names(summary(model)) # Melihat opsi pada summary

# Koefisien determinasi
summary(model)$r.squared
summary(model)$adj.r.squared

# Manual
R2 <- 1 - sum((fitted(model)-Y)^2)/sum((Y-mean(Y))^2)
R2

# Matriks varian-kovarian Penduga
summary(model)$cov

# Matriks Varian-Kovarian Data
cov(cbind(Y,X1,X2))

# Korelasi
r <- cor(cbind(Y,X1,X2))
r

# Manual utk r(Y,X1)
(10*sum(Y*X1)-sum(Y)*sum(X1)) /
  sqrt((10*sum(Y^2)-sum(Y)^2)*(10*sum(X1^2)-sum(X1)^2))

# Korelasi Parsial
(r['Y','X1'] - r['Y','X2']*r['X1','X2']) / 
  sqrt((1 - r['Y','X2']^2)*(1 - r['X1','X2']^2))

(r['Y','X2'] - r['Y','X1']*r['X1','X2']) / 
  sqrt((1 - r['Y','X1']^2)*(1 - r['X1','X2']^2))

(r['X1','X2'] - r['Y','X1']*r['Y','X2']) / 
  sqrt((1 - r['Y','X1']^2)*(1 - r['Y','X2']^2))

# Korelasi Parsial Lebih Mudah
library(ppcor)
pcor(cbind(Y,X1,X2))$estimate

# Standar error dugaan
sqrt(sum((model$residuals)^2)/7)
summary(model)$sigma


# Contoh 2 ----------------------------------------

Y_ <- c(3, 4, 5, 6, 6, 7, 8, 9, 10, 10)
X_1 <- c(8, 7, 7, 7, 6, 6, 6, 6, 5, 5)
X_2 <- c(10, 10, 8, 5, 4, 3, 2, 2, 1, 1)

# Menghitung koefisien regresi dengan fungsi `lm()`
model2 <- lm(Y_ ~ X_1 + X_2)
model2

# Koefisien determinasi
summary(model2)$r.squared
summary(model2)$adj.r.squared

# Matriks varian-kovarian Penduga
summary(model2)$cov

# Matriks Varian-Kovarian Data
cov(cbind(Y_,X_1,X_2))

# Korelasi
cor(cbind(Y_,X_1,X_2))

# Korelasi Parsial
pcor(cbind(Y_,X_1,X_2))$estimate

# Standar error dugaan
sqrt(sum((model2$residuals)^2)/7)
summary(model2)$sigma

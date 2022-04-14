##### Praktikum 8 Analisis Regresi Terapan ######
# Kamis, 14 April 2022, 10:10-11:20 WIB


# ----- Asumsi pada Regresi Linear Berganda ------------------------

"Y <- c(23, 7, 15, 17, 23, 22, 10, 14, 20, 19)
X1 <- c(10, 2, 4, 6, 8, 7, 4, 6, 7, 6)
X2 <- c(7, 3, 2, 4, 6, 5, 3, 3, 4, 3)"

set.seed(42)
X1 <- round(runif(100, 2, 10), 2)
X2 <- round(runif(100, 2, 7), 2)
Y <- 4 + 2.5*X1 - 0.5*X2 + rnorm(100)

# 3D Scatterplot
library(scatterplot3d)
scatterplot3d(Y, X1, X2, pch=16, color='blue', box = T,
              main='Plot Contoh Regresi 3D', angle=30,
              xlim=c(0,25), ylim=c(0,15), zlim=c(0,10))

# Matriks scatterplot
pairs(cbind(Y, X1, X2), pch=19, col='blue', asp=1, lower.panel=NULL,
      main='Judul Scatterplots')

# Menghitung koefisien regresi dengan fungsi `lm()`
model <- lm(Y ~ X1 + X2)
model$coefficients

# ----- Linearitas -----
# Menggunakan plot residual-prediksi terstandarisasi
plot(scale(model$fitted.values), scale(model$residuals), pch=16, 
     col='steelblue',main='Uji Linearitas dengan Plot', 
     xlab='Nilai Dugaan Terstandarisasi', 
     ylab='Residual Terstandarisasi')

plot(model,1) # Plot dari model

# ----- Multikolinearitas -----
# Menggunakan nilai VIF
TOL_X1_X2 <- (1-cor(X1,X2)^2)
TOL_X1_X2
VIF_X1_X2 <- 1/TOL_X1_X2
VIF_X1_X2

# Dengan package `car`
library(car)
vif(model)

# ---- Heteroskedastisitas -----
# Dengan plot
plot(model, 1)

# Uji Hipotesis (H0: Tidak ada masalah heteroskedastisitas)
library(skedastic)
glejser(model)
# Uji White 
white_lm(model)
# Uji Breush Pagan
library(lmtest)
bptest(model)


# ----- Normalitas -----
# Plot quatile-quantile
qqnorm(model$residuals)
qqline(model$residuals)

# Plot Density
plot(density(model$residuals), 
     main='Probability Density Funtion')

# Dengan Uji Hipotesis (H0: Residual Normal)
library(nortest)
# Shapiro-Wilk
shapiro.test(model$residuals)
# Lilliefors (Kolmogorov-Smirnov)
lillie.test(model$residuals)
# Cramer-von Mises
cvm.test(model$residuals)
# Anderson-Darling
ad.test(model$residuals)

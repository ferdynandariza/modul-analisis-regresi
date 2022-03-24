##### Praktikum 6 Analisis Regresi Terapan ######
# Kamis, 24 Maret 2022, 10:30-12:10 WIB

##### Pendekatan Matriks #####

# Membuat objek matriks dengan fungsi matrix()
# Dengan argumen terdiri dari barisan c(), jumlah kolom (ncol)
# (atau baris), dan cara penyusunannya (byrow)

# Data sebelumnya ke dalam  c()
Konsumsi_Pakan <- c(87.1, 93.1, 89.8, 91.4, 99.5, 92.1, 95.5, 99.3, 93.4, 94.4)
Bobot_Badan <- c(4.6, 5.1, 4.8, 4.4, 5.9, 4.7, 5.1, 5.2, 4.9, 5.1)

# Membuat objek matriks
satu <- rep(1, 10)
X <- matrix(c(satu, Bobot_Badan), ncol=2, byrow=FALSE)
X
Y <- matrix(Konsumsi_Pakan, ncol=1)

# Transpose matriks
t(X)
t(Y)

# Perkalian matriks menggunakan '%*%'
t(Y) %*% Y # Jumlah kuadrat 
Y %*% t(Y) # Perhatikan, urutan berarti

# Model regresi pada lambang matriks Y = X*beta
# Cek terlebih dahulu apakah X'X non-singular
det(t(X) %*% X) 

# beta = (X'X)^-1 X' Y (Persamaan Normal)
B <- solve(t(X) %*% X) %*% t(X) %*% Y
B
# m<-lm(Y~X[,2]);m$coefficients

# Analisis Ragam
Y_hat <- X %*% B
y_bar <- mean(Y)
Y_bar <- matrix(rep(y_bar, 10), ncol=1)
galat <- Y - Y_hat

# Jumlah kuadrat
JKT <- t(Y - Y_bar) %*% (Y - Y_bar)
JKR <- t(Y_hat - Y_bar) %*% (Y_hat - Y_bar)
JKG <- t(Y - Y_hat) %*% (Y - Y_hat)
JKT - (JKR+JKG)

c('(JKT)'=JKT, '=  (JKR) '=JKR, '+  (JKG) '=JKG)
# anova(m)
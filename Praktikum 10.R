##### Praktikum 10 Analisis Regresi Terapan ######
# Kamis, 28 April 2022, 10:10-11:20 WIB


# Regresi Dengan Peubah Boneka (Dummy Variable) -------------------------

# Data Mesin Bubut
Y <- c(18.73, 14.52, 17.43, 14.54, 13.44, 24.39, 13.34, 22.71, 12.68, 
       19.32, 30.16, 27.09, 25.4, 26.05, 33.49, 35.62, 26.07, 36.78, 
       34.95, 43.67) # Kecepatan Putaran
X1 <- c(610, 950, 720, 840, 980, 530, 680, 540, 890, 730, 670, 770, 880, 
        1000, 760, 590, 910, 650, 810, 500) # Umur Mesin
X2 <- c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 
        'B', 'B', 'B', 'B', 'B', 'B', 'B') # Jenis Alat

# Membuat Variabel Dummy
X_dummy <- ifelse(X2 == 'A', 0, 1)

# Menduga model regresi
mod <- lm(Y~X1+X_dummy)
summary(mod)

# pers utk A: Y = 36.98 - 0.02X1
# pers utk B: Y = 36.98 - 0.02X1 + 15.00 = 51.98 - 0.02X1

# Plot Data
plot(Y~X1, col=ifelse(X2=='A', 'blue', 'red'), pch=16, 
     main='Regresi dengan Dummy Variable', 
     xlab='Umur Mesin Bubut', ylab='Kecepatan Putaran')

# Garis Regresi, karena ada 2 kategori maka garisnya juga ada 2
# abline(intersep, slope)
abline(mod$coef[1], mod$coef[2]) # Untuk Alat A
abline(mod$coef[1]+mod$coef[3], mod$coef[2]) # Untuk Alat B

# Tambah legenda biar tidak bingung
legend('topright', legend=c('A', 'B'), pch=16, col=c('blue', 'red'))
#####


# Lebih dari 2 Kategori? -------------------------------------

# Generate Data (Skip) #####
D1 <- rep(c(1,0,0), each=30)
D2 <- rep(c(0,1,0), each=30)
cat <- c()
for (i in 1:90) {
  if (D1[i]==1) {cat = c(cat, 'A')}
  else if (D2[i]==1) {cat = c(cat, 'B')}
  else {cat = c(cat, 'C')}
}
X_ <- seq(2, 10, length=90)
Y_ <- 10 + 2*X_ + 30*D1 + 15*D2 + rnorm(90)

# Contoh dengan 3 kategori ---- 
df <- data.frame('Y'=Y_, 'X'=X_, 'cat'=cat)
df

# Buat dummy variable, karena 3 kategori, maka dummy variablenya ada 2
katB <- ifelse(df$cat=='B', 1, 0)
katC <- ifelse(df$cat=='C', 1, 0)

mdl <- lm(df$Y~df$X+katB+katC)
summary(mdl)

# pers utk A: Y = 39.82 + 2.09X
# pers utk B: Y = 39.82 + 2.09X - 15.58
# pers utk C: Y = 39.82 + 2.09X - 30.92

mdl1 <- lm(Y~X+cat, data=df)
summary(mdl1)

# Plot
wrn <- ifelse(df$cat=='A', 'orange', ifelse(df$cat=='B', 'navy', 'gray'))
plot(Y~X, data=df, col=wrn, pch=16)
abline(mdl$coef[1], mdl$coef[2])
abline(mdl$coef[1]+mdl$coef[3], mdl$coef[2])
abline(mdl$coef[1]+mdl$coef[4], mdl$coef[2])

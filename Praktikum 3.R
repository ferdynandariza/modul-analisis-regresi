##### Praktikum 2 Analisis Regresi Terapan ######
# Kamis, 3 Maret 2022, 10:30-12:10 WIB

# Menginput data pada suatu kolom
Konsumsi_Pakan <- c(87.1, 93.1, 89.8, 91.4, 99.5, 92.1, 95.5, 99.3, 93.4, 94.4)
Bobot_Badan <- c(4.6, 5.1, 4.8, 4.4, 5.4, 4.7, 5.1, 5.2, 4.9, 5.1)
Contoh <- data.frame(y=Konsumsi_Pakan, x=Bobot_Badan)

##### Membuat Scatter Plot #####
plot(x=Contoh$x, y=Contoh$y)

##### Membentuk Model Regresi #####
model = lm(formula=y ~ x, data=Contoh)
print(model)

##### Uji Normalitas #####

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

# Menggunakan Boxplot
boxplot(model$residuals)

##### Uji heteroskedastisitas #####

# Plot residual vs Fitted Value
plot(model$fitted.values, model$residuals)
lines(c(0,1000),c(0, 0), lty=2)

#Contoh yang hetero
#set.seed(3);plot(rnorm(100,0,1:100));lines(c(0,1000),c(0, 0), lty=2)
#set.seed(3);plot(rnorm(100,0,rep(c(1:23, 25:1),2)));lines(c(0,1000),c(0, 0), lty=2) 
#set.seed(3);plot(rnorm(100,0,c(1:50,50:1)));lines(c(0,1000),c(0, 0), lty=2)

# Uji Hipotesis (H0: Residual homogen/Tidak ada masalah heteroskedastisitas)
library(skedastic)
glejser(model)
# Uji White 
white_lm(model)
# Uji Breush Pagan
library(lmtest)
bptest(model, data=Contoh)

##### Menampilkan garis regresi #####
par(mfrow = c(1,1))
plot(x = Contoh$x, y = Contoh$y)
abline(model)


##### Data tidak normal #####

Konsumsi_Air <- c(278.8, 284.5, 280.3, 271.8, 401.4, 
                  276.5, 291.3, 288.5, 288.0, 286.2)
Tinggi_Badan <- c(9.0, 9.3, 9.1,  9.3, 10.1,  9.3, 
                  9.9, 10.0,  9.7,  9.5)
Contoh2 <- data.frame(y=Konsumsi_Air, x=Tinggi_Badan)

plot(Contoh2$x, Contoh2$y, 
     main='Pengaruh Tinggi badan terhadap Jumlah Konsumsi Air',
     xlab='Tinggi Badan (cm)', ylab='Jumlah Konsumsi Air (ml)')
model2 <- lm(y ~ x, data=Contoh2)
abline(model2)
print(model2)

# Plot quatile-quantile
qqnorm(model2$residuals)
qqline(model2$residuals)
# Plot Density
plot(density(model2$residuals), 
     main='Probability Density Funtion')

# Uji Normalitas
shapiro.test(model2$residuals) 
lillie.test(model2$residuals) 
cvm.test(model2$residuals) 
ad.test(model2$residuals) 
# Boxplot
boxplot(model2$residuals)

# Melakukan transformasi
# Log
model2log <- lm(log(y)~x, data=Contoh2)
shapiro.test(model2log$residuals)
plot(density(model2log$residuals))
# Akar kuadrat
model2sqrt <- lm(sqrt(y)~x, data=Contoh2)
shapiro.test(model2sqrt$residuals)
plot(density(model2sqrt$residuals))
# Eksponensial, kuadrat, Box-Cox, dll

# Melihat index nilai outlier
which.max(Contoh2$y)

# Menghapus nilai outlier
Contoh2_fixed <- Contoh2[-5,]
model2fixed <- lm(y~x, data=Contoh2_fixed)
shapiro.test(model2fixed$residuals)
qqnorm(model2fixed$residuals)
qqline(model2fixed$residuals)
plot(density(model2fixed$residuals))


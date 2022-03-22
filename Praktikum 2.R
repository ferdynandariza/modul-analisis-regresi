##### Praktikum 2 Analisis Regresi Terapan ######
# Kamis, 24 Februari 2022, 10:30-12:10 WIB

# Menginput data pada suatu kolom
Konsumsi_Pakan <- c(87.1, 93.1, 89.8, 91.4, 99.5, 92.1, 
                    95.5, 99.3, 93.4, 94.4)
Bobot_Badan <- c(4.6, 5.1, 4.8, 4.4, 5.9, 4.7, 5.1, 5.2, 
                 4.9, 5.1)
Contoh <- data.frame(Konsumsi_Pakan, Bobot_Badan)

##### Menghitung nilai korelasi variabel #####
Rho <- cor(Contoh)
print(Rho)

##### Membuat Scatter Plot #####
plot(x=Contoh$Bobot_Badan, y=Contoh$Konsumsi_Pakan)

##### Membentuk Model Regresi #####
model <- lm(formula=Konsumsi_Pakan ~ Bobot_Badan, data=Contoh)
print(model)
par(mfrow = c(2,2))
plot(model)

##### Menampilkan garis regresi #####
par(mfrow = c(1,1))
plot(x = Contoh$Bobot_Badan, y = Contoh$Konsumsi_Pakan,
     main='Pengaruh Bobot Badan terhadap Konsumsi Pakan',
     ylab='Konsumsi Pakan (gram)', xlab='Bobot Badan (kg)')
abline(model)

##### Menampilkan ANOVA dan t-test #####
anova(model)
summary(model)

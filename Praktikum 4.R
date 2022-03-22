##### Praktikum 4 Analisis Regresi Terapan ######
# Kamis, 10 Maret 2022, 10:30-12:10 WIB

# Memuat library yang diperlukan
library(lmtest)

# Membaca data dari file
contoh <- read.csv('Contoh.csv')

# Menampilkan scatterplot
plot(contoh$Bobot_Badan, contoh$Konsumsi_Pakan, pch=20,
     main='Pengaruh Bobot Badan terhadap Konsumsi Pakan',
     xlab = 'Bobot Badan (kg)', ylab = 'Konsumsi Pakan (gr)')

# Membentuk model linear
model <- lm(Konsumsi_Pakan~Bobot_Badan, data = contoh)
print(model)

# Menampilkan garis regresi
abline(model, lwd=2, col='blue')

# Summary model
summary(model)

# Uji kecocokan model (Goodness-of-fit test) (H0: Model cocok)
chisq.test(model$fitted.values, contoh$Konsumsi_Pakan)

# Selang kepercayaan penduga parameter
coefci(model, level=0.95)

# Uji inferensial penduga parameter (H0: beta = 0)
coeftest(model)

# Korelasi(rho) -> Kekuatan hubungan antar variabel
cor(contoh$Bobot_Badan, contoh$Konsumsi_Pakan, method='pearson')

# Uji Koefisien Korelasi (H0: rho = 0)
cor.test(x = contoh$Bobot_Badan, y = contoh$Konsumsi_Pakan)

##### Data tidak normal #####

contoh2 <- read.csv('Contoh2.csv')
contoh2

# Menampilkan scatterplot
plot(contoh2$Penghasilan, contoh2$Pengeluaran, pch=20,
     main='Penghasilan vs Pengeluaran',
     xlab='Penghasilan (jt/th)',ylab='Pengeluaran (jt/th)')


# Membentuk model linear
model2 <- lm(Pengeluaran ~ Penghasilan, data=contoh2)
print(model2)

# Menampilkan garis regresi
abline(model2, lwd=2, col='blue')
#curve(expr = -56.61+13.94*sqrt(x), xlim = c(20,140), add = TRUE,lwd=2, col='red')

# Summary model
summary(model2)

# Selang kepercayaan penduga parameter
coefci(model2, level = 0.95)

# Uji inferensial penduga parameter (H0: beta = 0)
coeftest(model2,.)

# Korelasi(rho) -> Kekuatan hubungan antar variabel
cor(contoh2$Penghasilan, contoh2$Pengeluaran)

# Uji Koefisien Korelasi (H0: rho = 0)
cor.test(x = contoh2$Penghasilan, y = contoh2$Pengeluaran)

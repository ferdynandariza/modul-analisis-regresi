##### Praktikum 9 Analisis Regresi Terapan ######
# Kamis, 21 April 2022, 10:10-11:20 WIB


# Regresi Stepwise Selection -------------------------------------

head(mtcars)
str(mtcars)

# Matriks scatterplot
pairs(mtcars, pch=19, cex=.5, col='blue', asp=1, lower.panel=NULL,
      main='Motor Trend Cars Road Test')

# Sebelum melakukan regresi stepwise, data harus dijadikan dalam 
# bentuk dataframe terlebih dahulu dengan fungsi `dataframe()`.


# Membuat model intercept-only (hanya mengandung intercept)
intercept_only <- lm(mpg ~ 1, data=mtcars)

# Membuat model yang mengandung semua prediktor
all <- lm(mpg ~ ., data=mtcars)


# Melakukan regresi forward stepwise 
forward <- step(intercept_only, direction='forward', 
                scope=formula(all), trace=F)
# Hasil prosedur regresi forward stepwise
forward$anova
# Model akhir regresi stepwise forward
forward$coefficients

# Melakukan regresi backward stepwise 
backward <- step(all, direction='backward', scope=formula(all), 
                 trace=F)
# Hasil prosedur regresi backward stepwise
backward$anova
# Model akhir regresi stepwise backward
backward$coefficients

# Melakukan regresi forward-backward stepwise 
both <- step(intercept_only, direction='both', scope=formula(all), 
             trace=F)
# Hasil prosedur regresi forward-backward stepwise
both$anova
# Model akhir regresi stepwise forward-backward
both$coefficients
summary(backward)

# Dengan data contoh
td_data <- data.frame(
  'tdr' = c(135, 122, 130, 148, 146, 129, 162, 160, 144, 180, 166, 138, 
           152, 138, 140, 134, 145, 142, 135, 142, 150, 144, 137, 132, 
           149, 132, 120, 126, 161, 170, 152, 162), # Tekanan darah
  'ukt' = c(2.87, 3.25, 3.1, 3.77, 2.98, 2.79, 3.67, 3.61, 2.37, 4.64, 
            3.88, 4.03, 4.12, 3.67, 3.56, 2.99, 3.36, 3.02, 3.17, 3.40,
            3.63, 3.75, 3.29, 3.21, 3.30, 3.02, 2.79, 2.95, 3.80, 4.13, 
            3.96, 4.01), # Ukuran Tubuh
  'umr' = c(45, 41, 49, 42, 54, 47, 60, 48, 44, 64, 59, 51, 64, 56, 54, 
            50, 49, 46, 57, 56, 56, 58, 53, 50, 54, 48, 43, 43, 63, 63, 
            62, 66), # Umur
  'rkk' = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 
            1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0) # Merokok (ya/tdk)
)

td_data
pairs(td_data, pch=19, col='red', main='Data Tekanan Darah')

intersep_aja <- lm(tdr ~ 1, data=td_data)
semua <- lm(tdr ~ ., data=td_data)

forward1 <- step(intersep_aja, direction='forward', 
                scope=formula(semua), trace=T)
backward1 <- step(semua, direction='backward', 
                 scope=formula(intersep_aja), trace=T)


# Regresi Polinomial ---------------------------

# Membangkitkan data polinomial

# Membangkitkan nilai X dengan membuat barisan
X <- seq(10, 20, len=100) # Barisan sepanjang 100, antara 10 sampai 20
# Menentukan nilai beta
beta0 <- 2    # |
beta1 <- -4.5 # Ini hanya contoh, gak harus segini
beta2 <- 0.5  # tentukan sesuai kebutuhan
beta3 <- 0.02 # |
# Membangkitkan galat
set.seed(42)
galat <- rnorm(100, 0, 10)
# Menentukan nilai Y berdasarkan persamaan
# Y = b0 + b1X + b2X^2 + b3X^3
Y <- beta0 + beta1*X + beta2*X^2 + beta3*X^3 + galat

# Membuat model

# Coba dengan model linear
linear <- lm(Y~X)
# Plot dan fit garis dugaan
plot(X,Y, main='Model Linear', pch=16)
abline(linear, lwd=4, col='red')

# Model regresi polinomial
polinom <- lm(Y~poly(X,3))
summary(polinom)

polinom1 <- lm(Y~X+I(X^2)+I(X^3))
summary(polinom1)

# Plot dan garis dugaan
plot(X,Y, pch=19, col='#aaaaaa', main='Model Polinomial')
lines(X,fitted(polinom), lwd=4, lty=2, col='blue')
lines(X,fitted(polinom1), lwd=4, lty=2, col='red')

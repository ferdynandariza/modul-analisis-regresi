##### Praktikum 9 Analisis Regresi Terapan ######
# Kamis, 21 April 2022, 10:10-11:20 WIB


# Regresi Stepwise Selection -------------------------------------

head(mtcars)
str(mtcars)

# Matriks scatterplot
pairs(mtcars, pch=19, cex=.5, col='blue', asp=1, lower.panel=NULL,
      main='MT-Cars Scatterplots')

# Sebelum melakukan regresi stepwise, data harus dijadikan dalam 
# bentuk dataframe terlebih dahulu dengan fungsi `dataframe`.


# Membuat model intercept-only (hanya mengandung intercept)
intercept_only <- lm(mpg ~ 1, data=mtcars)

# Membuat model yang mengandung semua prediktor
all <- lm(mpg ~ ., data=mtcars)


# Melakukan regresi forward stepwise 
forward <- step(intercept_only, direction='forward', 
                scope=formula(all), trace=0)
# Hasil prosedur regresi forward stepwise
forward$anova
# Model akhir regresi stepwise forward
forward$coefficients

# Melakukan regresi backward stepwise 
backward <- step(all, direction='backward', scope=formula(all), 
                 trace=0)
# Hasil prosedur regresi backward stepwise
backward$anova
# Model akhir regresi stepwise backward
backward$coefficients

# Melakukan regresi forward-backward stepwise 
both <- step(intercept_only, direction='both', scope=formula(all), 
             trace=0)
# Hasil prosedur regresi forward-backward stepwise
both$anova
# Model akhir regresi stepwise forward-backward
both$coefficients



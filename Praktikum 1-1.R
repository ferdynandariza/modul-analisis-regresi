##### Praktikum 1.2 Analisis Regresi Terapan ######
# Kamis, 17 Februari 2022, 10:30-12:10 WIB

# Menginput data pada suatu kolom
Konsumsi_Pakan <- c(87.1, 93.1, 89.8, 91.4, 99.5, 92.1, 95.5, 99.3, 93.4, 94.4)
Bobot_Badan <- c(4.6, 5.1, 4.8, 4.4, 5.9, 4.7, 5.1, 5.2, 4.9, 5.1)
Nama_Strain <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J') 
Contoh = data.frame(Nama_Strain, Konsumsi_Pakan, Bobot_Badan)

##### Analisis Deskriptif #####

# Melihat beberapa baris pertama data
head(Contoh, 3)

# Melihat struktur data tiap kolom
str(Contoh)

# Menampilkan Summary statistics
summary(Contoh)

# Menampilkan Summary statistics kolom tertentu
summary(Contoh$Konsumsi_Pakan)

# Mencari mean suatu kolom
mean(Contoh$Konsumsi_Pakan)

# Mencari standar deviasi suatu kolom
sd(Contoh$Konsumsi_Pakan)

# Membuat tabel frekuensi
frekuensi = table(Contoh$Konsumsi_Pakan)
persen = frekuensi/length(Contoh$Konsumsi_Pakan)
f_kumulatif = cumsum(frekuensi)
p_kumulatif = cumsum(persen)
tabel_frekuensi = data.frame(f_kumulatif, p_kumulatif)

# Menampilkan histogram data
hist(Contoh$Konsumsi_Pakan)
hist(Contoh$Konsumsi_Pakan, breaks = c(86,90,94,98,102))
hist(Contoh$Bobot_Badan)

# Membuat diagram pencar
plot(x = Contoh$Bobot_Badan, y = Contoh$Konsumsi_Pakan, main = "Scatter plot")

# Membuat Boxplot
boxplot(Contoh$Konsumsi_Pakan)

q()















# Tugas ###
# Silahkan cari data dengan 1 variabel bebas dan 1 variabel terikat
# (data dengan x dan y)
# Lakukan analisis deskriptif pada data tersebut
# Kerjakan di word, lampirkan data (beserta keterangan), sintaksis, 
# output, dan interpretasinya.
# Kumpul dalam format .pdf
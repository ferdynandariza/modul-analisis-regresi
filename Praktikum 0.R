##### Praktikum 1 Analisis Regresi Terapan ######
# Kamis, 17 Februari 2022, 10:30-12:10 WIB


##### Menginput Data #####

# Menginput data pada suatu kolom
Konsumsi_Pakan <- c(87.1, 93.1, 89.8, 91.4, 99.5, 
                    92.1, 95.5, 99.3, 93.4, 94.4)
Bobot_Badan <- c(4.6, 5.1, 4.8, 4.4, 5.9, 
                 4.7, 5.1, 5.2, 4.9, 5.1)

# Menggabungkan kolom-kolom menjadi suatu Dataframe
Contoh = data.frame(Konsumsi_Pakan, Bobot_Badan)

# Menampilkan Dataframe
print(Contoh)


##### Mengedit Dataframe #####

# Menginput data pada suatu kolom
Konsumsi_Pakan <- c(87.1, 93.1, 89.8, 91.4, 99.5,
                    92.1, 95.5, 99.3, 93.4, 94.4)
Bobot_Badan <- c(4.6, 5.1, 4.8, 4.4, 5.9, 4.7,
                 5.1, 5.2, 4.9, 5.1)
# Menggabungkan kolom-kolom menjadi suatu Dataframe
Contoh = data.frame(Konsumsi_Pakan, Bobot_Badan)

# Membuat kolom baru
Nama_Strain <- c('A', 'B', 'C', 'D', 'E', 'F', 
                 'G', 'H', 'I', 'J') #Membuat kolom baru

#Menambah kolom baru
Contoh = data.frame(Nama_Strain, Contoh) #Menambah kolom pada df yang sudah ada
Contoh = data.frame(Nama_Strain, Konsumsi_Pakan, Bobot_Badan) #Menambah kolom baru
print(Contoh)

# Mengedit suatu data
# misal akan mengedit data pada baris ke-3 dan kolom ke-2
Contoh[3,2] = 100 # 3 menunjukan baris, 2 menunjukkan kolom
Contoh[1,'Nama_Strain'] = 'Bejo' 
print(Contoh)


##### Menyimpan Dataframe #####

Contoh = data.frame(Nama_Strain, Konsumsi_Pakan, Bobot_Badan)

#Menyiman dalam file .csv
write.csv(Contoh, 'contoh.csv', row.names=FALSE) #row.names=FALSE untuk menghilankan indeks

# Membuka file .csv yang sudah disimpan
Databaru = read.csv('contoh.csv')


##### Menutup RStudio #####
quit()

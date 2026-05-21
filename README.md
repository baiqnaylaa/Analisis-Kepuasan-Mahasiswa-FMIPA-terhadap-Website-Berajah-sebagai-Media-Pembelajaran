# Analisis Kepuasan Mahasiswa FMIPA terhadap Website Berajah sebagai Media Pembelajaran

## Latar Belakang
Pemanfaatan teknologi dalam dunia pendidikan semakin berkembang, salah satunya melalui penggunaan website pembelajaran daring. Di, website Berajah digunakan sebagai sarana pendukung proses perkuliahan, seperti penyampaian materi, pengumpulan tugas, dan penyediaan informasi akademik bagi mahasiswa.

Penggunaan website pembelajaran yang efektif dapat meningkatkan kenyamanan dan kelancaran aktivitas akademik mahasiswa. Oleh karena itu, tingkat kepuasan mahasiswa terhadap website Berajah perlu diketahui sebagai bahan evaluasi untuk meningkatkan kualitas layanan yang diberikan.

Penelitian ini dilakukan untuk menganalisis kepuasan mahasiswa FMIPA terhadap penggunaan website Berajah melalui survei online menggunakan Google Form. Teknik pengambilan sampel yang digunakan adalah non-probability sampling dengan metode convenience sampling.

## Tujuan Penelitian
Penelitian ini bertujuan untuk : 
1. Mengetahui tingkat kepuasan mahasiswa FMIPA terhadap penggunaan website Berajah sebagai media pembelajaran daring.
2. Menganalisis aspek-aspek penggunaan website Berajah, seperti kemudahan akses, proses login, kecepatan sistem, dan kejelasan informasi yang disajikan.
3. Memberikan gambaran mengenai kualitas layanan website Berajah berdasarkan hasil survei online mahasiswa FMIPA.

## Metodologi Penelitian 
Penelitian ini menggunakan pendekatan kuantitatif dengan memanfaatkan survei online sebagai metode pengumpulan data. Kuesioner disebarkan melalui Google Form kepada mahasiswa FMIPA untuk memperoleh informasi mengenai kepuasan penggunaan website Berajah sebagai media pembelajaran.

Pengambilan responden dilakukan menggunakan teknik convenience sampling dalam metode non-probability sampling. Teknik ini dipilih karena responden dapat dijangkau dengan lebih mudah selama proses penelitian berlangsung. Total responden yang berpartisipasi dalam penelitian ini berjumlah 57 mahasiswa.

Tahap pengolahan dan analisis data dilakukan menggunakan software R. Data diolah dan diproses menggunakan software R untuk memudahkan analisis data penelitian, seperti yang tertera pada `ANALISISNONPROBABILITY.R`

## Analisis Data 
### 1. Memanggil Package yang Sekiranya Akan Digunakan dalam Analisis
```r
library(readxl)   # untuk membaca file Excel
library(ggplot2)  # untuk membuat grafik
library(psych)    # untuk uji reliabilitas
```

### 2. Impor Data 
Data hasil survei yang telah disimpan dalam file Excel diimpor ke dalam R menggunakan package `readxl`. Kolom yang tidak diperlukan seperti Timestamp dihapus, dan nama kolom disederhanakan agar lebih mudah digunakan.
```r
data <- read_excel("D:/SEMESTER 4/TEKSAM/Survei_Kepuasan_Mahasiswa_FMIPA_terhadap_Penggunaan_Website_BERAJAH_sebagai_Media_Pembelajaran_di_Universitas_Mataram__Jawaban_.xlsx")

data <- data[, 2:15]
colnames(data) <- c(
  "Nama", "Prodi", "Semester", "Frekuensi",
  "P1", "P2", "P3", "P4", "P5",
  "P6", "P7", "P8", "P9", "P10"
)

item <- data[, 5:14]
```

### 3. Struktur Data
Fungsi str() digunakan untuk melihat struktur data secara keseluruhan, termasuk tipe data setiap variabel dan jumlah observasi.
```r
str(data)
```

### 4. Menghitung Sampel dengan Rumus Slovin
Rumus Slovin digunakan untuk menentukan jumlah sampel minimum dari populasi yang diketahui. Populasi mahasiswa FMIPA Universitas Mataram sebanyak 290 mahasiswa dengan tingkat error 12%.
```r
N        <- 290
e        <- 0.12
n_slovin <- ceiling(N / (1 + N * e^2))
print(paste("Jumlah sampel Slovin:", n_slovin, "responden"))
```

### 5. Uji Validitas 
Uji validitas dilakukan untuk memastikan setiap item pertanyaan dalam kuesioner mampu mengukur apa yang seharusnya diukur. Item dinyatakan valid jika nilai r hitung lebih besar dari r tabel. Untuk n = 57 dengan alpha = 0.05, r tabel = 0.266.
```r
item$Total <- rowSums(item)
r_hitung   <- cor(item)[, "Total"]
r_hitung   <- r_hitung[names(r_hitung) != "Total"]

validitas <- data.frame(
  Item       = names(r_hitung),
  r_hitung   = round(r_hitung, 3),
  r_tabel    = 0.266,
  Keterangan = ifelse(r_hitung > 0.266, "Valid", "Tidak Valid")
)
print(validitas)
```

### 6. Uji Reliabilitas 
Uji reliabilitas dilakukan menggunakan metode Cronbach's Alpha untuk mengukur konsistensi instrumen penelitian. Instrumen dinyatakan reliabel jika nilai alpha lebih besar atau sama dengan 0.6.
```r
hasil_alpha <- alpha(item[, 1:10])
print(paste("Cronbach's Alpha:", round(hasil_alpha$total$raw_alpha, 3)))

if (hasil_alpha$total$raw_alpha >= 0.8) {
  print("Keterangan: Sangat Reliabel")
} else if (hasil_alpha$total$raw_alpha >= 0.6) {
  print("Keterangan: Reliabel")
} else {
  print("Keterangan: Tidak Reliabel")
}
```

### 7. Analisis Deskriptif 
Analisis deskriptif dilakukan untuk mengetahui distribusi responden berdasarkan program studi, semester, dan frekuensi penggunaan website BERAJAH. Fungsi `table()` digunakan untuk menghitung jumlah responden pada setiap kategori, sedangkan `prop.table()` digunakan untuk menghitung persentasenya.
```r
item$Total <- NULL

table(data$Prodi)
prop.table(table(data$Prodi)) * 100

table(data$Semester)
prop.table(table(data$Semester)) * 100

table(data$Frekuensi)
prop.table(table(data$Frekuensi)) * 100
```

### 8. Tabel Distribusi Frekuensi dan Persentase 
Tahap ini menyajikan distribusi responden berdasarkan program studi, semester, dan frekuensi penggunaan website BERAJAH. Fungsi `table()` digunakan untuk menghitung frekuensi dan `prop.table()` untuk menghitung persentasenya.
```r
table(data$Prodi)
prop.table(table(data$Prodi)) * 100

table(data$Semester)
prop.table(table(data$Semester)) * 100

table(data$Frekuensi)
prop.table(table(data$Frekuensi)) * 100
```

### 9. Grafik Distribusi Responden
Grafik batang dibuat menggunakan fungsi `barplot()` untuk memvisualisasikan distribusi responden berdasarkan program studi, semester, dan frekuensi penggunaan website BERAJAH.
```r
barplot(
  sort(table(data$Prodi), decreasing = TRUE),
  main = "Distribusi Responden Berdasarkan Program Studi",
  xlab = "Program Studi",
  ylab = "Frekuensi",
  col  = "lightblue"
)

barplot(
  table(data$Semester),
  main = "Distribusi Responden Berdasarkan Semester",
  xlab = "Semester",
  ylab = "Frekuensi",
  col  = "lightgreen"
)

barplot(
  table(data$Frekuensi),
  main = "Distribusi Frekuensi Penggunaan Website BERAJAH",
  xlab = "Frekuensi Penggunaan",
  ylab = "Jumlah Responden",
  col  = "lightyellow"
)
```

### 10. Naive Estimation 
Naive estimation digunakan untuk memperoleh estimasi awal tingkat kepuasan mahasiswa. Responden dianggap puas jika memberikan nilai lebih besar atau sama dengan 4 pada skala Likert. Proporsi dihitung dengan membagi jumlah responden yang puas dengan total responden.
```r
n           <- nrow(item)
jumlah_puas <- colSums(item >= 4)
naive       <- jumlah_puas / n

hasil_naive <- data.frame(
  Item        = colnames(item),
  Jumlah_Puas = jumlah_puas,
  Total       = n,
  P_hat       = paste0(round(naive * 100, 1), "%")
)
print(hasil_naive)
print(paste("Rata-rata Naive Estimation:", paste0(round(mean(naive) * 100, 1), "%")))
```

### 11. Weighting Sederhana 
Weighting dilakukan untuk mengurangi bias akibat ketidakseimbangan distribusi sampel dibandingkan populasi. Bobot dihitung berdasarkan perbandingan antara proporsi populasi dan proporsi sampel per program studi.
```r
pop <- c(Statistika=44, Kimia=54, Fisika=44,
         "Ilmu Lingkungan"=52, Matematika=49, Biologi=47)
prop_pop         <- pop / sum(pop)
prop_samp        <- as.numeric(table(data$Prodi)[names(pop)]) / n
names(prop_samp) <- names(pop)
bobot            <- prop_pop / prop_samp

bobot_df <- data.frame(
  Prodi         = names(pop),
  Prop_Populasi = round(prop_pop, 4),
  Prop_Sampel   = round(prop_samp, 4),
  Bobot         = round(bobot, 4)
)
print(bobot_df)

weighted <- sapply(colnames(item), function(p_item) {
  total_atas  <- 0
  total_bawah <- 0
  for (p in names(pop)) {
    idx         <- data$Prodi == p
    w           <- bobot[p]
    puas        <- sum(item[idx, p_item] >= 4)
    total_atas  <- total_atas  + w * puas
    total_bawah <- total_bawah + w * sum(idx)
  }
  return(total_atas / total_bawah)
})
```

### 12. Perbandingan Naive dan Weighted Estimation 
Tahap ini membandingkan hasil estimasi sebelum dan sesudah dilakukan pembobotan menggunakan tabel dan grafik batang.
```r
hasil <- data.frame(
  Item     = colnames(item),
  Naive    = paste0(round(naive * 100, 1), "%"),
  Weighted = paste0(round(weighted * 100, 1), "%"),
  Selisih  = paste0(round((weighted - naive) * 100, 1), "%")
)
print(hasil)
print(paste("Rata-rata Naive   :", paste0(round(mean(naive) * 100, 1), "%")))
print(paste("Rata-rata Weighted:", paste0(round(mean(weighted) * 100, 1), "%")))

nilai <- rbind(naive * 100, weighted * 100)

barplot(nilai,
        beside  = TRUE,
        names   = colnames(item),
        legend  = c("Naive", "Weighted"),
        col     = c("salmon", "turquoise"),
        ylab    = "Persentase (%)",
        main    = "Perbandingan Naive vs Weighted Estimation")
```

## Hasil dan Pembahasan

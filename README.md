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
Analisis deskriptif dilakukan untuk mengetahui distribusi responden berdasarkan program studi, semester, dan frekuensi penggunaan website BERAJAH. Tahap ini menyajikan distribusi responden berdasarkan program studi, semester, dan frekuensi penggunaan website BERAJAH. Fungsi `table()` digunakan untuk menghitung frekuensi dan `prop.table()` untuk menghitung persentasenya.
```r
freq_prodi            <- as.data.frame(table(data$Prodi))
freq_prodi$Persentase <- paste0(round(freq_prodi$Freq / nrow(data) * 100, 1), "%")
colnames(freq_prodi)  <- c("Program Studi", "Frekuensi", "Persentase")
print(freq_prodi)

freq_semester            <- as.data.frame(table(data$Semester))
freq_semester$Persentase <- paste0(round(freq_semester$Freq / nrow(data) * 100, 1), "%")
colnames(freq_semester)  <- c("Semester", "Frekuensi", "Persentase")
print(freq_semester)

freq_guna            <- as.data.frame(table(data$Frekuensi))
freq_guna$Persentase <- paste0(round(freq_guna$Freq / nrow(data) * 100, 1), "%")
colnames(freq_guna)  <- c("Frekuensi Penggunaan", "Jumlah", "Persentase")
print(freq_guna)
```

### 8. Pie-Chart Distribusi Responden
Grafik lingkaran (pie chart) dibuat menggunakan fungsi ggplot() dengan coord_polar() untuk memvisualisasikan distribusi responden berdasarkan program studi, semester, dan frekuensi penggunaan website BERAJAH.
```r
#Program Studi
freq_prodi_plot <- as.data.frame(table(data$Prodi))
colnames(freq_prodi_plot) <- c("Prodi", "Jumlah")
freq_prodi_plot$Persentase <- round(freq_prodi_plot$Jumlah / sum(freq_prodi_plot$Jumlah) * 100, 1)
freq_prodi_plot$Label <- paste0(freq_prodi_plot$Prodi, "\n", freq_prodi_plot$Jumlah, " (", freq_prodi_plot$Persentase, "%)")

ggplot(freq_prodi_plot, aes(x = "", y = Jumlah, fill = Prodi)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Jumlah, "\n(", Persentase, "%)")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribusi Responden per Program Studi", fill = "Program Studi") +
  theme_void()
#Semester
freq_sem_plot <- as.data.frame(table(data$Semester))
colnames(freq_sem_plot) <- c("Semester", "Jumlah")
freq_sem_plot$Persentase <- round(freq_sem_plot$Jumlah / sum(freq_sem_plot$Jumlah) * 100, 1)

ggplot(freq_sem_plot, aes(x = "", y = Jumlah, fill = Semester)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Jumlah, "\n(", Persentase, "%)")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribusi Responden per Semester", fill = "Semester") +
  theme_void()
#Frekuensi penggunaan berajah 
freq_guna_plot <- as.data.frame(table(data$Frekuensi))
colnames(freq_guna_plot) <- c("Frekuensi", "Jumlah")
freq_guna_plot$Persentase <- round(freq_guna_plot$Jumlah / sum(freq_guna_plot$Jumlah) * 100, 1)

ggplot(freq_guna_plot, aes(x = "", y = Jumlah, fill = Frekuensi)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Jumlah, "\n(", Persentase, "%)")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Frekuensi Penggunaan Website BERAJAH", fill = "Frekuensi") +
  theme_void()
```

### 9. Naive Estimation 
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

### 10. Weighting Sederhana 
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

### 11. Perbandingan Naive dan Weighted Estimation 
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
### Total Sampel Berdasarkan Rumus Slovin
Rumus Slovin digunakan untuk menentukan jumlah sampel minimum dari populasi yang diketahui. Populasi mahasiswa FMIPA Universitas Mataram sebanyak 290 mahasiswa dengan tingkat error 12%.  Sehingga jumlah sampel minimum yang diperlukan adalah 57 responden.
### Uji Validitas 
Uji validitas dilakukan untuk memastikan setiap item pertanyaan dalam kuesioner mampu mengukur apa yang seharusnya diukur. Item dinyatakan valid jika nilai r hitung lebih besar dari r tabel. Untuk n = 57 dengan alpha = 0.05, r tabel = 0.266.
| Item | r hitung | r tabel | Keterangan |
|------|----------|---------|------------|
| P1   | 0.862    | 0.266   | Valid      |
| P2   | 0.735    | 0.266   | Valid      |
| P3   | 0.817    | 0.266   | Valid      |
| P4   | 0.776    | 0.266   | Valid      |
| P5   | 0.824    | 0.266   | Valid      |
| P6   | 0.807    | 0.266   | Valid      |
| P7   | 0.724    | 0.266   | Valid      |
| P8   | 0.615    | 0.266   | Valid      |
| P9   | 0.791    | 0.266   | Valid      |
| P10  | 0.794    | 0.266   | Valid      |
Seluruh 10 item pertanyaan dinyatakan valid karena nilai r hitung lebih besar dari r tabel (0.266).
### Uji Reliabilitas
Uji reliabilitas dilakukan menggunakan metode Cronbach's Alpha untuk mengukur konsistensi instrumen penelitian. Instrumen dinyatakan reliabel jika nilai alpha lebih besar atau sama dengan 0.6.
| Cronbach's Alpha | N of Items | Keterangan      |
|------------------|------------|-----------------|
| 0.924            | 10         | Sangat Reliabel |
Instrumen penelitian dinyatakan sangat reliabel dengan nilai Cronbach's Alpha sebesar 0.924, jauh di atas ambang batas minimum 0.6.
### Analisis Deskriptif 
Analisis deskriptif digunakan untuk memperoleh gambaran umum mengenai karakteristik responden yang terlibat dalam survei. Pada penelitian ini, karakteristik yang ditinjau meliputi program studi, semester, serta intensitas penggunaan website BERAJAH.
##### Program Studi
| Program Studi   | Frekuensi | Persentase |
|-----------------|-----------|------------|
| Biologi         | 8         | 14.0%      |
| Fisika          | 5         | 8.8%       |
| Ilmu Lingkungan | 9         | 15.8%      |
| Kimia           | 5         | 8.8%       |
| Matematika      | 7         | 12.3%      |
| Statistika      | 23        | 40.4%      |
| Total           | 57        | 100%       |
Dari keseluruhan 57 responden, Program Studi Statistika mendominasi dengan jumlah 23 mahasiswa atau sekitar 40.4% dari total responden. Sementara itu, Program Studi Fisika dan Kimia menjadi yang paling sedikit dengan masing-masing hanya 5 responden (8.8%).
#### Semester 
| Semester   | Frekuensi | Persentase |
|------------|-----------|------------|
| Semester 4 | 37        | 64.9%      |
| Semester 6 | 20        | 35.1%      |
| Total      | 57        | 100%       |
Ditinjau dari semester, lebih dari separuh responden merupakan mahasiswa Semester 4 yakni sebanyak 37 orang (64.9%), sedangkan mahasiswa Semester 6 berjumlah 20 orang (35.1%).
#### Frekuensi Penggunaan Website Berajah 
| Frekuensi Penggunaan | Jumlah | Persentase |
|----------------------|--------|------------|
| Jarang               | 8      | 14.0%      |
| Kadang-Kadang        | 28     | 49.1%      |
| Sangat Sering        | 4      | 7.0%       |
| Sering               | 17     | 29.8%      |
| Total                | 57     | 100%       |
Terkait intensitas penggunaan, hampir setengah dari responden mengaku menggunakan website BERAJAH secara kadang-kadang (49.1%). Responden yang menggunakannya secara sering tercatat sebanyak 17 orang (29.8%), diikuti jarang 8 orang (14.0%), dan sangat sering hanya 4 orang (7.0%).
### PIE-CHART DISTRIBUSI RESPONDEN
#### Distribusi Responden per Program Studi
![Program Studi](PIE%20CHART%20DISTRIBUSI%20RESPONDEN%20PER%20PROGRAM%20STUDI.png)

Berdasarkan tabel di atas, dari 57 responden yang berpartisipasi dalam survei, mahasiswa Program Studi Statistika mendominasi dengan jumlah 23 orang (40.4%). Posisi berikutnya ditempati oleh Ilmu Lingkungan sebanyak 9 orang (15.8%), Biologi 8 orang (14.0%), Matematika 7 orang (12.3%), serta Fisika dan Kimia yang masing-masing berjumlah 5 orang (8.8%). Tingginya proporsi mahasiswa Statistika dalam sampel menunjukkan adanya ketidakseimbangan keterwakilan antar program studi, sehingga diperlukan metode weighting untuk mengoreksi bias tersebut.
#### Distribusi Responden per Semester
![Semester](PIE%20CHART%20DISTRIBUSI%20RESPONDEN%20PERSEMESTER.png)

Berdasarkan tabel di atas, responden dalam penelitian ini terdiri dari mahasiswa dua angkatan yakni Semester 4 dan Semester 6. Mayoritas responden berasal dari Semester 4 sebanyak 37 orang (64.9%), sedangkan mahasiswa Semester 6 berjumlah 20 orang (35.1%). Dominasi mahasiswa Semester 4 kemungkinan disebabkan karena mereka lebih aktif menggunakan website BERAJAH sebagai media pembelajaran pada semester tersebut.

#### Frekuensi Penggunaan Website Berajah
![Frekuensi](PIE%20CHART%20FREKUENSI%20PENGGUNAAN%20WEBSITE%20BERAJAH.png)

Berdasarkan tabel di atas, intensitas penggunaan website BERAJAH oleh responden cukup bervariasi. Sebagian besar responden menggunakan website BERAJAH secara kadang-kadang yakni sebanyak 28 orang (49.1%), diikuti oleh kategori sering sebanyak 17 orang (29.8%), jarang 8 orang (14.0%), dan sangat sering hanya 4 orang (7.0%). Kondisi ini mengindikasikan bahwa pemanfaatan website BERAJAH oleh mahasiswa FMIPA Universitas Mataram masih belum optimal dan perlu ditingkatkan lebih lanjut.
### Naive Estimation
| Item | Jumlah Puas | Total | P̂     |
|------|-------------|-------|-------|
| P1   | 29          | 57    | 50.9% |
| P2   | 33          | 57    | 57.9% |
| P3   | 40          | 57    | 70.2% |
| P4   | 40          | 57    | 70.2% |
| P5   | 31          | 57    | 54.4% |
| P6   | 23          | 57    | 40.4% |
| P7   | 20          | 57    | 35.1% |
| P8   | 26          | 57    | 45.6% |
| P9   | 35          | 57    | 61.4% |
| P10  | 35          | 57    | 61.4% |
| Rata-rata | -     | -     | 54.7% |

Berdasarkan hasil naive estimation, diperoleh rata-rata tingkat kepuasan mahasiswa terhadap website BERAJAH sebesar 54.7%. Item dengan tingkat kepuasan tertinggi adalah P3 (kemudahan mengakses materi) dan P4 (kemudahan mengunduh materi) yang keduanya mencapai 70.2%. Sementara itu, item dengan tingkat kepuasan terendah adalah P7 (kestabilan website) sebesar 35.1%, yang mengindikasikan bahwa mahasiswa masih cukup banyak mengalami gangguan saat menggunakan website BERAJAH. Secara keseluruhan, lebih dari separuh responden menyatakan puas terhadap layanan yang disediakan website BERAJAH.
### Weighting Estimation 
| Program Studi   | Prop. Populasi | Prop. Sampel | Bobot  |
|-----------------|----------------|--------------|--------|
| Statistika      | 0.1517         | 0.4035       | 0.3760 |
| Kimia           | 0.1862         | 0.0877       | 2.1228 |
| Fisika          | 0.1517         | 0.0877       | 1.7297 |
| Ilmu Lingkungan | 0.1793         | 0.1579       | 1.1356 |
| Matematika      | 0.1690         | 0.1228       | 1.3759 |
| Biologi         | 0.1621         | 0.1404       | 1.1547 |

Pembobotan dilakukan dengan membandingkan proporsi populasi dan proporsi sampel tiap program studi. Program Studi Statistika memperoleh bobot terendah sebesar 0.376 karena jumlah sampelnya terlalu mendominasi dibandingkan proporsi populasinya. Sebaliknya, Program Studi Kimia mendapat bobot tertinggi sebesar 2.123 karena jumlah sampelnya sangat kurang mewakili proporsi populasinya. Prodi lain seperti Fisika (1.730), Matematika (1.376), Biologi (1.155), dan Ilmu Lingkungan (1.136) mendapat bobot di atas 1 yang berarti sampel mereka masih kurang terwakili dibandingkan populasinya.

### Perbandingan Naive Estimation dan Weighting Estimation
| Item      | Naive  | Weighted | Selisih |
|-----------|--------|----------|---------|
| P1        | 50.9%  | 53.6%    | +2.7%   |
| P2        | 57.9%  | 62.0%    | +4.1%   |
| P3        | 70.2%  | 72.7%    | +2.5%   |
| P4        | 70.2%  | 68.2%    | -2.0%   |
| P5        | 54.4%  | 56.2%    | +1.8%   |
| P6        | 40.4%  | 45.9%    | +5.5%   |
| P7        | 35.1%  | 38.2%    | +3.1%   |
| P8        | 45.6%  | 51.3%    | +5.7%   |
| P9        | 61.4%  | 61.2%    | -0.2%   |
| P10       | 61.4%  | 69.4%    | +8.0%   |
| Rata-rata | 54.7%  | 57.9%    | +3.2%   |

Berdasarkan tabel perbandingan di atas, weighted estimation menghasilkan rata-rata tingkat kepuasan sebesar 57.9%, lebih tinggi dibandingkan naive estimation yang hanya 54.7% dengan selisih sebesar 3.2%. Sebagian besar item mengalami peningkatan estimasi setelah dilakukan pembobotan, dengan kenaikan terbesar terjadi pada P10 sebesar 8.0% dan P8 sebesar 5.7%. Hanya P4 dan P9 yang mengalami sedikit penurunan masing-masing sebesar 2.0% dan 0.2%. Peningkatan estimasi setelah pembobotan terjadi karena prodi yang kurang terwakili dalam sampel seperti Kimia dan Fisika mendapat bobot lebih besar, dan ternyata responden dari prodi tersebut cenderung memberikan penilaian yang lebih tinggi. Perbedaan sebesar 3.2% ini mengindikasikan adanya bias keterwakilan dalam sampel yang berhasil dikoreksi melalui metode weighting sederhana.

## Kesimpulan 
Berdasarkan hasil analisis yang telah dilakukan, dapat disimpulkan bahwa seluruh 10 item pertanyaan dalam kuesioner dinyatakan valid dengan nilai r hitung berkisar antara 0.615 hingga 0.862, seluruhnya melebihi r tabel sebesar 0.266. Instrumen penelitian juga terbukti sangat reliabel dengan nilai Cronbach's Alpha sebesar 0.924, sehingga kuesioner layak digunakan sebagai alat ukur dalam penelitian ini. Dari 57 responden yang berpartisipasi, mayoritas berasal dari Program Studi Statistika (40.4%) dan Semester 4 (64.9%). Sebagian besar responden menggunakan website BERAJAH secara kadang-kadang (49.1%), yang mengindikasikan bahwa pemanfaatan website oleh mahasiswa FMIPA Universitas Mataram masih belum optimal.

Hasil naive estimation menunjukkan rata-rata tingkat kepuasan mahasiswa sebesar 54.7%, dengan aspek tertinggi pada kemudahan mengakses dan mengunduh materi perkuliahan (70.2%), sedangkan aspek kestabilan website memperoleh penilaian terendah (35.1%). Setelah dilakukan pembobotan berdasarkan proporsi populasi per program studi, rata-rata tingkat kepuasan meningkat menjadi 57.9%. Selisih sebesar 3.2% antara kedua metode ini menunjukkan adanya bias keterwakilan dalam sampel yang berhasil dikoreksi melalui metode weighting sederhana. Secara keseluruhan, tingkat kepuasan mahasiswa FMIPA terhadap website BERAJAH berada pada kategori cukup puas dan masih memerlukan peningkatan terutama pada aspek kestabilan dan kecepatan website.

## Link Kuisioner 
https://forms.gle/qnxgCDHw6UDgbNcA8

library(readxl)
library(ggplot2)
library(psych)

# import data
data <- read_excel("D:/SEMESTER 4/TEKNIK SAMPLING/PAK HENDRA/TUGAS 2/Survei Kepuasan Mahasiswa FMIPA terhadap Penggunaan Website BERAJAH sebagai Media Pembelajaran di Universitas Mataram (Jawaban).xlsx")

# ambil kolom yang diperlukan
data <- data[, 2:15]
colnames(data) <- c(
  "Nama", "Prodi", "Semester", "Frekuensi",
  "P1", "P2", "P3", "P4", "P5",
  "P6", "P7", "P8", "P9", "P10"
)

item <- data[, 5:14]

# 1. STRUKTUR DATA
str(data)

# 2. SAMPEL SLOVIN
N        <- 290
e        <- 0.12
n_slovin <- ceiling(N / (1 + N * e^2))
print(paste("Jumlah sampel Slovin:", n_slovin, "responden"))

# 3. UJI VALIDITAS
# r tabel = 0.266 (n=57, alpha=0.05)
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

# 4. UJI RELIABILITAS
library(psych)
hasil_alpha <- alpha(item[, 1:10])
print(paste("Cronbach's Alpha:", round(hasil_alpha$total$raw_alpha, 3)))

if (hasil_alpha$total$raw_alpha >= 0.8) {
  print("Keterangan: Sangat Reliabel")
} else if (hasil_alpha$total$raw_alpha >= 0.6) {
  print("Keterangan: Reliabel")
} else {
  print("Keterangan: Tidak Reliabel")
}

# 5. ANALISIS DESKRIPTIF
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

# 5. PIE-CHART DISTRIBUSI RESPONDEN
# Program Studi
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
#semester
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

# 6. NAIVE ESTIMATION (puas = skor >= 4)
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
print(paste("Rata-rata Naive:", paste0(round(mean(naive) * 100, 1), "%")))

# 7. WEIGHTED ESTIMATION
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

# 8. PERBANDINGAN NAIVE vs WEIGHTED
hasil <- data.frame(
  Item     = colnames(item),
  Naive    = paste0(round(naive * 100, 1), "%"),
  Weighted = paste0(round(weighted * 100, 1), "%"),
  Selisih  = paste0(round((weighted - naive) * 100, 1), "%")
)
print(hasil)
print(paste("Rata-rata Naive   :", paste0(round(mean(naive) * 100, 1), "%")))
print(paste("Rata-rata Weighted:", paste0(round(mean(weighted) * 100, 1), "%")))

# grafik perbandingan
# 8. PERBANDINGAN NAIVE vs WEIGHTED
nilai <- rbind(naive * 100, weighted * 100)

barplot(nilai,
        beside  = TRUE,
        names   = colnames(item),
        legend  = c("Naive", "Weighted"),
        col     = c("salmon", "turquoise"),
        ylab    = "Persentase (%)",
        main    = "Perbandingan Naive vs Weighted Estimation")


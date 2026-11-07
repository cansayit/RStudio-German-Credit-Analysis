
# RSTUDIO ILE VERI MADENCILIGI
# Vize Odevi - German Credit Veri Seti

# R surumunu kontrol edelim
R.version.string

# Calisma klasorunu gorelim
getwd()

# Basit bir test
print("RStudio vize projemiz hazir!")



# ASAMA 2 - VERI SETINI TANIMA

# German Credit veri setini dogru yukleme
kredi <- read.table("data/german.data", header = FALSE)

# Veri setinin boyutlari
dim(kredi)

# Ilk 6 satiri goruntule
head(kredi)

# Sutun isimleri
names(kredi)

# Veri yapisi
str(kredi)

# ASAMA 3 - SUTUN ISIMLERINI DUZENLEME

colnames(kredi) <- c(
  "Checking_Status",
  "Duration",
  "Credit_History",
  "Purpose",
  "Credit_Amount",
  "Savings",
  "Employment",
  "Installment_Rate",
  "Personal_Status",
  "Other_Debtors",
  "Residence_Since",
  "Property",
  "Age",
  "Other_Installment",
  "Housing",
  "Existing_Credits",
  "Job",
  "Dependents",
  "Telephone",
  "Foreign_Worker",
  "Risk"
)

# Yeni sutun isimlerini kontrol et
names(kredi)

# Ilk 6 satiri goruntule
head(kredi)



# ASAMA 3 - VERI SETINI INCELEME

# Veri turlerini incele
str(kredi)

# Istatistiksel ozet
summary(kredi)

# Sayisal degiskenlerin ozeti
summary(kredi[, c(
  "Age",
  "Duration",
  "Credit_Amount"
)])

# Kredi risk siniflarinin sayisi
table(kredi$Risk)


# Kredi risk oranlari
round(
  prop.table(table(kredi$Risk)) * 100,
  2
)


# ASAMA 4 - VERI ON ISLEME

# 1. Eksik degerleri kontrol et
colSums(is.na(kredi))

# Toplam eksik deger sayisi
sum(is.na(kredi))

# 2. Tekrar eden satirlari kontrol et
sum(duplicated(kredi))


# 3. Hedef degiskeni kategorik hale getir
kredi$Risk <- factor(
  kredi$Risk,
  levels = c(1, 2),
  labels = c("Iyi", "Kotu")
)

# 4. Diger kategorik degiskenleri factor yap
kategorik <- c(
  "Checking_Status",
  "Credit_History",
  "Purpose",
  "Savings",
  "Employment",
  "Personal_Status",
  "Other_Debtors",
  "Property",
  "Other_Installment",
  "Housing",
  "Job",
  "Telephone",
  "Foreign_Worker"
)

kredi[kategorik] <- lapply(
  kredi[kategorik],
  factor
)

# 5. Sonuclari kontrol et
str(kredi)

table(kredi$Risk)


# 6. Islenmis veri setini kaydet
saveRDS(
  kredi,
  file = "data/german_credit_temiz.rds"
)

# Kayit kontrolu
file.exists("data/german_credit_temiz.rds")

# ASAMA 5 - VERI GORSELLESTIRME

library(ggplot2)

# Grafik 1 - Kredi riski dagilimi

grafik1 <- ggplot(kredi, aes(x = Risk, fill = Risk)) +
  geom_bar() +
  labs(
    title = "Kredi Riski Dagilimi",
    x = "Kredi Riski",
    y = "Kisi Sayisi"
  ) +
  scale_fill_manual(
    values = c("Iyi" = "#27AE60", "Kotu" = "#E74C3C")
  ) +
  theme_minimal()

print(grafik1)

# Grafik 2 - Yas dagilimi

grafik2 <- ggplot(kredi, aes(x = Age)) +
  geom_histogram(
    binwidth = 5,
    fill = "#3498DB",
    color = "white"
  ) +
  labs(
    title = "Kredi Basvurusu Yapanlarin Yas Dagilimi",
    x = "Yas",
    y = "Kisi Sayisi"
  ) +
  theme_minimal()

print(grafik2)

# Grafik 3 - Kredi miktari ve risk

grafik3 <- ggplot(
  kredi,
  aes(x = Risk, y = Credit_Amount, fill = Risk)
) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("Iyi" = "#27AE60", "Kotu" = "#E74C3C")
  ) +
  labs(
    title = "Kredi Miktari ve Risk Iliskisi",
    x = "Kredi Riski",
    y = "Kredi Miktari"
  ) +
  theme_minimal()

print(grafik3)

# Grafik 4 - Kredi suresi ve risk

grafik4 <- ggplot(
  kredi,
  aes(x = Risk, y = Duration, fill = Risk)
) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("Iyi" = "#27AE60", "Kotu" = "#E74C3C")
  ) +
  labs(
    title = "Kredi Suresi ve Risk Iliskisi",
    x = "Kredi Riski",
    y = "Kredi Suresi (Ay)"
  ) +
  theme_minimal()

print(grafik4)

# Grafik klasoru olustur
dir.create("grafikler", showWarnings = FALSE)

# Grafikleri PNG olarak kaydet
ggsave("grafikler/risk_dagilimi.png", grafik1,
       width = 8, height = 5, dpi = 300)

ggsave("grafikler/yas_dagilimi.png", grafik2,
       width = 8, height = 5, dpi = 300)

ggsave("grafikler/kredi_miktari.png", grafik3,
       width = 8, height = 5, dpi = 300)

ggsave("grafikler/kredi_suresi.png", grafik4,
       width = 8, height = 5, dpi = 300)


# ASAMA 6 - KARAR AGACI MODELI

library(rpart)
library(rpart.plot)

# Tekrarlanabilirlik icin
set.seed(42)

# Sinif oranlarini koruyarak ayirma
iyi <- which(kredi$Risk == "Iyi")
kotu <- which(kredi$Risk == "Kotu")

egitim_id <- c(
  sample(iyi, size = round(length(iyi) * 0.80)),
  sample(kotu, size = round(length(kotu) * 0.80))
)

egitim <- kredi[egitim_id, ]
test <- kredi[-egitim_id, ]

# Boyutlari kontrol et
dim(egitim)
dim(test)

# Sinif dagilimlarini kontrol et
table(egitim$Risk)
table(test$Risk)

# Karar agaci modelini olustur
model <- rpart(
  Risk ~ .,
  data = egitim,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 20
  )
)

# Model ozeti
print(model)

# Karar agacini ciz
rpart.plot(
  model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Kredi Riski Karar Agaci"
)

# Test verisi uzerinde tahmin
tahmin <- predict(
  model,
  newdata = test,
  type = "class"
)

# Gercek ve tahmin edilen siniflar
sonuc <- table(
  Gercek = test$Risk,
  Tahmin = tahmin
)

print(sonuc)

# Dogruluk orani
dogruluk <- mean(tahmin == test$Risk)

cat(
  "Model Dogruluk Orani: %",
  round(dogruluk * 100, 2),
  "\n"
)


# ASAMA 7 - MODEL PERFORMANSI

# Pozitif sinif: Kotu
TP <- sonuc["Kotu", "Kotu"]
TN <- sonuc["Iyi", "Iyi"]
FP <- sonuc["Iyi", "Kotu"]
FN <- sonuc["Kotu", "Iyi"]

# Performans metrikleri
accuracy <- (TP + TN) / sum(sonuc)

precision <- TP / (TP + FP)

recall <- TP / (TP + FN)

specificity <- TN / (TN + FP)

f1 <- 2 * precision * recall /
  (precision + recall)

# Sonuclari yazdir
metrikler <- data.frame(
  Metrik = c(
    "Accuracy",
    "Precision",
    "Recall",
    "Specificity",
    "F1 Score"
  ),
  Deger = round(
    100 * c(
      accuracy,
      precision,
      recall,
      specificity,
      f1
    ),
    2
  )
)

print(metrikler)

# Basit referans model
baseline <- mean(test$Risk == "Iyi")

cat(
  "Referans model dogrulugu: %",
  round(baseline * 100, 2),
  "\n"
)

cat(
  "Karar agaci dogrulugu: %",
  round(accuracy * 100, 2),
  "\n"
)

# Sonuclari kaydet
write.csv(
  metrikler,
  "data/model_performans.csv",
  row.names = FALSE
)

# Modeli kaydet
saveRDS(
  model,
  "data/karar_agaci_modeli.rds"
)

# ASAMA 8 - MODEL IYILESTIRME

library(rpart)
library(rpart.plot)

# Tekrarlanabilirlik
set.seed(42)

# Daha buyuk bir agac olustur
model_cv <- rpart(
  Risk ~ .,
  data = egitim,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    xval = 10
  )
)

# Capraz dogrulama tablosu
printcp(model_cv)

# Capraz dogrulama grafigi
plotcp(model_cv)

# En dusuk capraz dogrulama hatasi
en_iyi_satir <- which.min(
  model_cv$cptable[, "xerror"]
)

en_iyi_cp <- model_cv$cptable[
  en_iyi_satir,
  "CP"
]

cat("Secilen CP:", en_iyi_cp, "\n")

# Agaci budama
model_iyilestirilmis <- prune(
  model_cv,
  cp = en_iyi_cp
)

# Yeni karar agacini goster
rpart.plot(
  model_iyilestirilmis,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Capraz Dogrulama ile Budanmis Karar Agaci"
)

# Iyilestirilmis model tahminleri
tahmin_yeni <- predict(
  model_iyilestirilmis,
  newdata = test,
  type = "class"
)

# Karmasiklik matrisi
sonuc_yeni <- table(
  Gercek = test$Risk,
  Tahmin = tahmin_yeni
)

print(sonuc_yeni)

# Performans metrikleri
TP_yeni <- sonuc_yeni["Kotu", "Kotu"]
TN_yeni <- sonuc_yeni["Iyi", "Iyi"]
FP_yeni <- sonuc_yeni["Iyi", "Kotu"]
FN_yeni <- sonuc_yeni["Kotu", "Iyi"]

accuracy_yeni <- (TP_yeni + TN_yeni) /
  sum(sonuc_yeni)

precision_yeni <- if (
  TP_yeni + FP_yeni == 0
) 0 else TP_yeni / (TP_yeni + FP_yeni)

recall_yeni <- TP_yeni /
  (TP_yeni + FN_yeni)

f1_yeni <- if (
  precision_yeni + recall_yeni == 0
) 0 else 2 * precision_yeni * recall_yeni /
  (precision_yeni + recall_yeni)

# Sonuclari karsilastir
karsilastirma <- data.frame(
  Metrik = c("Accuracy", "Precision", "Recall", "F1"),
  Ilk_Model = round(
    100 * c(accuracy, precision, recall, f1), 2
  ),
  Yeni_Model = round(
    100 * c(
      accuracy_yeni,
      precision_yeni,
      recall_yeni,
      f1_yeni
    ), 2
  )
)

print(karsilastirma)

# Model karsilastirmasini kaydet
write.csv(
  karsilastirma,
  "data/model_karsilastirma.csv",
  row.names = FALSE
)

# Iyilestirilmis modeli kaydet
saveRDS(
  model_iyilestirilmis,
  "data/karar_agaci_iyilestirilmis.rds"
)


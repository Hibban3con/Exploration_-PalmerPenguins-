# Objektif part untuk pembelajaran hari ini 
# Pada bagian ini, kita akan masuk pada pembahasan data wrangling & visualization
library(tidyverse)
library (palmerpenguins)
# Kita buat khusus untuk penguins tadi ke dalam variabel agar mudah dipanggil
penguin_data <- penguins
# Kita lihat isi data tersebut
glimpse(penguin_data)
penguin_data
# Tugas yang diberikan adalah analisis penguin Adelie tinggal di pulau biscoe/dream 
  # serta kolom yang diperlukan hanyalah species, island & body_mass_g
  # step 1: filter select
View(penguin_data)
penguin_adelieBD <- penguin_data %>%
  filter(species == "Adelie" & island %in% c("Biscoe", "Dream")) %>%
  select(species, island, body_mass_g)
rm(penguin_adelie)
  # step 2: mutate (untuk diolah menjadi Kg serta 
  # klasifikasi besar jika > 4Kg & kecil jika < 4Kg)
rm(adelieBD_categoric)
penguin_classified <- penguin_data |> mutate (body_mass_kg = body_mass_g/1000,
  size_category = ifelse(body_mass_kg >= 4, "Besar", "Kecil")) |> 
  select(species, island, body_mass_kg, size_category)
head(penguin_classified)
View(penguin_classified)
# Coba bandingkan dengan penguin_data serta amati vektornya
View(penguin_data)
# Terlihat jelas bagaimana klasifikasi berdasarkan berat badan sudah terpisah antara kecil dan besar
# step 3: group & summarise (melihat rata-rata BB & jumlah populasi berdasarkan spesies & sex)
penguin_classifiedII <- penguin_data |> drop_na(sex, body_mass_g) |> 
  mutate(body_mass_kg = body_mass_g/1000) |> 
  group_by(species, sex) |> summarise(rata_berat_kg = mean(body_mass_kg),
  jumlah_populasi = n(), .groups = "drop")
View(penguin_classifiedII)
# Dari purpose kita pada soal terakhir tadi, penguin yang paling bongsor adalah spesies:
# Gentoo - Male & Gentoo - Female
# Tugas terakhir yang akan kita kerjakan yakni arrange (dengan tujuan mengurutkan penguin baik dari paling berat/ringan)
# Ascending
ascending_penguin <- penguin_data |> 
  drop_na(body_mass_g) |> 
  select(species, island, sex, body_mass_g) |> 
  arrange (body_mass_g)
head(ascending_penguin) # Dapat dilihat penguin dari species, asal, serta JK apa yang paling ringan
# Descending
descending_penguin <- penguin_data |> 
  drop_na(body_mass_g) |> 
  select(species, island, sex, body_mass_g) |> 
  arrange(desc(body_mass_g))
head(descending_penguin) # dapat dilihat penguin apa yang paling berattt
## EKSPLORASI MANDIRI ##
# Question 1: Pada penguin_data tadi, ada berapa data sih yang kosong pada vektor "sex"? Tunjukkan dimana aja data NA itu!
is.na(penguin_data$sex)
sum(is.na(penguin_data$sex))
is.na(penguin_data$sex)
penguin_data |> filter(is.na(penguin_data$sex))
which(is.na(penguin_data$sex)) # Dapat diketahui di baris berapa saja penguin_data$sex ini kosong
# Evaluasi mendalam



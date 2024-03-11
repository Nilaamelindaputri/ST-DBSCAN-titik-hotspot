# Membaca file CSV
data_awal <- read.csv("D:/SKRIPSI/data/data skripsi nila/data skripsi.csv", header = TRUE)
View(data_awal)

# deskriptif
summary(data_awal)

# data preprocessing eksternal excel
data <- read.csv("D:/SKRIPSI/data/data skripsi nila/data akhir.csv", header = TRUE)
View(data)

subset_data <- data[, c(2, 3)]

# Convert the subsetted data to a matrix
data_matrix <- as.matrix(subset_data)
data_matrix

#nilai k=2
dbscan::kNNdistplot(data_matrix,k = 2)
abline(h=0.075, lty=2, col="red")
abline(h=0.085, lty=2, col="red")

#nilai k=3
dbscan::kNNdistplot(data_matrix,k = 3)
abline(h=0.11, lty=2, col="red")
abline(h=0.12, lty=2, col="red")

#nilai k=4
dbscan::kNNdistplot(data_matrix,k = 4)
abline(h=0.12, lty=2, col="red")
abline(h=0.14, lty=2, col="red")

#nilai k=5
dbscan::kNNdistplot(data_matrix,k = 7)
abline(h=0.12, lty=2, col="red")
abline(h=0.14, lty=2, col="red")

#menghitung jarak euclidean spasial dan temporal
data_spasial=dist(cbind(data$latitude, data$longitude))
data_temporal=dist(cbind(data$tanggal))
data_spasial=as.matrix(data_spasial)
data_temporal=as.matrix(data_temporal)
data_spasial=data.frame(data_spasial)
data_temporal=data.frame(data_temporal)

#ubah data.frame ke dalam bentuk excel
library(writexl)
write_xlsx(data_spasial,"Data Spasial.xlsx")
write_xlsx(data_temporal, "Data Temporal.xlsx")

# Algoritma st-dbscan
stdbscan <- function(data, Eps, Eps2, MinPts, seeds = TRUE, countmode = 1:nrow(data)) {
  data_spasial <- dist(cbind(data$latitude, data$longitude))
  data_temporal <- dist(data$tanggal)
  data_spasial <- as.matrix(data_spasial)
  data_temporal <- as.matrix(data_temporal)
  n <- nrow(data_spasial)
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)
  
  for (i in 1:n) {
    if (i %in% countmode)
      unclass <- (1:n)[cv < 1]
    if (cv[i] == 0) {
      reachables <- intersect(unclass[data_spasial[i, unclass] <= Eps],
                              unclass[data_temporal[i, unclass] <= Eps2])
      if (length(reachables) + classn[i] < MinPts)
        cv[i] <- -1
      else {
        cn <- cn + 1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        inclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()
          for (i2 in seq_along(ap)) {
            j <- ap[i2]
            jreachables <- intersect(unclass[data_spasial[j, unclass] < Eps],
                                     unclass[data_temporal[j, unclass] < Eps2])
            if (length(jreachables) + classn[j] >= MinPts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[reachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 0])
            }
            classn[jreachables] <- classn[jreachables] + 1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break
  }
  nn <- classn
  if (any(cv == -1))
    cv[cv == -1] <- 0
  out <- list(cluster = cv, Eps = Eps, MinPts = MinPts)
  class(out) <- "stdbscan"
  out
}

# Masukkan nilai parameter yang akan diuji
result <- stdbscan(data, Eps = 0.025, Eps2 = 7, MinPts = 2)

# Buat tabel hasil clustering
hasil <- data.frame(
  Longitude = data$longitude,
  Latitude = data$latitude,
  Tanggal = data$tanggal,
  Cluster = result$cluster
)

# Menghitung jumlah cluster
jumlah_cluster <- length(unique(hasil$Cluster))

# Menghitung jumlah noise (data yang tidak terklasifikasi ke dalam cluster)
jumlah_noise <- sum(hasil$Cluster == 0)

# Menampilkan hasil
cat("Jumlah cluster:", jumlah_cluster, "\n")
cat("Jumlah noise:", jumlah_noise, "\n")

# Tampilkan tabel hasil clustering
head(hasil,n=10)
tail(hasil,n=10)

#ubah data.frame ke dalam bentuk excel
library(writexl)
write_xlsx(hasil,"hasil MinPts=2; Eps1= 0.045; Eps2= 30.xlsx")

# Memuat paket cluster
library(cluster)

#pengukuran kualitas cluster
result=stdbscan(data,Eps=0.065,Eps2=7,MinPts=2)
hasil_tabel=data.frame(Longitude=data$longitude,
                       Latitude=data$latitude,
                       Tanggal=data$tanggal,cluster=result$cluster)
data_spasial=dist(cbind(data$latitude,data$longitude))
data_temporal=dist(cbind(data$tanggal))
data_spasial=as.matrix(data_spasial)
data_temporal=as.matrix(data_temporal)
SI_Spasial=silhouette(result$cluster,dist(data_spasial,"euclidean"))
mean(SI_Spasial[,3])
SI_temporal=silhouette(result$cluster,dist(data_temporal,"euclidean"))
mean(SI_temporal[,3])

#visualisasi semua cluster
library(rworldmap)
library(rworldxtra)

# Masukkan nilai parameter yang akan diuji
result <- stdbscan(data, Eps = 0.025, Eps2 = 7, MinPts = 2)

newmap<-getMap(resolution = "high")
time_class314<- unique(result$cluster)
class314_col<-rainbow(length(time_class314))

##Plot newMap
plot(newmap, xlim = c(100.110, 103.766 ), ylim = c(-1.050, 2.480), asp = 1, col="grey90")

##menampilkan noise
points(data$longitude[result$cluster==0], data$latitude[result$cluster==0],
       col='black', bg='black', xlab='', ylab='',pch=21)

##membuat titik per cluster
for(q in c(2:length(time_class314))){
  points(data$longitude[result$cluster==time_class314[q]],
         data$latitude[result$cluster==time_class314[q]], col='black', 
         bg=class314_col[q], pch=21, cex=1.5)
}
legend("bottomleft", legend = time_class314[2:484], cex = 0.6, pch = 20, pt.cex = 1.5, col = class314_col[42:484], ncol = 3)



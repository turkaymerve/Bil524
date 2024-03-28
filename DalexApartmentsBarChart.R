# DALEX paketinde bulunan Apartments veri setini kullanarak bölgelere göre evlerin ortalama m2 fiyatlarının görselleştirilmesi
#---------------------------------------------------------------------------

install.packages("DALEX") #veri setine erişmek için 
install.packages("ggplot") #ggplot görselleştirme araçlarını kullanabilek için
install.packages("dplyr") #pipe (%>%) operatörü kullanabilmek için

library(DALEX)
library(ggplot2)
library(dplyr)

DALEX::apartments

ggplot(DALEX::apartments) +
  geom_bar(aes(x=district)) # veri setindex eksenine gelecek veriyi tanımlamak için

#veri setindeki bir değişkenin ortalamasının alınması için
M2_AVG <- DALEX::apartments %>% group_by(district) %>% summarise(mean_m2 = mean(m2.price))

 

#verileri sıralamak için
ggplot(M2_AVG) + 
geom_col(aes(x = reorder(district, -mean_m2), y = mean_m2))
                                                                 
# x ve y eksenlerinin yerlerini değiştirmek için kullanım
ggplot(M2_AVG) + 
  geom_col(aes(y = reorder(district,+mean_m2), x = mean_m2)) +
  
  #eksen isimlerini düzenlemek, başlık ve kaynakça bilgisi için
ggplot(M2_AVG) + 
geom_col(aes(y = reorder(district,+mean_m2), x = mean_m2)) +
labs(x="Ortalama M2 Fiyatı", y="Bölgeler", title="Bölgelere Göre Evlerin M2 Fiyatı", caption="Veri Kaynağı:DALEX") 

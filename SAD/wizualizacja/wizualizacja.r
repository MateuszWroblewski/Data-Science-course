library(rgl)

mean.years.of.schooling <- read.csv("C:/Users/mateu/OneDrive/Desktop/Mateusz/stat/wizualizacja/mean-years-of-schooling.csv")
SYB_indicator._Life_expectancy_at_birth <- read.csv("C:/Users/mateu/OneDrive/Desktop/Mateusz/stat/wizualizacja/SYB_indicator _Life_expectancy_at_birth.csv")
UNdata_Export_20220612_124912229 <- read.csv("C:/Users/mateu/OneDrive/Desktop/Mateusz/stat/wizualizacja/UNdata_Export_20220612_124912229.csv")
API_SP.POP.TOTL_DS2_en_csv_v2_4218816 <- read.csv("C:/Users/mateu/OneDrive/Desktop/Mateusz/stat/wizualizacja/API_SP.POP.TOTL_DS2_en_csv_v2_4218816.csv")

szkola <- data.frame(mean.years.of.schooling)
dochod <- data.frame(UNdata_Export_20220612_124912229)
dlugzycia <- data.frame(SYB_indicator._Life_expectancy_at_birth)
ludzie <- data.frame(API_SP.POP.TOTL_DS2_en_csv_v2_4218816)

#dlugosc edukacji
rok <- list()
licznik <- 0
for (i in 2:(nrow(szkola))){
  if(szkola$Code[i]!=szkola$Code[i-1] )
  {
    licznik <- licznik+1
    rok[licznik] <- (i-1)
  }
}
rok[length(rok)+1] <- nrow(szkola)
rok <- as.numeric(unlist(rok))
szkola <- szkola[rok,]

#dlugosc zycia
v <- c(11,14,15,19,54)
dlugzycia<- dlugzycia[v]
dlugzycia <- na.omit(dlugzycia)
dlugzycia <- dlugzycia[which(dlugzycia$sexCode=='_T'),]
colnames(dlugzycia) <- c("parentName","refAreaDesc","Code","sexCode","value_latest_year")

#populacja
ludzie <- ludzie[c(2,65)]
colnames(ludzie) <- c("Code", "Populacja")

#dochod na osobe GNI PPP
v <- colnames(dochod)
v[1] <- "Code"
v[4] <- "dochod"
colnames(dochod) <- v

#laczymy dane
dane <- merge.data.frame(szkola, dlugzycia, by='Code')
dane <- merge(dane, dochod, by='Code')
v <- c(3,6,7,9,10,12)
dane <- dane[-v]
v <- colnames(dane)
v <- c("Code", "Kraj", "DlugoscEdukacji", "Region", "DlugoscZycia", "DochodNaOsobe")
colnames(dane) <- v
dane <- merge(ludzie, dane, by="Code")



get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

poczatek <- dane$Populacja
dane$wielkosc <- sqrt(poczatek)/8


plot3d( 
  x=dane$DlugoscEdukacji, y=dane$DlugoscZycia, z=dane$DochodNaOsobe, 
  type = 's', 
  col=get_colors(dane$Region),
  radius = dane$wielkosc,
  xlab="Average age of schooling", ylab="Life expectancy", zlab="Income per capitae")
legend3d("topright", legend = c(levels(as.factor(dane$Region))), pch = 16, col = get_colors(dane$Region), cex=0.5, inset=c(0))
identify3d(x=dane$DlugoscEdukacji, y=dane$DlugoscZycia, z=dane$DochodNaOsobe, labels = dane$Kraj)





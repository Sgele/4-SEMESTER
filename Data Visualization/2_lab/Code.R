data<-read.csv("C:/Users/junes/Desktop/matYearCountry.csv") #nuskaitome duomenis

Sys.setlocale("LC_ALL","Lithuanian") #lietuvuviskos raides

install.packages("ggplot2")
library(ggplot2)

# 1 UZDUOTIS
# LINIJINIS GRAFIKAS
library(ggplot2)

temperatura <- c(data$Lithuania)
metai <- c(data$year)
lietuva <- data.frame(metai, temperatura)

ggplot(lietuva, aes(x=metai, y=temperatura)) +
  geom_line(color="blue") +
  scale_x_continuous(name="Metai", breaks=seq(2000,2013,1)) +
  scale_y_continuous(name="Temperatura", breaks=seq(6,8.5,0.25)) +
  ggtitle("Vidutine temperatura Lietuvoje nuo 2000 iki 2013 metu") +
  theme_bw()


# STULPELINE DIAGRAMA
#pasiimame 2000m. temp. eilute
metai_2000 <- data[1,]
#issirenkame Siaures Europos salis
temperatura <- c(metai_2000$Norway, metai_2000$Sweden, metai_2000$Denmark, metai_2000$Finland, metai_2000$Iceland, metai_2000$United.Kingdom, metai_2000$Ireland, metai_2000$Lithuania, metai_2000$Latvia, metai_2000$Estonia)
#pavadiname issiimtus duomenis
Salys <- c('Norvergija', 'Svedija', 'Danija', 'Suomija', 'Islandija', 'Jungtine Karalyste', 'Airija', 'Lietuva', 'Latvija', 'Estija')
North <- data.frame(Salys,temperatura) #sudedame reikiamus duomenis i atskira lentele
ggplot(North, aes(x=Salys, y=temperatura)) + geom_bar(stat ="identity", fill = "lightblue") +
  ggtitle("Vidutine temperatura Siaures Europoje 2000 metais") + labs(y = 'Temperatura', x = 'Salys') + scale_y_continuous(breaks = seq(-17, 11,2))


# GRUPUOTA STULPELINE DIAGRAMA
#pasiimame 2000m. ir 2013m. temperaturu eilutes
metai_2000 <- data[1,]
metai_2013 <- data[14,]


#issirenkame zemynus (atsizvelgiant i metus)
temperatura <- c(metai_2000$Africa, metai_2000$Asia, metai_2000$Australia,
                 metai_2000$Europe, metai_2000$North.America, metai_2000$South.America,
                 metai_2013$Africa, metai_2013$Asia, metai_2013$Australia,
                 metai_2013$Europe, metai_2013$North.America, metai_2013$South.America)


metai <- c(rep('2000',6), rep('2013',6))
#metai kartojami po 6 kartus, nes yra 6 zemynu informacija apie temperatura.

#pavadiname isrinktus duomenis
zemynai <- c('Afrika','Azija','Australija','Europa','Siaures Amerika','Pietu Amerika')

#naudojamus duomenis sudedame i atskira lentele
lentele <- data.frame(temperatura, metai, zemynai)

ggplot(lentele, aes(x=metai, y=temperatura, group=zemynai, fill=zemynai)) +
  geom_bar(position='dodge', stat = 'identity') +
  ggtitle("Vidutine oro temperatura Pasaulio zemynuose 2000m. ir 2013m.") +
  labs(y = 'Temperatura (Celsijaus laipsniais)', x = 'Metai') +
  scale_fill_discrete(name = 'Zemynai')+
  scale_y_continuous(expand=c(0,0.4), breaks = seq(0, 27, 2))


# 2 UZDUOTIS
# LYGIAGRECIUJU KOORDINACIU
install.packages("GGally")
install.packages("viridis")
install.packages("hrbrthemes")
library(GGally)
library(viridis)
library(hrbrthemes)
install.packages("readxl")
library("readxl")
data <- read_excel('C:/Users/junes/Desktop/breast-cancer-wisconsin.xlsx') #nuskaitome duomenis
attach(data)
#stulpeliai, kuriuos naudosime
df <- data[c(2,3,4,5)]
#stulpeliu pavadinimai
colnames(df) <- c('auglys','spindulys', 'tekstura', 'perimetras')
# scale - globalminmax -> parodo tikrasias reiksmes grafike
# alphaLines -> nustato liniju permatomuma
# groupColumn -> pagal ka grupuosime duomenis
# M - piktybinis auglys (malignant)
# B - neiktybinis auglys (benign)
ggparcoord(data = df, columns = 2:4, groupColumn = 1, scale = "globalminmax",
           showPoints = T , alphaLines = 0.4) +
  theme(
    plot.title = element_text(size=13)
  ) +
  xlab(' ') + ylab('Skaicius') +
  scale_color_brewer(palette = "Set2")


# 3 UzDUOTIS
# ZVAIGZDZIU GRAFIKAS

library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(GGally)

data$Diagnosis <- factor(data$Diagnosis,
                             levels = c("M", "B"),
                             labels = c("piktybinis", "nepiktybinis"))

#issirenkami duomenu stulpeliai, kuriuos norime vaizduoti grafike
naudojami_duom <- data.frame(data[3], data[4], data[5], data[2])

#%in% naudojama pasirinkti tik tuos duomenis, kur auglys yra piktybinis
piktybinis1 <- filter(naudojami_duom, data$Diagnosis %in% c("piktybinis"))

simbolinis <- data.frame(piktybinis1[1], piktybinis1[2], piktybinis1[3])

legenda <- c('Spindulys', 'Tekstura', 'Perimetras')
stars(simbolinis, main = "Piktybinis auglys", key.labels = legenda, draw.segments = T,
      key.loc=c(40,20))


# 4 UZDUOTIS
data <- read.csv('C:/Users/junes/Desktop/breast-cancer-wisconsin.csv') #nuskaitome duomenis
attach(data)
install.packages("corrplot")
library(corrplot)

parametr <- data.frame(data$radius, data$texture, data$perimeter, data$area, data$compactness, data$concavity)
P = cor(parametr)
colnames(P) <- c("Spindulys", "Tekstura", "Perimetras", "Plotas", "Kompaktiskumas", "Idubimas")
rownames(P) <- c("Spindulys", "Tekstura", "Perimetras", "Plotas", "Kompaktiskumas", "Idubimas")
corrplot(P, method = 'number')
corrplot(P, method = 'circle')

library(ggplot2)
library(plotly)

Sys.setlocale("LC_ALL","Lithuanian") #lietuviskos raides

data <- read.csv('C:/Users/gelzi/Desktop/Duomenø mokslas/4 semestras/Duomenø vizualizavimas/2 lab/breast-cancer-wisconsin.csv') #nuskaitome duomenis
#stulpeliai, kuriuos naudosime
df <- data[c(2,3,4,5)]
#stulpeliø pavadinimai
colnames(df) <- c('Auglys','spindulys', 'tekstûra', 'perimetras')
df
#data - pasirinkti duomenys, x - x asyje pasirinktas stulpelis, y - y asyje pasirinktas stulpelis
#color - pagal kuri stulpeli parinkti spalvas (t.y. kiek yra skirtingu kintamuju, kad zinoti kiek bus skirtingu spalvu)
#colors - spalvu rinkinys
fig <- plot_ly(data = df, x = ~perimetras, y = ~tekstûra, color = ~Auglys, colors = "Set2") 
#title - grafiko pavadinimas, legend title - legendos pavadinimas, legend list - nustatome koordinates, kad legena butu viduryje
fig <- fig %>% layout(title = '<b> Krûties vëþio auglio perimetras ir tekstûra <b>',legend=list(title=list(text='<b> Auglys </b>'), x=100, y =0.5))
fig

fig2 <- plot_ly(data = df, x = ~perimetras, y = ~spindulys, color = ~Auglys, colors = "Set2") 
fig2 <- fig2 %>% layout(title = '<b> Krûties vëþio auglio perimetras ir spindulys <b>',legend=list(title=list(text='<b> Auglys </b>'), x=100, y =0.5))
fig2

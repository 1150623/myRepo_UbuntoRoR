


#Importacao ficheiro .csv com os dados do inquerito.

library(ggplot2)
library(plotly)
library(readr)
dados <- read_csv("./RESOURCES/Inquerito_ANADI.csv")
#View(dados)
attach(dados)
# ----------------------------------------------------

col_plataformas = 8
col_satisfacao = c(9:14) #sao 5 perguntas, coluna 9 a 14
nr_Total = nrow(dados) # = 71
M_dados3 = dados[c(1:nr_Total), c(col_plataformas, col_satisfacao)]
#View(M_dados3)

nr_desktop = 0
nr_mobile = 0
nr_tablet = 0

s_desktop_geral = c()
s_mobile_geral = c()
s_tablet_geral = c()


aux = strsplit(toString(M_dados3[45, 1]),";")


inquirido = 1
while(inquirido <= nr_Total){
  
  aux = strsplit(toString(M_dados3[inquirido, 1]),";")
  if(aux != "NA"){
    ismobile = 0
    isdesktop = 0
    istablet = 0
    tam = 1
    print("---------")
    print(aux)
    while(toString(aux[[1]][tam]) !=  "NA"){
      plataforma = aux[[1]][tam]
      print(plataforma)
      if(plataforma == "Desktop"){
        nr_desktop = nr_desktop + 1
        isdesktop = 1
      }
      if(plataforma == "Mobile"){
        nr_mobile = nr_mobile + 1
        ismobile = 1
      }
      if(plataforma == "Tablet"){
        nr_tablet = nr_tablet + 1
        istablet = 1
      }
      tam = tam + 1
    }
    
    if(isdesktop == 1){
      s_desktop_geral = c(s_desktop_geral, (M_dados3[inquirido, 2:6]))
    }
    if(ismobile == 1){
      s_mobile_geral = c(s_mobile_geral, (M_dados3[inquirido, 2:6]))
    }
    if(istablet == 1){
      s_tablet_geral = c(s_tablet_geral, (M_dados3[inquirido, 2:6]))
    }
  }
  inquirido = inquirido + 1
}


s_media_desktop = do.call(sum,s_desktop_geral) / length(s_desktop_geral)
s_media_mobile = do.call(sum,s_mobile_geral) / length(s_mobile_geral)
s_media_tablet = do.call(sum,s_tablet_geral) / length(s_tablet_geral)
s_media_geral = sum(s_media_desktop, s_media_mobile, s_media_tablet) / 3

# s_media_desktop # = 4.000000
# s_media_mobile  # = 4.152941
# s_media_tablet  # = 3.485714
# s_media_geral   # = 3.879552


Plataforma = c("Desktop", "Mobile", "Tablet")
val_media_geral = c(s_media_desktop, s_media_mobile, s_media_tablet)

data <- data.frame(Plataforma, val_media_geral)

graph_bar_media_satisfacao_plataforma <- plot_ly(data, x = ~Plataforma, y = ~val_media_geral, type = 'bar', name = 'Pontuacao') %>%
  layout(yaxis = list(title = 'Ponuacao (1-5)'), title = "Grafico das Medias de Pontuacoes de Satisfacao (p/ plataforma)")
graph_bar_media_satisfacao_plataforma


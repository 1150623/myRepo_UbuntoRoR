
#
#   Main Script Usabilidade
#

# GLOBALS
#Sexo:

MASCULINO = "Masculino"
FEMININO = "Feminino"
SEXO_NAO_DEFINIDO = "Prefiro nao dizer"
UTILIZA_APP = "Sim / Ja utilizei"
NAO_UTILIZA_APP = "Nao"


#Importacao ficheiro .csv com os dados do inquerito.

library(ggplot2)
library(plotly)
library(readr)
dados <- read_csv("./RESOURCES/Inquerito_ANADI.csv")
#View(dados)
attach(dados)
# ----------------------------------------------------

# Obtencao dados do Genero dos inqueridos:
nr_masculino = 0
nr_feminino = 0
nr_SexoNaoDefinido = 0

# Obtencao dados do numero de utilizadores da aplicacao:
nr_utilizadores = 0
nr_naoUtilizadores = 0
nr_Total = nrow(dados)
col1 = 2; # Genero 
col5 = 5; # Utilizacao
#71 e o numero total (nr_Total) de inqueridos
M_dados = dados[c(1:nr_Total), c(col1, col5)]
#View(M_dados)

# M_dados[ INQUIRIDO , COLUNA ] -> Se COLUNA for 1, retorna Genero, se for 2 retorna Utilizacao

nr_fem_utiliza = 0
nr_fem_nao_utiliza = 0
nr_masc_utiliza = 0
nr_masc_nao_utiliza = 0

# Pesquisa numero de utilizadores da aplicacao
for(user in Utiliza){
  if(user == UTILIZA_APP){
      nr_utilizadores = nr_utilizadores + 1
  }else{
      nr_naoUtilizadores = nr_naoUtilizadores + 1
  }
}
#nr_utilizadores = 53
#nr_naoUtilizadores = 18

# Pesquisa genero e utilizacao de cada
inquirido = 1
while(inquirido <= nr_Total){
  if(M_dados[inquirido, 1] == FEMININO){
    # Genero = FEMININO
    if(M_dados[inquirido, 2] == UTILIZA_APP){
      nr_fem_utiliza = nr_fem_utiliza + 1
    }else{
      nr_fem_nao_utiliza = nr_fem_nao_utiliza + 1
    }
  }
  if(M_dados[inquirido, 1] == MASCULINO){
    # Genero = MASCULINO
    if(M_dados[inquirido, 2] == UTILIZA_APP){
      nr_masc_utiliza = nr_masc_utiliza + 1
    }else{
      nr_masc_nao_utiliza = nr_masc_nao_utiliza + 1
    }
  }
  if(M_dados[inquirido, 1] == SEXO_NAO_DEFINIDO){
    # Utilizadores sem restricao de sexo
    if(M_dados[inquirido, 2] == UTILIZA_APP){
      nr_utilizadores = nr_utilizadores + 1
    }else{
      nr_naoUtilizadores = nr_naoUtilizadores + 1
    }
  }
  inquirido = inquirido + 1
}


nr_feminino = nr_fem_nao_utiliza + nr_fem_utiliza
nr_masculino = nr_masc_nao_utiliza + nr_masc_utiliza
nr_SexoNaoDefinido = nr_Total - nr_feminino - nr_masculino
# -----------------
#  Grafico Geral
# -----------------
# definir valores do pie chart
graph_sexos_slices = c(nr_utilizadores, nr_naoUtilizadores)
graph_sexos_labels = c(UTILIZA_APP, NAO_UTILIZA_APP)

labels_ <- list(
  labels = graph_sexos_labels, 
  type = "pie", 
  values = graph_sexos_slices
)
data <- list(labels_)
layout <- list(title = "Utilizadores Por Genero")
pie <- plot_ly()
pie <- add_trace(p, labels=labels_$labels, type=labels_$type, values=labels_$values)
pie <- layout(p, title=layout$title)
pie
# ---------------------
#  Grafico Por Generos
# ---------------------

# Para ordenar os resultados obtidos por generos precisamos de saber quantos de cada genero utilizao ou nao firefox.


Utilizacao <- c("Nao Utiliza", "Utiliza")
mascValues = c(nr_masc_nao_utiliza, nr_masc_utiliza)
femValues = c(nr_fem_nao_utiliza, nr_fem_utiliza)
data <- data.frame(Utilizacao, mascValues, femValues)

dualBar <- plot_ly(data, x = ~Utilizacao, y = ~mascValues, type = 'bar', name = 'MASCULINO') %>%
  add_trace(y = ~femValues, name = 'FEMININO') %>%
  layout(yaxis = list(title = 'Quantidade'), barmode = 'group')
dualBar



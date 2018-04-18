
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
IDADE_m10 = "Menor que 10"
IDADE_10_17 = "10-17"
IDADE_18_25 = "18-25"
IDADE_26_40 = "26-40"
IDADE_41_50 = "41-50"
IDADE_M50 = "Maior que 50"

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
col1 = 2 # Genero 
col3 = 3 # Idades
col5 = 5 # Utilizacao
#71 e o numero total (nr_Total) de inqueridos
M_dados = dados[c(1:nr_Total), c(col1, col3, col5)]
#View(M_dados)

# M_dados[ INQUIRIDO , COLUNA ] -> Se COLUNA for 1, retorna Genero, se for 2 retorna Utilizacao

#deprecated:
nr_fem_utiliza = 0
nr_fem_nao_utiliza = 0
nr_masc_utiliza = 0
nr_masc_nao_utiliza = 0

nr_masc_utiliza_m10 = 0
nr_masc_utiliza_10_17 = 0
nr_masc_utiliza_18_25 = 0
nr_masc_utiliza_26_40 = 0
nr_masc_utiliza_41_50 = 0
nr_masc_utiliza_M50 = 0
nr_masc_nao_utiliza_m10 = 0
nr_masc_nao_utiliza_10_17 = 0
nr_masc_nao_utiliza_18_25 = 0
nr_masc_nao_utiliza_26_40 = 0
nr_masc_nao_utiliza_41_50 = 0
nr_masc_nao_utiliza_M50 = 0

nr_fem_utiliza_m10 = 0
nr_fem_utiliza_10_17 = 0
nr_fem_utiliza_18_25 = 0
nr_fem_utiliza_26_40 = 0
nr_fem_utiliza_41_50 = 0
nr_fem_utiliza_M50 = 0 
nr_fem_nao_utiliza_m10 = 0
nr_fem_nao_utiliza_10_17 = 0
nr_fem_nao_utiliza_18_25 = 0
nr_fem_nao_utiliza_26_40 = 0
nr_fem_nao_utiliza_41_50 = 0
nr_fem_nao_utiliza_M50 = 0

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
    if(M_dados[inquirido, 3] == UTILIZA_APP){
      if(M_dados[inquirido, 2] == IDADE_m10){
        nr_fem_utiliza_m10 = nr_fem_utiliza_m10 + 1
      }else
        if(M_dados[inquirido, 2] == IDADE_10_17){
          nr_fem_utiliza_10_17 = nr_fem_utiliza_10_17 + 1
        }else
          if(M_dados[inquirido, 2] == IDADE_18_25){
            nr_fem_utiliza_18_25 = nr_fem_utiliza_18_25 + 1
          }else
            if(M_dados[inquirido, 2] == IDADE_26_40){
              nr_fem_utiliza_26_40 = nr_fem_utiliza_26_40 + 1
            }else
              if(M_dados[inquirido, 2] == IDADE_41_50){
                nr_fem_utiliza_41_50 = nr_fem_utiliza_41_50 + 1
              }else
                if(M_dados[inquirido, 2] == IDADE_M50){
                  nr_fem_utiliza_M50 = nr_fem_utiliza_M50 + 1
                }
    }else{
      if(M_dados[inquirido, 2] == IDADE_m10){
        nr_fem_nao_utiliza_m10 = nr_fem_nao_utiliza_m10 + 1
      }else
        if(M_dados[inquirido, 2] == IDADE_10_17){
          nr_fem_nao_utiliza_10_17 = nr_fem_nao_utiliza_10_17 + 1
        }else
          if(M_dados[inquirido, 2] == IDADE_18_25){
            nr_fem_nao_utiliza_18_25 = nr_fem_nao_utiliza_18_25 + 1
          }else
            if(M_dados[inquirido, 2] == IDADE_26_40){
              nr_fem_nao_utiliza_26_40 = nr_fem_nao_utiliza_26_40 + 1
            }else
              if(M_dados[inquirido, 2] == IDADE_41_50){
                nr_fem_nao_utiliza_41_50 = nr_fem_nao_utiliza_41_50 + 1
              }else
                if(M_dados[inquirido, 2] == IDADE_M50){
                  nr_fem_nao_utiliza_M50 = nr_fem_nao_utiliza_M50 + 1
                }
    }
  }
  if(M_dados[inquirido, 1] == MASCULINO){
    # Genero = MASCULINO
    if(M_dados[inquirido, 3] == UTILIZA_APP){
      if(M_dados[inquirido, 2] == IDADE_m10){
        nr_masc_utiliza_m10 = nr_masc_utiliza_m10 + 1
      }else
        if(M_dados[inquirido, 2] == IDADE_10_17){
          nr_masc_utiliza_10_17 = nr_masc_utiliza_10_17 + 1
        }else
          if(M_dados[inquirido, 2] == IDADE_18_25){
            nr_masc_utiliza_18_25 = nr_masc_utiliza_18_25 + 1
          }else
            if(M_dados[inquirido, 2] == IDADE_26_40){
              nr_masc_utiliza_26_40 = nr_masc_utiliza_26_40 + 1
            }else
              if(M_dados[inquirido, 2] == IDADE_41_50){
                nr_masc_utiliza_41_50 = nr_masc_utiliza_41_50 + 1
              }else
                if(M_dados[inquirido, 2] == IDADE_M50){
                  nr_masc_utiliza_M50 = nr_masc_utiliza_M50 + 1
                }
      
    }else{
      if(M_dados[inquirido, 2] == IDADE_m10){
        nr_masc_nao_utiliza_m10 = nr_masc_nao_utiliza_m10 + 1
      }else
        if(M_dados[inquirido, 2] == IDADE_10_17){
          nr_masc_nao_utiliza_10_17 = nr_masc_nao_utiliza_10_17 + 1
        }else
          if(M_dados[inquirido, 2] == IDADE_18_25){
            nr_masc_nao_utiliza_18_25 = nr_masc_nao_utiliza_18_25 + 1
          }else
            if(M_dados[inquirido, 2] == IDADE_26_40){
              nr_masc_nao_utiliza_26_40 = nr_masc_nao_utiliza_26_40 + 1
            }else
              if(M_dados[inquirido, 2] == IDADE_41_50){
                nr_masc_nao_utiliza_41_50 = nr_masc_nao_utiliza_41_50 + 1
              }else
                if(M_dados[inquirido, 2] == IDADE_M50){
                  nr_masc_nao_utiliza_M50 = nr_masc_nao_utiliza_M50 + 1
                }
    }
  }
  if(M_dados[inquirido, 1] == SEXO_NAO_DEFINIDO){
    # Utilizadores sem restricao de sexo
    if(M_dados[inquirido, 3] == UTILIZA_APP){
      nr_utilizadores = nr_utilizadores + 1
    }else{
      nr_naoUtilizadores = nr_naoUtilizadores + 1
    }
  }
  inquirido = inquirido + 1
}



nr_fem_utiliza_porIdades = c(nr_fem_utiliza_m10, nr_fem_utiliza_10_17, nr_fem_utiliza_18_25, nr_fem_utiliza_26_40, nr_fem_utiliza_41_50, nr_fem_utiliza_M50)
nr_fem_nao_utiliza_porIdades = c(nr_fem_nao_utiliza_m10, nr_fem_nao_utiliza_10_17, nr_fem_nao_utiliza_18_25, nr_fem_nao_utiliza_26_40, nr_fem_nao_utiliza_41_50, nr_fem_nao_utiliza_M50)
nr_masc_nao_utiliza_porIdades = c(nr_masc_nao_utiliza_m10, nr_masc_nao_utiliza_10_17, nr_masc_nao_utiliza_18_25, nr_masc_nao_utiliza_26_40, nr_masc_nao_utiliza_41_50, nr_masc_nao_utiliza_M50)
nr_masc_utiliza_porIdades = c(nr_masc_utiliza_m10, nr_masc_utiliza_10_17, nr_masc_utiliza_18_25, nr_masc_utiliza_26_40, nr_masc_utiliza_41_50, nr_masc_utiliza_M50)
nr_feminino = sum(nr_fem_nao_utiliza_porIdades, nr_fem_utiliza_porIdades)
nr_masculino = sum(nr_masc_nao_utiliza_porIdades, nr_masc_utiliza_porIdades)
nr_masc_nao_utiliza = sum(nr_masc_nao_utiliza_porIdades)
nr_masc_utiliza = sum(nr_masc_utiliza_porIdades)
nr_fem_nao_utiliza = sum(nr_fem_nao_utiliza_porIdades)
nr_fem_utiliza = sum(nr_fem_utiliza_porIdades)
nr_SexoNaoDefinido = nr_Total - nr_feminino - nr_masculino
# ---------------------------
#  Grafico Geral Utilizadores
# ---------------------------
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
pie <- add_trace(pie, labels=labels_$labels, type=labels_$type, values=labels_$values)
pie <- layout(pie, title=layout$title)
pie
# ---------------------------------
#  Grafico Utilizadores Por Generos
# ---------------------------------

# Para ordenar os resultados obtidos por generos precisamos de saber quantos de cada genero utilizao ou nao firefox.


Utilizacao <- c("Nao Utiliza", "Utiliza")
mascValues = c(nr_masc_nao_utiliza, nr_masc_utiliza)
femValues = c(nr_fem_nao_utiliza, nr_fem_utiliza)
data <- data.frame(Utilizacao, mascValues, femValues)

dualBar <- plot_ly(data, x = ~Utilizacao, y = ~mascValues, type = 'bar', name = 'MASCULINO') %>%
  add_trace(y = ~femValues, name = 'FEMININO') %>%
  layout(yaxis = list(title = 'Quantidade'), barmode = 'group')
dualBar

# --------------------------------
#  Grafico Utilizadores Por Idades
# --------------------------------



Idades <- c("> 10",IDADE_10_17, IDADE_18_25, IDADE_26_40, IDADE_41_50, "50 < ")
Values_masc = nr_masc_utiliza_porIdades
Values_fem = nr_fem_utiliza_porIdades

data <- data.frame(Idades, Values_masc, Values_fem)
data
multi <- plot_ly(data, x = Idades, y = Values_masc, type = 'bar', name = 'MASCULINO') %>%
  add_trace(y = Values_fem, name = 'FEMININO') %>%
  layout(yaxis = list(title = 'Quantidade Utilizadores'), xaxis = list(Idades),barmode = 'group')
multi

# Para não utilizadores: (desnecessario?)

Values_masc2 = nr_masc_nao_utiliza_porIdades
Values_fem2 = nr_fem_nao_utiliza_porIdades

data2 <- data.frame(Idades, Values_masc2, Values_fem2)
data2
multi2 <- plot_ly(data, x = Idades, y = Values_masc2, type = 'bar', name = 'MASCULINO') %>%
  add_trace(y = Values_fem2, name = 'FEMININO') %>%
  layout(yaxis = list(title = 'Quantidade Não Utilizadores'), xaxis = list(Idades),barmode = 'group')
multi2
















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

respostasMasculino =c()
respostasFeminino = c()
respostasSexoNaoDefinido = 0
pass=0

# Obtencao dados do numero de utilizadores da aplicacao:
respostas = c()
respostasGeral = c()
nr_Total = nrow(dados)
col1 = 2; # Genero 
col21 = 21; # Razoes para não utilização
#71 e o numero total (nr_Total) de inqueridos
M_dados = dados[c(1:nr_Total), c(col1, col21)]
#View(M_dados)

# Verificacao de respostas
inquirido = 1
while(inquirido <= nr_Total){
 
  if( is.na(M_dados[inquirido, 2]) ){}else{
    answer=strsplit(toString(M_dados[inquirido, 2]),";")
    count = 1
    
    while(count <= length(answer[[1]])){
      if(!is.element(answer[[1]][count],respostas)){
        respostas=append(respostas,answer[[1]][count],after =length(respostas))
      }
      count = count + 1
    }
  }
  inquirido = inquirido + 1
}
# Contagem de respostas
# View(M_dados)
current = 1
while(current <= length(respostas)){
  tempM=0
  tempF=0
  tempND=0
  temp=0
  inquirido = 1
  while(inquirido <= nr_Total){
    
    if( !is.na(M_dados[inquirido, 2]) ){
      answer=strsplit(toString(M_dados[inquirido, 2]),';')
      if(is.element(respostas[current],answer[[1]])){
        if(M_dados[inquirido, 1]==MASCULINO){
          tempM=tempM+1
        }
        if(M_dados[inquirido, 1]==FEMININO){
          tempF=tempF+1
        }
        if(M_dados[inquirido, 1]==SEXO_NAO_DEFINIDO){
          tempND=tempND+1
        }
        temp= temp+1
      }
    }
    inquirido = inquirido + 1
  }
  current = current + 1
  respostasMasculino=c(respostasMasculino,tempM)
  respostasFeminino=c(respostasFeminino,tempF)
  respostasSexoNaoDefinido=c(respostasSexoNaoDefinido,tempND)
  respostasGeral=c(respostasGeral,temp)
}
# definir valores do pie chart

graph_sexos_slices = c(respostasGeral)
graph_sexos_labels = c(respostas)

labels_ <- list(
  labels = graph_sexos_labels, 
  type = "pie", 
  values = graph_sexos_slices
)
data <- list(labels_)
layout <- list(title = "Motivos de nao utilizacao")
pie <- plot_ly()
pie <- add_trace(pie, labels=labels_$labels, type=labels_$type, values=labels_$values)
pie <- layout(pie, title=layout$title,showlegend = TRUE)
pie

# grafo de respostas por genero

Values_masc = respostasMasculino
Values_fem = respostasFeminino

data <- data.frame(respostas, Values_masc, Values_fem)
data
multi <- plot_ly(data, x = respostas, y = Values_masc, type = 'bar', name = 'MASCULINO') %>%
  add_trace(y = Values_fem, name = 'FEMININO') %>%
  layout(yaxis = list(title = 'Motivos de não utilização por genero'), xaxis = list(respostas),barmode = 'group')
multi

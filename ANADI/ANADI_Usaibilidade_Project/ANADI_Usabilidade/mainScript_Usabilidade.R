
#
#   Main Script Usabilidade
#

# GLOBALS
#Sexo:
  MASCULINO = "Masculino"
  FEMININO = "Feminino"
  SEXO_NAO_DEFINIDO = "Prefiro nao dizer"

#Importacao ficheiro .csv com os dados do inquerito.


library(readr)
dados <- read_csv("./RESOURCES/Inquerito ANADI.csv")
View(dados)
attach(dados)

# Obtencao dados do Genero dos inqueridos:
nr_masculino = 0
nr_feminino = 0
nr_SexoNaoDefinido = 0
for(type in Sexo){
  if(type == MASCULINO){
    nr_masculino = nr_masculino + 1
  }
  if(type == FEMININO){
    nr_feminino = nr_feminino + 1
  }
  if(type == SEXO_NAO_DEFINIDO){
    nr_SexoNaoDefinido = nr_SexoNaoDefinido +1
  }
}
# nr_masculino = 38
# nr_feminino = 31
# nr_SexoNaoDefinido = 1

# definir valores do pie chart
graph_sexos_slices = c(nr_masculino, nr_feminino, nr_SexoNaoDefinido)
graph_sexos_labels = c(MASCULINO, FEMININO, SEXO_NAO_DEFINIDO)
#extra adicionar percentagens as labels
percentagens = round(graph_sexos_slices / sum(graph_sexos_slices) * 100)
graph_sexos_labels = paste(graph_sexos_labels, percentagens)
graph_sexos_labels = paste(graph_sexos_labels, "%", sep="")
#desenhar grafico (pie chart)
pie(graph_sexos_slices, labels=graph_sexos_labels, col=rainbow(length(graph_sexos_labels)), main = "Genero dos Inqueridos")
  

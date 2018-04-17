

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


#Importação ficheiro .csv com os dados do inquerito.


  library(readr)
  dados <- read_csv("./RESOURCES/Inquérito ANADI.csv")
  #View(dados)
  attach(dados)
# ----------------------------------------------------
  
# Obtencao dados do Género dos inqueridos:
nr_masculino = 0
nr_feminino = 0
nr_SexoNaoDefinido = 0

# Obtencao dados do número de utilizadores da aplicação:
nr_utilizadores = 0
nr_naoUtilizadores = 0
nr_Total = nrow(dados)
col1 = 2; # Genero 
col5 = 5; # Utilizacao
#71 é o número total (nr_Total) de inqueridos
M_dados = dados[c(1:nr_Total), c(col1, col5)]
View(M_dados)

# M_dados[ INQUIRIDO , COLUNA ] -> Se COLUNA for 1, retorna Genero, se for 2 retorna Utilizacao

nr_fem_utiliza = 0
nr_fem_nao_utiliza = 0
nr_masc_utiliza = 0
nr_masc_nao_utiliza = 0

inquirido = 1
while(inquirido <= nr_Total){
  if(M_dados[inquirido, 1] == FEMININO){
    # Genero = FEMININO
    if(M_dados[inquirido, 2] == UTILIZA_APP){
      nr_fem_utiliza = nr_fem_utiliza + 1
    }else{
      nr_fem_nao_utiliza = nr_fem_nao_utiliza + 1
    }
  }else if(M_dados[inquirido, 1] == MASCULINO){
    # Genero = MASCULINO
    if(M_dados[inquirido, 2] == UTILIZA_APP){
      nr_masc_utiliza = nr_masc_utiliza + 1
    }else{
      nr_masc_nao_utiliza = nr_masc_nao_utiliza + 1
    }
  }
  inquirido = inquirido + 1
}

nr_feminino = nr_fem_nao_utiliza + nr_fem_utiliza
nr_masculino = nr_masc_nao_utiliza + nr_masc_utiliza
nr_SexoNaoDefinido = nr_Total - nr_feminino - nr_masculino
# -----------------
#  Gráfico Geral
# -----------------
    # definir valores do pie chart
    graph_sexos_slices = c(nr_utilizadores, nr_naoUtilizadores)
    graph_sexos_labels = c(UTILIZA_APP, NAO_UTILIZA_APP)
    #extra adicionar percentagens às labels
    percentagens = round(graph_sexos_slices / sum(graph_sexos_slices) * 100)
    graph_sexos_labels = paste(graph_sexos_labels," (", percentagens)
    graph_sexos_labels = paste(graph_sexos_labels, "%)", sep="")
    #desenhar gráfico (pie chart)
    pie(graph_sexos_slices, labels=graph_sexos_labels, col=rainbow(length(graph_sexos_labels)), main = "Ja Utilizou Firefox?")


# ---------------------
#  Gráfico Por Generos
# ---------------------
    
    # Para ordenar os resultados obtidos por generos precisamos de saber quantos de cada genero utilizão ou nao firefox.

    library(plotly)
    
    mascValues = c(nr_masc_utiliza, nr_masc_nao_utiliza)
    femValues = c(nr_fem_utiliza, nr_fem_nao_utiliza)
    
    
    Animals <- c("giraffes", "orangutans", "monkeys")
    SF_Zoo <- c(20, 14, 23)
    LA_Zoo <- c(12, 18, 29)
    data <- data.frame(Animals, SF_Zoo, LA_Zoo)
    
    p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
      add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    # Create a shareable link to your chart
    # Set up API credentials: https://plot.ly/r/getting-started
    chart_link = api_create(p, filename="bar-grouped")
    chart_link
    
    
    
    
    


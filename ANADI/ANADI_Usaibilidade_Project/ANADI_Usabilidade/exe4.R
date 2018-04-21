
#
#   Main Script Usabilidade
#

# GLOBALS

#Importacao ficheiro .csv com os dados do inquerito.

library(ggplot2)
library(plotly)
library(readr)
dados <- read_csv("./RESOURCES/Inquerito_ANADI.csv")
#View(dados)
attach(dados)
# ----------------------------------------------------

# Obtencao dados do Genero dos inqueridos:
cotacoes = c("1-Muito Complicado","2-Complicado","3-Neutro","4-Intuitivo","5-Muito Intuitivo")
uso= c("Menos de 1 ano","1 - 2 anos","3 - 4 anos","Mais de 4 anos")
idade=c("Menor que 10","10-17","18-25","26-40","41-50","Maior que 50")
idade1=c()
idade2=c()
idade3=c()
idade4=c()
idade5=c()
idade6=c()
uso_menos_1=c()
uso_1_2=c()
uso_3_4=c()
uso_Mais_4=c()
respostasMI = c()
respostasI = c()
respostasN = c()
respostasS = c()
respostasMS = c()
desktop=c()
tablet=c()
mobile=c()
pass=0

# Obtencao dados do numero de utilizadores da aplicacao:
questoes = c("Primeira utilizacao","Facilidade de Instalacao","Pesquisa de extensao","Resolucao de Problemas")
nr_Total = nrow(dados)

colIdade = 3; # Idade
colPlataforma = 8; # Plataforma
colIntuitivo1 = 15; # Intuitivo 1
colIntuitivo2 = 16; # Intuitivo 2
colIntuitivo3 = 17; # Intuitivo 3
colIntuitivo4 = 18; # Intuitivo 4
colAnosUtilizacao = 20; # Anos de utilizacao

#71 e o numero total (nr_Total) de inqueridos
M_dados = dados[c(1:nr_Total), c(colIdade,colPlataforma, colIntuitivo1,colIntuitivo2,colIntuitivo3,colIntuitivo4,colAnosUtilizacao)]
#View(M_dados)

# respostas por grau de satisfacao

pass=3
while(pass < 7){
  count =1
  while (count<6) {
    cot=0
    cotDesktop=0
    cotMobile=0
    cotTablet=0
    inquirido = 1
    
    while(inquirido <= nr_Total){
      
      if( is.na(M_dados[inquirido, 3]) ){}else{
        
        if(M_dados[inquirido, pass]==count){
          cot=cot+1
        }
      }
      inquirido = inquirido + 1
    }
    if(count==1){
      respostasMI= c(respostasMI,cot)
    }
    if(count==2){
      respostasI= c(respostasI,cot)
    }
    if(count==3){
      respostasN= c(respostasN,cot)
    }
    if(count==4){
      respostasS= c(respostasS,cot)
    }
    if(count==5){
      respostasMS= c(respostasMS,cot)
    }
    count = count + 1
  }
  
  pass = pass + 1
}

count =1
while (count<6) {
  cot=0
  cotDesktop=0
  cotMobile=0
  cotTablet=0
  uso1=0
  uso2=0
  uso3=0
  uso4=0
  idade_1=0
  idade_2=0
  idade_3=0
  idade_4=0
  idade_5=0
  idade_6=0
  
  pass=3
  while(pass < 7){
    inquirido = 1
    while(inquirido <= nr_Total){
      if( is.na(M_dados[inquirido, 3]) ){}else{
        
        if(M_dados[inquirido,pass]==count){
          
          if(M_dados[inquirido, 7]==uso[1]){
            uso1=uso1+1
          }
          if(M_dados[inquirido, 7]==uso[2]){
            uso2=uso2+1
          }
          if(M_dados[inquirido, 7]==uso[3]){
            uso3=uso3+1
          }
          if(M_dados[inquirido, 7]==uso[4]){
            uso4=uso4+1
          }
          
          if(M_dados[inquirido, 1]==idade[1]){
            idade_1=idade_1+1
          }
          if(M_dados[inquirido, 1]==idade[2]){
            idade_2=idade_2+1
          }
          if(M_dados[inquirido, 1]==idade[3]){
            idade_3=idade_3+1
          }
          if(M_dados[inquirido, 1]==idade[4]){
            idade_4=idade_4+1
          }
          if(M_dados[inquirido, 1]==idade[5]){
            idade_5=idade_5+1
          }
          if(M_dados[inquirido, 1]==idade[6]){
            idade_6=idade_6+1
          }
          
          answer=strsplit(toString(M_dados[inquirido, 2]),';')
          platformCount=1
          while (platformCount<=length(answer[[1]])) {
            print(length(answer[[1]]))
            if(answer[[1]][platformCount]=="Desktop"){
              cotDesktop=cotDesktop+1
            }
            if(answer[[1]][platformCount]=="Tablet"){
              cotTablet=cotTablet+1
            }
            if(answer[[1]][platformCount]=="Mobile"){
              cotMobile=cotMobile+1
            }
            platformCount=platformCount+1
          }
        }
      }
      inquirido=inquirido+1
    }
    pass=pass+1
  }
  idade1=c(idade1,idade_1)
  idade2=c(idade2,idade_2)
  idade3=c(idade3,idade_3)
  idade4=c(idade4,idade_4)
  idade5=c(idade5,idade_5)
  idade6=c(idade6,idade_6)
  uso_menos_1=c(uso_menos_1,uso1)
  uso_1_2=c(uso_1_2,uso2)
  uso_3_4=c(uso_3_4,uso3)
  uso_Mais_4=c(uso_Mais_4,uso4)
  desktop=c(desktop,cotDesktop)
  tablet=c(tablet,cotTablet)
  mobile=c(mobile,cotMobile)
  count=count+1
}

# grafo de respostas geral

data <- data.frame(questoes, respostasMI, respostasI, respostasN, respostasS,respostasMS)
data
multi <- plot_ly(data, x = questoes, y = respostasMI, type = 'bar', name = cotacoes[1]) %>%
  add_trace(y = respostasI, name = cotacoes[2]) %>%
  add_trace(y = respostasN, name = cotacoes[3]) %>%
  add_trace(y = respostasS, name = cotacoes[4]) %>%
  add_trace(y = respostasMS, name = cotacoes[5]) %>%
  layout(yaxis = list(title = 'Niveis de Simplicidade de cada feature'), xaxis = list(questoes),barmode = 'group')
multi

# grafo de respostas por plataforma

dataPlatform <- data.frame(cotacoes, tablet,desktop,mobile)
dataPlatform
multiPlatform <- plot_ly(dataPlatform, x = cotacoes, y = desktop, type = 'bar', name = "Desktop") %>%
  add_trace(y = tablet, name = "Tablet") %>%
  add_trace(y = mobile, name = "Mobile") %>%
  layout(yaxis = list(title = 'Niveis de Simplicidade por Plataforma'), xaxis = list(cotacoes),barmode = 'group')
multiPlatform

# grafo de respostas por anos de uso

dataUso <- data.frame(cotacoes, uso_menos_1,uso_1_2,uso_3_4,uso_Mais_4)
dataUso
multiUso <- plot_ly(dataUso, x = cotacoes, y = uso_menos_1, type = 'bar', name = uso[1]) %>%
  add_trace(y = uso_1_2, name = uso[2]) %>%
  add_trace(y = uso_3_4, name = uso[3]) %>%
  add_trace(y = uso_Mais_4, name = uso[4]) %>%
  layout(yaxis = list(title = 'Niveis de Simplicidade por Anos de Uso'), xaxis = list(cotacoes),barmode = 'group')
multiUso

# grafo de respostas por idade

dataIdade <- data.frame(cotacoes, idade1,idade2,idade3,idade4,idade5,idade6)
dataIdade
multiIdade <- plot_ly(dataIdade, x = cotacoes, y = idade1, type = 'bar', name = idade[1]) %>%
  add_trace(y = idade2, name = idade[2]) %>%
  add_trace(y = idade3, name = idade[3]) %>%
  add_trace(y = idade4, name = idade[4]) %>%
  add_trace(y = idade5, name = idade[5]) %>%
  add_trace(y = idade6, name = idade[6]) %>%
  layout(yaxis = list(title = 'Niveis de Simplicidade por Idade'), xaxis = list(cotacoes),barmode = 'group')
multiIdade
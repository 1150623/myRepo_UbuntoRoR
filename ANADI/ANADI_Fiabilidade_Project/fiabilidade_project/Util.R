
# PACKAGES IMPORTS

library(readr)


#VARIAVEIS GLOBAIS

SERVIDORES = c("vsrv8", "vsrv10", "vsrv11", "vsrv16","vsrv17")
PROTOCOLOS = c("PPTP", "SSTP", "SOFTETHER", "OPENVPN_L2", "OPENVPN_L3")

COL_SERVIDORES = 1
COL_PROTOCOLOS = 2
COL_DATA = 3
COL_HORA_INICIO = 4
COL_HORA_FIM = 5
COL_DURACAO = 6

DURACAO_FALHA = 0
MAX_SIMULTANEO = 5000

# UTIL FUNCTIONS
isValidConnection <- function(duracao){
  # Verifica se uma conneccao e valida (se duracao <= 1, e uma falha), ou seja, duracao > 1
  return(duracao > 1)
}

calcMTBF <-function(vetorInicioFalhas){
  #Calculo MTBF (Mean Time Between Failure)
  for(var in vetorInicioFalhas[]){
    
  }
}

subtractDate <- function (date1, date2){
  # Retorna resultado em minutos
  return(as.numeric(difftime(date1,date2)) * 60)
}

incrementDate <- function (date, minuts){
  # Adiciona 'minuts' minutos a 'date'
  return(date + minuts * 60)
}

calcTaxaDeFalha <- function(dados, inicio, fim, nr_sessoes){
  #   Para calcular a taxa de falhas num período deve considerar a razão entre 
  # o número sessões classificadas como falha e o total de sessões nesse período.
  
  # [NOT-IMPLEMENTED]
}

getYear <- function(date){
  #[YYYY]-MM-DD HH:MM:SS
  return(substring(toString(date), 1, 4))
}
getMonth <- function(date){
  #YYYY-[MM]-DD HH:MM:SS
  return(substring(toString(date), 6, 7))
}
getDay <- function(date){
  #YYYY-MM-[DD] HH:MM:SS
  return(substring(toString(date), 9, 10))
}
getHour <- function(date){
  #YYYY-MM-DD [HH]:MM:SS
  return(substring(toString(date), 12, 13))
}
getMinuts <- function(date){
  #YYYY-MM-DD HH:[MM]:SS
  return(substring(toString(date), 15, 16))
}
getSeconds <- function(date){ # Not needed - nao temos segundos
  #YYYY-MM-DD HH:MM:[SS]
  return(substring(toString(date), 18, 19))
}

joinDateTime <- function(date,time){
  # Combines a 'date' and a 'time' string into a date+time type
  return(strptime(paste(date, time, sep= " "), format = "%Y-%m-%d %H:%M", tz = "GMT"))
}

compareDate <-function(dateA, dateB){
  #print(paste(dateA))
  #print(paste(dateB))
  diff = subtractDate(dateA, dateB)
  #print(diff)
  returnValue = 0 # DateA = DateB
  if(diff < 0){
    #DateB > DateA
    returnValue = -1
  }else if(diff > 0){
    #DateA > DateB
    returnValue = 1
  }
  
  return(returnValue)
}


#MOVE TO MAIN:
nr_sessoes_simultaneo <- function(input, lastLine, data){
  max = lastLine + MAX_SIMULTANEO
  if(max>qtd_amostra){
    max = qtd_amostra
  }
  values = input[lastLine:max]
  
  
  #(0 - Se receber um parametro válido como 'lastLine', iniciar a pesquisa nessa linha)
  # 1 - Encontrar a primeira data (dia+HORA_FIM) que seja menor que a recebida por parametro.
  #     Assim temos a certeza que não queremos as sessões anteriores por acabaram antes da hora pretendida.
  # 2 - Da primeira data maior para a frente, enquanto as data+HORA_INICIO forem menores que a data recebida por parametro (e duracao != 0),
  #     incrementamos um contador (que representa o numero de sessões em simultaneo)
  # 3 - Quando acabar a pesquisa, retornar esse número e o número da linha do ficheiro da primeira data+HORA_INICIO encontrada (com duracao != 0, senão retorna a seguinte)
  
} 

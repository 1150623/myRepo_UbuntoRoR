
# PACKAGES IMPORTS

library(readr)
library(lubridate)

PARTNAME = toString("part_aj")


INICIAL_FILE = toString("/Resources/VPN_SESSIONS_PARTS/")
INICIAL_FILE = paste(INICIAL_FILE, PARTNAME, sep="")
INICIAL_FILE = paste(INICIAL_FILE, "txt", sep=".")

MODIFIED_FILE = toString("./Resources/VPN_SESSIONS_PARTS/")
MODIFIED_FILE = paste(MODIFIED_FILE, PARTNAME, sep="")
MODIFIED_FILE = paste(MODIFIED_FILE, "_modified_file.txt", sep="")


Data_Hora_End_FILE = toString("./Resources/VPN_SESSIONS_PARTS/")
Data_Hora_End_FILE = paste(Data_Hora_End_FILE, PARTNAME, sep="")
Data_Hora_End_FILE = paste(Data_Hora_End_FILE, "_data_hora_end.txt", sep="")


Data_Hora_Ini_FILE = toString("./Resources/VPN_SESSIONS_PARTS/")
Data_Hora_Ini_FILE = paste(Data_Hora_Ini_FILE, PARTNAME, sep="")
Data_Hora_Ini_FILE = paste(Data_Hora_Ini_FILE, "_data_hora_ini.txt", sep="")


Prot_FILE <- toString("./Resources/VPN_SESSIONS_PARTS/")
Prot_FILE = paste(Prot_FILE, PARTNAME, sep="")
Prot_FILE = paste(Prot_FILE, "_prot.txt", sep="")


Serv_FILE <- toString("./Resources/VPN_SESSIONS_PARTS/")
Serv_FILE = paste(Serv_FILE, PARTNAME, sep="")
Serv_FILE = paste(Serv_FILE, "_serv.txt", sep="")


Duracao_FILE <- toString("./Resources/VPN_SESSIONS_PARTS/")
Duracao_FILE = paste(Duracao_FILE, PARTNAME, sep="")
Duracao_FILE = paste(Duracao_FILE, "_duracao.txt", sep="")

OUTPUT_FILE_SESSOES_SIMULTANEAS = toString("./Resources/VPN_SESSIONS_PARTS/")
OUTPUT_FILE_SESSOES_SIMULTANEAS = paste(OUTPUT_FILE_SESSOES_SIMULTANEAS, PARTNAME, sep = "")
OUTPUT_FILE_SESSOES_SIMULTANEAS = paste(OUTPUT_FILE_SESSOES_SIMULTANEAS, "_sess_simultaneas.txt", sep = "")


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
MAX_SIMULTANEO = 260

# UTIL FUNCTIONS

isValidConnection <- function(duracao){
  # Verifica se uma conneccao e valida (se duracao <= 1, e uma falha), ou seja, duracao > 1
  return(duracao > 1)
}

calcMTBF <-function(vetorInicioFalhas){
  #Calculo MTBF (Mean Time Between Failure)
  for(var in vetorInicioFalhas){
    #TODO
  }
}

subtractDate <- function (date1, date2){
  # Retorna resultado em minutos
  #return(as.numeric(difftime(date1,date2)) * 60)
  
  diff=toDate(toString(date1))-toDate(toString(date2))
  min1 = as.numeric(getHour(toDate(toString(date1)))) * 60 + as.numeric(getMinuts(toDate(toString(date1))))
  min2 = as.numeric(getHour(toDate(toString(date2)))) * 60 + as.numeric(getMinuts(toDate(toString(date2))))
  
  if(is.na(min1) || is.na(min2)){
    timediff = 0
  }else{
    timediff = min1-min2
  }
  
  diffNum=((as.numeric(diff)*24*60)+ timediff)
  
  return(diffNum)
}

incrementDate <- function (date, minut){
  # Adiciona 'minuts' minutos a 'date'
  result = toDate(toString(date)) + minutes(minut)
  return(result)
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

toDate <- function(date){
  # Combines a 'date' and a 'time' string into a date+time type
  return(as.POSIXct(date))
}


compareDate <-function(arg1, arg2){
  #print("---COMPARE----")
  #print(arg1)
  #print(arg2)
  sub = as.numeric(subtractDate(arg1, arg2))
  #print(sub)
  returnValue = 0 # DateA == DateB, retorna 0
  if(is.na(sub)){
    #print("SUB IS 'NA'")
    #print("arg1")
    #print(arg1)
    #print("arg2")
    #print(arg2)
    #print("sub")
    #print(sub)
  }
  if(sub < 0){
    returnValue = -1
  }else 
    if(sub > 0){
      returnValue = 1
    }
  return(returnValue)
}

concatTwoDates_toString <- function(date1, date2){
  
  y1 = toString(getYear(toDate(date1)))
  m1 = toString(getMonth(toDate(date1)))
  d1 = toString(getDay(toDate(date1)))
  h1 = toString(getHour(toDate(date1)))
  mn1 = toString(getMinuts(toDate(date1)))
  s1 = toString(getSeconds(toDate(date1)))
  
  y2 = toString(getYear(toDate(date2)))
  m2 = toString(getMonth(toDate(date2)))
  d2 = toString(getDay(toDate(date2)))
  h2 = toString(getHour(toDate(date2)))
  mn2 = toString(getMinuts(toDate(date2)))
  s2 = toString(getSeconds(toDate(date2)))
  
  string = paste("./Resources/VPN_SESSIONS_PARTS/", PARTNAME, sep="")
  string = paste(string, "_De_", y1, sep="")
  string = paste(string, m1, sep="-")
  string = paste(string, d1, sep="-")
  string = paste(string, h1, sep="_")
  string = paste(string, mn1, sep=":")
  string = paste(string, s1, sep=":")
  string = paste(string, "__Ate_", sep="")
  string = paste(string, y2, sep="")
  string = paste(string, m2, sep="-")
  string = paste(string, d2, sep="-")
  string = paste(string, h2, sep="_")
  string = paste(string, mn2, sep=":")
  string = paste(string, s2, sep=":")
  string = paste(string, "txt", sep=".")
  return(string)
}

nr_sessoes_simultaneo <- function(input, lastLine, data){
  startAt = lastLine
  #(0 - Se receber um parametro válido como 'lastLine', iniciar a pesquisa nessa linha)
  #print("PASSO 0...")
  max = lastLine + MAX_SIMULTANEO
  if(max>nrow(input)){
    max = nrow(input)
  }
  data = toDate(data)
  values1 = input[c(lastLine:max), c(3:5)]
  
  # 1 - Encontrar a primeira data (dia+HORA_FIM) que seja maior que a recebida por parametro.
  #     Assim temos a certeza que não queremos as sessões anteriores por acabarem antes da hora pretendida.
  count = 1
  end = nrow(values1)
  while(count <= end){
    
    if(is.na(values1[[count, 2]])){
      #print("IS NA")
      if(!is.na(values1[[count, 1]])){
        #print("INCREMENTING STARTING ON INICIAL DATE")
        aux = incrementDate(values1[[count, 1]], values1[[count, 3]])
      }
    }else{
      aux = toDate(toString(values1[[count, 2]]))
    }
    
    if(compareDate(data, aux) == -1 || compareDate(aux, data) == 0){# data >= aux
      startAt = count
      print(startAt)
      break;
    }
    count = count + 1
  }
  #print("startAt")
  #print(startAt)
  #print("max")
  #print(max)
  values = values1[c(startAt:nrow(values1)), c(1,3)]
  
  # 2 - Da primeira data maior para a frente, enquanto as data+HORA_INICIO forem menores que a data recebida por parametro (e duracao != 0),
  #     incrementamos um contador (que representa o numero de sessões em simultaneo)
  nr_sess = 0
  i = 1
  end = nrow(values)
  #View(values1)
  #View(values)
  while(i <= end){
    aux1 = toDate(values[[i, 1]])#Coluna Data_Hora_Ini
    duracao = as.numeric(values[[i,2]])#duracao da sessao COLUNA DURACAO
    #if(duracao == 125){View(values)}
    #print(duracao)
    #print("DataAux1")
    #print(aux1)
    #print("DataRecebida")
    #print(data)
    #print("compare: (data, aux1)")
    #print(compareDate(data, aux1))
    #print("1condicao")
    #print((compareDate(data,aux1) == 1 || compareDate(data,aux1) == 0))
    #print("2condicao")
    #print(isValidConnection(duracao))
    if((compareDate(data,aux1) == 1 || compareDate(data,aux1) == 0) && isValidConnection(duracao)){ # (data < aux1) AND (duracao != 0 -> FALHA)
      nr_sess = nr_sess + 1
      write(paste(toString(toDate(data), toString(toDate(aux1)))), file= "novoFile.txt", append = TRUE,sep = ",")
      #print("ENTROU AQUI!! -> incrementou nr_sess")
      if(nr_sess >= MAX_SIMULTANEO){
        MAX_SIMULTANEO <-nr_sess
      }
    }else if(compareDate(data, aux1) == -1){
      break;
    }
    i = i + 1
  }
  
  # 3 - Quando acabar a pesquisa, retornar esse número e o número da linha do ficheiro da primeira data+HORA_INICIO encontrada (com duracao != 0, senão retorna a seguinte)
  while(!isValidConnection(values1[[startAt,3]])){
    startAt = startAt + 1
  }
  
  return(c(nr_sess, startAt))
} 
findSessoesSimultaneoInFile<- function(input, startDate, deleteOld){
  fileName = concatTwoDates_toString(input[[1,3]],input[[nrow(input),3]])
  print(fileName)
  count = 0
  lastLine = 1
  dataInicial = toDate(toString(startDate))
  lastDate = toDate(toString(input[[nrow(input), 3]]))
  if(deleteOld){
    write("", file = fileName, append = FALSE, sep = " ")
  }
  ending = subtractDate(lastDate, dataInicial)
  #print("ending:")
  #print(ending)
  print("DataInicial:")
  print(dataInicial)
  while(count <= ending){
    
    
    dataAux <- incrementDate(dataInicial, count)
    
    print(dataAux)
    
    #Chama calculo de sessoes simultaneas
    result = nr_sessoes_simultaneo(input, lastLine, dataAux)
    
    nr_sessoes_encontradas  = result[[1]]
    print(paste("sessoes ", nr_sessoes_encontradas, sep = "= "))
    lastLine = result[[2]]
    
    texto = paste(nr_sessoes_encontradas, dataAux, sep = "\",\"")
    write(paste(paste("\"", texto, sep = ""), "\"", sep = ""), file = fileName, append = TRUE, sep = " ")
    
    count = count + 1 
  }
}

# PACKAGES IMPORTS

library(readr)
library(lubridate)

PARTNAME = toString("part_ag")


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
  
  diff=toDate(date1)-toDate(date2)
  min1 = as.numeric(getHour(toDate(date1))) * 60 + as.numeric(getMinuts(toDate(date1)))
  min2 = as.numeric(getHour(toDate(date2))) * 60 + as.numeric(getMinuts(toDate(date2)))
  
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
  result = toDate(date) + minutes(minut)
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
  return(strptime(date, format = "%Y-%m-%d %H:%M", tz = "GMT"))
}


compareDate <-function(arg1, arg2){
  #print("---COMPARE----")
  #print(arg1)
  #print(arg2)
  sub = as.numeric(subtractDate(arg1, arg2))
  #print(sub)
  returnValue = 0 # DateA == DateB, retorna 0
  #if(is.na(sub)){
    #print("SUB IS 'NA'")
    #print("arg1")
    #print(arg1)
    #print("arg2")
    #print(arg2)
    #print("sub")
    #print(sub)
  #}
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

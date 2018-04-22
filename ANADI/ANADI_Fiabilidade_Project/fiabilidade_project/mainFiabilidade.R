

source("Util.R")

Read_sessions <- read_csv(INICIAL_FILE)
#View(Read_sessions)
qtd_amostra = nrow(Read_sessions)

Serv = c()
Prot = c()
Data_Hora_Ini = c()
Data_Hora_End = c()
Duracao = c()

# Alteracoes aos valores lidos de ficheiro
# Ordenar melhor: criar tipos diferentes em colunas diferentes em vez de ser tudo strings
if(file.exists(MODIFIED_FILE)){
  sessions <- read_csv(MODIFIED_FILE)
  #View(sessions)
}else{
  count = 1
  while(count <= qtd_amostra){ 
    #ATENÇÃO: demora cerca de 30 a 40 minutos a correr! para 30.000 linhas
    auxData = Read_sessions[[count, COL_DATA]]
    auxHoraIni = Read_sessions[[count, COL_HORA_INICIO]]
    auxServ = Read_sessions[[count, COL_SERVIDORES]]
    auxProt = Read_sessions[[count, COL_PROTOCOLOS]]
    auxDuracao = Read_sessions[[count, COL_DURACAO]]
    auxDataIni = joinDateTime(auxData, auxHoraIni)
    auxDataEnd = incrementDate(auxDataIni, Read_sessions[[count, COL_DURACAO]])
    Serv = c(Serv, list(auxServ))
    Prot = c(Prot, list(auxProt))
    Duracao = c(Duracao, list(auxDuracao))
    Data_Hora_Ini = c(Data_Hora_Ini, list(auxDataIni))
    Data_Hora_End = c(Data_Hora_End, list(auxDataEnd))
    count = count+1
  }
  contador = 1
  write.table("Servidor", file = Serv_FILE,row.names=FALSE, na="",col.names=FALSE,append = FALSE,sep="")
  write.table("Protocolo", file = Prot_FILE,row.names=FALSE, na="",col.names=FALSE,append = FALSE, sep="")
  write.table("Data_Hora_Ini", file = Data_Hora_Ini_FILE,row.names=FALSE, na="",col.names=FALSE,append = FALSE, sep="")
  write.table("Data_Hora_End", file = Data_Hora_End_FILE,row.names=FALSE, na="",col.names=FALSE,append = FALSE, sep="")
  write.table("Duracao", file = Duracao_FILE,row.names=FALSE, na="",col.names=FALSE,append = FALSE, sep="")
  while(contador < length(Serv)){
    write.table(toString(Serv[[contador]]), file = Serv_FILE,row.names=FALSE, na="",col.names=FALSE,append = TRUE,sep="")
    write.table(toString(Prot[[contador]]), file = Prot_FILE,row.names=FALSE, na="",col.names=FALSE,append = TRUE, sep="")
    write.table(toString(Data_Hora_Ini[[contador]]), file = Data_Hora_Ini_FILE,row.names=FALSE, na="",col.names=FALSE,append = TRUE, sep="")
    write.table(toString(Data_Hora_End[[contador]]), file = Data_Hora_End_FILE,row.names=FALSE, na="",col.names=FALSE,append = TRUE, sep="")
    write.table(toString(Duracao[[contador]]), file = Duracao_FILE,row.names=FALSE, na="",col.names=FALSE,append = TRUE, sep="")
    contador = contador + 1
  }
  # sessions[linha, coluna] -> Ir buscar dados
  # View(sessions)
  
  Serv <- read.csv(Serv_FILE, sep="")
  Prot <- read.csv(Prot_FILE, sep="")
  Data_Hora_Ini <- read.csv(Data_Hora_Ini_FILE, sep="")
  Data_Hora_End <- read.csv(Data_Hora_End_FILE, sep="")
  Duracao <- read.csv(Duracao_FILE, sep="")
  
  sessions = Serv
  sessions = cbind(sessions, Prot)
  sessions = cbind(sessions, Data_Hora_Ini)
  sessions = cbind(sessions, Data_Hora_End)
  sessions = cbind(sessions, Duracao)
  #View(sessions)
  write.table(sessions, file = MODIFIED_FILE,row.names=FALSE, na="",col.names=FALSE,append = TRUE, sep=",")
}


# --------------------------------------------------------------------
#                     SESSOES SIMULTANEAS
# --------------------------------------------------------------------



#View(sessions)
nrow(sessions)
data = toDate(toString(sessions[[1,3]])) - minutes(5)
findSessoesSimultaneoInFile(sessions,data, TRUE)



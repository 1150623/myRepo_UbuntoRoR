

source("Util.R")

Read_sessions <- read_csv("./Resources/vpnsessions_testes.txt")
#View(Read_sessions)
qtd_amostra = nrow(Read_sessions)

Serv = c()
Prot = c()
Data_Hora_Ini = c()
Data_Hora_End = c()
Duracao = c()

# Alteracoes aos valores lidos de ficheiro
# Ordenar melhor: criar tipos diferentes em colunas diferentes em vez de ser tudo strings
if(file.exists("./Resources/modified_sessions.csv")){
  sessions <- read_csv("./Resources/modified_sessions.csv")
  
}else if(file.exists("./Resources/Data_Hora_End.txt")){
  Data_Hora_End <- read_csv("./Resources/Data_Hora_End.txt")
  Data_Hora_Ini <- read_csv("./Resources/Data_Hora_Ini.txt")
  Prot <- read_csv("./Resources/prot.txt")
  Serv <- read_csv("./Resources/serv.txt")
  Duracao <- read_csv("./Resources/Duracao.txt")
  
}else{
  count = 1
  while(count <= qtd_amostra){ 
    #ATENÇÃO: demora cerca de 15 minutos a correr!
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
  # write.table(Serv, file = "./Resources/serv.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
  # write.table(Prot, file = "./Resources/prot.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
  # write.table(Data_Hora_Ini, file = "./Resources/Data_Hora_Ini.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
  # write.table(Data_Hora_End, file = "./Resources/Data_Hora_End.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
  # write.table(Duracao, file = "./Resources/Duracao.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
  
  # sessions[linha, coluna] -> Ir buscar dados
  # View(sessions)
  sessions = data.frame(matrix("", ncol = 1, nrow=qtd_amostra))
  sessions = cbind(sessions, Serv)
  sessions = cbind(sessions, Prot)
  sessions = cbind(sessions, Data_Hora_Ini)
  sessions = cbind(sessions, Data_Hora_End)
  sessions = cbind(sessions, Duracao)
  write.table(sessions, file = "./Resources/modified_sessions.csv", sep=",")
}




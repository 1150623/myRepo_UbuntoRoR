

source("Util.R")

Read_sessions <- read_csv("./Resources/vpnsessions.txt")
#View(Read_sessions)
qtd_amostra = nrow(Read_sessions)
sessions = data.frame()
# Alteracoes aos valores lidos de ficheiro
# Ordenar melhor criar tipos diferentes em colunas diferentes em vez de ser tudo strings
if(file.exists("./Resources/modified_sessions.csv")){
  sessions <- read_csv("./Resources/modified_sessions.csv")
}else{
  Serv = Read_sessions$Servidor
  Prot = Read_sessions$Protocolo
  Data_Hora_Ini = c()
  Data_Hora_End = c()
  Duracao = Read_sessions$Duracao
  count = 1
  while(count <= qtd_amostra){ 
    #ATENÇÃO: demora cerca de 15 minutos a correr!
    auxData = Read_sessions[[count, COL_DATA]]
    auxHoraIni = Read_sessions[[count, COL_HORA_INICIO]]
    
    auxDataIni = joinDateTime(auxData, auxHoraIni)
    auxDataEnd = incrementDate(auxDataIni, Read_sessions[[count, COL_DURACAO]])
    
    Data_Hora_Ini = c(Data_Hora_Ini, auxDataIni)
    Data_Hora_End = c(Data_Hora_End, auxDataEnd)
    count = count+1
  }
  
  sessions = data.frame(Serv, Prot, Data_Hora_Ini, Data_Hora_End,Duracao)
  # sessions[linha, coluna] -> Ir buscar dados
  # View(sessions)
}
write.table(sessions, file = "./Resources/modified_sessions.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")


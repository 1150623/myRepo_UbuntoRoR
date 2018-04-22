

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
  View(sessions)
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



nr_sessoes_simultaneo <- function(input, lastLine, data){
  startAt = lastLine
  #(0 - Se receber um parametro válido como 'lastLine', iniciar a pesquisa nessa linha)
  #print("PASSO 0...")
  max = lastLine + MAX_SIMULTANEO
  if(max>nrow(input)){
    max = nrow(input)
  }
  
  values1 = input[c(lastLine:max), c(3:5)]
  
  # 1 - Encontrar a primeira data (dia+HORA_FIM) que seja maior que a recebida por parametro.
  #     Assim temos a certeza que não queremos as sessões anteriores por acabarem antes da hora pretendida.
  count = 1
  end = nrow(values1)
  if(lastLine==1){View(values1)}
  while(count <= end){
    aux = values1[[count, 2]]#Coluna Data_Hora_Fim
    if(compareDate(data, aux) == 1 || compareDate(aux, data) == 0){# data >= aux
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
    aux1 = values[[i, 1]]#Coluna Data_Hora_Ini
    duracao = as.numeric(values[[i,2]])#duracao da sessao COLUNA DURACAO
    if(duracao == 125){View(values)}
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
      #print("ENTROU AQUI!! -> incrementou nr_sess")
      if(nr_sess >= MAX_SIMULTANEO){
        MAX_SIMULTANEO <<-nr_sess
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
  count = 0
  lastLine = 1
  nprints = 1
  dataInicial = startDate
  lastDate = toDate(input[[qtd_amostra, 3]])
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
    
    print("A Calcular para:")
    print(dataAux)
    
    #Chama calculo de sessoes simultaneas
    result = nr_sessoes_simultaneo(input, lastLine, dataAux)
    
    nr_sessoes_encontradas  = result[[1]]
    lastLine = result[[2]]
    
    texto = paste(nr_sessoes_encontradas, dataAux, sep = "\",\"")
    write(paste(paste("\"", texto, sep = ""), "\"", sep = ""), file = fileName, append = TRUE, sep = " ")
    
    count = count + 1 
  }
}

findSessoesSimultaneoInFile(sessions,toDate(sessions[[1,3]]) - minutes(5), TRUE)



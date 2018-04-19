
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
isValidConnection <- function(duracao){
  # Verifica se uma conneccao e valida (se duracao <= 1, e uma falha), ou seja, duracao > 1
  return(duracao > 1)
}

calcMTBF <-function(vetorInicioFalhas){
  #Calculo MTBF (Mean Time Between Failure)
  for(var in vetorInicioFalhas[]){
    
  }
}

calcTaxaDeFalha <- function(dados, inicio, fim, nr_sessoes){
  #   Para calcular a taxa de falhas num período deve considerar a razão entre 
  # o número sessões classificadas como falha e o total de sessões nesse período.
  
  # [NOT-IMPLEMENTED]
  }


library(readr)
sessions <- read_csv("./Resources/vpnsessions.txt")
#View(sessions)

qtd_amostra = nrow(sessions)

# sessions[linha, coluna] -> Ir buscar dados


nr_sessoes_simultaneo <- function(lastLine, data){
  values = data.frame(sessions$Data, sessions$HoraInicio, sessions$HoraFim, sessions$Duracao)
  
  
  #(0 - Se receber um parametro válido como 'lastLine', iniciar a pesquisa nessa linha)
  # 1 - Encontrar a primeira data (dia+HORA_FIM) que seja menor que a recebida por parametro.
  #     Assim temos a certeza que não queremos as sessões anteriores por acabaram antes da hora pretendida.
  # 2 - Da primeira data maior para a frente, enquanto as data+HORA_INICIO forem menores que a data recebida por parametro (e duracao != 0),
  #     incrementamos um contador (que representa o numero de sessões em simultaneo)
  # 3 - Quando acabar a pesquisa, retornar esse número e o número da linha do ficheiro da primeira data+HORA_INICIO encontrada (com duracao != 0, senão retorna a seguinte)
    
} 


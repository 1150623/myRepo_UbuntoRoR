############################################################
#
# Fiabilidade
# 
# A taxa média de falhas, lambda, é dado por:
#
#     Lambda = ( nr de Falhas  /  tempo de operacao )  
#       
#        Unidade de medida: h^-1 (inverso de hora)
#
#
#
# Lambda = 15/3350; (TP4_Ex1)
# Lambda = 0.004477612
# Lambda
#
#
# MTTF (Mean Time To Failure) =  ( tempo de operacao  /  nr de falhas )
# Unidade de medida: h (horas)
#
# Se a taxa de falhas for considerada constante, no periodo de vida útil:
# 
# Funcao de fiabilidade:
#
#     R(t) = e^(-Lambda*t)
#
############################################################


###########
#   EX1   #
###########
  
  Lambda_1 = 15/3350; 
  Lambda_1


###########
#   EX2   #
###########
  
  # Assimindo que a taxa de falha é constante, tem-se que a probabilidade de sobrevivência no instante t, ou seja, a função de fiabilidade R(t) obedece a uma distribuição exponencial, isto é:
  # R(t) = e^(-Lambda*t)
  # Como:   MTTF = 1/Lambda <=> Lambda = 1/MTTF
  
  MTTF_2 = 17500
  Lambda_2 = 1/MTTF_2
  tf_2 = 3000
  
  # A fiabilidade do equipameto é a probabilidade de que o bem funcione corretamente nas condições e no periodo de tempo referidos
  
  R3000_2 = exp(-Lambda_2 * tf_2)
  R3000_2 = round(R3000_2*100,1)
  R3000_2 # = 0.8424604
  
  # Resposta: A probabilidade de o equipamento funcionar, sem falhas, durante 3000 horas é de 84.26%



###########
#   EX3   #
###########
  
  MTTF_3 = 4.5
  
  
  # Alínea A)
  
    # Seja T a variavel aleatoria "Tempo de funcionamento sem avarias da referida máquina". Sabemos que:
    # T ~ Exp(Lambda)   onde o valor esperado é 1/Lambda
    # T ~ Exp(1/4.5)
    
    Lambda_3 = 1/MTTF_3 # 1/4.5 = 0.2222222 avarias/hora
    Lambda_3
    
    # P(T >= 6) = R(6)
    R6_3 = exp(-Lambda_3*6)
    R6_3
    R6_3 = round(R6_3*100, 1) #26,3%
    
  # Alínea B)
    
    # P (T>6 | T>4) = P(T>6) / P(T>4) = (e^(-6t)) / (e^(-4t)) = e^(-2t)   =   P(T>2)
    # Assim:
    # P (T>6 | T>4) = P(T>2), pois a distribuição exponencial não tem memória dos acontecimentos passados
    # 
    # P(T>2) = R(2) = e^(-Lambda * 2), por isso:
    
    R2_3 = exp(-Lambda_3*2)
    R2_3
    R2_3 = round(R2_3*100,1) # 64.1%
    
    
###########
#   EX4   #
###########
    
MTTF_4 = 12000 # horas
Lambda_4 = 1/MTTF_4
vidaUtil_4 = 3625

  # Alínea A)
  
    R3625_4 = exp(-Lambda_4*vidaUtil_4)
    R3625_4 = round(R3625_4*100,1)
    R3625_4 # 73.9%
    # Logo, a probabilidade de a lampada funicionar, sem falhas, durante o seu tempo de vida útil, é de ~74%
  
  # Alínea B)
    
    # P(T>2000)= R(2000)
    R2000_4 = exp(-Lambda_4*2000)
    R2000_4 = round(R2000_4*100,1)
    R2000_4 # 84.6%
    
    
    

###########
#   EX5   #
###########    
    
    t.atividade = c(20.2,6.1, 24.4, 35.3, 5.3, 46.7, 4)
    t.reparacao = c(2.1, 7.1, 4.2, 1.8, 3.5, 8.3)
    
  # Alínea A)
    
    # A taxa média de falhas, Lambda, é dado por:
    # Lambda = Nr falhas / tempo operação
    
    n.falhas = length(t.reparacao)
    t.operacao = sum(t.atividade)
    t.operacao
    
    Lambda_5 = n.falhas / t.operacao
    Lambda_5
    
  # Alínea B)
    
    MTTF_5 = 1/Lambda_5
    MTTF_5 = round(MTTF_5, 1)
    MTTF_5 # 23.7 horas
    
    sum(t.atividade)/6
    
    

# Exercício 6
    
    # Alínea a)
    
      t.ativ = c(10, 20, 17, 6, 14, 9) # duraçãio dos tempos de atividade
      t.falh = c(15, 10, 7, 4, 8) # duração das falhas em minutos
      n.falhas = length(t.falh)
      
      lambda_6_min = n.falhas/sum(t.ativ);
      lambda_6_min # (em minutos = 0.06578947 falhas/minuto)
      lambda_6 = lambda_6_min*60;
      lambda_6 # (em horas = 3.947368 falhas/hora)
      
      
    # Alínea b)
    
      MTTF_6 = 1/lambda_6
      MTTF_6 # = 0.2533333 horas
      MTTR_6 = mean(t.falh)/60 # 0.2533333
      MTTR_6 # = 0.1466667 horas
      MTBF_6 = MTTF_6 + MTTR_6
      MTBF_6 # = 0.4 horas
      
    # Alínea c)
      
      D_6 = MTTF_6 / (MTTF_6 + MTTR_6)
      D_6 # = 0.6333333 -> 63%
      
      
    #Alínea d
      
      I_6 = 1 - D_6
      I_6 # 0.366666667
      

# Exercício 7
      
      
    # Alínea a)
      
      # Em 1000 horas falharam 0.5% dos carburadores testados, logo sobreviveram 99.5% dos carburadores testados. Deste modo R(1000) = 1 - F(1000) = 1 - 0.005 = 0.995
      # Logo, a probabilidade de o equipamento funcionar, sem falhas, durante 1000 horas, é de 99.5%
      
    # Alínea b)
      
      Na_7 = 0.005 * 200
      Na_7 # = 1
      # Resposta: 1 carburador
      
      
    # Alínea c)
      
      # A função de fiabilidade R(t) - probabilidade de sobrevivência no intante t - é definida pela expressão:
      #   R(t) = exp(-lambda * t) = exp(-t / MTTF)
      # Como R(1000) = 0.995, tem-se que: 0.995 = exp(- 1000 / MTTF) <=> MTTF = - 1000 / log(0.995)
      
      MTTF_7 = - 1000 / log(0.995) # atenção: log = ln
      MTTF_7 # 199499.6 horas
      
      RMTTF_7 = exp(-MTTF_7 / MTTF_7)
      RMTTF_7 # = 0.3678794 (36.8%)
    
    
    
    
    
    
    
    
    
    
    
    
    

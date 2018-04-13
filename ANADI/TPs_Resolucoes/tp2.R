#=====#
# TP2 #
#=====#

#Exercício 1

potencia=c(8.9,9.1,9.2,9.1,8.4,9.5,9.0,9.6,9.3,9.3,8.9,9.7,8.7,9.4,8.5,8.9,8.4,9.5,9.3,8.8,9.4,8.9,9.3,9.0,9.2,9.1,9.8,9.6,9.3,9.2,9.1,9.6,9.8,9.5,10.0,9.3)

  # Resolução A
  # Não sabemos se é normal. Amostra>30, variância desconhecida, estatistica de teste normal
  
    x_bar=mean(potencia);x_bar
    sd_sample=sd(potencia);sd_sample
    n=length(potencia);n
    z_q=qnorm(0.975,0,1);z_q
    
    
    IC_min=x_bar-z_q*sd_sample/sqrt(n)
    IC_max=x_bar+z_q*sd_sample/sqrt(n)
    c(IC_min,IC_max)


  # Resolução B
  # Partindo do pressuposto de que a distribuição é normal, teste t, como as caudas da t são mais pesadas que a normal e também é mais achatada, os IC calculados por esya estatística, terão maior amplitude

    t.test(potencia)
    # IC = [9.081252, 9.340970]
    # Conclusões: Há evidencia estatística que nos permite concluir, com um grau de confiança de 95%, que a potencia média é diferente de 9.0

    
#Exercícío 2
    
  ex2=c(39,41,42,41,34,45,40,46,43,43,39,47,37,44,35,39,34,45,43,38,44,39,43,40,42,41,48,46,43,42,41,46,48,45,50,43)  
  # Partindo deste pressuposto, a distribuição é normal
  
  t.test(ex2)
  # IC = [40.81252, 43.40970]
  # Conclusões: Há evidencia estatística que nos permite concluir, com um grau de confiança de 95%, que as condições de funcionamento não estarão dentro das normas
  

# Exercícío 3
  
  tconsulta_a = c(11,12,10,15,13,11,12,14)
  tconsulta_b = c(12,9,11,13,9,10,11,12)
  
  # Alínea A)
    
    # As amostras são independentes (os individuos foram selecionados aleatoriamente e de forma independente)
  
  # Alínea B)
  
    # pressupostos: populações normais, variancias iguais mas desconhecidas | teste T
  
    t.test(tconsulta_a, tconsulta_b, conf.level = 0.99, var.equal = TRUE, paired = FALSE)
    # paired = FALSE -> não são emparelhadas
    # var.equal = TRUE -> as variancias são iguais
    # conf.level = 0.99 -> confiança 99%
    
    # IC = [-0.9572937, 3.7072937] 
    

  
  # Alínea C)
    
    # Conclusões: Com um grau de confiança de 99%, não  é possível concluir que uma das app tem tempo de consulta superior à outra
    # Aumentando o tamanho das amostras ou diminuindo a confiança
    
    

#Exercício 4
    
  # Alínea A)
    
    ram1 = c(3.1,0.5,3.2,1.5,6.7,4.3,2.2,6.1,1.1,2.8,5.3,2.7)
    ram2 = c(3.4,0.7,3.0,2.0,7.0,4.5,2.8,6.3,1.5,3.9,5.8,3.0)
    
    # as amostras são emparelhadas
    # (foram testadas no mesmo pc os mesmo algoritmos)
    
    
  # Alínea B)
    
    t.test(ram1, ram2, conf.level = 0.99, var.equal = TRUE, paired = TRUE)
    
    # IC = [-0.64322811, -0.09010523]
    # Conclusões: Há evidencia estatística que nos permite concluir, com um grau de confiança de 99%, que a RAM_1 é mais eficiente

    
#Exercício 5
    
    t_a = c(67,70,66,68,68,72,65,65,66,68,69,67)
    t_b = c(69,65,65,70,71,71,69,70,71,66,68,69)
    
    # segue uma distribuição normal segundo o enunciado
    # não é emparelhada

    t.test(t_a, t_b, conf.level = 0.99, var.equal = TRUE, paired = FALSE)
    # IC = [-3.556092, 1.389425]
    # Conclusões: Com um grau de confiança de 99%, não é possível concluir qual dos tecidos tem maior resistência a altas temperaturas
    
    
    
#Exercício 6
    
    t_com = c(61,12,56,75,13,30,58,63,29,66,25,40)
    t_sem = c(48,13,33,55,22,28,54,14,9,46,30,36)

    # segue uma distribuição normal segundo o enunciado
    # não é emparelhada
    
    t.test(t_com, t_sem, conf.level = 0.95, var.equal = TRUE, paired = FALSE)
    # IC = [-4.511343, 27.844676]
    # Conclusões: Com um grau de confiança de 95%, não é possível concluir qual dos computadores tem melhor tempo de arranque
    
    
#Exercício 7

    Dados <- read.csv("~/R/DadosTP1AnaliseDesempenhocsv.csv", sep=";")
    View(Dados)
    
    attach(Dados)
    
    
    
    # Alínea A)
        
      sucessosACO = sum((Optimo-ACO)==0)
      sucessosPSO = sum((Optimo-PSO)==0)
      sucessosABC = sum((Optimo-ABC)==0)
        
      
    # Alínea B)
      
      # np >= 5 e n(1 - p) >= 5
      # só devemos fazer para MH PSO e ABC
      #prop.test(n sucessos, tamanho amostra, %)
      prop.test(sucessosPSO, length(PSO), conf.level = 0.97)
      # IC = [0.04697881, 0.21654486]
      # Podemos dizer com 97% de confiança que esta meta-heuristica, na maioria das vezes, dá valores neste intervalo
      prop.test(sucessosABC, length(ABC), conf.level = 0.97)
      # IC = [0.6325241, 0.8549172]
    
      
    # Alínea C)
      
      
      prop.test(c(sucessosABC, sucessosPSO), c(length(ABC), length(PSO)), conf.level = 0.95)
      # IC = [0.5207395, 0.7859272]; prop 1 = 0.7600000; prop 2 = 0.1066667
      prop.test(c(sucessosPSO, sucessosABC), c(length(PSO), length(ABC)), conf.level = 0.95)
      # IC = [-0.7859272, -0.5207395]; prop 1 = 0.1066667; prop 2 = 0.7600000
      
      # Com 95% de confiança podemos dizer que a probabilidade de sucessos do ABC é maior que a do PSO
      
      
      
      
      
      

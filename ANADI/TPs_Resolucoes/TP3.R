
# --------
#   EX1 
# --------

  # H0: u == 32.5
  # H1: u != 32.5
  
  espessura = c(30,30,30,30,31,32,32,32,32,33,33,34,34,34,35)
  t.test(espessura, alternative = "two.sided", mu = 32.5, conf.level = 0.95)
  # "alternative hypothesis: true mean is not equal to 32.5"
  # t_observado ((media da amostra - 32.5) / (s / srqr(15))) = -0.847295
  # p_value = 0.4134
  # p_value > 0.05 -> Nao ha evidencia estatista que permita rejeitar H0
  
  # MANUALMENTE:
  
  t_obs = (mean(espessura) - 32.5) / (sd(espessura)/sqrt(15))
  t_obs # = -0.8429534, como demonstrado acima no t.test
  
  # Concluindo: 
  # p_value = 0.4134 > 0.05 => Nao rejeita H0
  # Nao ha evidencia estatistica que nos permita concluir, com um nivel de significancia de 5% que 
  # a espessura media do computador e diferente de 32.5

# --------
#   EX2
# --------

  # Teste de hipotestes para a media do tempo da entrega das encomendas

  entrega = c(5,4,4,5,5,5,6,5,5,4,4,3,4,4,5,5,7,6,5,6,4,6,5,5,6,6,6,4,4,5,5,5,3,6,3,6,5)

  #Alinea a)
  
    #H0: mu == 5 dias 		H1: mu > 5 dias
    #Teste Unilateral
    
  #Alinea b)
  
    #pressupostos: população normal, variancia desconhecida
    t.test(entrega,alternative="greater", mu = 5, conf.level = 0.99)
    # O parametro "alternative" refere-se ao tipo de teste (unilateral ou não, e, em caso de unilateral, qual o lado)
    # t_obs = -0.68086
  
  
  # Alinea c)
  
    # p-value = 0.7498 > 0.01 => Nao rejeitar H0
    # Nao ha evidencia estatistica que nos permita  concluir , com u nivel de significancia de 1%,
    #que o tempo medio de entrega foi superior a 5 dias


# --------
#   EX3
# --------

  peso = c(2550,2550,2450,2560,2520,2530,2530,2500,2490,2510,2520,2520,2530,2510,2550,2550)
           
  # Alinea a)
    
    # H0: mu = 2500 g  ; H1: mu > 2500 g
    # Teste unilateral
    
  # Alinea b)  
    # pressupostos: populacao normal, variancias 
    t.test(peso, alternative="greater", mu = 2500)
    
    t_obs = (mean(peso)-2500) / (sd(peso)/4)
    t_obs = ...

    
# --------
#   EX4
# --------

  comfio = c(2300,2000,1800,2000,2400,2200,2000,1800,1900,2100,2200,2400)
  semfio = c(2400,2200,1800,1900,1800,1900,2100,2050,2200,2000,1900,2000)

  # Alinea a)
    # Amostras independentes
    
  # Alinea b)
    
    # pressupostos: populacoes normais, variancias desconhecidas, iguais? ou diferentes? (verificar primeiro)
    # Teste para igualdade de medias:
    # H0: mu1 = mu2
    # H1: mu1 != mu2
    # Teste para igualdade de variancias:
    # H0: v1 = v2
    # H1: v1 != v2
    
  
    # Fazemos previamente um teste a igualdade de variancias
  
    # Teste de Levene para igualdade de variancias
    # Criar data.frame
    dados <- as.data.frame(cbind(comfio,semfio))
    View(dados)
    # Recriar a data.frame com 1 unica variavel de velocidade e um fator Tipo
    #que identifica a que tipo de rato pertence
    
    library(reshape2) # para utilizar a funao 'melt()'
    Mdados = melt(dados, variable.name = "Tipo", value.name = "Velocidade")
    View(Mdados)
    
    
    # Fazer teste de Levene
    library(car) # para utilizar a funcao Levene
    leveneTest(Velocidade ~ Tipo, Mdados, center = median) # center = mean
    #p-value = 0.4338 > 0.01 => Nao rejeitar H0. Aceitar variancias iguais
    
  # OU (em aleternativa ao teste Levene, utilizamos o teste F que e menos robusto e mais sensivel a normalidade)
    var.test(comfio,semfio,alternative="two.sided", conf.level = 0.99)
    #tira-se a mesma conclusao
    
    
    # Teste para igualdade de medias, considerando variancias iguais (dados os resultados dos testes anteriores)
    
    t.test(comfio, semfio, alternative = "two.sided", var.equal = TRUE, paired = FALSE)
    
    # Concluindo: Não a evidencia estistica que nos permita concluir,
    # com um nivel de significancia de 1%, que as medias dos dois tipos de rato sao diferentes
    
    
# --------
#   EX5
# -------- 
    
    maoEsquerda = c(140,90,125,130,95,121)
    maoDireita =  c(138,89,126,128,92,122)
    
    # Alinea a)
      # As amostras sao emparelhadas
      
    # Alinea b)
      
      # pressupostos: populacoes normais
    
      # Teste de Hipoteses:
      # H0: muE = muD 
      # H1: muE > muD
      
      t.test(maoEsquerda, maoDireita, alternative="greater", conf.level=0.95, paired=TRUE)
      
    
# --------
#   EX6
# -------- 
      
  antesObras = c(20,35,40,55,60,75,95,100,90,80,45,25) 
  depoisObras = c(25,30,45,75,80,100,100,100,100,85,65,30)
    
    # Alinea a)
  
      # Sao amostras emparelhadas
  
    # Alinea b)
  
      # Teste de hipotestes:
      # H0: muAntes = muDepois 
      # H1: muAntes < muDepois
      # teste unilateral
  
    # Aliena c)
      
      t.test(antesObras, depoisObras, alternative="less", paired = TRUE)
      # p-value = 0.002358 < 0.05 => Rejeitar H0
      
    # Alinea d)
  
      # Ha evidencias que nos permie concluir, com um nivel de significancia de 5%,
      # que a remodelacao teve um efeito  positivo.
      
       
# --------
#   EX7
# -------- 
  MarcaA = c(6.3,5.2,6.0,6.1,6.5,5.6,5.8,6.0,5.9,5.8,5.9,6.2)
  MarcaB = c(4.8,6.7,7.1,5.0,6.2,6.1,6.0,5.9,7.0,4.5,5.3,6.2)
      
  # Queremos fazer um teste de hipotese a diferenca entre a duracao media das baterias A e B.
  # O Teste de Hipoteses será:
  # H0: mu1 == mu2
  # H1: mu1 != mu2
  # teste bilateral
  # amostras independentes
  
  # Teste igualdade de Variancias
  # H0: vA == vB
  # H1: va != vB
  
  # Com teste F
  var.test(MarcaA,MarcaB,alternative="two.sided", conf.level = 0.90)
  # p-value < 0.1 -> rejeitar H0 - Variancias sao diferentes
  
  # OU Com teste de levine
  library(reshape2)
  library(car)
  dados = as.data.frame(cbind(MarcaA, MarcaB))
  Mdados = melt(dados, variable.name = "Marca", value.name = "Duracao")
  leveneTest(Duracao ~ Marca, Mdados, center=median)
  # tiramos as mesmas conclusoes do que com o teste F (Nao rejeitar H0)    
      
      
      
      
      
      
      
# --------
#   EX8
# -------- 
    
  # Alinea a)
      
      
  # Alinea b)
    
    
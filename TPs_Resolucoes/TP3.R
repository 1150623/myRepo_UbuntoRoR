
# --------
#   EX1 
# --------

  # H0: u == 32.5
  # H1: u <> 32.5

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
  # Nao ha evidência estatistica que nos permita concluir, com um nivel de significancia de 5% que 
  # a espessura media do computador e diferente de 32.5
  
# --------
#   EX2
# --------
  
  
  
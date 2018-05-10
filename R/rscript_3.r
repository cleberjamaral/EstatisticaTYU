# INE 6006 – Métodos Estatísticos - Lista de exercícios 3 – Estimação de parâmetros e Testes de
# Hipóteses
# ENTREGA EM 12/06/2018
# Considere que o arquivo com os dados usados na Lista de Exercícios 2, referente aos alunos da
# universidade TYU seja a POPULAÇÃO da sua pesquisa. Desta população você irá retirar amostras
# aleatórias para resolver os problemas a seguir. O relatório com as respostas deverá ser entregue
# IMPRESSO com todas as tabelas, medidas e gráficos julgados convenientes, produzidos através do
# 
# R, Excel, GPower ou qualquer outro aplicativo. As amostras coletadas devem ser enviadas por e-
#   mail ao professor, devidamente identificadas com os nomes dos alunos e nomes das variáveis.
# 
# Recomenda-se que antes de qualquer procedimento remova todas as linhas com dados perdidos.

# ##############################################################################################################
# Limpando workspace e historicos
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()

closeAllConnections()
rm(list=ls())

# ##############################################################################################################
# Adicionando bibliotecas
library(readxl) # Para importar dados do excel
library(data.table) # Para trabalhar com dados aleatorios ja que a tabela é indexada

# ##############################################################################################################
# Adicionando funções
diffperc <- function(x,y) {round(100*abs(x-y)/x, digits = 2)} # Retorna a diferença percentual entre 2 valores

# ##############################################################################################################
# Importando base de dados do arquivo excel
tyu <- read_excel("TYU07.xlsx")

# ##############################################################################################################
# Pre-processamento: corrigindo dados com erros de preenchimento

#CODE: Fixing data by matching using data templates
curso.var <- data.frame(A = c("Computação", "Produção", "Elétrica", "Civil", "Química", "Mecânica"), stringsAsFactors = FALSE)
curso.df <- data.frame(curso.var$A[charmatch(substr(data.frame(A = tyu$Curso)$A,1,3), substr(curso.var$A,1,3))], stringsAsFactors = TRUE)
turno.var <- data.frame(A = c("Diurno", "Integral", "Noturno"), stringsAsFactors = FALSE)
turno.df <- data.frame(turno.var$A[charmatch(substr(data.frame(A = tyu$Turno)$A,1,5), substr(turno.var$A,1,5))], stringsAsFactors = TRUE)
ensinomedio.var <- data.frame(A = c("Maior parte em particular", "Maior parte em pública", "Somente em particular", "Somente em pública"), stringsAsFactors = FALSE)
ensinomedio.df <- data.frame(ensinomedio.var$A[charmatch(substr(data.frame(A = tyu$'Ensino médio')$A,1,17), substr(ensinomedio.var$A,1,17))], stringsAsFactors = TRUE)
opiniao.var <- data.frame(A = c("Indiferente", "Insatisfeito", "Muito insatisfeito", "Muito satisfeito", "Satisfeito"), stringsAsFactors = FALSE)
opiniao.df <- data.frame(opiniao.var$A[charmatch(substr(data.frame(A = tyu$Opinião)$A,1,7), substr(opiniao.var$A,1,7))], stringsAsFactors = TRUE)

# Pre-processamento: removendo linhas com dados perdidos. Dos 10.000 registros passamos para 9.874.
fulltyu = data.frame(curso.df, turno.df, ensinomedio.df, opiniao.df, tyu$Renda, tyu$`Nota ENEM`, tyu$IAA)
newtyu = na.omit(fulltyu)
colnames(newtyu) = c("Curso", "Turno", "EnsinoMedio", "Opiniao", "Renda", "NotaENEM", "IAA")

# ##############################################################################################################
# Primeira parte – Distribuição Amostral da Média
# 1) Usando qualquer aplicativo que considere apropriado1
# 
# proceda a retirada de 1000 amostras
# aleatórias da variável Renda (primeiramente exclua as linhas com dados perdidos), com os
# seguintes tamanhos: 4, 16, 64 e 256 elementos. Com base nos resultados encontrados responda os
# itens a seguir:

#Obtendo amostras probabilisticas
amostras004 <- list()
amostras016 <- list()
amostras064 <- list()
amostras256 <- list()
dttbtyu = data.table(newtyu)
for (i in 1:1000)
{
  amostras004[[i]] = dttbtyu[sample(.N, 004)]
  amostras016[[i]] = dttbtyu[sample(.N, 016)]
  amostras064[[i]] = dttbtyu[sample(.N, 064)]
  amostras256[[i]] = dttbtyu[sample(.N, 256)]
}

###############################################################################################################
# a) Pelo teorema central do limite admite-se que o valor esperado da média amostral seja a
# média populacional que se pretende estimar, e eles serão tão mais próximos à medida que
# aumenta o tamanho da amostra. Isso é confirmado pelos resultados obtidos nas amostras?
# JUSTIFIQUE.

# p266 CHAPTER 5 NORMAL PROBABILITY DISTRIBUTIONS SAMPLING DISTRIBUTIONS 

media004 = c() # Inicializando como um vetor vazio
media016 = c() # Inicializando como um vetor vazio
media064 = c() # Inicializando como um vetor vazio
media256 = c() # Inicializando como um vetor vazio
for (i in 1:1000)
{
  media004[i] = mean(amostras004[[i]]$Renda)
  media016[i] = mean(amostras016[[i]]$Renda)
  media064[i] = mean(amostras064[[i]]$Renda)
  media256[i] = mean(amostras256[[i]]$Renda)
}
mediapop = mean(dttbtyu$Renda)
message("A diferença entre a renda média populacional e a média das médias das amostragens de   4 registros é: ",diffperc(mediapop,mean(media004)),"%")
message("A diferença entre a renda média populacional e a média das médias das amostragens de  16 registros é: ",diffperc(mediapop,mean(media016)),"%")
message("A diferença entre a renda média populacional e a média das médias das amostragens de  64 registros é: ",diffperc(mediapop,mean(media064)),"%")
message("A diferença entre a renda média populacional e a média das médias das amostragens de 256 registros é: ",diffperc(mediapop,mean(media256)),"%")

###############################################################################################################
# b) Além do que foi dito acima, admite-se que o desvio padrão das médias amostrais será
# igual ao desvio padrão populacional dividido pela raiz quadrada do tamanho da amostra, e
# eles serão tão mais próximos à medida que aumenta o tamanho da amostra. As amostras
# retiradas confirmam essa afirmação? JUSTIFIQUE.

message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de   4 registros é: ",diffperc(sd(dttbtyu$Renda)/sqrt(4),sd(media004)),"%")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de  16 registros é: ",diffperc(sd(dttbtyu$Renda)/sqrt(16),sd(media016)),"%")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de  64 registros é: ",diffperc(sd(dttbtyu$Renda)/sqrt(64),sd(media064)),"%")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de 256 registros é: ",diffperc(sd(dttbtyu$Renda)/sqrt(256),sd(media256)),"%")

###############################################################################################################
# c) Outra característica muito importante do teorema central do limite é que a distribuição
# amostral da média será cada vez mais próxima de uma distribuição normal, à medida que o
# tamanho da amostra aumenta, independentemente da forma da distribuição da variável na
# população. Sabe-se que a variável Renda não tem distribuição normal na população. Os
# resultados das amostras corroboram esta afirmação (usem os gráficos apropriados2
# ).JUSTIFIQUE.


###############################################################################################################
# d) Obtenha os intervalos de 95% de confiança para as 1000 amostras de cada tamanho.
# Construa gráficos apropriados para mostrá-los. Analisando os gráficos, o que você pode
# concluir sobre a precisão dos intervalos à medida que aumenta o tamanho de amostra?
# JUSTIFIQUE.



###############################################################################################################

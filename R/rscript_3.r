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
library(plotrix) # Para plotar graficos dos intervalos de confiança
library(pwr) # Para calculo do poder do teste
library(e1071) # Para calculo da curtose
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
# proceda a retirada de 1000 amostras aleatórias da variável Renda (primeiramente exclua as linhas
# com dados perdidos), com os seguintes tamanhos: 4, 16, 64 e 256 elementos. Com base nos resultados 
# encontrados responda os itens a seguir:

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

message("\n - - - > 1a")
message("A diferença entre a renda média populacional e a média das médias das amostragens de   4 registros é: ",
        diffperc(mediapop,mean(media004)),"%")
message("A diferença entre a renda média populacional e a média das médias das amostragens de  16 registros é: ",
        diffperc(mediapop,mean(media016)),"%")
message("A diferença entre a renda média populacional e a média das médias das amostragens de  64 registros é: ",
        diffperc(mediapop,mean(media064)),"%")
message("A diferença entre a renda média populacional e a média das médias das amostragens de 256 registros é: ",
        diffperc(mediapop,mean(media256)),"%")

# Observa-se que de fato a média das médias é praticamente igual a média populacional, sendo menos de 1% diferentes
# uma da outra. De fato ao aumentar o tamanho da amostragem a diferença é ainda menor, bem próxima de zero.

###############################################################################################################
# b) Além do que foi dito acima, admite-se que o desvio padrão das médias amostrais será
# igual ao desvio padrão populacional dividido pela raiz quadrada do tamanho da amostra, e
# eles serão tão mais próximos à medida que aumenta o tamanho da amostra. As amostras
# retiradas confirmam essa afirmação? JUSTIFIQUE.

message("\n - - - > 1b")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de   4 registros é: ",
        diffperc(sd(dttbtyu$Renda)/sqrt(4),sd(media004)),"%")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de  16 registros é: ",
        diffperc(sd(dttbtyu$Renda)/sqrt(16),sd(media016)),"%")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de  64 registros é: ",
        diffperc(sd(dttbtyu$Renda)/sqrt(64),sd(media064)),"%")
message("A diferença entre o desvio padrão da renda do populacional dividido pela raiz do número de amostras e do desvio padrão da médias das amostras de 256 registros é: ",
        diffperc(sd(dttbtyu$Renda)/sqrt(256),sd(media256)),"%")

###############################################################################################################
# c) Outra característica muito importante do teorema central do limite é que a distribuição
# amostral da média será cada vez mais próxima de uma distribuição normal, à medida que o
# tamanho da amostra aumenta, independentemente da forma da distribuição da variável na
# população. Sabe-se que a variável Renda não tem distribuição normal na população. Os
# resultados das amostras corroboram esta afirmação (usem os gráficos apropriados2
# ).JUSTIFIQUE.

#installmessage(" - - - > 1c")
plot(density(amostras004[[sample(1:1000,1,replace=T)]]$Renda), main="Distribuição 'renda': amostragem de 4") # Arbitrada uma amostragem qualquer das 1000 obtidas
plot(density(amostras016[[sample(1:1000,1,replace=T)]]$Renda), main="Distribuição 'renda': amostragem de 16") # Arbitrada uma amostragem qualquer das 1000 obtidas
plot(density(amostras064[[sample(1:1000,1,replace=T)]]$Renda), main="Distribuição 'renda': amostragem de 64") # Arbitrada uma amostragem qualquer das 1000 obtidas
plot(density(amostras256[[sample(1:1000,1,replace=T)]]$Renda), main="Distribuição 'renda': amostragem de 256") # Arbitrada uma amostragem qualquer das 1000 obtidas
plot(density(dttbtyu$Renda), main="Distribuição 'renda': população")

###############################################################################################################
# d) Obtenha os intervalos de 95% de confiança para as 1000 amostras de cada tamanho.
# Construa gráficos apropriados para mostrá-los. Analisando os gráficos, o que você pode
# concluir sobre a precisão dos intervalos à medida que aumenta o tamanho de amostra?
# JUSTIFIQUE.

# Para 95% de confiança, zc = 1.96. Pelo teorema do limite central, para tamanhos de amostras superiores a 30
# a distribuição das médias tende a ser normal. O nivel de conficanca c é o intervalo entre -zc e +zc. A area
# que sobra é de 1 - c, portanto, cada cauda tem area (1 - c)/2. A margem de erro é E = (zc * sd)/sqrt(n)

message("\n - - - > 1d")
Zc = 1.96 # Obtido da tabela de distribuiçào normal padrão
e004 = (Zc * sd(media004)) / sqrt(4)
e016 = (Zc * sd(media016)) / sqrt(16)
e064 = (Zc * sd(media064)) / sqrt(64)
e256 = (Zc * sd(media256)) / sqrt(256)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

k = kurtosis(media004)
me = mean(media004)
mn = median(media004)
md = getmode(media004)
message("Com 95% de confiança pode-se dizer que a média populacional está entre ",
        round(mean(media004)-e004, digits = 2)," e ", round(mean(media004)+e004, digits = 2)," para tamanho   4 ",
        "\n> Média     : ", round(mean(media004), digits = 2)," Mediana: ",round(median(media004), digits = 2), "     Moda: ",round(getmode(media004), digits = 2),
        "\n> Curtose   : ", round(k, digits = 2)," ",
        "Assimetria: ",round(skewness(media004), digits = 2))
if (k == 0.263) message("> Curva mesocúrtica") else if (k > 0.263) message("> Curva platocúrtica") else message("> Curva leptocúrtica") 
if ((me == mn) && (me == md)) message("> Curva simética") else if ((me > mn) && (me > md)) message("> Curva assimétrica a direita") else message("> Curva assimétrica a esquerda") 
k = kurtosis(media016)
me = mean(media004)
mn = median(media004)
md = getmode(media004)
message("Com 95% de confiança pode-se dizer que a média populacional está entre ",
        round(mean(media016)-e016, digits = 2)," e ", round(mean(media016)+e016, digits = 2)," para tamanho  16 ",
        "\n> Média     : ", round(mean(media016), digits = 2)," Mediana: ",round(median(media016), digits = 2), "     Moda: ",round(getmode(media016), digits = 2),
        "\n> Curtose   : ",round(k, digits = 2)," ",
        "Assimetria: ",round(skewness(media016), digits = 2))
if (k == 0.263) message("> Curva mesocúrtica") else if (k > 0.263) message("> Curva platocúrtica") else message("> Curva leptocúrtica") 
if ((me == mn) && (me == md)) message("> Curva simética") else if ((me > mn) && (me > md)) message("> Curva assimétrica a direita") else message("> Curva assimétrica a esquerda") 
k = kurtosis(media064)
me = mean(media004)
mn = median(media004)
md = getmode(media004)
message("Com 95% de confiança pode-se dizer que a média populacional está entre ",
        round(mean(media064)-e064, digits = 2)," e ", round(mean(media064)+e064, digits = 2)," para tamanho  64 ",
        "\n> Média     : ", round(mean(media064), digits = 2)," Mediana: ",round(median(media064), digits = 2), "     Moda: ",round(getmode(media064), digits = 2),
        "\n> Curtose   : ",round(k, digits = 2)," ",
        "Assimetria: ",round(skewness(media064), digits = 2))
if (k == 0.263) message("> Curva mesocúrtica") else if (k > 0.263) message("> Curva platocúrtica") else message("> Curva leptocúrtica") 
if ((me == mn) && (me == md)) message("> Curva simética") else if ((me > mn) && (me > md)) message("> Curva assimétrica a direita") else message("> Curva assimétrica a esquerda") 
k = kurtosis(media256)
me = mean(media004)
mn = median(media004)
md = getmode(media004)
message("Com 95% de confiança pode-se dizer que a média populacional está entre ",
        round(mean(media256)-e256, digits = 2)," e ", round(mean(media256)+e256, digits = 2)," para tamanho 256 ",
        "\n> Média     : ", round(mean(media256), digits = 2)," Mediana: ",round(median(media256), digits = 2), "     Moda: ",round(getmode(media256), digits = 2),
        "\n> Curtose   : ",round(k, digits = 2)," ",
        "Assimetria: ",round(skewness(media256), digits = 2))
if (k == 0.263) message("> Curva mesocúrtica") else if (k > 0.263) message("> Curva platocúrtica") else message("> Curva leptocúrtica") 
if ((me == mn) && (me == md)) message("> Curva simética") else if ((me > mn) && (me > md)) message("> Curva assimétrica a direita") else message("> Curva assimétrica a esquerda") 
message("A média populacional é ", round(mediapop, digits = 2))

xplot <- c(4,16,64,256)
yplot <- c(mediapop,mediapop,mediapop,mediapop)
lplot <- c(mean(media004)-e004,mean(media016)-e016,mean(media064)-e064,mean(media256)-e256)
uplot <- c(mean(media004)+e004,mean(media016)+e016,mean(media064)+e064,mean(media256)+e256)

require(plotrix)
plotCI(xplot, yplot, ui=uplot, li=lplot, main="Intervalos de confiança para os tamanhos das amostragens")
###############################################################################################################

# Segunda parte – Análise da variável Nota no ENEM
# 2) Há uma grande preocupação em estimar o comportamento da variável Nota no ENEM dos
# alunos, de maneira a caracterizar melhor seu o perfil. Alguém da universidade sugeriu que você
# retirasse uma amostra de 25 alunos e registrasse os valores de Nota no ENEM. Considere que a
# variância populacional de Nota no ENEM é DESCONHECIDA. Com base nos resultados
# encontrados responda os itens a seguir (lembre-se de excluir inicialmente as linhas com dados perdidos).

# Dos slides do professor: Ver inferencia estatistica slide 16

n = 25
amostras025 <- list()
amostras025 = dttbtyu[sample(.N, n)]

# a) Construa um gráfico de probabilidade normal para os valores da amostra.
# a.1 - É possível considerar que os dados provêm de uma população com distribuição
# normal? JUSTIFIQUE.

message("\n - - - > 2a")

qqnorm(amostras025$NotaENEM)
qqline(amostras025$NotaENEM)

message("Sim, a distribuição se aproxima de uma normal, porém a quantidade de elementos é pequena.")


# a.2 – Com base na resposta da letra a.1 você recomendaria a utilização de técnicas
# como Intervalo de confiança e teste paramétrico de média? JUSTIFIQUE.

# Sim, utilizando distribuição t.

# b) Independente dos resultados da letra a), encontre o intervalo de 95% de confiança para a
# média populacional da Idade dos hóspedes. Interprete o resultado.

# De acordo com a tabela t para 24 graus de liberdade (25-1) e intervalo de confiança de 95%
# o valor de tc é 2.064. Neste caso E = tc * (s / sqrt(n))

message("\n - - - > 2b")

media025 = mean(amostras025$NotaENEM)
mediaRealENEM = mean(newtyu$NotaENEM)

tc = qt(0.975, df=n-1) # 2.064 http://www.stats4stem.org/r-t-distribution
S = sd(amostras025$NotaENEM)
e025 = (tc * S) / sqrt(n)
message("O Intervalo de confiança para a média da nota do ENEM está entre ", round(media025-e025, digits = 2),
        " e ", round(media025+e025, digits = 2), 
        ", a média das amostras foi de ", round(media025, digits = 2),
        ", a margem de erro é de ", round(e025, digits = 2), " pontos ",
        " e, para referência, a média populacional é ", round(mediaRealENEM, digits = 2))

# c) Independente dos resultados da letra a), qual seria o tamanho mínimo de amostra
# necessário para obter um intervalo de 95% de confiança para a média populacional da Nota
# no ENEM, com uma precisão de 5 pontos? A amostra coletada é suficiente? JUSTIFIQUE.

# Dado uma margem de erro E e um nível de confiança o tamanho mínimo de amostra é dado por:
# n0 = (z^2 * var^2) / (E0^2) Barbetta p.190
# Supondo desconhecer a variância, vamos utilizar a variância desta amostra de 25 elementos como
# base. Para um E0 de 5 pontos conforme dado

message("\n - - - > 2c")

# O nível de confiança c dado é de 95% que leva a um Zc de 1.96 conforme tabela
# O erro E dado é de 5 pontos e um Zc de 1.96 para a confiança c dada

E0 = 5
Zc = 1.96
n0 = (Zc^2 * S^2) / (E0^2)

message("Não é suficiente, para um nível de confiança de 95% (Zc de 1.96 desvios padrão) e 5 pontos de erro tolerado ", 
        "o tamanho mínimo seria de ",round(n0, digits = 2)," elementos.")

N = nrow(dttbtyu)
n = (N * n0) / (N + n0)

message("Como o tamanho da população é conhecido, sendo de ", N, " elementos, o n mais precisamente",
        " seria de ",round(n, digits = 2)," elementos.")

# d) Independente dos resultados da letra a), a direção da TYU acredita que a média de Nota
# no ENEM dos alunos é maior do que 450 pontos, por pesquisas anteriores. Aplicando o teste
# estatístico apropriado, os dados confirmam isso, a 5% de significância? JUSTIFIQUE.


message("\n - - - > 2d")
message("A direção espera que a média é maior que 450, vamos considerar então como H0 supondo que ",
        "a média das notas do ENEM é menor ou igual a  450. Logo, H1 diz que a nota é maior que 450.")
message("Como H1 tem o sinal > isso leva este teste a ser de cauda a direita.")
message("Basicamente é necessário obter o valor-p, se valor-p <= alpha então rejeita-se H0.")
message("Como é uma amostra de menos de 30 elementos será utilizado o teste-t (no lugar do teste-z).")
alpha = 0.05
mediaH = 450
t0 = 1.711
message("Pela tabela de distribuição t, para 24 graus de liberdade e alpha 0.05 uma cauda t0 = ",round(t0, digits = 2))
message("A média amostral para estes 25 elementos é ",media025)
message("O desvio padrão desta amostra de 25 elementos é ",S)
t = (media025 - mediaH) * sqrt(n) / S
message("O cálculo de t é dado por t = (mean(X) - mediaHipotetica) * sqrt(n) / S  = ", round(t, digits = 2) )
if (t > t0) message("t é maior que t0, portanto com 95% de certeza a média das notas do ENEM é superior a 450 pontos") else message("t é menor ou igual a t0, portanto com 95% de certeza a média das notas do ENEM é inferior a 450 pontos")

# e) Analise o intervalo de confiança da letra b). Os limites encontrados corroboram os
# resultados encontrados na letra d)? JUSTIFIQUE.

message("\n - - - > 2e")

message("O Intervalo de confiança para a média da nota do ENEM ficou entre ", round(media025-e025, digits = 2),
        " e ", round(media025+e025, digits = 2))
if ((450 < media025-e025) & (450 < media025+e025)) message("Corrobora pois 450 é inferior ao intervalo") else message("Não corrobora pois 450 NÃO é inferior ao invervalo")

# f) Independente dos resultados da letra a), suponha 5% de significância e o desvio padrão
# amostral como boa estimativa do desvio padrão populacional. Se a média real da Nota no
# ENEM fosse de 470 pontos, qual seria o poder do teste? Você acha o valor aceitável? JUSTIFIQUE.

message("\n - - - > 2f")

sig = 0.05 # alpha
altern = "greater"
mediaReal = 470
stdP = sd(amostras025$NotaENEM)/sqrt(n)
distStd = (mediaReal-mediaH)/stdP
Zc = 1.711 # 1.711 24df one tail 0.05
Xc = mediaH + Zc * S / sqrt(25)
Zb = (Xc - mediaReal) / (S / sqrt(25))
p = pwr.p.test(h = distStd, sig.level = sig, n = n, alternative = altern)

message("power = ", p)


# g) Independente dos resultados da letra a), qual deveria ser o tamanho mínimo de amostra
# para detectar com 95% de probabilidade que a média da Nota no ENEM dos alunos é igual a
# 470 pontos. Suponha 5% de significância e o desvio padrão amostral como boa estimativa
# do desvio padrão populacional. A amostra coletada é suficiente? JUSTIFIQUE.



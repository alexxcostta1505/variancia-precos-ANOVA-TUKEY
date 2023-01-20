# variancia-precos-ANOVA-TUKEY
Variância de Preços testes ANOVA e de TUKEY

#teste de alterações github

primeiro projeto no github

#Projeto R de Análise de Variancia de Precos, com Anova
#Tukey e retirada de Outliers da Base Total


#Analise de Preços de Forma Geral(Unica Varivel, isto eh,
# todos os preços juntos e retirada de outliers)

#carregar banco de dados

dados <- read.csv(file = "C:\\Users\\admin\\Desktop\\Alexandre\\Thales\\BD1_Anova.csv"
                  ,header = TRUE, sep = ";")
                  
#mostrar dados

View(dados)

#ver a dimensao de dados(qtdes de linhas e colunas)

dim(dados)

#boxplot dos dados totais do data set

boxplot(dados$Price)

#estatisicas do data set

boxplot.stats(dados$Price) 

#acima ja podemos ver o(s)
#outliers em $out(neste caso 150)

#retirar outlier do data set total(unica variavel)

# demonstrar qtas linhas o set de dados tem
# antes de retirar os outliers

dim(dados)

#resultado = 33 linhas e 2 colunas

#definição dos 1º quartil(25%) e o 2º quartil(75%)

quartiles <- quantile(dados$Price, probs=c(.25, .75), na.rm = FALSE)
quartiles

#resultado = 1> Quartil(25%) = 37 e 2> Quartil(25%) = 77

#IQR = Interquartil range(caixa 50% mais proximos da mediana)

IQR <- IQR(dados$Price)
IQR

#resultado = range de 40

# Agora usamos o coeficiente de multiplicar o resultado
# o IQR por 1.5. O resultado sera quanto deverem subir(somar)
# do 1> quartil, para definir o limite superior(upper), e
# quanto devemos descer(subtrair) do 3> quartil para definir o limite
# inferior(lower). As observaçoes apontadas acima destes
# limites serão considerados outliers, e portanto, serao
# removidos posteriormente

#Definiçãodo Limite inferior(lower)

lower <- quartiles[1] - 1.5*IQR
# 1? quartil + 1.5 x Interquartil range)
# 37 - (1.5*40)
# 37 - 60
# -23
lower
#resultado = -23 (limite inferior, antes de outliers)

#Definição do Limite inferior(lower)

upper <- quartiles[2] + 1.5*IQR
# 3? quartil + 1.5 x Interquartil range)
# 77 + (1.5*40)
# 77 + 60
# 137
upper
#resultado = 137 (limite superior, antes de outliers )

#Código subset com a atribuição do corte, isto eh,
# remoçao dados que estao fora dos limites superiores 
# e inferiores, que sao considerados outliers

sem_outlier <- subset(dados, dados$Price > lower & dados$Price < upper )

#abaixo codigo que chama o dataset, e por ser um
#dataset pequeno eh possivel ver que foi removido
# a ultima linha que tinha o valor UF=SP > Price=150
sem_outlier

#abaixo código comparativos do dataset com outliers e
# depois o data set sem outlier
dim(dados)
#resultado = 33 linhas, 2 colunas
dim(sem_outlier)
#resultado = 32 linhas, 2 colunas

#boxplots comparativos(sem outliers vs. anterior)

boxplot(dados$Price)
boxplot(sem_outlier$Price)

#estatísicas comparativas(sem outliers vs. anterior)

boxplot.stats(dados$Price)
#veja que aqui aparece o valor 150 em $out
boxplot.stats(sem_outlier$Price)
#veja que aqui aparece o valor 0(zero) em $out

#Agora iremos tratar os dados avaliando os grupos(Estados)
# avaliando com a Anova e Teste Tukey se ha diferencas
# relevante aos preco medio geral de todos Estados

#Trazer novamente os Dados de tabela .csv completa(sem a retirada dos outliers anterior)

dados <- read.csv(file = "C:\\Users\\admin\\Desktop\\Alexandre\\Thales\\BD1_Anova.csv"
                  ,header = TRUE, sep = ";")
#codigo que chama os dados
dados

#boxplot dos grupos para visualizar outliers
boxplot(dados$Price~dados$UF)
#veja que que as medianas est?o bem proximas

#Estatisticas do data set(bloxpot)
boxplot.stats(dados$Price)

#veja que ha um outlier(150), mas este outlier nao esta
#influenciando no alinhamento da mediana. O teste Tukey
#vai nos demonstrar se realmente nao h? varia??o de media




#Anova

anova = aov(dados$Price~dados$UF)
summary.aov(anova) #chama o resultado da anova
#resultado

#             Df Sum Sq Mean Sq F value Pr(>F)
#dados$UF     3   2636   878.6   1.005  0.404
#Residuals   29  25342   873.9 


# valor que importa no resultado eh o F value 
# foi 1.005 que ? chamado de F Experimental

# F tabelado(funcao qf) (entrar com nivel de confianca 95%)
# como primeiro parametro, a seguir df1(=3) e o df2(=29) 
# , que sao os valores que deu no resultado de anova

qf(0.95, df1 = 3, df2 = 29) #F Cr?tico
#resultado foi de 2.93403

##Interpreta??o 1##se o F Cr?tico for menor que o F Experimental da Anova,
#rejeitamos a hipotese nula, isto ?, ha 2 ou mais 
#diferen?as nas m?dias.

##interpretacao 2## se o F critico for maior que F Experimental da Anova
# aceitamos a hipotese nula, isto ?, nao ha diferen?as 
# significantes nas m?dias

# Nossos resultados:

#F Experimental = 1.005
#F Critico = 2.93403

#Portanto, nossa analise recaiu sobre a interpretacao 1
# nosso F critico ? maior que o F experimental, portanto,
# nao h? diferencas, ainda sim faremos uma Teste Tukey para
# comprovar que os grupos est?o dentro da m?dia

#Teste de Tukey

tk_teste <- TukeyHSD(anova)
tk_teste

#Resultado

#$`dados$UF`
#diff       lwr      upr     p adj
#MG-ES  12.015873 -28.57261 52.60435 0.8508073
#RJ-ES  -7.571429 -50.62201 35.47916 0.9630980
#SP-ES  14.771429 -24.91925 54.46211 0.7426487
#RJ-MG -19.587302 -60.17578 21.00118 0.5612407
#SP-MG   2.755556 -34.25014 39.76126 0.9969759
#SP-RJ  22.342857 -17.34782 62.03353 0.4312103

#interprecao do Teste Tukey, o valor P(p adj) nao pode
#ser menor que 5%(0.5), os grupos comparativos que apresentarem 
# o valor abaixo de 5%, sao os comparativos que tem medias diferentes
# Em nosso exemplo veja abaixo no plot que todas as 
# analises tocam a m?dia

plot(tk_teste)

#a seguir faremos uma analise que haver? medias difentes na Anova
# e Tukey(usaremos um novo dataset)

#carregar banco de dados

dados2 <- read.csv(file = "C:\\Users\\admin\\Desktop\\Alexandre\\Thales\\BD2_Anova.csv"
                  ,header = TRUE, sep = ";")


#c?digo que chama os dados
dados2

#boxplot dos grupos para visualizar outliers
boxplot(dados2$Price~dados2$UF)
#veja que a linhas medianas de cada grupo esta bem
#proximas para ES,MG,RJ, mas SP esta bem mais alta

#Estatisticas do datset(bloxpot)
boxplot.stats(dados2$Price)
#veja que n?o h? outlier em rela??o aos grupos

#Anova

anova = aov(dados2$Price~dados2$UF)
summary.aov(anova) #chama o resultado da anova
#resultado

#            Df Sum Sq Mean Sq F value   Pr(>F)    
#dados2$UF    3  40121   13374   26.09 2.21e-08 ***
#Residuals   29  14868     513  


# valor que importa no resultado eh o F value 
# foi 26.09 que ? chamado de F Experimental

# F tabelado(funcao qf) (entrar com nivel de confianca 95%)
# como primeiro parametro, a seguir df1(=3) e o df2(=28) 
# , que sao os valores que deu no resultado de anova

qf(0.95, df1 = 3, df2 = 29) #F Cr?tico
#resultado foi de 2.93403

##Interpreta??o 1##se o F Cr?tico for menor que o F Experimental da Anova,
#rejeitamos a hipotese nula, isto eh, ha 2 ou mais 
#diferen?as nas medias.

##interpretacao 2## se o F critico for maior que F Experimental da Anova
# aceitamos a hipotese nula, isto eh, nao ha diferencaas 
# significantes nas m?dias

# Nossos resultados:

#F Experimental = 26.93
#F Critico = 2.946685

#Portanto, nossanova analise recaiu sobre a interpretacao 2
# nosso F critico eh menor que o F experimental, portanto,
# ha diferen?as nas medias, e agora faremos o Teste Tukey para
# para verificar qual ou quais os grupos est?o fora da m?dia

#Teste de Tukey

tk_teste <- TukeyHSD(anova)
tk_teste

#Resultado

#$`dados2$UF`
#         diff       lwr       upr     p adj
#MG-ES  12.015873 -19.07343  43.10518 0.7201054
#RJ-ES  -7.571429 -40.54662  25.40376 0.9230122
#SP-ES  76.771429  46.36981 107.17305 0.0000009
#RJ-MG -19.587302 -50.67661  11.50200 0.3337124
#SP-MG  64.755556  36.41053  93.10058 0.0000050
#SP-RJ  84.342857  53.94124 114.74448 0.0000001

#interprecao do Teste Tukey, o valor P(p adj) n?o pode
#ser menor que 5%(0.5), os grupos comparativos que apresentarem 
# o valor abaixo de 5%, s?o os comparativos que tem m?dias diferentes
# Em nosso novo exemplo veja abaixo no plot que SP
# tem todos seus compartivos com todos Estados abaixo
# de 5%, e no plot SP n?o toca a linha pontilhada de m?dia
# portanto, SP apresenta m?dia superiores de pre?os dos
#demais Estados

plot(tk_teste)


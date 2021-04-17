#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# Modelagem Estatistica Avançada 
# 
# Aplicações de Machine Learning
# Estudo de Caso: Concessão de Crédito, dataset: german credit data
#
#   Antonio Henrique Trotta - henrique_trotta@yahoo.com.br
#   Joao Ignacio de Almeida Junior - jigjunior@gmail.com
#   Rafael Nonato - rf.nonato@gmail.com
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#
#--------------------------------------------------------------------------------#
#                           RISCO DE CRÉDITO
#--------------------------------------------------------------------------------#
# O que é um default de crédito?
#  Um default de crédito ocorre quando um tomador de empréstimo não honra a sua 
#  obrigação de pagamento gerando uma perda para o emprestante (normalmente um banco).
#
# O que é a Perda Esperada (EL - Expected Loss)
#  É o produto de três outros fatores:
#    Probabilidade de Default (PD)
#    Exposição ao default (EAD)
#    Perda dado default (LGD - Loss Given Default)
#  EL = PD x EAD x LDG
#
# Informações utilizadas pelos bancos para quantificar o risco de crédito:
#  Pessoais do aplicante: renda, estado civil…
#  Comportamentais: saldo atual da conta corrente, prestações de empréstimos ou 
#    contas anteriores em atraso
#
#--------------------------------------------------------------------------------#
#                         CONCESSÃO DE CRÉTIDO
#--------------------------------------------------------------------------------#
# Um gerente de empréstimo quer desenvolver um modelo que forneça a probabilidade 
#   de um cliente em potencial ficar inadimplente. 
#
# A partir deste modelo deve-se escolher uma probabilidade de corte 
#   (aquela a partir da qual o cliente terá o crédito negado) que irá maximizar 
#   o lucro esperado para a carteira de clientes do banco. 
#
# Para calcular o lucro deve-se levar em conta as regras de negócios descritas na tabela a seguir. 
# De acordo com a mesma, o custo de oportunidade 
#   (proveniente do lucro de um crédito bom e do prejuízo decorrente de um crédito ruim) será :
#--------------------------------------------------------------------------------#
#  True Positive TP =  $100 ... para um cliente que paga o empréstimo
# False Negative FN = -$100 ... representa uma perda potencial (era mal pagador mas poderia pagar)
# False Positive FP = -$500 ... para um cliente que não paga o empréstimo (era bom pagador, mas não pagou)
#  True Negative TN =  $  0 ... O cliente que pagaria, mas não recebeu crédito...
#  
#--------------------------------------------------------------------------------#
#
# A base de dados sobre concessão de crédito na Alemanha contém observações de 
#   20 variáveis para 1000 casos de avaliação de crédito. 
#   Cada caso foi classificado como “good credit” (700 casos) ou “bad credit” (300 casos). 
#   Os novos casos podem ser avaliados com base nestas 20 variáveis de previsão. 
#
# Tomando por base estas 20 variáveis (ou um subconjunto delas), 
#   deseja-se desenvolver uma regra de credit score 
#   que possa ser utilizada para determinar 
#   se um novo pedido de crédito deverá ser classificado como 
#      "good credit risk" 
#            ou 
#      "bad credit risk".
#
#--------------------------------------------------------------------------------#
#                       DESENVOLVIMENTO DO MODELO
#--------------------------------------------------------------------------------#
# Divida os dados aleatoriamente entre treino (70%) e validação (30%) 
#   e desenvolva modelos de classificação a partir das seguintes técnicas:
#
#  - Escolha intuitiva das variáveis de entrada
#  - Correlação individual das variáveis de entrada com a
#      variável de resultado (***Excel***, R ou a sciki-learn)
#  - Análise discriminatória linear (***Excel***, ***R*** ou a scikit-learn)
#  - Regressão logística (Excel, ***R*** ou a scikit-learn)
#  - Opcional: Redes neurais (utilize a ***keras***)
#--------------------------------------------------------------------------------#
# Para cada um dos modelos, determine a probabilidade de corte ótima, isto é, 
#   o nível de corte para o qual o lucro obtido no conjunto de validação 
#   é máximo (vide a seguir). 
#
#  - Classifique os dados  de validação de acordo com a probabilidade prevista de sucesso.
#  - Para cada caso, calcule o custo/lucro de extender o crédito para este aplicante.
#  - Acrescente uma terceira coluna para calcular o lucro acumulado.
#  - Até qual aplicante deve-se ir para obter o maior lucro possível? 
#      Especifique este nível em percentis.
#  - Se este modelo for aplicado para novos créditos, 
#      qual probabilidade de corte deverá ser utilizada para a concessão 
#      (ou não) do crédito?
#
#--------------------------------------------------------------------------------#
#                           RELATÓRIO FINAL
#--------------------------------------------------------------------------------#
# Os resultados deverão ser apresentados em um relatório de no máximo 5 páginas 
#   no seguinte formato:
#  - Resumo
#  - Contexto:
#     - Definições de negócio para o caso
#     - Produto a ser ofertado
#     - Análise de variáveis
#     - Modelos desenvolvivos
#     - Resultados estatísticos e de negócios
#     - Modelo escolhido
#  - Conclusão
#
# A apresentação do relatório deverá ser feita em até 10 min, contendo as 
#   seguintes informações:
#  - Problema de negócios
#  - Modelos analisados e resultados financeiros
#  - Modelo escolhido
#
# Códigos
#  - Deverá ser fornecido ou um jupyter notebook ou um script R ou Rmd, 
#      com o código comentado.
#--------------------------------------------------------------------------------#
#                         FIM DO ENUNCIADO
#--------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------#
# 1) LENDO A BASE DE DADOS

arquivo = "german_data.csv"
dicionario = "german_dicionario.csv"

file = file.choose()
# Lendo os arquivos no formato .csv (SEMICOLON separated values)
dd = read.csv2(file, sep = ";", dec = '.', stringsAsFactors = T)
str(dd)

# Correcao do tipo da variavel resposta
dd = dd %>% mutate(response = as.factor(response))
levels(dd$response) = c("Good", "Bad")
str(dd)

#--------------------------------------------------------------------------------#
#                          ANÁLISE DAS VARIÁVEIS
#--------------------------------------------------------------------------------#
#                           Variáveis CONTÍNUAS
#--------------------------------------------------------------------------------#

# Variavel duration - meses com credito ativo
dfSummary(dd$duration)
summary(dd$duration)
par(mfrow=c(1,2))
hist(dd$duration, main="",xlab = "", col = 7)
boxplot(data=dd, duration~response,  main="", col = 7)
mtext("Duration - Meses com crédito ativo", side = 3, line = -2, outer = TRUE)


# Variável amount - Crédito concedido
dfSummary(dd$amount)
summary(dd$amount)
par(mfrow=c(1,2))
hist(dd$amount, main="",xlab = "", col = 7)
boxplot(data=dd, amount~response, outline=F,  main="", col = 7)
mtext("Amount - Crédito Concedido", side = 3, line = -2, outer = TRUE)

# Variável install_rate  - Installment rate: p% máx.de comprom.de renda
dfSummary(dd$install_rate)
summary(dd$install_rate)
par(mfrow=c(1,2))
hist(dd$install_rate, main="", breaks=seq(0,4), xlab = "", col = 7)
boxplot(data=dd, install_rate~response,  main="", col = 7)
mtext("Installment rate: p% máx.de comprom.de renda", side = 3, line = -2, outer = TRUE)

# Variável present_residence  - Tempo de residência no endereço atual
dfSummary(dd$present_residence)
summary(dd$present_residence)
par(mfrow=c(1,2))
hist(dd$present_residence, main="",xlab = "", breaks = seq(0,4), col = 7)
boxplot(data=dd, present_residence~response,  main="", col = 7)
mtext("Tempo de residência no endereço atual", side = 3, line = -2, outer = TRUE)

# Variável age - Idade 
dfSummary(dd$age)
summary(dd$age)
par(mfrow=c(1,2))
hist(dd$age, main="",xlab = "", col = 7)
boxplot(data=dd, age~response,  main="", col = 7)
mtext("Idade", side = 3, line = -2, outer = TRUE)

# Variável num_credits - Número de créditos neste banco 
dfSummary(dd$num_credits)
summary(dd$num_credits)
par(mfrow=c(1,2))
hist(dd$num_credits, main="",xlab = "", breaks=seq(0,5), col = 7)
boxplot(data=dd, num_credits~response,  main="", col = 7)
mtext("Número de créditos neste banco", side = 3, line = -2, outer = TRUE)

# Variável num_dependents - Número de dependentes
dfSummary(dd$num_dependents)
summary(dd$num_dependents)
par(mfrow=c(1,2))
hist(dd$num_dependents, main="",xlab = "", breaks=seq(-0.5,3.5), col = 7)
boxplot(data=dd, num_dependents~response,  main="", col = 7)
mtext("Número de dependentes", side = 3, line = -2, outer = TRUE)


#--------------------------------------------------------------------------------#
#                          ANÁLISE DAS VARIÁVEIS
#--------------------------------------------------------------------------------#
#                          Variáveis CATEGÓRICAS
#--------------------------------------------------------------------------------#

par(mfrow=c(1,1))

#--------------------------------------------------------------------------------#
# Variavel chk_acc - Conta Corrente
# A11 => x < 0 DM
# A12 => 0 < x < 200 DM
# A13 => x > 200 DM / salary 1 year
# A14 => no checking account
View(dfSummary(dd$chk_acc))
f = factor(dd$chk_acc)
levels(f)
levels(f) = c(" x < 0", "0 < x < 200", "x > 200", "no check")
plot(f, ylim = c(0,400), main = "Saldo Conta Corrente", col = 4)

#--------------------------------------------------------------------------------#
# Variavel history - historico de credito
# A30 =	nenhum crédito ou todos pagos em dia
# A31	= todos os créditos deste banco pagos em dia
# A32	= créditos sendo pagos em dia até o momento
# A33	= atrasos no pagamento no passado
# A34	= conta considerada crítica ou existência de créditos em outros bancos
dfSummary(dd$history)
f = factor(dd$history)
levels(f)
levels(f) = c("nenhum", "pago", "em dia", "em atraso", "critico")
plot(f, ylim = c(0,600), main = "Histórico de pagador", col = 4)
boxplot(data=dd, history~response,  main="", col = 7)

#--------------------------------------------------------------------------------#
# Variavel purpouse - Objetivo quando da tomada do crédito
# A40	car (new)
# A41	car (used)
# A42	furniture/equipment
# A43	radio/television
# A44	domestic appliances
# A45	repairs
# A46	education
# A47	vacation
# A48	retraining
# A49	business
# A410 others
dfSummary(dd$purpose)
f = factor(dd$purpose)
levels(f)
levels(f) = c("car (new)", "car (used)", "furniture/equipment", 
              "radio/tv", "domestic apply", "repairs", "education",
              "vacation", "retraining", "business", "others")
plot(f, ylim = c(0,300), main = "Objetivo do crédito", col = 4)

#--------------------------------------------------------------------------------#
# Variável savings - Saldo Poupança e Renda Fixa
# A61	=> x < 100 DM
# A62	=> 100 <= x <  500 DM
# A63	=> 500 <= x < 1000 DM
# A64	=> x >= 1000 DM
# A65	=> saldo desconhecido ou não existente
dfSummary(dd$savings)
f = factor(dd$savings)
levels(f)
levels(f) = c("x < 100 DM", "100 <= x < 500 DM", "500 <= x < 1000 DM", 
              "x >= 1000 DM", "desconhecido ou zero")
plot(f, ylim = c(0,600), main = "Saldo Poupança ou Renda Fixa", col = 4)

#--------------------------------------------------------------------------------#
# Variável employment - Tempo empregado
# A71	desempregado
# A72	x < 1 year
# A73	1 <= x < 4 years
# A74	4 <= x < 7 years
# A75	x >= 7 years
dfSummary(dd$employment)
f = factor(dd$employment)
levels(f)
levels(f) = c("desempregado", "x < 1 year", "1 <= x < 4 years", 
              "4 <= x < 7 years", "x >= 7 years")
plot(f, ylim = c(0,350), main = "Tempo empregado", col = 4)

#--------------------------------------------------------------------------------#
# Variável pers_status - Estado Civil e Sexo
# A91	masculino: divorciado/separado
# A92	feminimo: divorciada/separada/casada
# A93	masculino: solteiro
# A94	masculino: casado/viúvo
# A95	feminimo: solteira
dfSummary(dd$pers_status)
f = factor(dd$pers_status)
levels(f)
levels(f) = c("Masc. separado", "Fem. separada/casada", "Masc. solteiro", 
              "Masc. casado/viúvos", "Fem. solteira")
plot(f, ylim = c(0,600), main = "Estado Civil e Sexo", col = 4)

#--------------------------------------------------------------------------------#
# Variável guarantor - Fiadores
# A101	nenhum
# A102	co-aplicante
# A103	fiador
dfSummary(dd$guarantor)
f = factor(dd$guarantor)
levels(f)
levels(f) = c("nenhum", "co-aplicante", "fiador")
plot(f, ylim = c(0,1000), main = "Fiador", col = 4)

#--------------------------------------------------------------------------------#
# Variável real_state - Propriedades
# A121	imóveis
# A122	empresa, seguro de vida
# A123	carros ou outras propriedades não listadas acima
# A124	desconhecido ou nenhuma propriedade
dfSummary(dd$real_state)
f = factor(dd$real_state)
levels(f)
levels(f) = c("imóveis", "empresa", "carros+", "desconhecido")
plot(f, ylim = c(0,350), main = "Propriedades", col = 4)

#--------------------------------------------------------------------------------#
# Variável other_installment - Outros empréstimos
# A141	outros bancos
# A142	lojas
# A143	nenhum
dfSummary(dd$other_installment)
f = factor(dd$other_installment)
levels(f)
levels(f) = c("outros bancos", "lojas", "nenhum")
plot(f, ylim = c(0,800), main = "Outros empréstimos", col = 4)


#--------------------------------------------------------------------------------#
# Variável housing - Tipo de Residência
# A151	alugada
# A152	própria
# A153	sem custo
dfSummary(dd$housing)
f = factor(dd$housing)
levels(f)
levels(f) = c("alugada", "própria", "sem custo")
plot(f, ylim = c(0,700), main = "Tipo de Residência", col = 4)


#--------------------------------------------------------------------------------#
# Variável job - Emprego
# A171	Desempregado/Não especializado/Não residente
# A172	Não especializado/Residente
# A173	Especializado/Oficial
# A174	Gerência/Empresário/Altamente Qualificado/Oficial
dfSummary(dd$job)
f = factor(dd$job)
levels(f)
levels(f) = c("Desempregado\nNão especializado\nNão residente", 
              "Não especializado\nResidente", "Especializado\nOficial",
              "Gerência\nEmpresário\nAltamente Qualificado\nOficial")
plot(f, ylim = c(-50,700), main = "Emprego", col = 4)


#--------------------------------------------------------------------------------#
# Variável telephone - Possui linha telefônica
# A191	Não
# A192	Sim e registrada no seu nome
dfSummary(dd$telephone)
f = factor(dd$telephone)
levels(f)
levels(f) = c("Não", "Sim e registrada no seu nome")
plot(f, ylim = c(0,600), main = "Possui linha telefônica", col = 4)

#--------------------------------------------------------------------------------#
# Variável foreign - Trabalhador estrangeiro
# A201	Sim
# A202	Não
dfSummary(dd$foreign)
f = factor(dd$foreign)
levels(f)
levels(f) = c("Sim", "Não")
plot(f, ylim = c(0,1000), main = "Trabalhador estrangeiro", col = 4)

#--------------------------------------------------------------------------------#
# Variável response - Variável de saida
# 1	Credit Rating GOOD
# 2	Credit Rating BAD
plot(dd$response, ylim = c(0,1000), main = "Variável de Saída\nCREDIT RATING", col = 4)
dfSummary(dd$response)


#--------------------------------------------------------------------------------#
#                 PRE-PROCESSAMENTO DA BASE DE DADOS
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 1) Identificar variáveis

# Variaveis explicativas
X = dd %>% select(-response);
str(X)

# Variavel resposta
Y <- dd %>% select(response);
str(Y)

#--------------------------------------------------------------------------------#
# 2) Criacao de dummy variables para variaveis categoricas

library(caret)

# Criando o objeto que constroi as dummies das variaveis categoricas
# Formato_Saida: NOMECOLUNA_categoria
DUMMY_MODEL <- dummyVars(' ~ .', 
                         data = X, 
                         sep = '_', 
                         fullRank = T)
# Quantidade: apenas n - 1 dummies, onde n e o numero de categorias
# Obs: o algortimo deleta a primeira na ordem alfabetica)
DUMMY_MODEL

# Aplicando o objeto na base com as variaveis explicativas 
# (aqui precisa converter novamente em data.frame)
X <- as.data.frame(predict(DUMMY_MODEL, newdata = X))
str(X)

#--------------------------------------------------------------------------------#
# 3) [EXTRA] Padronizacao das variaveis

# Necessario para diversos algortimos (PCA, clusterizacao, SVM, Redes neurais)
# Criando o objeto com os valores de padronizacao das variaveis
PREPROC_VALUES <- preProcess(X, method = c('center', 'scale'))

# Aplicando o objeto na base com as variaveis explicativas
X_STANDARD <- predict(PREPROC_VALUES, X)
str(X_STANDARD)

#--------------------------------------------------------------------------------#
# 4) TRANSFORMANDO A VARIAVEL RESPOSTA

# Se a variável de resposta ainda não for Factor, precisamos tranformá-la
# Para classificacao muitos pacotes conseguem lidar com factor como variavel resposta

# Aqui precisamos saber: que evento queremos modelar?
#       Yes: aconteceu o evento de churn, o cliente cancelou o contrato
#     OU No: nao aconteceu o evento de churn, o cliente permaneceu como cliente

# Queremos medir a probabilidade do cliente cancelar o contrato e deixar de ser
# cliente da empresa ou a probabilidade do cliente estar fidelizado com ela?
# Aqui seguiremos olhando para o evento, e com isso nao e necessario alterar os 
# niveis do fator, mas caso precisasse inverter no momento antes da modelagem
# poderiamos usar:

head(Y)
str(Y)

levels(Y) = c("GOOD", "BAD");
str(Y)

#--------------------------------------------------------------------------------#
# 5) BASE FINAL 

# Conectando as variaveis explicativas e a variavel resposta processadas
# DATA_CLAS_PREPROC <- cbind(X_CLAS,Y_CLAS)
dd_PREPROCESS <- bind_cols(X_STANDARD,Y)
dd_PREPROCESS



#--------------------------------------------------------------------------------#
#                           REGRESSÃO LINEAR
#--------------------------------------------------------------------------------#
#                              LOGISTICA
#--------------------------------------------------------------------------------#


set.seed(123) # garantindo reprodutibilidade da amostra

#--------------------------------------------------------------------------------#
# 1) Divisao de Dataset - Treino e Teste

index_treino <- createDataPartition(y = dd_PREPROCESS$response, p = 0.7, list = F)
trainning_set <- dd_PREPROCESS[index_treino, ] # base de treino: 70%
testing_set  <- dd_PREPROCESS[-index_treino, ] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(trainning_set$response)
summary(testing_set$response)

table(trainning_set$response)
table(testing_set$response)

prop.table(table(trainning_set$response))
prop.table(table(testing_set$response))


#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de regressao logistica

?glm
modelo <- glm(data = trainning_set, 
               formula = response ~ .,
               family = binomial(link="logit"))
modelo

# Determinando os coeficientes das variaveis explicativas
summary(modelo)

#--------------------------------------------------------------------------------#
# 3) Avaliar correlação de multicolinearidade

library(car)
vif(modelo)
round(vif(modelo),2)

# https://www.statisticshowto.com/variance-inflation-factor/
# O calculo do vif nao encontrou correlacao, geralmente valores > 5 de VIF indicam correlacao

# Interpreting the Variance Inflation Factor
# ------------------------------------------
# 1 = not correlated.
# Between 1 and 5 = moderately correlated.
# Greater than 5 = highly correlated.
# ------------------------------------------

# 6 - history_A32 = 7.31
# 8 - history_A34 = 5.30
# 24- employment_A73 = 6.24
# 26- employment_A75 = 5.27
# 29- pers_status_A93 = 5.13
# 43- job_A172 = 10.08
# 44- job_A173 = 13.96
# 45- job_A174 =  7.85

# Removendo as altas correlações
names(trainning_set)
trainning_set = trainning_set[,-c(6,8,24,26,29,43,44,45) ]
names(trainning_set)

# treinando novamente sem correlacoes
modelo <- glm(data = trainning_set, formula = response ~ .,family = binomial(link="logit"))
modelo
summary(modelo)
round(vif(modelo),2)


#--------------------------------------------------------------------------------#
# 4) Refinando o ajuste atraves do processo stepwise

library(MASS)
?stepAIC
modelo_fit_STEP = stepAIC(modelo, direction = 'both', trace = TRUE)
modelo_fit_STEP

# Determinando os coeficientes das variaveis explicativas
summary(modelo_fit_STEP)

#--------------------------------------------------------------------------------#
# 5) Realizando as predicoes

# Probabilidade pela regressao full
Y_PROB_TRAIN <- predict(modelo, type = 'response') 
Y_PROB_TEST  <- predict(modelo, newdata = testing_set, type = 'response')
head(Y_PROB_TRAIN)

# Probabilidade pela regressao stepwise
Y_PROB_TRAIN_STEP <- predict(modelo_fit_STEP, type = 'response')  
Y_PROB_TEST_STEP  <- predict(modelo_fit_STEP, newdata = testing_set, type = 'response')
head(Y_PROB_TRAIN_STEP)


# [EXTRA] Verificando a aderencia do ajuste logistico (teste Spiegelhalter)
library(rms)
?val.prob
val.prob(Y_PROB_TRAIN, 
         ifelse(trainning_set$response == 'Good', 1, 0), 
         smooth = F)[c('S:z','S:p')]
# p valor > 5%, nao podemos rejeitar a hipotese nula

#--------------------------------------------------------------------------------#
# 6) Avaliando a performance dos modelos e existencia de overfitting

# Regressao full
library(hmeasure) 
HMeasure(trainning_set$response,Y_PROB_TRAIN)$metrics
HMeasure(testing_set$response, Y_PROB_TEST)$metrics

# Regressao com stepwise
HMeasure(trainning_set$response,Y_PROB_TRAIN_STEP)$metrics
HMeasure(testing_set$response, Y_PROB_TEST_STEP)$metrics

# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", com mesma performance

# Modelo final
# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", i.e. com menos vari?veis e com mesma performance
MDL_FINAL <- modelo_fit.STEP

#--------------------------------------------------------------------------------#
# 7) Importancia das variaveis (Modelo final)

#https://cran.r-project.org/web/packages/dominanceanalysis/vignettes/da-logistic-regression.html
anova(MDL_FINAL, test= "Chisq")

#--------------------------------------------------------------------------------#
# 8) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)

# Label observado
Y_OBS <- TEST_SET$CHURN

# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)
Y_CLAS1 <- factor(ifelse(Y_PROB_TEST.STEP > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes')) 
Y_CLAS2 <- factor(ifelse(Y_PROB_TEST.STEP > 0.3,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes'))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Yes')
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = 'Yes')

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))


# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST.STEP,
                  Y_OBS  = TEST_SET$CHURN)

boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Boxplot probs', cex.main = 1.2, cex.axis = 1.2, 
        xlab = 'PROBABILITIES', ylab = 'Target',
        ylim = c(0,1), horizontal = T,
        col = c('darkorange','darkorange4'), border = 'gray20')
hist(AUX$Y_PROB, breaks = 20, xlim = c(0,1),
     main = 'Histogram probs', cex.main = 1.2, 
     xlab = 'PROBABILITIES', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

graphics.off()

#--------------------------------------------------------------------------------#
# 9) Curva ROC

library(pROC)
ROC1 <- roc(TRAIN_SET$CHURN,Y_PROB_TRAIN.STEP)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$CHURN,Y_PROB_TEST.STEP)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FIM
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

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
#  Comportamentais: saldo atual da conta corrente, prestações de empréstimos ou contas anteriores em atraso
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
#  $100 para um cliente que paga o empréstimo
#  -$500 para um cliente que não paga o empréstimo
#   O cliente que pagaria, mas não recebeu crédito...
#   ... representa uma perda potencial de (-$100)
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
#  - Correlação individual das variáveis de entrada com a variável de resultado (***Excel***, R ou a sciki-learn)
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

library(summarytools)
library(dplyr)

# file = "german_data.csv"
# dic = "german_dicionario.csv"
file = file.choose()
dd = read.csv2(file)
names(dd)
head(dd)


#--------------------------------------------------------------------------------#
#                          ANÁLISE DAS VARIÁVEIS
#--------------------------------------------------------------------------------#
#                           Variáveis CONTÍNUAS
#--------------------------------------------------------------------------------#

# Variavel duration - meses com credito ativo
dfSummary(dd$duration)
summary(dd$duration)
par(mfrow=c(1,2))
boxplot(dd$duration, main="", col = 7)
hist(dd$duration, main="",xlab = "", col = 7)
mtext("Duration - Meses com crédito ativo", side = 3, line = -2, outer = TRUE)

# Variável amount - Crédito concedido
dfSummary(dd$amount)
summary(dd$amount)
par(mfrow=c(1,2))
boxplot(dd$amount, main="", col = 7)
hist(dd$amount, main="",xlab = "", col = 7)
mtext("Amount - Crédito Concedido", side = 3, line = -2, outer = TRUE)

# Variável install_rate  - Installment rate: p% máx.de comprom.de renda
dfSummary(dd$install_rate)
summary(dd$install_rate)
par(mfrow=c(1,2))
boxplot(dd$install_rate, main="", col = 7)
hist(dd$install_rate, main="",xlab = "", col = 7)
mtext("Installment rate: p% máx.de comprom.de renda", side = 3, line = -2, outer = TRUE)

# Variável present_residence  - Tempo de residência no endereço atual
dfSummary(dd$present_residence)
summary(dd$present_residence)
par(mfrow=c(1,2))
boxplot(dd$present_residence, main="", col = 7)
hist(dd$present_residence, main="",xlab = "", breaks = seq(0,5), col = 7)
mtext("Tempo de residência no endereço atual", side = 3, line = -2, outer = TRUE)

# Variável age - Idade 
dfSummary(dd$age)
summary(dd$age)
par(mfrow=c(1,2))
boxplot(dd$age, main="", col = 7)
hist(dd$age, main="",xlab = "", col = 7)
mtext("Idade", side = 3, line = -2, outer = TRUE)

# Variável num_credits - Número de créditos neste banco 
dfSummary(dd$num_credits)
summary(dd$num_credits)
par(mfrow=c(1,2))
boxplot(dd$num_credits, main="", col = 7)
hist(dd$num_credits, main="",xlab = "", breaks=seq(0,5), col = 7)
mtext("Número de créditos neste banco", side = 3, line = -2, outer = TRUE)

# Variável num_dependents - Número de dependentes
dfSummary(dd$num_dependents)
summary(dd$num_dependents)
par(mfrow=c(1,2))
boxplot(dd$num_dependents, main="", col = 7)
hist(dd$num_dependents, main="",xlab = "", breaks=seq(-1,3), col = 7)
mtext("Número de dependentes", side = 3, line = -2, outer = TRUE)


#--------------------------------------------------------------------------------#
#                          ANÁLISE DAS VARIÁVEIS
#--------------------------------------------------------------------------------#
#                          Variáveis CATEGÓRICAS
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# Variavel chk_acc - Conta Corrente
# A11 => x < 0 DM
# A12 => 0 < x < 200 DM
# A13 => x > 200 DM / salary 1 year
# A14 => no checking account
dfSummary(dd$chk_acc)

#--------------------------------------------------------------------------------#
# Variavel history - historico de credito
# A30 =	nenhum crédito ou todos pagos em dia
# A31	= todos os créditos deste banco pagos em dia
# A32	= créditos sendo pagos em dia até o momento
# A33	= atrasos no pagamento no passado
# A34	= conta considerada crítica ou existência de créditos em outros bancos
dfSummary(dd$history)

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

#--------------------------------------------------------------------------------#
# Variável savings - Saldo Poupança e Renda Fixa
# A61	=> x < 100 DM
# A62	=> 100 <= x <  500 DM
# A63	=> 500 <= x < 1000 DM
# A64	=> x >= 1000 DM
# A65	=> saldo desconhecido ou não existente
dfSummary(dd$savings)

#--------------------------------------------------------------------------------#
# Variável employment - Tempo empregado
# A71	desempregado
# A72	x < 1 year
# A73	1 <= x < 4 years
# A74	4 <= x < 7 years
# A75	x >= 7 years
dfSummary(dd$employment)
modelo1 = lm(dados = dd, formula = dd$response ~ dd$employment)
summary(modelo1)

#--------------------------------------------------------------------------------#
# Variável pers_status - Estado Civil e Sexo
# A91	masculino: divorciado/separado
# A92	feminimo: divorciada/separada/casada
# A93	masculino: solteiro
# A94	masculino: casado/viúvo
# A95	feminimo: solteira
dfSummary(dd$pers_status)

#--------------------------------------------------------------------------------#
# Variável guarantor - Fiadores
# A101	nenhum
# A102	co-aplicante
# A103	fiador
dfSummary(dd$guarantor)

#--------------------------------------------------------------------------------#
# Variável real_state - Propriedades
# A121	imóveis
# A122	empresa, seguro de vida
# A123	carros ou outras propriedades não listadas acima
# A124	desconhecido ou nenhuma propriedade
dfSummary(dd$real_state)

#--------------------------------------------------------------------------------#
# Variável other_installment - Outros empréstimos
# A141	outros bancos
# A142	lojas
# A143	nenhum
dfSummary(dd$other_installment)

#--------------------------------------------------------------------------------#
# Variável housing - Tipo de Residência
# A151	alugada
# A152	própria
# A153	sem custo
dfSummary(dd$housing)

#--------------------------------------------------------------------------------#
# Variável job - Emprego
# A171	Desempregado/Não especializado/Não residente
# A172	Não especializado/Residente
# A173	Especializado/Oficial
# A174	Gerência/Empresário/Altamente Qualificado/Oficial
dfSummary(dd$job)

#--------------------------------------------------------------------------------#
# Variável telephone - Possui linha telefônica
# A191	Não
# A192	Sim e registrada no seu nome
dfSummary(dd$telephone)

#--------------------------------------------------------------------------------#
# Variável foreign - Trabalhador estrangeiro
# A201	Sim
# A202	Não
dfSummary(dd$foreign)

#--------------------------------------------------------------------------------#
# Variável response - Variável de saida
# 1	Credit Rating GOOD
# 2	Credit Rating BAD
dfSummary(dd$response)





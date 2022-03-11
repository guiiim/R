#
#O objetivo deste mini-projeto é praticar o conhecimento adquirido ao longo dos
#capítulos anteriores.
#
#Neste mini-projeto, seu trabalho é avaliar o risco de concessão de crédito a clientes
#de instituições financeiras, tarefa cada vez mais comum atualmente!
#
#O dataset fornecido terá diversas variáveis e nem todas serão relevantes para
#construção do modelo. 
#Seu trabalho requer uma análise no dataset, conversão e 
#normalização das variáveis e então construção do modelo.
#
#Guilherme Magalhães - 07/03/2022
#
#
######################################################################################
#Configura local de trabalho
setwd("D:/Dados/Downloads/18")
getwd()

######################################################################################
#Carrega o dataset
credit.df = read.csv("credit_dataset.csv", header = TRUE, sep = ",")

#Primeira visualização do dataset
#View(credit.df)
str(credit.df)

######################################################################################
# Alterando variáveis para fator
varAlterar <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
            'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
            'marital.status', 'guarantor', 'residence.duration', 'current.assets',
            'other.credits', 'apartment.type', 'bank.credits', 'occupation',
            'dependents', 'telephone', 'foreign.worker')

for (variable in varAlterar)
  {
   credit.df[[variable]] <- as.factor(credit.df[[variable]])
  }
str(credit.df)

######################################################################################
# Colocando variáveis na mesma escala
varAlterar <- c("credit.duration.months", "age", "credit.amount")

for (variable in varAlterar)
  {
    credit.df[[variable]] <- scale(credit.df[[variable]], center=T, scale=T)
  }
str(credit.df)

######################################################################################
#View(credit.df)
######################################################################################
#Verificando variáveis importantes (Fearture Selection)
#Será utilizado o algoritmo Random Forest para avaliar quais são os melhores parâmetros para o modelo
library(randomForest)
modelo <- randomForest( credit.rating ~ ., 
                        data = credit.df, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo)

#Variáveis mais importantes de acordo com o modelo:
#account.balance, credit.duration.months, previous.credit.payment.status, credit.amount
#current.assets
#
#account.balance, credit.amount, credit.duration.months, previous.credit.payment.status,
#age, savings, current.assets
#
#Essas serão as características utilizadas para criar o modelo

######################################################################################
#Divide dados em treino  teste
indexes <- sample(1:nrow(credit.df), size = 0.6 * nrow(credit.df))
dadosTreino <- credit.df[indexes,]
dadosTeste <- credit.df[-indexes,]

######################################################################################
#Cria modelo de Classificação com todas variáveis
modeloRFall <- randomForest(credit.rating ~ .,
                            data = dadosTreino, 
                            ntree = 100, 
                            nodesize = 10)

print(modeloRFall)

#Cria modelo de Classificação com variáveis selecionadas
modeloRFsome <- randomForest(credit.rating ~ account.balance
                             + credit.amount 
                             + credit.duration.months 
                             + previous.credit.payment.status 
                             + age 
                             + savings 
                             + current.assets,
                             data = dadosTreino, 
                             ntree = 100, 
                             nodesize = 10)

print(modeloRFsome)

######################################################################################
#Verifica resultados dos dois modelos
#?predict
#?data.frame
previsoesRFall <- data.frame(original = dadosTeste$credit.rating,
                             previsto = predict(modeloRFall, newdata = dadosTeste))

previsoesRFsome <- data.frame(original = dadosTeste$credit.rating,
                              previsto = predict(modeloRFsome, newdata = dadosTeste))

#View(previsoesRFall)
#View(previsoesRFsome)

######################################################################################
#Gera Confusion Matrix com a biblioteca caret
library(caret)
confusionMatrix(previsoesRFall$original, previsoesRFall$previsto)

######################################################################################
#Gera Confusion Matrix com a biblioteca caret
#Gera curva ROC
library("ROCR")

class1 <- predict(modeloRFsome, newdata = dadosTeste, type = 'prob')
class2 <- dadosTeste$credit.rating

pred <- prediction(class1[,2], class2)
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))


pacotes <- c('rattle','rnn','ggplot2','dplyr','tidyverse','viridis','rpart','rpart.plot','lme4',
             'gtools','Rmisc','scales','caret','neuralnet','gamlss','gamlss.add','readr','MLmetrics','nnfor')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#**********************************************************************************


#carregando database
df_levels<- read_csv("data_base_filtro.csv", 
                               col_types = cols(date_00 = col_date(format = "%Y-%m-%d")), 
                               locale = locale(decimal_mark = ",", grouping_mark = "."))
date_fix<- c(strtrim(df_levels$date_00,10))
df_levels$date_00<-as.Date(date_fix)
df_levels$Index<-43

I<-43

data_for_forecast <- read_csv("data_for_forecast.csv")
date_fix<- c(strtrim(data_for_forecast$date_00,10))
data_for_forecast$date_00<-as.Date(date_fix)
names(data_for_forecast)[names(data_for_forecast)=="date_00"]<-"date"
names(data_for_forecast)[names(data_for_forecast)=="dolar_06"]<-"dolar"
names(data_for_forecast)[names(data_for_forecast)=="aco_06"]<-"aco"
names(data_for_forecast)[names(data_for_forecast)=="ipca_06"]<-"ipca"

#preparando tabela de previsão final
df_results<-as.data.frame(df_levels$Index)
names(df_results)[names(df_results)=="df_levels$Index"]<-"Index"
#df_results['Partnumber']<-df_levels$`Partnumber`
df_results['material']<-df_levels$material



#preparando tabela de regressão com apenas os dados para c�lccular a regressão
df_regressao<-df_levels[df_levels['Index']==I,]
names(df_regressao)[names(df_regressao)=="Index"]<-"Index"
names(df_regressao)[names(df_regressao)=="PN-Short"]<-"PN-Short"
names(df_regressao)[names(df_regressao)=="date_00"]<-"date"
names(df_regressao)[names(df_regressao)=="material"]<-"material"
names(df_regressao)[names(df_regressao)=="dolar_00"]<-"dolar"
names(df_regressao)[names(df_regressao)=="aco_00"]<-"aco"
names(df_regressao)[names(df_regressao)=="ipca_00"]<-"ipca"
df_regressao<-df_regressao[,c("Index","date","material","dolar","aco","ipca")]
#df_regressao<-bind_rows(df_regressao,data_for_forecast)
#df_regressao$`PN-Short`<-df_regressao[1,"Partnumber"]
df_regressao$Index<-df_regressao[1,"Index"]


#df_regressao<-df_regressao[,-c(1:2)]

df_regressao<-df_regressao[-c(120:127),]# retirando o forcast que est� com os valores errados
db_filters_scale<-as.data.frame(scale(df_regressao[,-c(1:2)])) #normaliza os dados


set.seed(100)
db_filters_scale['amostragem'] <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
                                         size=nrow(db_filters_scale), # O tamanho da amostragem 
                                         replace=TRUE, # Amostragem com reposição (de c(1,2))
                                         prob=c(0.75,0.25)) # A probabilidade de ser 1 é 80%, de ser 2 é 20%


#fazendo os ultimos dados do forecast serem teste
db_filters_scale[c(nrow(db_filters_scale)-nrow(data_for_forecast)):nrow(db_filters_scale),'amostragem']<-2

# Dividir amostras de treino e teste e resultado #
db_filters_treino <- db_filters_scale[db_filters_scale$amostragem==1,]# Amostra de treino: n==1 (os 80%)

#teste
db_filters_teste <- db_filters_scale[db_filters_scale$amostragem==2,]# Amostra de treino: n==1 (os 80%)

#preparando para os resultados
df_regressao['amostragem']<-db_filters_scale$amostragem


set.seed(100)
nn_00<-neuralnet(material ~ ipca  + aco +dolar ,
                 data=db_filters_treino,
                 hidden = c(20,10,5),
                 act.fct="tanh",
                 linear.output = T,
                 err.fct = "sse",
                 rep=1)

result_treino_00<-predict(nn_00,db_filters_treino)
result_teste_00<-predict(nn_00,db_filters_teste)
result<-predict(nn_00,db_filters_scale)

db_filters_teste['result']<-result_teste_00

r2_nn_00_treino<-cor.test(db_filters_treino$material,result_treino_00, method = "pearson")
r2_nn_00_teste<-cor.test(db_filters_teste$material,result_teste_00, method = "pearson")
r2_nn_00_total<-cor.test(db_filters_scale$material,result, method = "pearson")

df_regressao['result']<-c(result*sd(df_regressao$material, na.rm = TRUE) + mean(df_regressao$material, na.rm = TRUE))

r2_nn_teste<-r2_nn_00_teste$estimate

print(r2_nn_00_treino)
print(r2_nn_00_teste)
print(r2_nn_00_total)

df_final<-as.matrix(df_regressao)
write.csv(df_final,file = "Result_NN_TCC.csv")

df_scale<-db_filters_scale
df_scale['result']<-result
df_scale<-as.matrix(df_scale)
write.csv(df_scale,file = "df_scale.csv")

a<-as_data_frame(db_filters_scale$material)
a$total<-as_data_frame(result)
ggplot(a, aes(a$value, a$total$V1))
ggplot(a) +
  geom_point(aes(a$value, a$total$V1))

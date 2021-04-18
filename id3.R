library(dplyr)

# implementando o id3 na unha -------------------------------------------------

#carregando dataset jogar tenis
df <- readr::read_csv(file = "jogar-tenis.csv") %>% 
  select(-Dia) %>% 
  rename(Jogartenis=`Jogar Tênis`)

#criando a função definiemodelo
defineformula <- function(dados,formula){
  dados <- dados
  formula <- formula
  atributos <- as.vector(strsplit(x = formula ,split = '~')[[1]])
  resposta <- trimws(atributos[1])
  preditoras <- as.vector(
    trimws(
      strsplit(
        x = trimws(atributos[2]) ,
        split = "[+]"
      )[[1]]
    )
  )
  classes <- levels(as.factor(dados[[resposta]]))
  return(list(dados = dados, formula = formula,resposta=resposta,preditoras=preditoras, classes=classes))
}


desenho_modelo <-  defineformula(df,"Jogartenis~Tempo+Temperatura+Umidade+Vento")
desenho_modelo$dados
desenho_modelo$formula
desenho_modelo$resposta
desenho_modelo$preditoras
desenho_modelo$classes

#definindo a função entropia
entropia <- function(vetor){
  entropia_temp = - sum(vetor/sum(vetor) * log2(vetor/sum(vetor)))
  entropia_temp
  entropia_final <- ifelse(is.nan(entropia_temp),0,entropia_temp)
  return(entropia_final)
} 

#testando a função entropia
entropia(table(desenho_modelo$dados[[desenho_modelo$resposta]]))

#Criando a função que calcula o ganho de informação
ganhoinfo <- function(dados,variavel,alvo){
  return(
    entropia(table(dados[,alvo])) -
      sum(prop.table(table(dados[,variavel]))*  
            apply(table(dados[[variavel]],dados[[alvo]]),1,entropia)
      )
  )
}

#testando a função Ganho de informação
ganhoinfo(desenho_modelo$dados,desenho_modelo$preditoras[1],desenho_modelo$resposta)

#criando vetor auxiliar
pred_restantes <- desenho_modelo$preditoras

#Calculando ganho de informação de todas as preditodas
ganhodeinformacao <- NULL
for(i in seq_along(pred_restantes)){
  ganhodeinformacao[i] <- ganhoinfo(desenho_modelo$dados,pred_restantes[i],desenho_modelo$resposta) 
}
names(ganhodeinformacao) <- pred_restantes
ganhodeinformacao

#selecionando informações do atributo com maior ganho de informação
fatiar = list(nome = NULL, classes = NULL, nclasses = NULL)
fatiar[[1]] <- names(ganhodeinformacao[ganhodeinformacao==max(ganhodeinformacao)])
fatiar[[2]] <- levels(as.factor(desenho_modelo$dados[[fatiar[[1]]]]))
fatiar[[3]] <- length(fatiar[[2]])

#atualizando pred_restante
pred_restantes <- pred_restantes[pred_restantes!=fatiar[[1]]]
pred_restantes

#construindo a árvore

arvore <- data.frame(
  no = 1,
  no_up = NA,
  var = fatiar[[1]],
  rot = NA,
  int = 1,
  n = nrow(desenho_modelo$dados),
  folha = 0
  )

# vetor auxiliar de preditores restantes

arvore[arvore[['no']]==1,pred_fatiar]

df_temp <- desenho_modelo$dados[desenho_modelo$dados[[fatiar[[1]]]]==fatiar[[2]][1],]

#Calculando ganho de informação de todas as preditodas
ganhodeinformacao <- NULL
for(i in seq_along(pred_restantes)){
  ganhodeinformacao[i] <- ganhoinfo(df_temp,pred_restantes[i],desenho_modelo$resposta) 
}
names(ganhodeinformacao) <- pred_restantes
ganhodeinformacao

# entropia_temp = - sum(0/sum(0) * log2(0/sum(0)))
# is.nan(entropia_temp)
# ifelse(is.nan(entropia_temp),5,6)
# entropia(c(1,0))

# entropia2 <- function(vetor){
#   tab_vet <- table(vetor)
#   return(- sum(tab_vet/sum(tab_vet) * log2(tab_vet/sum(tab_vet))))
# } 

#entropia2(df[,"Jogartenis"])

#ganho de informação
# dados <- df
# variavel <- "Vento"
# alvo <- "Jogartenis"
# 
# entropia(table(dados[,alvo])) -
#   sum(
#     prop.table(table(dados[,variavel]))*
#       apply(with(dados, table(Vento, Jogartenis)), 1, entropia)
#   )

# ganhoinfo <- function(dados,variavel,alvo){
#   return(
#     entropia(table(dados[,alvo])) -
#       sum(prop.table(table(dados[,variavel]))*  
#             apply(
#               table(dplyr::select(dados,variavel,alvo)),
#               1,
#               entropia
#             )
#       )
#   )
# }



#apply(
#  table(
#    # paste0(substitute(data),"$",variavel), 
#    # paste0(substitute(data),"$",alvo)
#    )
#  ), 
#  1, 
#  entropia
#)

# teste_substitute <- function(data,variavel){
#   print(paste0(substitute(data),"$",variavel))
# }
  

# teste_apply <- function(dados,variavel,alvo){
#   return(
#     apply(
#           table(
#             dplyr::select(dados,variavel,alvo)
#         ),
#         1,
#         entropia
#     )
#   )
# }
# 
# 
# apply(with(dados, table(variavel, alvo)), 1, entropia)
# 
# table(select(df,Vento,Jogartenis))

# dados <- df
# variavel <- "Vento"
# alvo <- "Jogartenis"
# 
# table(dados[[variavel]],dados[[alvo]])
# 
# paste0(substitute(df),"$","Vento") %>% noquote()
# 
# ganhoinfo(df,"Vento","Jogartenis")


# utilizando o rpart para ajustar a árvore ------------------------------------


#carregando o dataset
jogartenis <- readr::read_csv(file = "jogar-tenis.csv") %>%
  select(-Dia) %>%
  rename(Jogartenis=`Jogar Tênis`)




arvore_ajustada <- rpart(
  #formula = Jogartenis ~ Tempo+Temperatura+Umidade+Vento,
  Jogartenis ~ .,
  data = jogartenis,
  method="class",
  parms=list(split="information")
)

rpart.plot(arvore_ajustada)


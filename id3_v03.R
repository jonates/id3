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

entropia2 <- function(vetor){
  tab_vetor <- table(vetor)
  entropia_temp = - sum(tab_vetor/sum(tab_vetor) * log2(tab_vetor/sum(tab_vetor)))
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
pred_restantes

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
fatiar

#atualizando pred_restante
pred_restantes <- pred_restantes[pred_restantes!=fatiar[[1]]]
pred_restantes

#construindo a árvore

arvore <- data.frame(
  no = as.numeric(1),
  profundidade = as.numeric(0),
  no_up = as.numeric(0),
  var = fatiar[[1]],
  rot = "",
  int = 1,
  n = nrow(desenho_modelo$dados),
  entropia = entropia(table(desenho_modelo$dados[[desenho_modelo$resposta]]))
  )
arvore$folha <- ifelse(arvore$entropia==0,1,0)
arvore$condicao <- ""

#Atualizando a árvore

arvore_temp <- data.frame(
  no = (as.numeric(max(arvore[['no']]))+1):(as.numeric(max(arvore[['no']]))+as.numeric(fatiar[[3]])),
  profundidade = as.numeric(1),
  no_up = rep(tail(arvore[['no']],1),fatiar[[3]]),
  var = rep(fatiar[[1]],fatiar[[3]]),
  rot = fatiar[[2]],
  int = rep(0,fatiar[[3]]),
  n = as.numeric(table(desenho_modelo$dados[[fatiar[[1]]]])),
  entropia = tapply(
    desenho_modelo$dados[[desenho_modelo$resposta]],
    desenho_modelo$dados[[fatiar[[1]]]],
    entropia2
  )
)
arvore_temp$folha <- ifelse(arvore_temp$entropia==0,1,0)
arvore_temp$condicao = paste(arvore_temp$var,"==",arvore_temp$rot)
arvore <- rbind(arvore,arvore_temp)
View(arvore)

profundidade <- 1

nomes_no <-  unique(arvore[arvore[['profundidade']]==profundidade,'var'])
nomes_no
for (j in seq_along(nomes_no)) {
  
  ramos_no <- arvore[arvore[['var']]==nomes_no[j],'rot']
  ramos_no <- ramos_no[ramos_no!=""]
  ramos_no


  for (k in seq_along(ramos_no)) {
    
    if (arvore[(arvore$var==nomes_no[j]) & (arvore$rot==ramos_no[k]) , 'entropia']!=0){
    
      df_temp <- desenho_modelo$dados[desenho_modelo$dados[[nomes_no]]==ramos_no[k],]
      df_temp
      
      #Calculando ganho de informação de todas as preditodas
      ganhodeinformacao <- NULL
      for(l in seq_along(pred_restantes)){
        ganhodeinformacao[l] <- ganhoinfo(df_temp,pred_restantes[l],desenho_modelo$resposta) 
      }
      names(ganhodeinformacao) <- pred_restantes
      ganhodeinformacao
      
      novo_no <- names(ganhodeinformacao[ganhodeinformacao==max(ganhodeinformacao)])
      novo_no
      novos_ramos <- levels(as.factor(desenho_modelo$dados[[novo_no]]))
      novos_ramos
      n_novos_ramos <- length(novos_ramos)
      n_novos_ramos
      
      arvore_temp <- data.frame(
        no = (as.numeric(max(arvore[['no']]))+1):(as.numeric(max(arvore[['no']]))+as.numeric(n_novos_ramos)),
        profundidade = rep(profundidade+1,n_novos_ramos),
        no_up = rep(arvore[(arvore$profundidade==1 & arvore$rot==ramos_no[k]), 'no'],n_novos_ramos),
        var = rep(novo_no,n_novos_ramos),
        rot = novos_ramos,
        int = rep(0,n_novos_ramos),
        n = as.numeric(table(df_temp[[novo_no]])),
        entropia = tapply(
          df_temp[[desenho_modelo$resposta]],
          df_temp[[novo_no]],
          entropia2
        )
      )
      arvore_temp$folha <- ifelse(arvore_temp$entropia==0,1,0)
      arvore_temp$condicao = paste(
        arvore$condicao[arvore$no==arvore_temp$no_up],
        "&",arvore_temp$var,"==",arvore_temp$rot
      )
      arvore <- rbind(arvore,arvore_temp)
      pred_restantes <- pred_restantes[pred_restantes!=novo_no] 
      pred_restantes
      }
  }      
}

View(arvore)

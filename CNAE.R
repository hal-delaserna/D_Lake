#        rm(list = ls())
#     options(editor = 'notepad')
library(tidyverse)

source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R')

#      estrutura da CNAE: A  x x.x x-x/x x
#       seção ____________|  | | | | | | |      B e C - INDÚSTRIAS EXTRATIVAS e INDÚSTRIAS DE TRANSFORMAÇÃO
#       divisão _____________|_| | | | | |
#       grupo ___________________| | | | |
#       classe ____________________|_| | |
#       subclasse _____________________|_|



# Carregamento CNAE 2.0 ----
CNAE_Subclasses_2_0 <-
  read.table(file = 'D:/Users/humberto.serna/Documents/D_Lake/CNAE_2_0.txt', 
             header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
             colClasses = c('character'), encoding = "ANSI")

colnames(CNAE_Subclasses_2_0) <- c("subclasse", "subclasse.descrição", "classe", 
                                   "classe.descrição", "grupo", "grupo.descrição", "divisão", 
                                   "divisão.descrição", "seção", "seção.descrição", "grupamento", 
                                   "grande.Grupamento")

CNAE_Subclasses_2_0$grupo.descrição <- 
  CNAE_Subclasses_2_0$grupo.descrição %>% FUNA_removeAcentos() %>% FUNA_maiusculas()
  


# Carregamento CNAE 2.3 ----
CNAE_Subclasses_2_3 <-
  read.table(file = 'D:/Users/humberto.serna/Documents/D_Lake/CNAE_Subclasses_2_3_Estrutura_Detalhada.csv', 
             header = TRUE, sep = ";", stringsAsFactors = FALSE, 
             colClasses = c('character'), encoding = "ISO-8859")


CNAE_Subclasses_2_3[CNAE_Subclasses_2_3$grupo != "0",]$denominação <- 
  CNAE_Subclasses_2_3[CNAE_Subclasses_2_3$grupo != "0",]$denominação %>% FUNA_removeAcentos() %>% FUNA_maiusculas()


# __________ formato painel DataFrame ----
cnae <- CNAE_Subclasses_2_3
for (i in 2:nrow(cnae)) {
  if (cnae[i, 1] == "0") {
    cnae[i, 1] <- cnae[i - 1, 1]
  }}

for (i in 2:nrow(cnae)) {
  for (j in 2:length(cnae)) {
    if ((cnae[i, j] == "0") & (cnae[i, j - 1] == cnae[i - 1, j - 1])) {
      cnae[i, j] <- cnae[i - 1, j]
    }}}


  


# __________ uniformização dos Cods CNAE ----
# na base Novo Caged as CNAEs são números inteiros

cnae$grupo <- 
  cnae$grupo %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "")  %>% as.integer() 

cnae$classe <- 
  cnae$classe %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "")  %>% as.integer() 

cnae$subclasse <- 
  cnae$subclasse %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "")  %>% as.integer() 



# __________ Função de busca CNAE ----

FUNA_CNAE_busca <-
  function(seção = ".",
           divisão = ".",
           grupo = ".",
           classe = ".",
           subclasse = ".") {
    x <- 
      unique(cnae[grepl(cnae$seção, pattern = seção) == TRUE &          # consulta Regex Exclusão "[^_]"
                    grepl(cnae$divisão, pattern = divisão) == TRUE &
                    grepl(cnae$grupo, pattern = grupo) == TRUE &
                    grepl(cnae$classe, pattern = classe) == TRUE &
                    grepl(cnae$subclasse, pattern = subclasse) == TRUE,])
    return(x)}

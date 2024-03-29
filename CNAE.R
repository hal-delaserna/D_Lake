#        rm(list = ls())
#     options(editor = 'notepad')
library(tidyverse)

source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R')

#      estrutura da CNAE: A  x x.x x-x/x x
#       se��o ____________|  | | | | | | |      B e C - IND�STRIAS EXTRATIVAS e IND�STRIAS DE TRANSFORMA��O
#       divis�o _____________|_| | | | | |
#       grupo ___________________| | | | |
#       classe ____________________|_| | |
#       subclasse _____________________|_|



# Carregamento CNAE 2.0 ----
CNAE_Subclasses_2_0 <-
  read.table(file = 'D:/Users/humberto.serna/Documents/D_Lake/CNAE_2_0.txt', 
             header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
             colClasses = c('character'), encoding = "ANSI")

colnames(CNAE_Subclasses_2_0) <- c("subclasse", "subclasse.descri��o", "classe", 
                                   "classe.descri��o", "grupo", "grupo.descri��o", "divis�o", 
                                   "divis�o.descri��o", "se��o", "se��o.descri��o", "grupamento", 
                                   "grande.Grupamento")

CNAE_Subclasses_2_0$grupo.descri��o <- 
  CNAE_Subclasses_2_0$grupo.descri��o %>% FUNA_removeAcentos() %>% FUNA_maiusculas()
  


# Carregamento CNAE 2.3 ----
CNAE_Subclasses_2_3 <-
  read.table(file = 'D:/Users/humberto.serna/Documents/D_Lake/CNAE_Subclasses_2_3_Estrutura_Detalhada.csv', 
             header = TRUE, sep = ";", stringsAsFactors = FALSE, 
             colClasses = c('character'), encoding = "ISO-8859")


CNAE_Subclasses_2_3[CNAE_Subclasses_2_3$grupo != "0",]$denomina��o <- 
  CNAE_Subclasses_2_3[CNAE_Subclasses_2_3$grupo != "0",]$denomina��o %>% FUNA_removeAcentos() %>% FUNA_maiusculas()


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


  


# __________ uniformiza��o dos Cods CNAE ----
# na base Novo Caged as CNAEs s�o n�meros inteiros

cnae$grupo <- 
  cnae$grupo %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "")  %>% as.integer() 

cnae$classe <- 
  cnae$classe %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "")  %>% as.integer() 

cnae$subclasse <- 
  cnae$subclasse %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "")  %>% as.integer() 



# __________ Fun��o de busca CNAE ----

FUNA_CNAE_busca <-
  function(se��o = ".",
           divis�o = ".",
           grupo = ".",
           classe = ".",
           subclasse = ".") {
    x <- 
      unique(cnae[grepl(cnae$se��o, pattern = se��o) == TRUE &          # consulta Regex Exclus�o "[^_]"
                    grepl(cnae$divis�o, pattern = divis�o) == TRUE &
                    grepl(cnae$grupo, pattern = grupo) == TRUE &
                    grepl(cnae$classe, pattern = classe) == TRUE &
                    grepl(cnae$subclasse, pattern = subclasse) == TRUE,])
    return(x)}



# _____ lista subclasses se��o B (Extrativa Mineral - Exceto petr�leo & G�s) ----

subclasses_alvo_SECAO_B <-                    
  c(#subclasse  denomina��o
    "500301",	#Extra��o de carv�o mineral
    "500302",	#Beneficiamento de carv�o mineral
    "710301",	#Extra��o de min�rio de ferro
    "710302",	#Pelotiza��o, sinteriza��o e outros beneficiamentos de min�rio de ferro
    "721901",	#Extra��o de min�rio de alum�nio
    "721902",	#Beneficiamento de min�rio de alum�nio
    "722701",	#Extra��o de min�rio de estanho
    "722702",	#Beneficiamento de min�rio de estanho
    "723501",	#Extra��o de min�rio de mangan�s
    "723502",	#Beneficiamento de min�rio de mangan�s
    "724301",	#Extra��o de min�rio de metais preciosos
    "724302",	#Beneficiamento de min�rio de metais preciosos
    "725100",	#Extra��o de minerais radioativos
    "729401",	#Extra��o de min�rios de ni�bio e tit�nio
    "729402",	#Extra��o de min�rio de tungst�nio
    "729403",	#Extra��o de min�rio de n�quel
    "729404",	#Extra��o de min�rios de cobre, chumbo, zinco e outros minerais met�licos n�o ferrosos n�o especificados anteriormente
    "729405",	#Beneficiamento de min�rios de cobre, chumbo, zinco e outros minerais met�licos n�o ferrosos n�o especificados anteriormente
    "810001",	#Extra��o de ard�sia e beneficiamento associado
    "810002",	#Extra��o de granito e beneficiamento associado
    "810003",	#Extra��o de m�rmore e beneficiamento associado
    "810004",	#Extra��o de calc�rio e dolomita e beneficiamento associado
    "810005",	#Extra��o de gesso e caulim
    "810006",	#Extra��o de areia, cascalho ou pedregulho e beneficiamento associado
    "810007",	#Extra��o de argila e beneficiamento associado
    "810008",	#Extra��o de saibro e beneficiamento associado
    "810009",	#Extra��o de basalto e beneficiamento associado
    "810010",	#Beneficiamento de gesso e caulim associado � extra��o
    "810099",	#Extra��o e britamento de pedras e outros materiais para constru��o e beneficiamento associado
    "891600",	#Extra��o de minerais para fabrica��o de adubos, fertilizantes e outros produtos qu�micos
    "892401",	#Extra��o de sal marinho
    "892402",	#Extra��o de salgema
    "892403",	#Refino e outros tratamentos do sal
    "893200",	#Extra��o de gemas (pedras preciosas e semipreciosas)
    "899101",	#Extra��o de grafita
    "899102",	#Extra��o de quartzo
    "899103",	#Extra��o de amianto
    "899199",	#Extra��o de outros minerais n�o met�licos n�o especificados anteriormente
    "990401",	#Atividades de apoio � extra��o de min�rio de ferro
    "990402",	#Atividades de apoio � extra��o de minerais met�licos n�o ferrosos
    "990403"	#Atividades de apoio � extra��o de minerais n�o met�licos
  )


# _____ Lista classes Alvo na Se��o C (Ind�stria de Transforma��o Relacionada n�o associada/consecutiva � minera��o) ----

classes_alvo_SECAO_C <- 
  c(
    "20126",	  #    Fabrica��o de Intermedi�rios para Fertilizantes 
    "20134",    #    Fabrica��o de Adubos e Fertilizantes 
    "23206",    #    Fabrica��o de Cimento 
    "23303",    #    Fabrica��o de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
    "23419",    #    Fabrica��o de Produtos Cer�micos Refrat�rios 
    "23427",    #    Fabrica��o de Produtos Cer�micos N�oRefrat�rios para Uso Estrutural na Constru��o 
    "23494",    #    Fabrica��o de Produtos Cer�micos N�oRefrat�rios n�o Especificados Anteriormente 
    "23915",    #    Aparelhamento e Outros Trabalhos em Pedras 
    "23923",    #    Fabrica��o de Cal e Gesso 
    "23991",    #    Fabrica��o de Produtos de Minerais N�oMet�licos n�o Especificados Anteriormente 
    "24113",    #    Produ��o de FerroGusa 
    "24121",    #    Produ��o de Ferroligas 
    "24211",    #    Produ��o de SemiAcabados de A�o 
    "24229",    #    Produ��o de Laminados Planos de A�o 
    "24237",    #    Produ��o de Laminados Longos de A�o 
    "24245",    #    Produ��o de Relaminados, Trefilados e Perfilados de A�o 
    "24318",    #    Produ��o de Tubos de A�o com Costura 
    "24393",    #    Produ��o de Outros Tubos de Ferro e A�o 
    "24415",    #    Metalurgia do Alum�nio e Suas Ligas 
    "24423",    #    Metalurgia dos Metais Preciosos 
    "24431",    #    Metalurgia do Cobre 
    "24491",    #    Metalurgia dos Metais N�oFerrosos e Suas Ligas n�o Especificados Anteriormente 
    "24512",    #    Fundi��o de Ferro e A�o 
    "25314",    #    Produ��o de forjados de a�o e de metais n�oferrosos e suas ligas 
    "24521",    #    Fundi��o de Metais N�oFerrosos e Suas Ligas 
    "32116")    #    Lapida��o de Gemas e Fabrica��o de Artefatos de Ourivesaria e Joalheria 


# __________ SUBCLASSES alvo na Se��o C ----
subclasses_alvo_SECAO_C <- 
  cnae[cnae$classe %in% classes_alvo_SECAO_C & cnae$subclasse != 0, c("subclasse")]


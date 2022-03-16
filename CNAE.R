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



# _____ lista subclasses seção B (Extrativa Mineral - Exceto petróleo & Gás) ----

subclasses_alvo_SECAO_B <-                    
  c(#subclasse  denominação
    "500301",	#Extração de carvão mineral
    "500302",	#Beneficiamento de carvão mineral
    "710301",	#Extração de minério de ferro
    "710302",	#Pelotização, sinterização e outros beneficiamentos de minério de ferro
    "721901",	#Extração de minério de alumínio
    "721902",	#Beneficiamento de minério de alumínio
    "722701",	#Extração de minério de estanho
    "722702",	#Beneficiamento de minério de estanho
    "723501",	#Extração de minério de manganês
    "723502",	#Beneficiamento de minério de manganês
    "724301",	#Extração de minério de metais preciosos
    "724302",	#Beneficiamento de minério de metais preciosos
    "725100",	#Extração de minerais radioativos
    "729401",	#Extração de minérios de nióbio e titânio
    "729402",	#Extração de minério de tungstênio
    "729403",	#Extração de minério de níquel
    "729404",	#Extração de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
    "729405",	#Beneficiamento de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
    "810001",	#Extração de ardósia e beneficiamento associado
    "810002",	#Extração de granito e beneficiamento associado
    "810003",	#Extração de mármore e beneficiamento associado
    "810004",	#Extração de calcário e dolomita e beneficiamento associado
    "810005",	#Extração de gesso e caulim
    "810006",	#Extração de areia, cascalho ou pedregulho e beneficiamento associado
    "810007",	#Extração de argila e beneficiamento associado
    "810008",	#Extração de saibro e beneficiamento associado
    "810009",	#Extração de basalto e beneficiamento associado
    "810010",	#Beneficiamento de gesso e caulim associado à extração
    "810099",	#Extração e britamento de pedras e outros materiais para construção e beneficiamento associado
    "891600",	#Extração de minerais para fabricação de adubos, fertilizantes e outros produtos químicos
    "892401",	#Extração de sal marinho
    "892402",	#Extração de salgema
    "892403",	#Refino e outros tratamentos do sal
    "893200",	#Extração de gemas (pedras preciosas e semipreciosas)
    "899101",	#Extração de grafita
    "899102",	#Extração de quartzo
    "899103",	#Extração de amianto
    "899199",	#Extração de outros minerais não metálicos não especificados anteriormente
    "990401",	#Atividades de apoio à extração de minério de ferro
    "990402",	#Atividades de apoio à extração de minerais metálicos não ferrosos
    "990403"	#Atividades de apoio à extração de minerais não metálicos
  )


# _____ Lista classes Alvo na Seção C (Indústria de Transformação Relacionada não associada/consecutiva à mineração) ----

classes_alvo_SECAO_C <- 
  c(
    "20126",	  #    Fabricação de Intermediários para Fertilizantes 
    "20134",    #    Fabricação de Adubos e Fertilizantes 
    "23206",    #    Fabricação de Cimento 
    "23303",    #    Fabricação de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
    "23419",    #    Fabricação de Produtos Cerâmicos Refratários 
    "23427",    #    Fabricação de Produtos Cerâmicos NãoRefratários para Uso Estrutural na Construção 
    "23494",    #    Fabricação de Produtos Cerâmicos NãoRefratários não Especificados Anteriormente 
    "23915",    #    Aparelhamento e Outros Trabalhos em Pedras 
    "23923",    #    Fabricação de Cal e Gesso 
    "23991",    #    Fabricação de Produtos de Minerais NãoMetálicos não Especificados Anteriormente 
    "24113",    #    Produção de FerroGusa 
    "24121",    #    Produção de Ferroligas 
    "24211",    #    Produção de SemiAcabados de Aço 
    "24229",    #    Produção de Laminados Planos de Aço 
    "24237",    #    Produção de Laminados Longos de Aço 
    "24245",    #    Produção de Relaminados, Trefilados e Perfilados de Aço 
    "24318",    #    Produção de Tubos de Aço com Costura 
    "24393",    #    Produção de Outros Tubos de Ferro e Aço 
    "24415",    #    Metalurgia do Alumínio e Suas Ligas 
    "24423",    #    Metalurgia dos Metais Preciosos 
    "24431",    #    Metalurgia do Cobre 
    "24491",    #    Metalurgia dos Metais NãoFerrosos e Suas Ligas não Especificados Anteriormente 
    "24512",    #    Fundição de Ferro e Aço 
    "25314",    #    Produção de forjados de aço e de metais nãoferrosos e suas ligas 
    "24521",    #    Fundição de Metais NãoFerrosos e Suas Ligas 
    "32116")    #    Lapidação de Gemas e Fabricação de Artefatos de Ourivesaria e Joalheria 


# __________ SUBCLASSES alvo na Seção C ----
subclasses_alvo_SECAO_C <- 
  cnae[cnae$classe %in% classes_alvo_SECAO_C & cnae$subclasse != 0, c("subclasse")]


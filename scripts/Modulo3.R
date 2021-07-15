# Curso de R 
# Mod. 3 - Usando o R para explorar o Panel Study of Entrepreneurial Dynamics (PSED) 
# Desenvolvido por Daniel do Prado Pagotto (LAPEI-UFG)

# Os "#" representam um comentario. O R pula as linhas que tiverem esse #
# Para rodar cada linha, coloque o cursor sobre a linha do comando e 
# aperte ctrl + Enter (ou cmd + Enter, para o usuarios de Mac)

# Os pacotes abaixo serao usados ao longo da nossa aula. Caso nao tenha eles 
# instalados no seu R, e necessario instalar usando os comandos abaixo: 
# 1) retire os # da frente de cada linha; 2) rode cada comando (ctrl + Enter)

# install.packages("tidyverse")
# install.packages("foreign")
# install.packages("webr")
# intall.packages("devtools")
# devtools::install_github("thomasp85/patchwork")



library(readr) 
library(tidyverse)
library(dplyr) 
library(ggplot2)
library(foreign)
library(webr)
library(patchwork)
library(tidyr)

#Vamos comecar lendo a base PSED a partir do meu repositorio do github:
psed_df <- read_csv("https://raw.githubusercontent.com/danielppagotto/R_empreendedorismo3/main/arquivos%20de%20bases/psed_2.csv")

# Filtrando para manter os negócios operantes e selecionando os dados

psed_tratado <- psed_df %>% 
  filter(AG2 <2 & BA50 != 3 & CA50 != 3 & DA50 != 50 & EA50 != 3)

# selecionado as variáveis

psed_tratado <- psed_tratado %>%  
  select(AG2, AH1_1, AH2_1, AH6_1, AH11_1,AH12_1, AH13_1,
         AQ4_1, AQ5_1, AQ6_1, AQ7_1, AQ8_1, AQ9_1, AQ10_1,  
         BQ4_1, BQ5_1, BQ6_1, BQ7_1, BQ8_1, BQ9_1, BQ10_1,  
         CQ4_1, CQ5_1, CQ6_1, CQ7_1, CQ8_1, CQ9_1, CQ10_1, 
         DQ4_1, DQ5_1, DQ6_1, DQ7_1, DQ8_1, DQ9_1, DQ10_1, 
         EQ4_1, EQ5_1, EQ6_1, EQ7_1, EQ8_1, EQ9_1, EQ10_1, 
         AS5, AS8, AS9, ES5, ES8, ES9, 
         AP1, AP2, AP3, AP4, AP5, AP6, AP7, AP8, AP9, AP10, AP11, AP12, 
         EP1, EP2, EP3, EP4, EP5, EP6, EP7, EP8, EP9, EP10, EP11, EP12)

# Tratamentos - demografia
# Faremos muitos tratamentos na base antes de iniciar qualquer análise. 

psed_tratado <- psed_tratado%>% 
  mutate(sexo = ifelse(AH1_1 == "1", "Masculino","Feminino"),.after=AH1_1) %>% 
  mutate(escolaridade = case_when(AH6_1 < 4 ~ "Até EM",
                                  AH6_1 >= 4 & AH6_1 < 8 ~ "Ensino Superior",
                                  AH6_1 >= 8 ~ "Pós-graduação"),.after=AH6_1) %>% 
  rename(idade = AH2_1, industria = AH11_1, empreendedorismo = AH12_1,
         negocios_possui = AH13_1, pesq_desenv_a = AS5, pesq_desenv_e = ES5, 
         regional_a = AS8, regional_e = ES8, nacional_a = AS9, nacional_e = ES9)

# adicionando o status "NA" em variáveis preenchidas pelo valor 999999998. 
# Isso foi feito conforme o dicionário de dados

psed_tratado[psed_tratado == 999999998] <- NA
psed_tratado[psed_tratado == 999999999] <- NA

# Tratamentos - finanças

psed_tratado <- psed_tratado %>% 
  rowwise() %>% 
  mutate(personal_savings_A = sum(c(AQ4_1),na.rm = TRUE),.after = AQ4_1) %>% 
  mutate(fff_A = sum(c(AQ5_1,AQ6_1),na.rm = TRUE), .after = AQ6_1) %>% 
  mutate(credit_A = sum(c(AQ7_1,AQ8_1,AQ9_1,AQ10_1),na.rm = TRUE),.after = AQ10_1) %>% 
  mutate(personal_savings_B = sum(c(BQ4_1),na.rm = TRUE),.after = BQ4_1) %>% 
  mutate(fff_B = sum(c(BQ5_1,BQ6_1),na.rm = TRUE), .after = BQ6_1) %>% 
  mutate(credit_B = sum(c(BQ7_1,BQ8_1,BQ9_1,BQ10_1),na.rm = TRUE),.after = BQ10_1) %>% 
  mutate(personal_savings_C = sum(c(CQ4_1),na.rm = TRUE),.after = CQ4_1) %>% 
  mutate(fff_C = sum(c(CQ5_1,CQ6_1),na.rm = TRUE), .after = CQ6_1) %>% 
  mutate(credit_C = sum(c(CQ7_1,CQ8_1,CQ9_1,CQ10_1),na.rm = TRUE),.after = CQ10_1) %>%
  mutate(personal_savings_D = sum(c(DQ4_1),na.rm = TRUE, .after = DQ4_1)) %>%
  mutate(fff_D = sum(c(DQ5_1,DQ6_1),na.rm = TRUE), .after = DQ6_1) %>% 
  mutate(credit_D = sum(c(DQ7_1,DQ8_1,DQ9_1,DQ10_1),na.rm = TRUE),.after = DQ10_1) %>% 
  mutate(personal_savings_E = sum(c(EQ4_1),na.rm = TRUE),.after = EQ4_1) %>% 
  mutate(fff_E = sum(c(EQ5_1,EQ6_1),na.rm = TRUE), .after = EQ6_1) %>% 
  mutate(credit_E = sum(c(EQ7_1,EQ8_1,EQ9_1,EQ10_1),na.rm = TRUE),.after = EQ10_1)  

# Agora vamos criar variáveis financeiras com o acumulado de cada wave.

psed_tratado <- psed_tratado %>%
  mutate(ps_acumulado_B = personal_savings_A + personal_savings_B,
         ps_acumulado_C = ps_acumulado_B + personal_savings_C,
         ps_acumulado_D = ps_acumulado_C + personal_savings_D,
         ps_acumulado_E = ps_acumulado_D + personal_savings_E,
         fff_acumulado_B = fff_A + fff_B,
         fff_acumulado_C = fff_acumulado_B + fff_C,
         fff_acumulado_D = fff_acumulado_C + fff_D,
         fff_acumulado_E = fff_acumulado_D + fff_E,
         credit_acumulado_B = credit_B + credit_A,
         credit_acumulado_C = credit_acumulado_B + credit_C,
         credit_acumulado_D = credit_acumulado_C + credit_D,
         credit_acumulado_E = credit_acumulado_D + credit_E) 

# Tratamento - normas sociedade
# Renomeando as variáveis sobre normas sociais 

psed_tratado <- psed_tratado %>%
  rename(suporte_comunidade_a = AP1, autonomia_a = AP2, risco_a = AP3,
         criatividade_a = AP4, suporte_comunidade_e = EP1, 
         autonomia_e = EP2, risco_e = EP3, criatividade_e = EP4)

# Selecionando os dados após tratamentos realizados

psed_tratado <- psed_tratado %>% 
  select(sexo, idade, escolaridade, industria, empreendedorismo, negocios_possui,
         personal_savings_A, personal_savings_B, personal_savings_C, personal_savings_D,
         personal_savings_E, ps_acumulado_B, ps_acumulado_C, ps_acumulado_D, 
         ps_acumulado_E, fff_A, fff_B, fff_C, fff_D, fff_E, fff_acumulado_B,
         fff_acumulado_C, fff_acumulado_D, fff_acumulado_E, credit_A, credit_B, 
         credit_C, credit_D, credit_E, credit_acumulado_B, credit_acumulado_C,
         credit_acumulado_D, credit_acumulado_E, pesq_desenv_a, pesq_desenv_e,
         regional_a,nacional_a, regional_e,nacional_e, suporte_comunidade_a, 
         autonomia_a, risco_a, criatividade_a, suporte_comunidade_e, autonomia_e, 
         risco_e, criatividade_e) %>% 
  filter(idade < 99) %>% 
  filter(industria < 98) %>% 
  filter(empreendedorismo < 10) %>% 
  filter(regional_e < 998) %>% 
  filter(regional_a < 998)

# Análises - Estatística Descritiva

summary(psed_tratado)

# Análises - Sexo
# Comparações serão feitas entre empreendedores e empreendedoras

total_sexo <- psed_tratado %>% 
    group_by(sexo) %>% 
    count()
  
total_sexo %>%   
    ggplot(aes(x = sexo, y = n)) + 
    geom_col(aes(fill=sexo)) +
    geom_text(aes(label = n), 
              vjust = -0.5) + 
    ylab("Total") +
    ggtitle("Frequência por sexo") +
    theme_classic()

total_sexo %>%   
    ggplot(aes(x = sexo, y = n)) + geom_col(aes(fill=sexo)) +
    geom_text(aes(label = n), vjust = -0.5) + ylab("Total") +
    ggtitle("Frequência por sexo") +
    theme_classic()


# Análises - Idade
# Vamos criar um boxplot da variável

psed_tratado %>% 
  ggplot(aes(x = sexo, y = idade, fill = sexo)) + geom_boxplot() + 
  theme_minimal() + theme(legend.position = "none") + 
  ggtitle("Boxplot de idades por sexo") +
  scale_y_continuous(breaks = seq(20,90,5)) 

# Análises - Escolaridade
# Vamos comparar a escolaridade de ambos os grupos da amostra


library(webr)
escolaridade <- psed_tratado %>% 
    group_by(sexo,escolaridade) %>% 
    summarise(total = n()) 
  
escolaridade %>%  
    PieDonut(aes(sexo,escolaridade,
                 count = total),
             explode = 2,
             title = "Escolaridade por sexo")

escolaridade %>%  
    PieDonut(aes(sexo,escolaridade,count = total),
             explode = 2,
             title = "Escolaridade por sexo")



# Análises - Experiência
# Analisando a experiência prévia na criação de empreendimentos por sexo

psed_tratado %>% 
  group_by(sexo) %>% 
  summarise(media = mean(empreendedorismo)) %>% 
  ggplot(aes(x=sexo, y = media, fill = sexo)) + 
  geom_text(aes(label = round(media,2)), vjust = -0.5) + geom_col() + 
  theme_classic() +  ggtitle("Experiência na criação de empresas",
                             "Quantos negócios teve previamente?")

# Dados sobre a experiência na indústria
# Criando gráficos e atribuindo a objetos. 
# Usando pacote `patchwork` para plotar ambos os gráficos. 


industria_colunas <- psed_tratado %>% 
  group_by(sexo) %>% 
  summarise(media = mean(industria)) %>% 
  ggplot(aes(x=sexo, y = media, fill = sexo)) + 
  geom_text(aes(label = round(media,2)), vjust = -0.5) + 
  geom_col() + theme_minimal() + theme(legend.position = "none") + 
  ggtitle("Experiência média na mesma indústria",
          "Quantos anos atuou na mesma indústria do novo empreendimento?")

industria_histogramas <- psed_tratado %>% 
  ggplot(aes(x = industria, fill = sexo)) + 
  geom_histogram(bins=15) + theme_minimal() + facet_wrap(~sexo, nrow = 2) +
  theme(legend.position = "none") +
  ggtitle("Histograma da experiência na mesma indústria",
          "Quantos anos atuou na mesma indústria do novo empreendimento?")


industria_colunas / industria_histogramas

# Análises - Inovação 

psed_tratado %>% 
  group_by(sexo,pesq_desenv_a) %>% 
  summarise(total = n()) %>% 
  mutate(pesq_desenv_a = ifelse(pesq_desenv_a == 1, "Sim","Não")) %>% 
  PieDonut(aes(sexo,pesq_desenv_a,count = total),
           explode = 2, title = 
             "P&D é uma prioridade p/ o empreendimento wave A")

psed_tratado %>% 
  group_by(sexo,pesq_desenv_e) %>% 
  summarise(total = n()) %>% 
  filter(pesq_desenv_e != 8) %>% 
  mutate(pesq_desenv_e = ifelse(pesq_desenv_e == 1, "Sim","Não")) %>% 
  PieDonut(aes(sexo,pesq_desenv_e,count = total),
           explode = 2,title = 
             "P&D é uma prioridade p/ empreendimento wave E")

  psed_tratado %>% 
    group_by(sexo,pesq_desenv_a) %>% 
    summarise(total = n()) %>% 
    mutate(pesq_desenv_a = ifelse(pesq_desenv_a == 1, "Sim","Não")) %>% 
    PieDonut(aes(sexo,pesq_desenv_a,count = total),
             explode = 2, title = "P&D é uma prioridade p/ empreendimento wave A")

  psed_tratado %>% 
    group_by(sexo,pesq_desenv_e) %>% 
    summarise(total = n()) %>% 
    filter(pesq_desenv_e != 8) %>% 
    mutate(pesq_desenv_e = ifelse(pesq_desenv_e == 1, "Sim","Não")) %>% 
    PieDonut(aes(sexo,pesq_desenv_e,count = total),
             explode = 2,title = "P&D é uma prioridade p/ empreendimento wave E")


# Análises - Finanças

financas_acumulada <- psed_tratado %>% 
  group_by(sexo) %>% 
  summarise(ps_a = mean(personal_savings_A,na.rm = TRUE),
            fff_a = mean(fff_A,na.rm = TRUE),
            credit_a = mean(credit_A,na.rm = TRUE),
            ps_b = mean(ps_acumulado_B, na.rm = TRUE),
            fff_b = mean(fff_acumulado_B, na.rm = TRUE),
            credit_b = mean(credit_acumulado_B, na.rm = TRUE),
            ps_c = mean(ps_acumulado_C, na.rm = TRUE),
            fff_c = mean(fff_acumulado_C, na.rm = TRUE),
            credit_c = mean(credit_acumulado_C, na.rm = TRUE),
            ps_d = mean(ps_acumulado_D, na.rm = TRUE),
            fff_d = mean(fff_acumulado_D, na.rm = TRUE),
            credit_d = mean(credit_acumulado_D, na.rm = TRUE),
            ps_e = mean(ps_acumulado_E, na.rm = TRUE),
            fff_e = mean(fff_acumulado_E, na.rm = TRUE),
            credit_e = mean(credit_acumulado_E, na.rm = TRUE)) 

# Usando a função pivot_longer para inverter a base de um formato "amplo" para "longo". 

library(tidyr)
financas_acumulada <- financas_acumulada %>% 
  pivot_longer(!sexo,names_to = "fontes",values_to="valores") %>% 
  separate(fontes, into = c("fonte","wave"), sep = "_") %>% 
  mutate(ano = case_when(wave == "a" ~ 2006,
                         wave == "b" ~ 2007,
                         wave == "c" ~ 2008,
                         wave == "d" ~ 2009,
                         wave == "e" ~ 2010))


# visualizar a evolução do capital financeiro de modo acumulado entre as diferentas ondas. 

financas_acumulada %>% 
  ggplot(aes(x = ano, y = valores, fill = sexo)) + geom_col(position = "dodge") + 
  facet_grid(~fonte) + theme_minimal()

# Análises - Expansões
# Vamos calcular a variação entre o esperado na wave A e o alcançado na wave E.

psed_tratado <- psed_tratado %>% 
    rowwise() %>% 
    mutate(expansao_regional = regional_e - regional_a,
           expansao_nacional = nacional_e - nacional_a)
  
psed_tratado %>% 
    ggplot(aes(x = expansao_regional)) + geom_histogram(bins = 10) + 
    theme_minimal()
  
psed_tratado %>% 
    ggplot(aes(x = expansao_regional)) + geom_histogram(bins = 10) + 
    theme_minimal()
  
  
  



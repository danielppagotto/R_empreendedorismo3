# Curso de R 
# Mod. 2 - R aplicado ao GEM 
# Desenvolvido por Daniel do Prado Pagotto (LAPEI-UFG)

# Os "#" representam um comentario. O R pula as linhas que tiverem esse #
# Para rodar cada linha, coloque o cursor sobre a linha do comando e 
# aperte ctrl + Enter (ou cmd + Enter, para o usuarios de Mac)

# Os pacotes abaixo serao usados ao longo da nossa aula. Caso nao tenha eles 
# instalados no seu R, e necessario instalar usando as instrucoes abaixo: 
# 1) retire os # da frente de cada linha; 2) rode cada comando (ctrl + Enter)

#install.packages("ggrepel")
#install.packages("directlabels")
#install.packages("GGally")
#install.packages("skimr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")

library(readr) 
library(dplyr) 
library(skimr)
library(ggplot2) 
library(ggrepel) 
library(directlabels)
library(GGally)



#  Bases GEM-NES - National Expert Survey ---------------------------------

#Vamos comecar lendo a base GEM a partir do meu repositorio do github:

gem_especialistas <- read_delim("https://raw.githubusercontent.com/danielppagotto/R_empreendedorismo2/main/arquivos%20de%20bases/gem_nes_historico.csv", ";", 
                                escape_double = FALSE, trim_ws = TRUE)

#Vamos usar o glimpse para conferir a base 
glimpse(gem_especialistas)

#Vamos baixar a base e comparar alguns paises
paises <- c("Brazil","Chile","Colombia","Mexico")

#vamos separar algumas variaveis de interesse
gem_paises_selecionados <- gem_especialistas %>% 
  filter(economy %in% paises) %>% 
  select(economy, year, gov_support, taxes_bureaucracy, 
         gov_programs, internal_market_dynamics)

# Estatisticas descritivas
gem_paises_selecionados %>%
  select(-year) %>% 
  group_by(economy) %>% 
  skim()

#Plotando variaveis
gem_paises_selecionados %>% 
  ggplot(aes(year,gov_support)) + geom_line()

#Adicionando o parametro col
gem_paises_selecionados %>% 
  ggplot(aes(year,gov_support, col = economy)) + geom_line(size = 2) +
  theme_minimal()

#Adicionando titulos
gem_paises_selecionados %>% 
  ggplot(aes(year,gov_support, col = economy)) + geom_line(size = 2) +
  theme_minimal() + xlab("Ano") + ylab("Suporte Governamental") +
  ggtitle("Suporte Governamental", "Comparação entre Brasil, Chile, Colombia e México - Dados GEM")

#Adicionando textos as linhas
gem_paises_selecionados %>% 
  ggplot(aes(year,gov_support, col = economy)) + geom_line(size = 2) +
  geom_dl(aes(label = economy), method = list(dl.combine("first.points", "last.points"))) +
  theme_minimal() + xlab("Ano") + ylab("Suporte Governamental") +
  ggtitle("Suporte Governamental", "Comparação entre Brasil, Chile, Colombia e México - Dados GEM")

#Aumentando fontes
gem_paises_selecionados %>% 
  ggplot(aes(year,gov_support, col = economy)) + geom_line(size = 2) +
  geom_dl(aes(label = economy), method = list(dl.combine("first.points", "last.points"))) +
  theme_minimal() + xlab("Ano") + ylab("Suporte Governamental") + theme(legend.position = "none") +
  theme(plot.title = element_text(size=22)) + theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.2), angle = 0)) + ggtitle("Suporte Governamental", "Comparação entre Brasil, Chile, Colombia e México - Dados GEM")

#Mudando a escala
gem_paises_selecionados %>% 
  ggplot(aes(year,gov_support, col = economy)) + geom_line(size = 2) +
  geom_dl(aes(label = economy), method = list(dl.combine("first.points", "last.points"))) +
  theme_minimal() + xlab("Ano") + ylab("Suporte Governamental") + theme(legend.position = "none") +
  theme(plot.title = element_text(size=22)) + theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) + theme(axis.title.x = element_text(size = rel(1.2), angle = 0)) + ggtitle("Suporte Governamental", "Comparação entre Brasil, Chile, Colombia e México - Dados GEM") +
  scale_x_continuous(breaks = seq(2010,2020,1))


# Base APS - Adult Population Survey --------------------------------------

# Juntando o GEM e a base WGIDATASET

#Lendo as bases a partir do GitHub

wgidataset <- read_csv("https://raw.githubusercontent.com/danielppagotto/R_empreendedorismo2/main/arquivos%20de%20bases/wgi.csv")

gem_aps <- read_delim("https://raw.githubusercontent.com/danielppagotto/R_empreendedorismo2/main/arquivos%20de%20bases/gem_2019_aps.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#Juntando bases
gem_wgid <- gem_aps %>% 
  left_join(wgidataset, by = c("abrev" = "code")) %>% 
  select(economy, continent, tea, perceived_opportunities,
         fear_failure, established_ownership, 
         entrepreneurship_as_good_carrer_choice,
         corruption, rule_of_law, regulatory_quality, 
         political_stability, voice_accountability)


gem_wgid %>%
  group_by(continent) %>% 
  skim()

#Estatistica descritiva - GGally
gem_wgid %>% select(-economy,-continent) %>% 
  ggpairs()

#Explorando mais a fundo a variavel Entrepreneurship as good carrer choice
gem_wgid %>% 
  ggplot(aes(x = political_stability, y = entrepreneurship_as_good_carrer_choice)) +
  geom_point() + theme_minimal() + geom_smooth(method = "lm", se = FALSE)

gem_wgid %>% 
  ggplot(aes(x = political_stability, y = entrepreneurship_as_good_carrer_choice)) +
  geom_point(aes(col = continent, size = 1.5)) + 
  theme_minimal() + geom_text_repel(aes(label = economy)) + 
  theme(legend.position = "none") + geom_smooth(method = "lm", se = FALSE)

#Aplicando facet_grid()
gem_wgid %>% 
  ggplot(aes(x = political_stability, y = entrepreneurship_as_good_carrer_choice)) +
  geom_point(aes(col = continent, size = 1.5)) + facet_grid(~continent) +
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method = "lm", se = FALSE)

#Filtrando de um continente apenas e verificando os resultados das variaveis
gem_wgid %>% 
  filter(continent == "Europa") %>% 
  select(-economy,-continent) %>% 
  ggpairs()


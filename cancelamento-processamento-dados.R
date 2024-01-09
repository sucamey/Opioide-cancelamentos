library(qcc)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggQC)
library(data.table)
library(forcats)

setwd("G:/Drives compartilhados/3-CCD/25 Data Stewards/GIT/Opioide-cancelamentos/Dados")


#Lista com usuários ligados a Farmácia
usuarios_farmacia <- c("Deliberal, Ana",
                       "Benfica, Guilherme",
                       "Cas, Franciele",
                       "Faleiro, Aline",
                       "Heider, Janaina",
                       "Jardim, Marcelo",
                       "Loureiro, Bruna",
                       "Oliveira, Carmen",
                       "Pereira, Maria",
                       "Pinto, Victoria",
                       "Schenini, Lucia",
                       "Silva, Rosita",
                       "Vieira, Aline",
                       "Brasil, Grifols")

#Leitura da base
Cancelamentos_2022_a_2023_junho <- read_excel(
  "Cancelamentos  2022 a 2023 junho enfe.xlsx",
  col_types = c("text", "text", "text", "date", "date", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "text",
                "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric")
  ) %>% 
  filter(Estacion != "NA",
         Clase != "99",
         !Usuario %in% usuarios_farmacia) 

Cancelamentos_2022_a_2023_junho_8S <- read_excel(
  "Cancelamentos 8S 2022 a junho 2023.xlsx",
  col_types = c("text", "text", "text", "date", "date", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "text",
                "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric")
) %>% 
  filter(Estacion != "NA",
         Clase != "99",
         !Usuario %in% usuarios_farmacia) 

Cancelamentos_2022_a_2023_junho_9N <- read_excel(
  "Cancelamentos  2022 a 2023 junho 9N.xlsx",
  col_types = c("text", "text", "text", "date", "date", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "text",
                "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric")
) %>% 
  filter(Estacion != "NA",
         Clase != "99",
         !Usuario %in% usuarios_farmacia) 

Cancelamentos_2022_a_2023_junho <- bind_rows(Cancelamentos_2022_a_2023_junho, 
                                             Cancelamentos_2022_a_2023_junho_8S,
                                             Cancelamentos_2022_a_2023_junho_9N) %>% 
  mutate(
    dispensario = fct_collapse(Estacion,
                               PYXIS5_N = c("PYXIS5N","PYXIS5NB"),
                               PYXIS5_S = c("PYXIS5SB"),
                               PYXIS6_N = c("PYXIS6N","PYXIS6NB"),
                               PYXIS6_S = c("PYXIS6S","PYXIS6SB"),
                               PYXIS7_N = c("PYXIS7N","PYXIS7NB")
    )
    # hora = as.numeric(hour(Hora)),
    # faixa_hora = if_else(hour(Hora)<=7,"0 a 7",
    #                           if_else(hour(Hora)<=19, "8 a 19",
    #                                   "20 a 24"))
  ) 

#Cria uma id para os usuários para apresentar no pailnel e não expor os colaboradores.    
Cancelamentos_2022_a_2023_junho <- Cancelamentos_2022_a_2023_junho %>% 
  group_by(Usuario) %>% 
  mutate(id_usuario = stringi::stri_rand_strings(1, length=4)) %>% 
  ungroup()

#Cria a tabela com a correspondência
mascara_cancelamento <- Cancelamentos_2022_a_2023_junho %>% 
  select(Usuario,id_usuario) %>% 
  group_by(Usuario,id_usuario) %>% 
  summarize(n=n()) %>% 
  select(-n)

# armazena o número de cancelamentos por usuário, por dia e por dispensário
Cancelamentos_usuario_dia <- Cancelamentos_2022_a_2023_junho %>% 
  select(dispensario,Clase,Fecha,id_usuario) %>%
  group_by(dispensario,Clase,Fecha,id_usuario) %>% 
  summarize(n=n()) 

# armazena o número de usuários por dia e por dispensário
usuarios_dia <- Cancelamentos_2022_a_2023_junho %>% 
  select(dispensario,Clase,Fecha,id_usuario) %>%
  group_by(dispensario,Clase,Fecha,id_usuario) %>% 
  summarize(n=n()) %>% 
  count(Fecha) %>% 
  rename(n_usuarios = n)

# armazena o número de cancelamentos por dia e por dispensário
cancelamentos_dia <- Cancelamentos_2022_a_2023_junho %>% 
  select(dispensario,Clase,Fecha,id_usuario) %>%
  count(dispensario,Clase,Fecha)%>% 
  rename(n_cancelamentos = n)


Cancelamentos_reduzido <- full_join(usuarios_dia, cancelamentos_dia)

# armazena o número de cancelamentos por usuário, por dia e por dispensário
Cancelamentos_usuario_dia_geral <- Cancelamentos_2022_a_2023_junho %>% 
  select(Clase,Fecha,id_usuario) %>%
  group_by(Clase,Fecha,id_usuario) %>% 
  summarize(n=n()) 

# armazena o número de usuários por dia e por dispensário
usuarios_dia_geral <- Cancelamentos_2022_a_2023_junho %>% 
  select(Clase,Fecha,id_usuario) %>%
  group_by(Clase,Fecha,id_usuario) %>% 
  summarize(n=n()) %>% 
  count(Fecha) %>% 
  rename(n_usuarios = n)

# armazena o número de cancelamentos por dia e por dispensário
cancelamentos_dia_geral <- Cancelamentos_2022_a_2023_junho %>% 
  select(Clase,Fecha,id_usuario) %>%
  count(Clase,Fecha)%>% 
  rename(n_cancelamentos = n)


Cancelamentos_reduzido_geral <- full_join(usuarios_dia_geral, cancelamentos_dia_geral)

save.image(file = "Cancelamentos_reduzido.RData")

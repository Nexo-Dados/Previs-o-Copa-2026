
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(brms)
library(tidybayes)


# Parametros gerais -------------------------------------------------------

# Número de simulações
n_sim <- 10000

# Número da semente para garantir reprodutibilidade
seed_number <- 2026

# Parâmetros do modelo
Rating <- 0.6
Rating_change <- 0.05
Home <- 0.497
Polymarket <- 0.08

#-- Os betas acima se referem ao impacto de um desvio padrão sobre as probabilidades no confronto direto entre os times. 

#-- O impacto maior é do rating Elo. Calibrei o modelo tendo como base Marrocos e Escócia, assumindo que Marrocos teria uma chance maior do que a Escócia, no grupo do Brasil. Os dois possuem valores de Elo e variação de 1 ano similares. Então, o ajuste foi para que a probabilidade do Marrocos fosse mais alta.


# Dados e preparação ------------------------------------------------------

#-- Rating elo e Change 1y de 09/12/2025
#-- Probabilidades Polymarket de 10/12/2025 às 16h45
# Quando a probabilidade no Polymarket era menor que 1%, ficou como 1% como padrão. 

#--- Dados
dados <- tribble(
  ~pais, ~rating_elo, ~grupo, ~change_1y, ~polymarket,
  "Espanha", 2171, "H", -6, 15.5,
  "Argentina", 2113, "J", 5, 9,
  "França", 2062, "I", 7, 12.5,
  "Inglaterra", 2042, "L", 36, 12.5,
  "Colômbia", 1998, "K", 21, 1.9,
  "Brasil", 1978, "C", -18, 8.5,
  "Portugal", 1976, "K", -17, 8.5,
  "Holanda", 1959, "F", 21, 3.3,
  "Equador", 1933, "E", 23, 1,
  "Croácia", 1932, "L", 46, 1.1,
  "Noruega", 1922, "I", 116, 4.1,
  "Alemanha", 1910, "E", -61, 6.5,
  "Suíça", 1897, "B", 83, 1,
  "Uruguai", 1890, "H", -58, 1.7,
  "Turquia", 1880, "D*", 106, 1,
  "Japão", 1878, "F", -11, 1.5,
  "Dinamarca", 1864, "A*", 8, 1,
  "Itália", 1859, "B*", -71, 2.1,
  "Bélgica", 1849, "G", -1, 2.2,
  "México", 1835, "A", 66, 1,
  "Paraguai", 1833, "D", 58, 1,
  "Marrocos", 1825, "C", 23, 1.9,
  "Áustria", 1818, "J", -53, 1,
  "Senegal", 1803, "I", 33, 1,
  "Ucrânia", 1802, "F*", 0, 1,
  "Canadá", 1802, "B", 18, 1.1,
  "Escócia", 1790, "C", 31, 1,
  "Coreia do Sul", 1785, "A", 10, 1,
  "Austrália", 1774, "D", 55, 1,
  "Irã", 1754, "G", -83, 1,
  "Estados Unidos", 1747, "D", -12, 1.3,
  "Panamá", 1742, "L", 16, 1,
  "Polônia", 1735, "F*", 36, 1,
  "Uzbequistão", 1735, "K", 50, 1,
  "Tchéquia", 1731, "A*", -22, 1,
  "País de Gales", 1715, "B*", -17, 1,
  "Kosovo", 1714, "D*", 160, 1,
  "Argélia", 1703, "J", 29, 1,
  "Eslováquia", 1687, "D*", -14, 1,
  "Irlanda", 1688, "A*", 80, 1,
  "Bolívia", 1675, "I*", 18, 1,
  "Albânia", 1664, "F*", 29, 1,
  "Suécia", 1660, "F*", -84, 1,
  "Romênia", 1642, "D*", -77, 1,
  "Egito", 1625, "G", -32, 1,
  "Jordânia", 1637, "J", 19, 1,
  "Rep. Dem. do Congo", 1616, "K*", 75, 1,
  "Tunísia", 1641, "F", 43, 1,
  "Costa do Marfim", 1607, "E", 28, 1,
  "Iraque", 1621, "I*", -46, 1,
  "Irlanda do Norte", 1602, "B*", 43, 1,
  "Arábia Saudita", 1593, "H", 61, 1,
  "Macedônia do Norte", 1593, "A*", -18, 1,
  "Nova Zelândia", 1586, "G", 2, 1,
  "Bósnia e Herzegovina", 1571, "B*", 87, 1,
  "Cabo Verde", 1560, "H", 89, 1,
  "Haiti", 1542, "C", 3, 1,
  "África do Sul", 1530, "A", -46, 1,
  "Jamaica", 1527, "K*", -47, 1,
  "Gana", 1509, "L", 80, 1,
  "Curaçao", 1467, "E", 131, 1,
  "Catar", 1427, "B", -126, 1,
  "Suriname", 1441, "I*", 84, 1,
  "Nova Caledônia", 1295, "K*", 33, 1
)


# Preparacao dos dados ----------------------------------------------------


dados_modelo <- dados %>%
  mutate(
    home_bonus = if_else(pais %in% c("Estados Unidos", "México", "Canadá"), 1, 0),
    rating_std = scale(rating_elo)[,1],
    change_std = scale(change_1y)[,1],
    polymarket_log = log(polymarket + 0.1),
    polymarket_std = scale(polymarket_log)[,1],
    grupo_clean = str_remove(grupo, "\\*"),
    playoff = str_detect(grupo, "\\*")
  )

dados %>% 
  select(rating_elo, change_1y, polymarket) %>% 
  summary()

# Playoffs ----------------------------------------------------------------


#-- lista dos playoffs para simular situacao

playoffs <- list(
  A = list(
    semi1 = c("Tchéquia", "Irlanda"),
    semi2 = c("Dinamarca", "Macedônia do Norte")
  ),
  B = list(
    semi1 = c("País de Gales", "Bósnia e Herzegovina"),
    semi2 = c("Itália", "Irlanda do Norte")
  ),
  D = list(
    semi1 = c("Turquia", "Romênia"),
    semi2 = c("Eslováquia", "Kosovo")
  ),
  `F` = list(
    semi1 = c("Ucrânia", "Suécia"),
    semi2 = c("Polônia", "Albânia")
  ),
  I = list(
    semi1 = c("Nova Caledônia", "Jamaica"),
    final_contra = "Rep. Dem. do Congo"
  ),
  K = list(
    semi1 = c("Bolívia", "Suriname"),
    final_contra = "Iraque"
  )
)

# Função para simular o confronto entre dois times
#-- parametros aqui tem bastante impacto no resultado
simular_partida_com_gols <- function(rating1, change1, home1, poly1,
                                     rating2, change2, home2, poly2) {
  
  forca1 <- Rating * rating1 + Rating_change * change1 + 
    Home * home1 + Polymarket * poly1
  forca2 <- Rating * rating2 + Rating_change * change2 + 
    Home * home2 + Polymarket * poly2
  
  prob_vitoria <- plogis(forca1 - forca2)
  resultado <- runif(1)
  
  if (resultado < prob_vitoria * 0.7) {
    gols1 <- rpois(1, lambda = 1.5 + max(0, (forca1 - forca2) * 0.3))
    gols2 <- rpois(1, lambda = 0.8 + max(0, (forca2 - forca1) * 0.2))
    if (gols1 <= gols2) gols1 <- gols2 + sample(1:2, 1)
    return(list(pontos = c(3, 0), gols_pro = c(gols1, gols2)))
    
  } else if (resultado < prob_vitoria * 0.7 + 0.25) {
    gols <- rpois(1, lambda = 1.2)
    return(list(pontos = c(1, 1), gols_pro = c(gols, gols)))
    
  } else {
    gols1 <- rpois(1, lambda = 0.8 + max(0, (forca1 - forca2) * 0.2))
    gols2 <- rpois(1, lambda = 1.5 + max(0, (forca2 - forca1) * 0.3))
    if (gols2 <= gols1) gols2 <- gols1 + sample(1:2, 1)
    return(list(pontos = c(0, 3), gols_pro = c(gols1, gols2)))
  }
}

#-- função para simular confronto direto
simular_eliminatoria <- function(time1, time2, dados_times) {
  info1 <- dados_times %>% filter(pais == time1)
  info2 <- dados_times %>% filter(pais == time2)
  
  if (nrow(info1) == 0 || nrow(info2) == 0) {
    return(NA_character_)
  }
  
  forca1 <- Rating * info1$rating_std + Rating_change * info1$change_std + 
    Home * info1$home_bonus + Polymarket * info1$polymarket_std
  forca2 <- Rating * info2$rating_std + Rating_change * info2$change_std + 
    Home * info2$home_bonus + Polymarket * info2$polymarket_std
  
  prob_vitoria <- plogis(forca1 - forca2)
  
  if (runif(1) < prob_vitoria) {
    return(time1)
  } else {
    return(time2)
  }
}

#-- funcao para simular playoff (repescagem)
simular_playoff_grupo <- function(grupo_letra, dados_times) {
  playoff_info <- playoffs[[grupo_letra]]
  
  if (is.null(playoff_info)) {
    return(NULL)
  }
  
  if (!is.null(playoff_info$final_contra)) {
    vencedor_semi <- simular_eliminatoria(
      playoff_info$semi1[1], 
      playoff_info$semi1[2], 
      dados_times
    )
    
    if (is.na(vencedor_semi)) return(NA_character_)
    
    vencedor_final <- simular_eliminatoria(
      vencedor_semi,
      playoff_info$final_contra,
      dados_times
    )
    return(vencedor_final)
  } else {
    vencedor_semi1 <- simular_eliminatoria(
      playoff_info$semi1[1], 
      playoff_info$semi1[2], 
      dados_times
    )
    vencedor_semi2 <- simular_eliminatoria(
      playoff_info$semi2[1], 
      playoff_info$semi2[2], 
      dados_times
    )
    
    if (is.na(vencedor_semi1) || is.na(vencedor_semi2)) return(NA_character_)
    
    vencedor_final <- simular_eliminatoria(
      vencedor_semi1,
      vencedor_semi2,
      dados_times
    )
    return(vencedor_final)
  }
}

#-- funcao para simular os grupos em si
simular_grupos <- function(dados, n_sim) {
  
  resultados_simulacoes <- list()
  
  for (sim in 1:n_sim) {
    
    set.seed(seed_number + sim)
    
    vencedores_playoffs <- map_chr(names(playoffs), ~simular_playoff_grupo(.x, dados))
    vencedores_validos <- vencedores_playoffs[!is.na(vencedores_playoffs)]
    grupos_validos <- names(playoffs)[!is.na(vencedores_playoffs)]
    
    times_diretos <- dados %>% filter(!playoff)
    
    if (length(vencedores_validos) > 0) {
      times_playoffs <- tibble(
        pais = vencedores_validos,
        grupo_clean = grupos_validos
      ) %>%
        left_join(dados %>% select(pais, rating_std, change_std, home_bonus, polymarket_std), 
                  by = "pais")
    } else {
      times_playoffs <- tibble(
        pais = character(),
        grupo_clean = character(),
        rating_std = numeric(),
        change_std = numeric(),
        home_bonus = numeric(),
        polymarket_std = numeric()
      )
    }
    
    todos_times <- bind_rows(
      times_diretos %>% select(pais, grupo_clean, rating_std, change_std, home_bonus, polymarket_std),
      times_playoffs
    )
    
    grupos_unicos <- todos_times$grupo_clean %>% unique()
    
    resultado_grupos <- map_df(grupos_unicos, function(g) {
      
      times_grupo <- todos_times %>% filter(grupo_clean == g)
      n_times <- nrow(times_grupo)
      
      pontos <- rep(0, n_times)
      gols_pro <- rep(0, n_times)
      gols_contra <- rep(0, n_times)
      
      for (i in 1:(n_times-1)) {
        for (j in (i+1):n_times) {
          resultado <- simular_partida_com_gols(
            times_grupo$rating_std[i], times_grupo$change_std[i], 
            times_grupo$home_bonus[i], times_grupo$polymarket_std[i],
            times_grupo$rating_std[j], times_grupo$change_std[j], 
            times_grupo$home_bonus[j], times_grupo$polymarket_std[j]
          )
          
          pontos[i] <- pontos[i] + resultado$pontos[1]
          pontos[j] <- pontos[j] + resultado$pontos[2]
          
          gols_pro[i] <- gols_pro[i] + resultado$gols_pro[1]
          gols_contra[i] <- gols_contra[i] + resultado$gols_pro[2]
          gols_pro[j] <- gols_pro[j] + resultado$gols_pro[2]
          gols_contra[j] <- gols_contra[j] + resultado$gols_pro[1]
        }
      }
      
      saldo_gols <- gols_pro - gols_contra
      
      tibble(
        simulacao = sim,
        grupo = g,
        pais = times_grupo$pais,
        pontos = pontos,
        gols_pro = gols_pro,
        gols_contra = gols_contra,
        saldo_gols = saldo_gols
      ) %>%
        arrange(desc(pontos), desc(saldo_gols), desc(gols_pro), runif(n())) %>%
        mutate(posicao = row_number())
    })
    
    terceiros <- resultado_grupos %>%
      filter(posicao == 3) %>%
      arrange(desc(pontos), desc(saldo_gols), desc(gols_pro)) %>%
      slice_head(n = 8) %>%
      mutate(classifica_terceiro = TRUE)
    
    resultado_final <- resultado_grupos %>%
      left_join(terceiros %>% select(simulacao, pais, classifica_terceiro), 
                by = c("simulacao", "pais")) %>%
      mutate(
        classifica_terceiro = replace_na(classifica_terceiro, FALSE),
        classificado = (posicao <= 2) | classifica_terceiro
      )
    
    resultados_simulacoes[[sim]] <- resultado_final
  }
  
  bind_rows(resultados_simulacoes)
}

# Simulação ---------------------------------------------------------------

# semente para garantir reproducibilidade
set.seed(seed_number)

# simulacao (com n_sim definido acima, de 10 mil. alterar no inicio do código para rodar mais simulações)
simulacoes <- simular_grupos(dados_modelo, n_sim)


# Tabelas finais ----------------------------------------------------------

prob_classificacao <- simulacoes %>%
  group_by(pais, posicao) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pais) %>%
  mutate(
    prob = n / sum(n),
    total_sims = sum(n)) %>%
  ungroup()

todos_paises <- dados %>%
  select(pais, rating_elo, grupo, change_1y, polymarket)

prob_avancar <- todos_paises %>%
  left_join(
    simulacoes %>%
      group_by(pais) %>%
      summarise(
        n_apareceu = n(),
        n_classificou = sum(classificado),
        n_1o = sum(posicao == 1),
        n_2o = sum(posicao == 2),
        n_3o_classifica = sum(posicao == 3 & classifica_terceiro),
        media_pontos = mean(pontos),
        media_saldo = mean(saldo_gols)
      ),
    by = "pais"  
  ) %>%
  mutate(
    prob_classificar = n_classificou / n_sim,
    prob_1o = n_1o / n_sim,
    prob_2o = n_2o / n_sim,
    prob_3o_classifica = n_3o_classifica / n_sim,
    prob_chegar_grupo = n_apareceu / n_sim,
    n_simulacoes = replace_na(n_apareceu, 0)
  ) %>%
  mutate(across(starts_with("prob_"), ~replace_na(.x, 0))) %>%
  mutate(across(starts_with("media_"), ~replace_na(.x, 0)))

dados_final <- dados %>%
  left_join(prob_avancar %>% select(pais, prob_chegar_grupo, prob_classificar, 
                                    prob_1o, prob_2o, prob_3o_classifica, 
                                    media_pontos, media_saldo, n_simulacoes), 
            by = "pais") %>%
  arrange(desc(prob_classificar))
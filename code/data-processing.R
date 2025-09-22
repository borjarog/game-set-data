library(dplyr)
library(stringr)

variables_ganador <- c("tourney_id", "tourney_name", "surface", "draw_size",
                       "tourney_level", "tourney_date", "match_num","score", "best_of",
                       "round", "minutes", "winner_id", "winner_seed", "winner_entry", "winner_name", "winner_hand",
                       "winner_ht", "winner_ioc", "winner_age",  "w_ace", "w_df", "w_svpt", "w_1stIn",
                       "w_1stWon", "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced",
                       "winner_rank", "winner_rank_points")

variables_perdedor <- c("tourney_id", "tourney_name", "surface", "draw_size",
                        "tourney_level", "tourney_date", "match_num","score", "best_of",
                        "round", "minutes", "loser_id", "loser_seed", "loser_entry", "loser_name", "loser_hand",
                        "loser_ht", "loser_ioc", "loser_age",  "l_ace", "l_df", "l_svpt", "l_1stIn",
                        "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced",
                        "loser_rank", "loser_rank_points")

jugadores_list <- list()
matches_list <- list()
medias_list <- list()

for (i in 1:24) {
  
  camino <- "../data/raw-data/atp_matches_" # Mi directorio
  cola <- '.csv'
  anyo <- 1999 + i
  direccion <- paste(camino, anyo, cola, sep = "")
  datos <- read.csv(direccion)
  
  datos_ganador <- datos %>% select(all_of(variables_ganador)) %>%
    rename_with(~str_replace(., "^(winner_|w_)", ""), starts_with(c("winner_", "w_"))) %>%
    mutate(player_type = "winner")
  
  datos_perdedor <- datos %>% select(all_of(variables_perdedor)) %>%
    rename_with(~str_replace(., "^(loser_|l_)", ""), starts_with(c("loser_", "l_"))) %>%
    mutate(player_type = "loser")
  
  datos_final <- bind_rows(datos_ganador, datos_perdedor)
  datos_final <- datos_final %>% arrange(tourney_id, match_num)
  datos_final <- subset(datos_final, select = -entry)
  datos_final$tourney_date <- as.character(datos_final$tourney_date)
  datos_final$Year <- as.numeric(substr(datos_final$tourney_date, 1, 4))
  datos_final$Month <- as.numeric(substr(datos_final$tourney_date, 6, 6))
  datos_final <- datos_final[datos_final$tourney_level != "D", ]
  datos_final <- datos_final[!is.na(datos_final$ace), ]
  datos_final <- datos_final[datos_final$surface != "Carpet", ]
  
  players <- read.csv("../data/raw-data/atp_matches_2000.csv")
  matches <- datos_final
  m <- matches[!is.na(matches$ace), ]
  
  colnames(matches)[colnames(matches) == "1stIn"] <- "X1stIn"
  colnames(matches)[colnames(matches) == "1stWon"] <- "X1stWon"
  colnames(matches)[colnames(matches) == "2ndWon"] <- "X2ndWon"
  
  matches$match_id=paste(as.character(matches$tourney_id), as.character(matches$match_num), sep='-')
  matches2 <- merge(matches, matches, by = "match_id", suffixes = c("", "_rival"))
  matches2 <- subset(matches2, select = -c(tourney_id_rival, tourney_name_rival, surface_rival, draw_size_rival, tourney_date_rival, match_num_rival, score_rival, best_of_rival, round_rival, minutes_rival, name_rival, hand_rival, ht_rival))
  matches2 <- subset(matches2, select = -c(tourney_level_rival, ioc_rival, age_rival, rank_points_rival))
  matches2 <- subset(matches2, select = -c(rank_rival, player_type_rival, Year_rival, Month_rival ))     
  names(matches2)[names(matches2) == "ace_rival"] <- "ace_resto"
  names(matches2)[names(matches2) == "df_rival"] <- "df_resto"
  names(matches2)[names(matches2) == "svpt_rival"] <- "puntos_resto"
  names(matches2)[names(matches2) == "X1stIn_rival"] <- "X1stIn_resto"
  matches2$X1stWon_resto = matches2$X1stIn_resto - matches2$X1stWon_rival 
  matches2 <- subset(matches2, select = -c(X1stWon_rival ))  
  matches2$X2ndPlayed_resto = matches2$puntos_resto - matches2$X1stIn_resto - matches2$df_resto 
  matches2$X2ndWon_resto = matches2$X2ndPlayed_resto - matches2$X2ndWon_rival
  names(matches2)[names(matches2) == "SvGms_rival"] <- "SvGms_resto"
  names(matches2)[names(matches2) == "bpFaced_rival"] <- "bpFaced_resto"
  matches2$bpWon = matches2$bpFaced_resto - matches2$bpSaved_rival
  matches2 <- subset(matches2, select = -c(X2ndWon_rival ))  
  matches2 <- subset(matches2, select = -c(bpSaved_rival ))  
  matches3 <- subset(matches2, id_rival != id)
  
  players$appear <- ifelse(!is.na(match(players$player_id, unique(m$id))), 1, 0)
  players2 <- players[players$appear == 1, ]
  
  players2 <- players2[, -c(8, 9)]  # Eliminamos columnas no necesarias
  playersTotal <- players2
  frecuenciaTotal <- table(m$id)
  playersTotal$appearences <- frecuenciaTotal[match(playersTotal$player_id, names(frecuenciaTotal))]
  
  # Inicialización de columnas de resultados
  playersTotal$wins <- 0
  playersTotal$loses <- 0
  playersTotal$win_per <- 0
  playersTotal$aces <- 0
  playersTotal$df <- 0
  playersTotal$svpt <- 0
  playersTotal$X1stIn <- 0
  playersTotal$X1stWon <- 0
  playersTotal$X2ndIn <- 0
  playersTotal$X2ndWon <- 0
  playersTotal$SvGms <- 0
  playersTotal$bpSaved <- 0
  playersTotal$bpFaced <- 0
  playersTotal$bpConc <- 0
  playersTotal$SvGmsWon <- 0
  playersTotal$aces_resto <- 0
  playersTotal$df_resto <- 0
  playersTotal$puntos_resto = 0
  playersTotal$X1stIn_resto = 0
  playersTotal$`%1stIn_resto` = 0
  playersTotal$X1stWon_resto = 0
  playersTotal$`%1stWon_resto` = 0
  playersTotal$X2ndIn_resto = 0
  playersTotal$X2ndWon_resto = 0
  playersTotal$`%2ndWon_resto` = 0
  playersTotal$SvGms_resto = 0
  playersTotal$bpWon_resto = 0
  playersTotal$bpFaced_resto = 0
  playersTotal$`%bpWon_resto` = 0
  playersTotal$bpNotWon_resto = 0
  playersTotal$RestoGmsWon = 0
  playersTotal$`%RestoGmsWon` = 0
  
  # Calculamos victorias
  winsTotal <- table(m$id[m$player_type == 'winner'])
  winning_players <- names(winsTotal)
  playersTotal <- playersTotal %>%
    mutate(wins = ifelse(player_id %in% winning_players,
                         winsTotal[match(player_id, winning_players)],
                         0))
  playersTotal$loses <- playersTotal$appearences - playersTotal$wins
  playersTotal$win_per <- round(playersTotal$wins / playersTotal$appearences * 100, 2)
  
  # Agregamos estadísticas adicionales
  tabla_aces <- aggregate(ace ~ id, matches, sum)
  playersTotal$aces <- tabla_aces$ace[match(playersTotal$player_id, tabla_aces$id)]
  
  tabla_df <- aggregate(df ~ id, matches, sum)
  playersTotal$df <- tabla_df$df[match(playersTotal$player_id, tabla_df$id)]
  
  tabla_svpt <- aggregate(svpt ~ id, matches, sum)
  playersTotal$svpt <- tabla_svpt$svpt[match(playersTotal$player_id, tabla_svpt$id)]
  
  
  tabla_X1stIn <- aggregate(X1stIn ~ id, matches, sum)
  playersTotal$X1stIn <- tabla_X1stIn$X1stIn[match(playersTotal$player_id, tabla_X1stIn$id)]
  playersTotal$`%1stIn` <- round(playersTotal$X1stIn / playersTotal$svpt * 100, 2)
  
  tabla_X1stWon <- aggregate(X1stWon ~ id, matches, sum)
  playersTotal$X1stWon <- tabla_X1stWon$X1stWon[match(playersTotal$player_id, tabla_X1stWon$id)]
  playersTotal$`%1stWon` <- round(playersTotal$X1stWon / playersTotal$X1stIn * 100, 2)
  
  playersTotal$X2ndIn <- playersTotal$svpt - playersTotal$X1stIn - playersTotal$df
  
  tabla_X2ndWon <- aggregate(X2ndWon ~ id, matches, sum)
  playersTotal$X2ndWon <- tabla_X2ndWon$X2ndWon[match(playersTotal$player_id, tabla_X2ndWon$id)]
  playersTotal$`%2ndWon` <- round(playersTotal$X2ndWon / playersTotal$X2ndIn * 100, 2)
  
  tabla_SvGms <- aggregate(SvGms ~ id, matches, sum)
  playersTotal$SvGms <- tabla_SvGms$SvGms[match(playersTotal$player_id, tabla_SvGms$id)]
  
  tabla_bpSaved <- aggregate(bpSaved ~ id, matches, sum)
  playersTotal$bpSaved <- tabla_bpSaved$bpSaved[match(playersTotal$player_id, tabla_bpSaved$id)]
  
  tabla_bpFaced <- aggregate(bpFaced ~ id, matches, sum)
  playersTotal$bpFaced <- tabla_bpFaced$bpFaced[match(playersTotal$player_id, tabla_bpFaced$id)]
  
  playersTotal$bpConc <- playersTotal$bpFaced - playersTotal$bpSaved
  playersTotal$`%bpSaved` <- round(playersTotal$bpSaved / playersTotal$bpFaced * 100, 2)
  playersTotal$SvGmsWon <- playersTotal$SvGms - playersTotal$bpConc
  playersTotal$`%SvGmsWon` <- round(playersTotal$SvGmsWon / playersTotal$SvGms * 100, 2)
  
  #Resto
  
  tabla_aces3 = aggregate(ace_resto ~ id, matches3, sum)
  playersTotal$aces_resto=tabla_aces3$ace_resto[playersTotal$player_id==tabla_aces3$id]
  #Dobles Faltas
  tabla_df3 = aggregate(df_resto ~ id, matches3, sum)
  playersTotal$df_resto=tabla_df3$df_resto[playersTotal$player_id==tabla_df3$id]
  #Puntos de servicio
  tabla_svpt3 = aggregate(puntos_resto ~ id, matches3, sum)
  playersTotal$puntos_resto=tabla_svpt3$puntos_resto[playersTotal$player_id==tabla_svpt3$id]
  #Primeros saques metidos 
  tabla_X1stIn3 = aggregate(X1stIn_resto ~ id, matches3, sum)
  playersTotal$X1stIn_resto=tabla_X1stIn3$X1stIn_resto[playersTotal$player_id==tabla_X1stIn3$id]
  #% de primeros saques metidos
  playersTotal$`%1stIn_resto`=round(playersTotal$X1stIn_resto/playersTotal$puntos_resto*100, 2)
  #Primeros saques ganados
  tabla_X1stWon3 = aggregate(X1stWon_resto ~ id, matches3, sum)
  playersTotal$X1stWon_resto=tabla_X1stWon3$X1stWon_resto[playersTotal$player_id==tabla_X1stWon3$id]
  #% de primeros saques ganados
  playersTotal$`%1stWon_resto`=round(playersTotal$X1stWon_resto/playersTotal$X1stIn_resto*100, 2)
  #Segundos saques metidos 
  playersTotal$X2ndIn_resto=playersTotal$puntos_resto - playersTotal$X1stIn_resto - playersTotal$df_resto
  #Segundos saques ganados
  tabla_X2ndWon3 = aggregate(X2ndWon_resto ~ id, matches3, sum)
  playersTotal$X2ndWon_resto=tabla_X2ndWon3$X2ndWon_resto[playersTotal$player_id==tabla_X2ndWon3$id]
  #% de segundos saques ganados 
  playersTotal$`%2ndWon_resto`=round(playersTotal$X2ndWon_resto/playersTotal$X2ndIn_resto*100,2)
  #Juegos de servicios
  tabla_SvGms3 = aggregate(SvGms_resto ~ id, matches3, sum)
  playersTotal$SvGms_resto=tabla_SvGms3$SvGms_resto[playersTotal$player_id==tabla_SvGms3$id]
  #Puntos de break ganados
  tabla_bpSaved3 = aggregate(bpWon ~ id, matches3, sum)
  playersTotal$bpWon_resto=tabla_bpSaved3$bpWon[playersTotal$player_id==tabla_bpSaved3$id]
  #Puntos de break 
  tabla_bpFaced3 = aggregate(bpFaced_resto ~ id, matches3, sum)
  playersTotal$bpFaced_resto=tabla_bpFaced3$bpFaced_resto[playersTotal$player_id==tabla_bpFaced3$id]
  #% de puntos de break salvados
  playersTotal$`%bpWon_resto`=round(playersTotal$bpWon_resto/playersTotal$bpFaced_resto*100,2)
  playersTotal$bpNotWon_resto=playersTotal$bpFaced_resto - playersTotal$bpWon_resto
  playersTotal$RestoGmsWon=playersTotal$bpWon_resto
  playersTotal$`%RestoGmsWon`=round(playersTotal$RestoGmsWon/playersTotal$SvGms_resto*100, 2)
  
  
  #Medias
  playersTotal_Avg = playersTotal[,1:11, drop = FALSE]
  playersTotal_Avg$aces = playersTotal$aces/playersTotal$appearences
  playersTotal_Avg$df = playersTotal$df/playersTotal$appearences
  playersTotal_Avg$svpt = playersTotal$svpt/playersTotal$appearences
  playersTotal_Avg$x1stIn = playersTotal$X1stIn/playersTotal$appearences
  playersTotal_Avg$`%1stIn` = playersTotal$`%1stIn`
  playersTotal_Avg$x1stWon = playersTotal$X1stWon/playersTotal$appearences
  playersTotal_Avg$`%1stWon` = playersTotal$`%1stWon`
  playersTotal_Avg$x2ndIn = playersTotal$X2ndIn/playersTotal$appearences
  playersTotal_Avg$`%2ndWon` = playersTotal$`%2ndWon`
  playersTotal_Avg$x2ndWon = playersTotal$X2ndWon/playersTotal$appearences
  playersTotal_Avg$SvGms = playersTotal$SvGms/playersTotal$appearences
  playersTotal_Avg$bpSaved = playersTotal$bpSaved/playersTotal$appearences
  playersTotal_Avg$bpFaced = playersTotal$bpFaced/playersTotal$appearences
  playersTotal_Avg$bpConc = playersTotal$bpConc/playersTotal$appearences
  playersTotal_Avg$`%bpSaved` = playersTotal$`%bpSaved`
  playersTotal_Avg$SvGmsWon = playersTotal$SvGmsWon/playersTotal$appearences
  playersTotal_Avg$`%SvGmsWon` = playersTotal$`%SvGmsWon`
  playersTotal_Avg$aces_resto = playersTotal$aces_resto/playersTotal$appearences
  playersTotal_Avg$df_resto = playersTotal$df_resto/playersTotal$appearences
  playersTotal_Avg$puntos_resto = playersTotal$puntos_resto/playersTotal$appearences
  playersTotal_Avg$x1stIn_resto = playersTotal$X1stIn_resto/playersTotal$appearences
  playersTotal_Avg$`%1stIn_resto` = playersTotal$`%1stIn_resto`
  playersTotal_Avg$x1stWon_resto = playersTotal$X1stWon_resto/playersTotal$appearences
  playersTotal_Avg$`%1stWon_resto` = playersTotal$`%1stWon_resto`
  playersTotal_Avg$x2ndIn_resto = playersTotal$X2ndIn_resto/playersTotal$appearences
  playersTotal_Avg$x2ndWon_resto = playersTotal$X2ndWon_resto/playersTotal$appearences
  playersTotal_Avg$`%2ndWon_resto` = playersTotal$`%2ndWon_resto`
  playersTotal_Avg$SvGms_resto = playersTotal$SvGms_resto/playersTotal$appearences
  playersTotal_Avg$bpWon_resto = playersTotal$bpWon_resto/playersTotal$appearences
  playersTotal_Avg$bpFaced_resto = playersTotal$bpFaced_resto/playersTotal$appearences
  playersTotal_Avg$`%bpWon_resto` = playersTotal$`%bpWon_resto`
  playersTotal_Avg$bpNotWon_resto = playersTotal$bpNotWon_resto/playersTotal$appearences
  playersTotal_Avg$RestoGmsWon = playersTotal$RestoGmsWon/playersTotal$appearences
  playersTotal_Avg$`%RestoGmsWon` = playersTotal$`%RestoGmsWon`
  
  matches_list[[i]] <- matches3
  jugadores_list[[i]] <- playersTotal
  medias_list[[i]]<- playersTotal_Avg
  
}

directorio <- "../data/processed-data/year-data"

# Crear la carpeta si no existe
if (!dir.exists(directorio)) {
  dir.create(directorio)
}

# Guardar cada data frame de la lista como un archivo CSV
for (i in seq_along(jugadores_list)) {
  # Generar el nombre del archivo
  archivo_csv <- paste0(directorio, "/players_", 1999 + i, ".csv")
  archivo_csv2 <- paste0(directorio, "/matches_", 1999 + i, ".csv")
  archivo_csv3 <- paste0(directorio, "/players_avg_", 1999 + i, ".csv")
  
  # Guardar el data frame como CSV
  write.csv(jugadores_list[[i]], archivo_csv, row.names = FALSE)
  write.csv(matches_list[[i]], archivo_csv2, row.names = FALSE)
  write.csv(medias_list[[i]], archivo_csv3, row.names = FALSE)
  
}

player_ids <- unlist(lapply(jugadores_list, function(df) df$player_id))

unique_player_ids <- unique(player_ids)


players2 = subset(players, player_id %in% unique_player_ids)

# save(medias_list, jugadores_list, matches_list, players2, file='AVG_año.Rdata')

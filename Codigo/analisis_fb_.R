rm(list=ls())

library(tidyverse)
library(plyr)
library(readr)
library(janitor)
library(lubridate)
library(dplyr)
library(plyr)
library(tidyquant)
library(gridExtra)
library(RColorBrewer)
library(reshape)
library(gtools)
library(chron)
library(treemapify)
library(Hmisc)

data <- read_csv("out/movimientos_entre_administraciones_08062020.csv", locale = locale(encoding = "UTF-8"))
min(as.Date(data$fecha))
max(as.Date(data$fecha))
dir.create("graficas/08062020/")
###### lugares originales #####
data <- tempo

del <- c( "Álvaro Obregón", 
          "Azcapotzalco",	
          "Benito Juárez",	
          "Coyoacán",	
          "Cuajimalpa de Morelos",
          "Cuauhtémoc",	
          "Gustavo A. Madero",
          "Iztacalco",
          "Iztapalapa",	
          "La Magdalena Contreras",
          "Miguel Hidalgo",
          "Milpa Alta",
          "Tláhuac",
          "Tlalpan",	
          "Venustiano Carranza",
          "Xochimilco")

mun <-setdiff(ending_region_name_orig, del)

lugares <- data.frame(name=c(as.character(data$ending_region_name), as.character(data$starting_region_name)) %>% unique())
lugares <- lugares %>% 
        mutate(edo = ifelse(name %in% del, "CDMX", "Otro")) %>% 
        arrange(edo) %>% 
        mutate(start_num = seq(1:nrow(lugares)))

rm(ending_region_name_orig, starting_region_name_orig, data_orig, end, tempo)
min(as.Date(data$fecha))
max(as.Date(data$fecha))

##### ORIGEN - DESTINO #####

#data$starting_region_name <- as.factor(data$starting_region_name)
#data$ending_region_name <- as.factor(data$ending_region_name)

tempo <- data %>% 
        #filter(fecha < as.Date("2020-04-06")) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        mutate(dia = weekdays(fecha),
               percent_change = round(percent_change, 1),
               sem = strftime(fecha, format = "%V"),
               ) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        #filter(ending_region_name %in% del) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(
               sem = as.numeric(sem),
               hr = as.character(substr(hora, 1,2)),
               texto_colores = ifelse(percent_change <= -80, "Menores a -80",
                                      ifelse(percent_change > -80 & percent_change <= -60, "-80 a -60",
                                             ifelse(percent_change > -60 & percent_change <= -40, "-60 a -40",
                                                    ifelse(percent_change > -40 & percent_change <= -20, "-40 a -20",
                                                           ifelse(percent_change > -20 & percent_change <= 0, "-20 a 0",
                                                                  ifelse(percent_change > 0 & percent_change <= 20, "0 a 20",
                                                                         ifelse(percent_change > 20 & percent_change <= 40, "20 a 40", 
                                                                                ifelse(percent_change > 40, "Mayores a 40","Fallaste")))))))))

tempo$texto_colores <- factor(tempo$texto_colores, levels = c(
        "Menores a -80", "-80 a -60", "-60 a -40", "-40 a -20", "-20 a 0", "0 a 20", "20 a 40", "Mayores a 40"))

palette_perc <- c("#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0","#ffed6f", "#ff7f00", "#e31a1c")

table(unique(tempo$sem))
tempo$ending_region_name <- as.factor(tempo$ending_region_name)
tempo$end_num = as.numeric(tempo$ending_region_name)

expansor_tempo <- expand.grid(starting_region_name = unique(tempo$starting_region_name),
                            ending_region_name = unique(tempo$ending_region_name),
                            fecha = unique(tempo$fecha),
                            hr = unique(tempo$hr))

expansor_tempo <- left_join(expansor_tempo, tempo[c("fecha",
                                                    "hr",
                                                  "starting_region_name", 
                                                  "ending_region_name", 
                                                  "percent_change", 
                                                  "start_edo",
                                                  "end_edo",
                                                  "end_num", 
                                                  "texto_colores",
                                                  "dia")])
   
expansor_tempo <- expansor_tempo %>% 
        mutate(percent_change_texto = ifelse(is.na(percent_change), "", as.character(percent_change)),
               #end_num = as.numeric(ending_region_name),
               dia = weekdays(fecha),
               sem = strftime(fecha, format = "%V"))

expansor_tempo <- expansor_tempo %>%
        mutate(
                texto_sem = ifelse(sem == 09, "27/feb al\n01/mar",
                                   ifelse(sem == 10, "02/mar al\n08/mar",
                                          ifelse(sem == 11, "09/mar al\n15/mar",
                                                 ifelse(sem == 12, "16/mar al\n22/mar",
                                                        ifelse(sem == 13, "23/mar al\n29/mar", 
                                                               ifelse(sem == 14, "30/mar al\n05/abr",
                                                                      ifelse(sem == 15, "06/abr al\n12/abr", 
                                                                             ifelse(sem == 16, "13/abr al\n19/abr",
                                                                                    ifelse(sem == 17, "20/abr al\n27/abr",
                                                                                           ifelse(sem == 18, "28/abr al\n03/may",
                                                                                                  ifelse(sem == 19, "04/may al\n10/may",
                                                                                                         ifelse(sem == 20, "11/may al\n17/may", 
                                                                                                                ifelse(sem == 21, "18/may al\n24/may",
                                                                                                                       ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                              ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                                     ))))))))))))))))

expansor_tempo$texto_sem = factor(expansor_tempo$texto_sem, 
                                  levels=c("27/feb al\n01/mar",
                                           "02/mar al\n08/mar",
                                           "09/mar al\n15/mar", 
                                           "16/mar al\n22/mar", 
                                           "23/mar al\n29/mar", 
                                           "30/mar al\n05/abr",
                                           "06/abr al\n12/abr",
                                           "13/abr al\n19/abr",
                                           "20/abr al\n27/abr",
                                           "28/abr al\n03/may",
                                           "04/may al\n10/may",
                                           "11/may al\n17/may",
                                           "18/may al\n24/may",
                                           "25/may al\n31/may",
                                           "01/jun al\n07/jun"))


dias <- as.character(unique(tempo$dia))
horas <- unique(tempo$hr)

semanas <- data.frame(sem = expansor_tempo$sem, fecha = expansor_tempo$fecha, dia = expansor_tempo$dia)
semanas <- distinct(semanas)
semanas <- arrange(semanas, -as.numeric(sem), as.Date(fecha))
View(semanas)
max(as.character(semanas$sem))
min(as.character(semanas$sem))
rm(semanas)


##### cdmx-cdmx####

dir.create("graficas/08062020/CDMX_CDMX_hr/") # esto debe de ser automatico

for( j in 1:length(horas)){
        for(i in 1:length(dias)){
        
dias_tempo <- expansor_tempo %>% 
        filter(ending_region_name %in% del) %>% 
        filter(fecha >= as.Date("2020-03-16")) %>% 
        #filter(fecha <= as.Date("2020-04-19")) %>% 
        filter(dia == dias[i]  & hr == horas[j]) %>% 
        filter(start_edo == "CDMX" & end_edo == "CDMX") %>% 
        select(dia, starting_region_name, ending_region_name, percent_change, texto_sem, 
               percent_change_texto, end_num, hr, fecha, sem, texto_colores) %>% 
        arrange(starting_region_name, ending_region_name, dia, texto_sem) %>% 
        distinct() 

ggplot(dias_tempo, aes(x = starting_region_name, y = reorder(ending_region_name, -end_num),
              fill= texto_colores)) + 
        geom_tile() +
        geom_text(aes(label = percent_change_texto),color = "black", size = 5.4) + 
        facet_grid(texto_sem ~ .) +
        scale_fill_manual(values = palette_perc, na.value = "grey98") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 16),
              axis.title.x = element_text(size = 25),
              axis.text.y = element_text(size = 20),
              axis.title.y = element_text(size = 25),
              plot.title = element_text(size = 29, face = "bold"),
              plot.caption = element_text(size = 18),
              legend.title=element_text(size=10), 
              legend.text=element_text(size=20),
              strip.text = element_text(size=20)) +
        labs(x = "Origen", y = "Destino", 
             title = paste(capitalize(as.character(dias[i])), 
                           ".\nVariación porcentual de la movilidad por\norigen-destino por semana"),
             caption = paste("Variaciones correspondientes a las", dias_tempo$hr, "horas")) +
        guides(fill = guide_legend(title = ""))

ggsave(paste("graficas/08062020/CDMX_CDMX_hr/CDMX_CDMX_", 
             paste(dias[i], horas[j], sep = ""),".jpg", sep=""),  width = 16, height = 31)
}}
        

##### cdmx - edo ####

dir.create("graficas/08062020/CDMX_EDO_hr/") # esto debe de ser automatico


for( j in 1:length(horas)){
        for(i in 1:length(dias)){
                
dias_tempo <- expansor_tempo %>% 
        #filter(ending_region_name %in% del) %>% 
        filter(fecha >= as.Date("2020-03-16")) %>% 
        #filter(fecha < as.Date("2020-04-19")) %>% 
        filter(dia == dias[i]  & hr == horas[j]) %>% 
        filter(start_edo == "CDMX" & end_edo == "Otro") %>% 
        select(dia, starting_region_name, ending_region_name, percent_change, texto_sem, 
               percent_change_texto, end_num, hr, fecha, sem, texto_colores) %>% 
        arrange(starting_region_name, ending_region_name, dia, texto_sem) %>% 
        distinct() 

ggplot(dias_tempo, aes(x = starting_region_name, y = reorder(ending_region_name, -end_num),
                       fill= texto_colores)) + 
        geom_tile() +
        geom_text(aes(label = percent_change_texto),color = "black", size = 3.5) + 
        facet_grid(texto_sem ~ .) +
        #scale_fill_gradientn(colours=c("#238443","#c0dda8","white", "yellow","red"), na.value = "grey98") +
        scale_fill_manual(values = palette_perc, na.value = "grey98") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 16),
              axis.title.x = element_text(size = 25),
              axis.text.y = element_text(size = 20),
              axis.title.y = element_text(size = 10),
              plot.title = element_text(size = 29, face = "bold"),
              plot.caption = element_text(size = 18),
              legend.title=element_text(size=10), 
              legend.text=element_text(size=15),
              strip.text = element_text(size=20)) +
        labs(x = "Origen", y = "Destino", 
             title = paste(capitalize(as.character(dias[i])), 
                           ".\nVariación porcentual de la movilidad por\norigen-destino por semana"),
             caption = paste("Variaciones correspondientes a las", dias_tempo$hr, "horas")) +
        guides(fill = guide_legend(title = ""))
        
ggsave(paste("graficas/08062020/CDMX_EDO_hr/CDMX_EDO_hr_", 
             paste(dias[i], horas[j], sep = ""),".jpg", sep=""),  width = 16, height = 38)
        }}


##### edo - edo ####

dir.create("graficas/08062020/EDO_EDO_hr/") # esto debe de ser automatico


for( j in 1:length(horas)){
        for(i in 1:length(dias)){
                
                dias_tempo <- expansor_tempo %>% 
                        filter(ending_region_name %in% mun) %>% 
                        filter(fecha >= as.Date("2020-03-16")) %>% 
                        #filter(fecha < as.Date("2020-04-19")) %>% 
                        filter(dia == dias[i]  & hr == horas[j]) %>% 
                        filter(start_edo == "Otro" & end_edo == "Otro") %>% 
                        select(dia, starting_region_name, ending_region_name, percent_change, texto_sem, 
                               percent_change_texto, end_num, hr, fecha, sem, texto_colores) %>% 
                        arrange(starting_region_name, ending_region_name, dia, texto_sem) %>% 
                        distinct()
                
                ggplot(dias_tempo, aes(x = starting_region_name, y = reorder(ending_region_name, -end_num),
                                       fill= texto_colores)) + 
                        geom_tile() +
                        geom_text(aes(label = percent_change_texto),color = "black", size = 3) + 
                        facet_grid(texto_sem ~ .) +
                        #scale_fill_gradientn(colours=c("#238443","#c0dda8","white", "yellow","red"), na.value = "grey98") +
                        scale_fill_manual(values = palette_perc, na.value = "grey98") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 16),
                              axis.title.x = element_text(size = 25),
                              axis.text.y = element_text(size = 13),
                              axis.title.y = element_text(size = 25),
                              plot.title = element_text(size = 29, face = "bold"),
                              plot.caption = element_text(size = 18),
                              legend.title=element_text(size=10), 
                              legend.text=element_text(size=15),
                              strip.text = element_text(size=20)) +
                        labs(x = "Origen", y = "Destino", 
                             title = paste(capitalize(as.character(dias[i])), 
                                           ".\nVariación porcentual de la movilidad por\norigen-destino por semana"),
                             caption = paste("Variaciones correspondientes a las", dias_tempo$hr, "horas")) +
                        guides(fill = guide_legend(title = ""))
                
                ggsave(paste("graficas/08062020/EDO_EDO_hr/EDO_EDO_hr_", 
                             paste(dias[i], horas[j], sep = ""),".jpg", sep=""),  width = 18, height = 35)
}}


##### edo - cdmx #### 

dir.create("graficas/08062020/EDO_CDMX_hr/") #### cambia

for( j in 1:length(horas)){
        for(i in 1:length(dias)){
                
                dias_tempo <- expansor_tempo %>% 
                        filter(ending_region_name %in% del) %>% 
                        filter(fecha >= as.Date("2020-03-16")) %>% 
                        #filter(fecha < as.Date("2020-04-19")) %>% 
                        filter(dia == dias[i]  & hr == horas[j]) %>% 
                        filter(start_edo == "Otro" & end_edo == "CDMX") %>% 
                        select(dia, starting_region_name, ending_region_name, percent_change, texto_sem, 
                               percent_change_texto, end_num, hr, fecha, sem, texto_colores) %>% 
                        arrange(starting_region_name, ending_region_name, dia, texto_sem) %>% 
                        distinct()
                
                ggplot(dias_tempo, aes(x = starting_region_name, y = reorder(ending_region_name, -end_num),
                                       fill= texto_colores)) + 
                        geom_tile() +
                        geom_text(aes(label = percent_change_texto),color = "black", size = 3.5) + 
                        facet_grid(texto_sem ~ .) +
                        #scale_fill_gradientn(colours=c("#238443","#c0dda8","white", "yellow","red"), na.value = "grey98") +
                        scale_fill_manual(values = palette_perc, na.value = "grey98") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 16),
                              axis.title.x = element_text(size = 25),
                              axis.text.y = element_text(size = 20),
                              axis.title.y = element_text(size = 25),
                              plot.title = element_text(size = 29, face = "bold"),
                              plot.caption = element_text(size = 18),
                              legend.title=element_text(size=10), 
                              legend.text=element_text(size=15),
                              strip.text = element_text(size=20)) +
                        labs(x = "Origen", y = "Destino", 
                             title = paste(capitalize(as.character(dias[i])), 
                                           ".\nVariación porcentual de la movilidad por\norigen-destino por semana"),
                             caption = paste("Variaciones correspondientes a las", dias_tempo$hr, "horas")) +
                        guides(fill = guide_legend(title = ""))
                
                ggsave(paste("graficas/08062020/EDO_CDMX_hr/EDO_CDMX_hr_", 
                             paste(dias[i], horas[j], sep = ""),".jpg", sep=""),  width = 16, height = 31)
}}

rm( dias, horas, i, dias_tempo, expansor_tempo, palette_perc, j)


##### FIEBRES ##### 
##### fiebre cdmx-cdmx ####

data$starting_region_name <- as.character(data$starting_region_name)
data$ending_region_name <- as.character(data$ending_region_name)

tempo <- data %>% 
        filter(!is.na(difference)) %>% 
        #filter(fecha >= as.Date("2020-03-16")) %>%  # filtro para informe
        mutate(dia = weekdays(fecha),
               sem = strftime(fecha, format = "%V"),
               percent_change = round(percent_change, 1)) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        filter(ending_region_name %in% del) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(
               end_num = as.numeric(ending_region_name),
               sem = as.numeric(sem),
               hr = as.character(substr(hora, 1,2)),
               texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                  ifelse(sem == 10, "02/mar al\n08/mar",
                                         ifelse(sem == 11, "09/mar al\n15/mar",
                                                ifelse(sem == 12, "16/mar al\n22/mar",
                                                       ifelse(sem == 13, "23/mar al\n29/mar", 
                                                              ifelse(sem == 14, "30/mar al\n05/abr",
                                                                     ifelse(sem == 15, "06/abr al\n12/abr", 
                                                                            ifelse(sem == 16, "13/abr al\n19/abr", 
                                                                                   ifelse(sem == 17, "20/abr al\n27/abr",
                                                                                          ifelse(sem == 19, "04/may al\n10/may",
                                                                                                 ifelse(sem == 20, "11/may al\n17/may",
                                                                                                        ifelse(sem == 21, "18/may al\n24/may",
                                                                                                               ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                      ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                      )))))))))))))),
               fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "))

tempo$ending_region_name <- as.character(tempo$ending_region_name)
tempo$end_num = as.numeric(tempo$ending_region_name)

alcaldias <- tempo %>%
        select(start_edo, 
               end_edo,
               starting_region_name, 
               ending_region_name, 
               fecha, 
               hr,
               baseline_people_moving,
               crisis_people_moving,
               difference,
               percent_change)
        
##### ending #####
alcaldias_cdmx_cdmx <- alcaldias %>%
        filter(start_edo == "CDMX" & end_edo == "CDMX") %>% 
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        dplyr::group_by(ending_region_name, 
                       fecha, 
                       hr
                       ) %>% 
        dplyr::summarize(
                baseline_people_moving = mean(baseline_people_moving),
                crisis_people_moving = mean(crisis_people_moving),
                difference = mean(difference),
                percent_change = mean(percent_change)) %>% 
        dplyr::ungroup() %>% 
        arrange(ending_region_name, fecha, hr)

expansor_alcaldias <- expand.grid(
                                  ending_region_name = unique(tempo$ending_region_name),
                                  #starting_region_name = unique(tempo$starting_region_name),
                                  fecha = seq(min(as.Date(tempo$fecha)), max(as.Date(tempo$fecha)), by="day"),
                                  hr = unique(tempo$hr))

expansor_alcaldias$hr <- as.character(expansor_alcaldias$hr)

tempo_alcaldias_cdmx <- left_join(expansor_alcaldias, alcaldias_cdmx_cdmx)

tempo_alcaldias_cdmx <- tempo_alcaldias_cdmx %>% 
        mutate(#fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
               percent_change = ifelse(is.na(percent_change), 0, percent_change)
               )

#tempo_alcaldias_cdmx$fecha_hora_1 <- as.POSIXct(strftime(tempo_alcaldias_cdmx$fecha_hora, "%Y-%m-%d %H:%M:S"))

##### startin ####
alcaldias_cdmx_cdmx <- alcaldias %>%
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        filter(start_edo == "CDMX" & end_edo == "CDMX") %>% 
        dplyr::group_by(starting_region_name, 
                        fecha, 
                        hr
        ) %>% 
        dplyr::summarize(
                baseline_people_moving = mean(baseline_people_moving),
                crisis_people_moving = mean(crisis_people_moving),
                difference = mean(difference),
                percent_change = mean(percent_change)) %>% 
        dplyr::ungroup() %>% 
        arrange(starting_region_name, fecha, hr)


expansor_alcaldias <- expand.grid(
        #ending_region_name = unique(tempo$ending_region_name),
        starting_region_name = unique(tempo$starting_region_name),
        fecha = seq(min(as.Date(tempo$fecha)), max(as.Date(tempo$fecha)), by="day"),
        hr = unique(tempo$hr))

expansor_alcaldias$hr <- as.character(expansor_alcaldias$hr)

tempo_alcaldias_cdmx_start <- left_join(expansor_alcaldias, alcaldias_cdmx_cdmx)

tempo_alcaldias_cdmx_start <- tempo_alcaldias_cdmx_start %>% 
        mutate(#fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
               percent_change = ifelse(is.na(percent_change), 0, percent_change)
               )

#tempo_alcaldias_cdmx_start$fecha_hora <- as.POSIXct(strftime(tempo_alcaldias_cdmx_start$fecha_hora, "%Y-%m-%d %H:%M:S"))

##### fiebre grafica #####

rm(expansor_alcaldias)

tempo_alcaldias_cdmx$lugar <- "Destino"
tempo_alcaldias_cdmx$punto <- tempo_alcaldias_cdmx$ending_region_name
tempo_alcaldias_cdmx$ending_region_name <- NULL

tempo_alcaldias_cdmx_start$lugar <- "Origen"
tempo_alcaldias_cdmx_start$punto <- tempo_alcaldias_cdmx_start$starting_region_name
tempo_alcaldias_cdmx_start$starting_region_name <- NULL

tempo_alcaldias_cdmx_todo <- rbind(tempo_alcaldias_cdmx, tempo_alcaldias_cdmx_start)

tempo_alcaldias_cdmx_todo <- tempo_alcaldias_cdmx_todo %>% 
        mutate(fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
               percent_change_ = (percent_change/100),
               percent_change = ifelse(is.na(percent_change), 0, percent_change),
               difference = ifelse(is.na(difference), 0, difference))

##### fechas ########
#tempo_alcaldias_cdmx_todo$fecha_hora_1 <- strptime(as.character(tempo_alcaldias_cdmx_todo$fecha_hora), "%Y-%m-%d %H:%M:S")
#tempo_alcaldias_cdmx_todo$fecha_hora_1 <- ymd_hms(tempo_alcaldias_cdmx_todo$fecha_hora, truncated=2)

#horas <- as.POSIXct(tempo_alcaldias_cdmx_todo$fecha_hora)

# min(tempo_alcaldias_cdmx_todo$fecha_hora_1)
# max(tempo_alcaldias_cdmx_todo$fecha_hora_1)
# str(tempo_alcaldias_cdmx_todo$fecha_hora_1)

dtparts = t(as.data.frame(strsplit(tempo_alcaldias_cdmx_todo$fecha_hora,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                                 format=c('y-m-d','h:m:s'))
thetimes = as.POSIXct(thetimes)
min(thetimes)
max(thetimes)

tempo_alcaldias_cdmx_todo$fecha_hora_1 <- thetimes

min(tempo_alcaldias_cdmx_todo$fecha_hora_1)
max(tempo_alcaldias_cdmx_todo$fecha_hora_1)
str(tempo_alcaldias_cdmx_todo$fecha_hora_1)

dir.create("graficas/08062020/Fiebres/")

ggplot(tempo_alcaldias_cdmx_todo[tempo_alcaldias_cdmx_todo$punto %in% del & tempo_alcaldias_cdmx_todo$fecha > as.Date("2020-03-15"),]) +
        geom_line( tempo_alcaldias_cdmx_todo[tempo_alcaldias_cdmx_todo$lugar == "Destino" & tempo_alcaldias_cdmx_todo$punto %in% del,],
                  mapping = aes(x = fecha_hora_1, y = percent_change, group = punto, color = lugar),
                  size = 1, alpha = 1) +
        geom_line( tempo_alcaldias_cdmx_todo[tempo_alcaldias_cdmx_todo$lugar == "Origen" & tempo_alcaldias_cdmx_todo$punto %in% del,],
                mapping = aes(x = fecha_hora_1, y = percent_change, group = punto, color = lugar),
                size = 1, alpha = 0.8) +
        geom_hline(aes(yintercept = 0), color = "red") +
        facet_wrap(. ~ punto, scales = "free_y") +
        scale_x_datetime(
               breaks = seq(as.POSIXct("2020-03-15 00:00:00"),
                           as.POSIXct("2020-06-07 16:00:00"), "2 days"),
             date_labels = "%b %d",
             expand = c(0, 0),
             limits = c(
                     as.POSIXct("2020-03-15 00:00:00"),
                     as.POSIXct("2020-06-07 16:00:00")
                     )) +
        theme_bw() +
        theme(axis.text.x = element_text( size = 11, angle = 90, vjust = 0.5),
              axis.title.x = element_text(size = 13),
              axis.text.y = element_text(size = 11),
              axis.title.y = element_text(size = 13),
              plot.title  = element_text(size = 18),
              plot.caption = element_text(size = 10),
              strip.text.x = element_text(size = 12),
              legend.position =  "bottom",
              legend.text=element_text(size=14)) +
        scale_color_manual(values = c(
                "#377eb8",
                "#4daf4a"
        )) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) + 
        labs(x = "\nFecha", y = "Porcentaje de cambio respecto a la movilidad base",
             title = "Comparación entre el movimiendo de personas según la alcaldía de origen y destino:\n16 de marzo al 7 de junio",
             caption = "Nota: Se excluyó el movimiento dentro de la misma alcaldía
                              La línea roja refleja la movilidad base",
             color = "")
        
ggsave("graficas/08062020/Fiebres/fiebre_CDMX_origen_destino.jpg", width = 16, height = 12)

rm(alcaldias, alcaldias_cdmx_cdmx, tempo_alcaldias_cdmx, thetimes)

##### fiebre edo_cdmx #####

alcaldias <- tempo %>%
        select(start_edo, 
               end_edo,
               starting_region_name, 
               ending_region_name, 
               fecha, 
               hr,
               baseline_people_moving,
               crisis_people_moving,
               difference,
               percent_change)

alcaldias_edo_cdmx <- alcaldias %>%
        filter(start_edo == "Otro" & end_edo == "CDMX") %>% 
        dplyr::group_by(ending_region_name, 
                        fecha, 
                        hr
        ) %>% 
        dplyr::summarize(
                baseline_people_moving = mean(baseline_people_moving),
                crisis_people_moving = mean(crisis_people_moving),
                difference = mean(difference),
                percent_change = mean(percent_change)) %>% 
        dplyr::ungroup() 

expansor_alcaldias <- expand.grid(
        ending_region_name = unique(tempo$ending_region_name),
        #starting_region_name = unique(tempo$starting_region_name),
        fecha = seq(min(as.Date(tempo$fecha)), max(as.Date(tempo$fecha)), by="day"),
        hr = unique(tempo$hr))

expansor_alcaldias$hr <- as.character(as.factor(expansor_alcaldias$hr))

tempo_alcaldias_edo <- left_join(expansor_alcaldias, alcaldias_edo_cdmx)

tempo_alcaldias_edo <- tempo_alcaldias_edo %>% 
        mutate(fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
               percent_change = ifelse(is.na(percent_change), 0, percent_change),
               difference = ifelse(is.na(difference), 0, difference))


##### fechas #####
dtparts = t(as.data.frame(strsplit(tempo_alcaldias_edo$fecha_hora,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                 format=c('y-m-d','h:m:s'))
thetimes = as.POSIXct(thetimes)
min(thetimes)
max(thetimes)

tempo_alcaldias_edo$fecha_hora_1 <- thetimes

min(tempo_alcaldias_edo$fecha_hora_1)
max(tempo_alcaldias_edo$fecha_hora_1)
str(tempo_alcaldias_edo$fecha_hora_1)

rm(alcaldias_edo_cdmx, expansor_alcaldias)

ggplot(tempo_alcaldias_edo, 
       aes(x = fecha_hora_1, y = percent_change, group = ending_region_name)) +
        geom_line(color = "#4d4d4d", size = 0.5) +
        geom_hline(aes(yintercept = 0), color = "red") +
        facet_wrap(. ~ ending_region_name, scales = "free_y") +
        scale_x_datetime(
                breaks = seq(as.POSIXct("2020-03-16 00:00:00"),
                             as.POSIXct("2020-06-07 21:00:00"), "2 days"),
                date_labels = "%b %d",
                expand = c(0, 0),
                limits = c(
                        as.POSIXct("2020-03-16 00:00:00"),
                        as.POSIXct("2020-06-07 21:00:00")
                )) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
              axis.title.x = element_text(size = 12),
              axis.text.y = element_text(size = 10),
              axis.title.y = element_text(size = 12),
              plot.title  = element_text(size = 20),
              plot.caption = element_text(size = 10),
              strip.text.x = element_text(size = 10)) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) + 
        labs(x = "Fecha", y = "Cambio porcentual",
             title = "Cambio en el tiempo del movimiento de personas cuyo origen no es la CDMX por alcaldía de destino:\n16 de marzo al 7 de junio",
             caption = "Nota: La línea roja representa la movilidad base"
             )

ggsave("graficas/08062020/Fiebres/fiebre_EDO_CDMX.jpg", width = 16, height = 10)

rm(alcaldias, alcaldias_cdmx_cdmx, alcaldias_edo_cdmx, dtparts, expanspuesor_alcaldias, tempo_alcaldias_cdmx, tempo_alcaldias_cdmx_start,
   tempo_alcaldias_cdmx_todo, tempo_alcaldias_edo, thetimes)

##### fiebre edo_edo #####
##### ending #####
data$starting_region_name <- as.factor(data$starting_region_name)
data$ending_region_name <- as.factor(data$ending_region_name)

data$starting_region_name <- as.character(data$starting_region_name)
data$ending_region_name <- as.character(data$ending_region_name)

lugares$name <- as.character(lugares$name)

tempo <- data %>% 
        #filter(fecha <= as.Date("2020-04-19")) %>% #cambia
        mutate(dia = weekdays(fecha),
               sem = strftime(fecha, format = "%V"),
               percent_change = round(percent_change, 1)) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(
                end_num = as.numeric(ending_region_name),
                sem = as.numeric(sem),
                hr = as.character(substr(hora, 1,2)),
                texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                   ifelse(sem == 10, "02/mar al\n08/mar",
                                          ifelse(sem == 11, "09/mar al\n15/mar",
                                                 ifelse(sem == 12, "16/mar al\n22/mar",
                                                        ifelse(sem == 13, "23/mar al\n29/mar", 
                                                               ifelse(sem == 14, "30/mar al\n05/abr",
                                                                      ifelse(sem == 15, "06/abr al\n12/abr", 
                                                                             ifelse(sem == 16, "13/abr al\n19/abr",
                                                                                    ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                           ifelse(sem == 18, "27/abr al\n03/may",
                                                                                                  ifelse(sem == 19, "04/may al\n10/may",
                                                                                                         ifelse(sem == 20, "11/may al\n17/may",
                                                                                                                ifelse(sem == 21, "18/may al\n24/may",
                                                                                                                       ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                              ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                              ))))))))))))))),
                fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "))

edomex <- tempo %>%
        select(start_edo, 
               end_edo,
               starting_region_name, 
               ending_region_name, 
               fecha, 
               hr,
               baseline_people_moving,
               crisis_people_moving,
               difference,
               percent_change)

movimiento_edo <- edomex %>%
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        filter(start_edo == "Otro" & end_edo == "Otro") %>% 
        dplyr::group_by(ending_region_name, 
                        fecha, 
                        hr
        ) %>% 
        dplyr::summarize(
                baseline_people_moving = mean(baseline_people_moving),
                crisis_people_moving = mean(crisis_people_moving),
                difference = mean(difference),
                percent_change = mean(percent_change)) %>% 
        dplyr::ungroup() 

expansor_edo <- expand.grid(
        ending_region_name = unique(tempo$ending_region_name),
        #starting_region_name = unique(tempo$starting_region_name),
        fecha = seq(min(as.Date(tempo$fecha)), max(as.Date(tempo$fecha)), by="day"),
        hr = unique(tempo$hr))

expansor_edo$hr <- as.character(as.facter(expansor_edo$hr))

tempo_edo_end <- left_join(expansor_edo, movimiento_edo)

# tempo_edo_end <- tempo_alcaldias_edo %>% 
#         mutate(fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
#                percent_change = ifelse(is.na(percent_change), 0, percent_change),
#                difference = ifelse(is.na(difference), 0, difference))

##### starting #####

movimiento_edo <- edomex %>%
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        filter(start_edo == "Otro" & end_edo == "Otro") %>% 
        dplyr::group_by(starting_region_name, 
                        fecha, 
                        hr
        ) %>% 
        dplyr::summarize(
                baseline_people_moving = mean(baseline_people_moving),
                crisis_people_moving = mean(crisis_people_moving),
                difference = mean(difference),
                percent_change = mean(percent_change)) %>% 
        dplyr::ungroup() 

expansor_edo <- expand.grid(
        #ending_region_name = unique(tempo$ending_region_name),
        starting_region_name = unique(tempo$starting_region_name),
        fecha = seq(min(as.Date(tempo$fecha)), max(as.Date(tempo$fecha)), by="day"),
        hr = unique(tempo$hr))

expansor_edo$hr <- as.character(as.factor(expansor_edo$hr))

tempo_edo_start <- left_join(expansor_edo, movimiento_edo)

# tempo_edo_start <- tempo_edo_start %>% 
#         mutate(fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
#                percent_change = ifelse(is.na(percent_change), 0, percent_change),
#                difference = ifelse(is.na(difference), 0, difference))

##### fiebre grafica #####

rm(expansor_edo)

tempo_edo_end$lugar <- "Destino"
tempo_edo_end$punto <- tempo_edo_end$ending_region_name
tempo_edo_end$ending_region_name <- NULL

tempo_edo_start$lugar <- "Origen"
tempo_edo_start$punto <- tempo_edo_start$starting_region_name
tempo_edo_start$starting_region_name <- NULL

tempo_edo_todo <- rbind(tempo_edo_end, tempo_edo_start)

tempo_edo_todo <- tempo_edo_todo %>% 
        mutate(fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " "),
               percent_change = ifelse(is.na(percent_change), 0, percent_change),
               difference = ifelse(is.na(difference), 0, difference))

rm(tempo_edo_end, tempo_edo_start, edomex, movimiento_edo)

##### fechas #####
dtparts = t(as.data.frame(strsplit(tempo_edo_todo$fecha_hora,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                 format=c('y-m-d','h:m:s'))
thetimes = as.POSIXct(thetimes)
min(thetimes)
max(thetimes)

tempo_edo_todo$fecha_hora_1 <- thetimes

min(tempo_edo_todo$fecha_hora_1)
max(tempo_edo_todo$fecha_hora_1)
str(tempo_edo_todo$fecha_hora_1)

rm(dtparts, thetimes)

tempo_edo_todo$punto <- as.character(tempo_edo_todo$punto )
tempo_edo_todo <- tempo_edo_todo %>% dplyr::filter(!punto %in% del)


ggplot(tempo_edo_todo, 
       aes(x = fecha_hora_1, y = difference, group = ending_region_name)) +
        geom_line( tempo_edo_todo[tempo_edo_todo$lugar == "Destino" & tempo_edo_todo$punto %in% mun,],
                   mapping = aes(x = fecha_hora_1, y = percent_change, group = punto, color = lugar),
                   size = 1, alpha = 1) +
        geom_line( tempo_edo_todo[tempo_edo_todo$lugar == "Origen" & tempo_edo_todo$punto %in% mun,],
                   mapping = aes(x = fecha_hora_1, y = percent_change, group = punto, color = lugar),
                   size = 1, alpha = 0.8) +
        geom_hline(aes(yintercept = 0), color = "red") +
        facet_wrap(. ~ punto, scales = "free_y", ncol = 5) +
        scale_x_datetime(
                breaks = seq(as.POSIXct("2020-03-16 00:00:00"),
                             as.POSIXct("2020-06-07 21:00:00"), "2 days"),
                date_labels = "%b %d",
                expand = c(0, 0),
                limits = c(
                        as.POSIXct("2020-03-16 00:00:00"),
                        as.POSIXct("2020-06-07 21:00:00")
                )) +
        theme_bw() +
        theme(axis.text.x = element_text( size = 11, angle = 90, vjust = 0.5),
              axis.title.x = element_text(size = 20),
              axis.text.y = element_text(size = 11),
              axis.title.y = element_text(size = 13),
              plot.title  = element_text(size = 24),
              plot.caption = element_text(size = 18),
              strip.text.x = element_text(size = 22),
              legend.position =  "bottom",
              legend.text=element_text(size=14)) +
        scale_color_manual(values = c(
                "#377eb8",
                "#4daf4a"
        )) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) + 
        labs(x = "\nFecha", y = "Porcentaje de cambio respecto a la movilidad base",
             title = "Edo. de México:\nComparación entre el movimiendo de personas según la municipio de origen y destino:\n16 de marzo al 07 de junio",
             caption = "Nota: Se excluyó el movimiento dentro de la misma alcaldía
                              La línea roja refleja la movilidad base",
             color = "")

ggsave("graficas/08062020/Fiebres/fiebre_EDO_EDO.jpg", width = 36, height = 25)

##### CAMBIOS ALCALDIAS #####
##### diferencia entre semanas #####

data$starting_region_name <- as.factor(data$starting_region_name)
data$ending_region_name <- as.factor(data$ending_region_name)

tempo <- data %>% 
        #filter(fecha < as.Date("2020-04-13")) %>% 
        mutate(dia = weekdays(fecha),
               #sem = week(fecha),
               sem = strftime(fecha, format = "%V")) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        filter(ending_region_name %in% del) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(dia = factor(dia, levels = c("lunes",
                                            "martes",
                                            "miércoles",
                                            "jueves",
                                            "viernes",
                                            "sábado",
                                            "domingo")),
               end_num = as.numeric(ending_region_name),
               sem = as.numeric(sem),
               hr = as.character(substr(hora, 1,2)),
               texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                  ifelse(sem == 10, "02/mar al\n08/mar",
                                         ifelse(sem == 11, "09/mar al\n15/mar",
                                                ifelse(sem == 12, "16/mar al\n22/mar",
                                                       ifelse(sem == 13, "23/mar al\n29/mar", 
                                                              ifelse(sem == 14, "30/mar al\n05/abr",
                                                                     ifelse(sem == 15, "06/abr al\n12/abr", 
                                                                            ifelse(sem == 16, "13/abr al\n19/abr",
                                                                                   ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                          ifelse(sem == 18, "27/abr al\n03/may",
                                                                                                 ifelse(sem == 19, "04/may al\n10/may",
                                                                                                        ifelse(sem == 20, "11/may al\n17/may",
                                                                                                               ifelse(sem == 21, "18/may al\n24/may", 
                                                                                                                      ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                             ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                             ))))))))))))))),
               fecha_hora = paste(fecha, paste(hr, "00:00", sep = ":"), sep = " ")) %>% 
        arrange(fecha_hora)

tempo$ending_region_name <- as.factor(tempo$ending_region_name)
tempo$end_num = as.numeric(tempo$ending_region_name)

tempo$texto_sem = factor(tempo$texto_sem, levels=c("27/feb al\n01/mar",
                                                   "02/mar al\n08/mar",
                                                   "09/mar al\n15/mar", 
                                                   "16/mar al\n22/mar", 
                                                   "23/mar al\n29/mar", 
                                                   "30/mar al\n04/abr",
                                                   "06/abr al\n12/abr",
                                                   "13/abr al\n19/abr",
                                                   "20/abr al\n26/abr",
                                                   "27/abr al\n03/may",
                                                   "04/may al\n10/may",
                                                   "11/may al\n17/may",
                                                   "18/may al\n24/may",
                                                   "25/may al\n31/may",
                                                   "01/jun al\n07/jun"))
               
dtparts = t(as.data.frame(strsplit(tempo$fecha_hora,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                 format=c('y-m-d','h:m:s'))
thetimes = as.POSIXct(thetimes)
min(thetimes)
max(thetimes)

tempo$fecha_hora_1 <- thetimes

min(tempo$fecha_hora_1)
max(tempo$fecha_hora_1)
str(tempo$fecha_hora_1)

##### aqui hay que moverle ####
##### origen ####

tempo$ending_region_name <- as.character(tempo$ending_region_name)
tempo$starting_region_name <- as.character(tempo$starting_region_name)


tempo_lag <- tempo %>% 
        select(percent_change, 
               starting_region_name, ending_region_name, 
               dia, sem, fecha, hr, baseline_people_moving, 
               crisis_people_moving,
               start_edo, end_edo,
               fecha_hora, fecha_hora_1) %>% 
        arrange(ending_region_name, starting_region_name, dia, sem, hr) %>% 
        dplyr::group_by(starting_region_name,
                        sem,
                        dia,
                        hr) %>% 
        dplyr::summarise(
                #baseline_people_moving = sum(baseline_people_moving),
                #crisis_people_moving = sum(crisis_people_moving),
                percent_change = mean(percent_change)) %>% 
        arrange(starting_region_name, dia, sem, hr) %>% 
        mutate(#diferencia = crisis_people_moving-crisis_people_moving_sem_pasada
                #crisis_people_moving_sem_pasada = ifelse(sem != 9, lag(crisis_people_moving, 3), NA),
                percent_change_sem_pasada = ifelse(sem != 9, lag(percent_change, 3), NA),
                diferencia = percent_change-percent_change_sem_pasada) %>% 
        dplyr::group_by(starting_region_name, sem, dia) %>% 
        dplyr::summarise(
                diferencia = mean(diferencia)
        ) %>% 
        dplyr::ungroup()

#prueba <- tempo_lag[(tempo_lag$starting_region_name == "Coyoacán"),]
prueba <- tempo_lag
prueba$lugar <- "Origen"
prueba$nombre <- prueba$starting_region_name
prueba$starting_region_name <- NULL
max(prueba$sem)

##### destino ####
tempo_lag <- tempo %>% 
        select(percent_change, 
               starting_region_name, ending_region_name, 
               dia, sem,fecha, hr, baseline_people_moving, 
               crisis_people_moving,
               percent_change,
               start_edo, end_edo,
               fecha_hora) %>% 
        arrange(ending_region_name, starting_region_name, dia, sem, hr) %>% 
        dplyr::group_by(ending_region_name,
                        sem,
                        dia,
                        hr) %>% 
        dplyr::summarise(
                #baseline_people_moving = sum(baseline_people_moving),
                #crisis_people_moving = sum(crisis_people_moving),
                percent_change = mean(percent_change)) %>% 
        arrange(ending_region_name, dia, sem, hr) %>% 
        mutate(#diferencia = crisis_people_moving-crisis_people_moving_sem_pasada
                #crisis_people_moving_sem_pasada = ifelse(sem != 9, lag(crisis_people_moving, 3), NA),
               percent_change_sem_pasada = ifelse(sem != 9, lag(percent_change, 3), NA),
               diferencia = percent_change-percent_change_sem_pasada) %>% 
        dplyr::group_by(ending_region_name, sem, dia) %>% 
        dplyr::summarise(
                diferencia = mean(diferencia)
        ) %>% 
        dplyr::ungroup()

#prueba_f <- tempo_lag[(tempo_lag$ending_region_name == "Coyoacán"),]
prueba_f <- tempo_lag
prueba_f$lugar <- "Destino"
prueba_f$nombre <- prueba_f$ending_region_name
prueba_f$ending_region_name <- NULL
max(prueba_f$sem)

##### junto ####

prueba_f <- rbind(prueba, prueba_f) 
max(prueba_f$sem)


prueba_f <- prueba_f %>% 
        mutate(
                texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                   ifelse(sem == 10, "02/mar al\n08/mar",
                                          ifelse(sem == 11, "09/mar al\n15/mar",
                                                 ifelse(sem == 12, "16/mar al\n22/mar",
                                                        ifelse(sem == 13, "23/mar al\n29/mar", 
                                                               ifelse(sem == 14, "30/mar al\n05/abr",
                                                                      ifelse(sem == 15, "06/abr al\n12/abr", 
                                                                             ifelse(sem == 16, "13/abr al\n19/abr", 
                                                                                    ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                           ifelse(sem == 18, "27/abr al\n03/may",
                                                                                                  ifelse(sem == 19, "04/may al\n10/may", 
                                                                                                         ifelse(sem == 20, "11/may al\n17/may",
                                                                                                                ifelse(sem == 21, "18/may al\n24/may",
                                                                                                                       ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                              ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                              ))))))))))))))))
                

prueba_f$texto_sem = factor(prueba_f$texto_sem, 
                            levels=c(
                                    "27/feb al\n01/mar",
                                    "02/mar al\n08/mar",
                                    "09/mar al\n15/mar", 
                                    "16/mar al\n22/mar", 
                                    "23/mar al\n29/mar", 
                                    "30/mar al\n05/abr",
                                    "06/abr al\n12/abr",
                                    "13/abr al\n19/abr",
                                    "20/abr al\n26/abr",
                                    "27/abr al\n03/may",
                                    "04/may al\n10/may",
                                    "11/may al\n17/may",
                                    "18/may al\n24/may",
                                    "25/may al\n31/may",
                                    "01/jun al\n07/jun"))

dir.create("graficas/08062020/CAMBIOS_alcaldias/")

for(i in 1:length(del)){
        
options(scipen=999)
        
tempo_prueba_f <- prueba_f %>% 
        filter(as.character(nombre) == del[i]) 

ggplot(tempo_prueba_f[tempo_prueba_f$sem >= 12,]) +
        geom_line(tempo_prueba_f[tempo_prueba_f$sem >= 12,], mapping = aes(x= texto_sem, y = diferencia, group = dia, color = dia), size = 1.5, alpha = .88) +
        geom_point(tempo_prueba_f[tempo_prueba_f$sem >= 12,], mapping = aes(x= texto_sem, y = diferencia, color = dia), size = 3, alpha = .88) +
        facet_wrap(lugar ~ .) +
        scale_color_manual(values= c(
                "#e41a1c",
                "#377eb8",
                "#4daf4a",
                "#984ea3",
                "#ff7f00",
                "#ffff33",
                "#a65628"
        )) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) + 
        theme_bw() +
        theme(legend.position =  "bottom",
              legend.text=element_text(size=12),
              axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title  = element_text(size = 13),
              plot.caption = element_text(size = 10),
              strip.text.x = element_text(size = 12)) +
        labs(x = "Semana", y = "Porcentaje de cambio",
             title = paste(del[i], ".\nCambio en el porcentaje de personas en movimiento respecto a la semana anterior"),
             caption = "Nota: Se refiere a la diferencia del mismo día con respecto a la semana anterior\nLos porcentajes están calculados en relación al movimiento base",
             color = "")

ggsave(paste("graficas/08062020/CAMBIOS_alcaldias/cambios_", del[i], ".png", sep=""),  width = 11, height = 7)
}

rm(prueba, prueba_f, tempo_lag, tempo_prueba_f, i, dtparts, thetimes)

##### PROMEDIOS SEMANALES #####

#data$starting_region_name <- as.factor(data$starting_region_name)
#data$ending_region_name <- as.factor(data$ending_region_name)

tempo <- data %>% 
        filter(fecha >= as.Date("2020-03-16")) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        mutate(dia = weekdays(fecha),
               percent_change = round(percent_change, 1),
               sem = strftime(fecha, format = "%V"),
        ) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        filter(ending_region_name %in% del) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(dia = factor(dia, levels = c("lunes",
                                            "martes",
                                            "miércoles",
                                            "jueves",
                                            "viernes",
                                            "sábado",
                                            "domingo")),
               sem = as.numeric(sem),
               hr = as.character(substr(hora, 1,2)),
               texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                  ifelse(sem == 10, "02/mar al\n08/mar",
                                         ifelse(sem == 11, "09/mar al\n15/mar",
                                                ifelse(sem == 12, "16/mar al\n22/mar",
                                                       ifelse(sem == 13, "23/mar al\n29/mar", 
                                                              ifelse(sem == 14, "30/mar al\n05/abr",
                                                                     ifelse(sem == 15, "06/abr al\n12/abr",
                                                                            ifelse(sem == 16, "13/abr al\n19/abr",
                                                                                   ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                          ifelse(sem == 18, "27/abr al\n03/may", 
                                                                                                 ifelse(sem == 19, "14/may al\n10/may", 
                                                                                                        ifelse(sem == 20, "11/may al\n17/may",
                                                                                                               ifelse(sem == 21, "18/may al\n24/may",
                                                                                                                      ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                             ifelse(sem == 23, "01/jun al\n08/jun","0"
                                                                                                                             ))))))))))))))))

tempo <- tempo %>% 
        mutate(hr = as.character(hr),
               fecha_hora = paste(fecha, hora, sep = " "))

semanas <- data.frame(fecha = tempo$fecha, sem = tempo$sem, dia = tempo$dia)
semanas <- distinct(semanas) 
semanas <- arrange(semanas, fecha, sem)

tempo_group <- tempo %>% 
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        filter(end_edo == "CDMX" & start_edo == "CDMX") %>% 
        select(1:4,6,13) %>% 
        group_by(ending_region_name, sem) %>% 
        dplyr::summarise_all("mean") %>% 
        ungroup() 

dir.create("graficas/08062020/Cambios porcentuales/")

ggplot(tempo_group) +
        geom_bar(mapping = aes(x = as.character(sem), y = percent_change, fill = as.factor(sem)), stat="identity",size = 1) +
        geom_label(mapping = aes(x = as.character(sem), y = percent_change-8, label = paste(round(percent_change, 0), "%", sep = "")), 
                   size = 4.8) +
        facet_wrap(ending_region_name ~ .) +
        labs(fill = "Semana",
             title = "Cambios porcentuales semanales",
             caption = "\nNota: Se excluye la movilidad dentro de la misma delegación") +
        xlab("\nSemana") +
        ylab("Cambio porcentual") + 
        theme_bw() +
        guides(fill = guide_legend(override.aes = list(size = 7))) +
        theme(
                title = element_text(size = 20),
                strip.text = element_text(size = 15),
                axis.text.x = element_text(size = 14, angle = 90, vjust = .5),
                axis.text.y = element_text(size = 14),
                legend.text=element_text(size=22),
                plot.caption = element_text(size = 18),
                legend.position = "right"
        ) +
        scale_x_discrete(
                limits=c(
                        "12",
                        "13",
                        "14",
                        "15",
                        "16",
                        "17",
                        "18",
                        "19", 
                        "20",
                        "21",
                        "22",
                        "23"
                )) +
        scale_fill_manual(values = c(
                "#a6cee3",
                "#1f78b4",
                "#b2df8a",
                "#33a02c",
                "#fb9a99",
                "#e31a1c",
                "#fdbf6f",
                "#ff7f00",
                "#cab2d6",
                "#6a3d9a",
                "#ffff99",
                "#b15928",
                "#8dd3c7"),
                labels = c(
                        "16/mar al 22/mar",
                        "23/mar al 29/mar",
                        "30/mar al 05/abr",
                        "06/abr al 12/abr",
                        "13/abr al 19/abr",
                        "20/abr al 26/abr",
                        "27/abr al 03/may",
                        "04/may al 10/may",
                        "11/may al 17/may",
                        "18/may al 24/may",
                        "25/may al 31/may",
                        "01/jun al 07/jun"))

ggsave("graficas/08062020/Cambios porcentuales/cambio_porcentual_CDMX-CDMX.jpg", width = 28, height = 12)


##### edo-cdmx ####

tempo_group <- tempo %>% 
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        filter(end_edo == "CDMX" & start_edo == "Otro") %>% 
        select(1:4,6,13) %>% 
        group_by(ending_region_name, sem) %>% 
        dplyr::summarise_all("mean") %>% 
        ungroup()

ggplot(tempo_group) +
        geom_bar(mapping = aes(x = as.character(sem), y = percent_change, fill = as.factor(sem)), stat="identity",size = 1) +
        geom_label(mapping = aes(x = as.character(sem), y = percent_change-8, label = paste(round(percent_change, 0), "%", sep = "")), 
                   size = 5) +
        facet_wrap(ending_region_name ~ .) +
        labs(fill = "Semana",
             title = "Cambios porcentuales semanales") +
        xlab("Semana") +
        ylab("Cambio porcentual")+ 
        theme_bw() +
        guides(fill = guide_legend(override.aes = list(size = 7))) +
        theme(
                title = element_text(size = 20),
                strip.text = element_text(size = 15),
                axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                legend.text=element_text(size=22)
        )    +
        scale_x_discrete(
                limits=c(
                        "12",
                        "13",
                        "14",
                        "15",
                        "16",
                        "17",
                        "18",
                        "19", 
                        "20",
                        "21",
                        "22",
                        "23"
                )) +
        scale_fill_manual(values = c(
                "#a6cee3",
                "#1f78b4",
                "#b2df8a",
                "#33a02c",
                "#fb9a99",
                "#e31a1c",
                "#fdbf6f",
                "#ff7f00",
                "#cab2d6",
                "#6a3d9a",
                "#ffff99",
                "#b15928",
                "#8dd3c7"),
                labels = c(
                        "16/mar al 22/mar",
                        "23/mar al 29/mar",
                        "30/mar al 05/abr",
                        "06/abr al 12/abr",
                        "13/abr al 19/abr",
                        "20/abr al 26/abr",
                        "27/abr al 03/may",
                        "04/may al 10/may",
                        "11/may al 17/may",
                        "18/may al 24/may",
                        "25/may al 31/may",
                        "01/jun al 07/jun"))

ggsave("graficas/08062020/Cambios porcentuales/cambio_porcentual_EDO-CDMX.jpg", width = 28, height = 12)
        
##### edo_edo ####
tempo <- data %>% 
        filter(fecha >= as.Date("2020-03-16")) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        mutate(dia = weekdays(fecha),
               percent_change = round(percent_change, 1),
               sem = strftime(fecha, format = "%V"),
        ) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        filter(ending_region_name %in% mun) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, hr, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(dia = factor(dia, levels = c("lunes",
                                            "martes",
                                            "miércoles",
                                            "jueves",
                                            "viernes",
                                            "sábado",
                                            "domingo")),
               sem = as.numeric(sem),
               #hr = as.character(substr(hora, 1,2)),
               texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                  ifelse(sem == 10, "02/mar al\n08/mar",
                                         ifelse(sem == 11, "09/mar al\n15/mar",
                                                ifelse(sem == 12, "16/mar al\n22/mar",
                                                       ifelse(sem == 13, "23/mar al\n29/mar", 
                                                              ifelse(sem == 14, "30/mar al\n05/abr",
                                                                     ifelse(sem == 15, "06/abr al\n12/abr",
                                                                            ifelse(sem == 16, "13/abr al\n19/abr",
                                                                                   ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                          ifelse(sem == 18, "27/abr al\n03/may", 
                                                                                                 ifelse(sem == 19, "04/may al\n10/may", 
                                                                                                        ifelse(sem == 20, "11/may al\n17/may", 
                                                                                                               ifelse(sem == 21, "18/may al\n24/may", 
                                                                                                                      ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                             ifelse(sem == 23, "01/jun al\n07/jun",
                                                                                                                      "0"))))))))))))))))

tempo <- tempo %>% 
        mutate(hr = as.character(hr),
               fecha_hora = paste(fecha, hora, sep = " "))

semanas <- data.frame(fecha = tempo$fecha, sem = tempo$sem, dia = tempo$dia)
semanas <- distinct(semanas) 
semanas <- arrange(semanas, fecha, sem)

tempo_group <- tempo %>% 
        mutate(intramov = ifelse(starting_region_name == ending_region_name, "intra", "trans")) %>% 
        filter(intramov != "intra") %>%
        filter(end_edo == "Otro" & start_edo == "Otro") %>% 
        select(1:4,6,14) %>% #difference, baseline, crisis, percent, ending, sem
        dplyr::group_by(ending_region_name, sem) %>% 
        dplyr::summarise_all("mean") %>% 
        ungroup()

##### EDO-EDO todos

ggplot(tempo_group[tempo_group$ending_region_name %in% mun,]) +
        geom_bar(mapping = aes(x = as.character(sem), y = percent_change, fill = as.factor(sem)), stat="identity",size = 1) +
        geom_label(mapping = aes(x = as.character(sem), y = percent_change-8, label = paste(round(percent_change, 0), "%", sep = "")), 
                   size = 5.5) +
        facet_wrap(ending_region_name ~ .) +
        labs(fill = "Semana",
             title = "Cambios porcentuales semanales") +
        xlab("Semana") +
        ylab("Cambio porcentual")+ 
        theme_bw() +
        guides(fill = guide_legend(override.aes = list(size = 7))) +
        theme(
                 title = element_text(size = 24),
                 strip.text = element_text(size = 22),
                 axis.text.x = element_text(size = 18, angle = 90),
                 axis.text.y = element_text(size = 18),
                 legend.text=element_text(size=26)
         ) +
        scale_x_discrete(
                limits=c(
                        "12",
                        "13",
                        "14",
                        "15",
                        "16",
                        "17",
                        "18",
                        "19", 
                        "20",
                        "21",
                        "22",
                        "23"
                )) +
        scale_fill_manual(values = c(
                "#a6cee3",
                "#1f78b4",
                "#b2df8a",
                "#33a02c",
                "#fb9a99",
                "#e31a1c",
                "#fdbf6f",
                "#ff7f00",
                "#cab2d6",
                "#6a3d9a",
                "#ffff99",
                "#b15928",
                "#8dd3c7"),
                labels = c(
                        "16/mar al 22/mar",
                        "23/mar al 29/mar",
                        "30/mar al 05/abr",
                        "06/abr al 12/abr",
                        "13/abr al 19/abr",
                        "20/abr al 26/abr",
                        "27/abr al 03/may",
                        "04/may al 10/may",
                        "11/may al 17/may",
                        "18/may al 24/may",
                        "25/may al 31/may",
                        "01/jun al 07/jun"))

ggsave("graficas/08062020/Cambios porcentuales/cambio_porcentual_EDO-EDO.jpg", width = 38, height = 22)

##### EDO-EDO mun eleccionados ####

mun_1 <- c("Ecatepec de Morelos", "Nezahualcóyotl", "Toluca", "Naucalpan de Juárez", "Tlalnepantla de Baz", 
           "Chimalhuacán", "Cuautitlán Izcalli", "Atizapán de Zaragoza", "Tultitlán")

ggplot(tempo_group[tempo_group$ending_region_name %in% mun_1,]) +
        geom_bar(mapping = aes(x = as.character(sem), y = percent_change, fill = as.factor(sem)), stat="identity",size = 1) +
        geom_label(mapping = aes(x = as.character(sem), y = percent_change-5, label = paste(round(percent_change, 0), "%", sep = "")), 
                   size = 6) +
        facet_wrap(ending_region_name ~ ., ncol = 4) +
        labs(fill = "Semana",
             title = "Cambios porcentuales semanales") +
        xlab("Semana") +
        ylab("Cambio porcentual")+ 
        theme_bw() +
        guides(fill = guide_legend(override.aes = list(size = 7))) +
        theme(
                title = element_text(size = 20),
                strip.text = element_text(size = 15),
                axis.text.x = element_text(size = 14, angle = 90),
                axis.text.y = element_text(size = 14),
                legend.text=element_text(size=22)
        ) +
        scale_x_discrete(
                limits=c(
                        "12",
                        "13",
                        "14",
                        "15",
                        "16",
                        "17",
                        "18",
                        "19", 
                        "20",
                        "21",
                        "22",
                        "23"
                )) +
        scale_fill_manual(values = c(
                "#a6cee3",
                "#1f78b4",
                "#b2df8a",
                "#33a02c",
                "#fb9a99",
                "#e31a1c",
                "#fdbf6f",
                "#ff7f00",
                "#cab2d6",
                "#6a3d9a",
                "#ffff99",
                "#b15928",
                "#8dd3c7"),
                labels = c(
                        "16/mar al 22/mar",
                        "23/mar al 29/mar",
                        "30/mar al 05/abr",
                        "06/abr al 12/abr",
                        "13/abr al 19/abr",
                        "20/abr al 26/abr",
                        "27/abr al 03/may",
                        "04/may al 10/may",
                        "11/may al 17/may",
                        "18/may al 24/may",
                        "25/may al 31/may",
                        "01/jun al 07/jun"))

ggsave("graficas/08062020/Cambios porcentuales/cambio_porcentual_EDO-EDO_seleccionados.jpg", width = 30, height = 12)


rm(tempo, tempo_group, mun_1, semanas, tempo_alcaldias_edo, tempo_edo_todo)

##### ANÁLISIS DE LAS ALCALDÍAS CON MÁS CONTAGIOS ####
dir.create("graficas/08062020/analisis_alcaldias/")
contagios <- read_csv("inp/datos_abiertos_covid19/Casos_Diarios_Estado_Nacional_Confirmados_reporte.csv", locale = locale(encoding = "Latin1"))
ncol(contagios)
colnames(contagios[,ncol(contagios)])
contagios$casos <- rowSums( contagios[,4:ncol(contagios)] ) # cambia
contagios <- contagios[,c(1,2,3,ncol(contagios))] # cambia
contagios$num_ent <- ifelse(nchar(contagios$cve_ent) == 4, substr(contagios$cve_ent, 1,1), substr(contagios$cve_ent, 1,2))
contagios$num_ent <- as.numeric(contagios$num_ent)
contagios <- contagios %>% arrange(-casos)

municipios_mx <- readxl::read_excel("out/municipios_mx.xlsx", sheet = 2)

contagios <- left_join(contagios, municipios_mx, by = c("num_ent" = "cve_ent"))
rm(municipios_mx)

contagios <- contagios %>% filter(num_ent == 9 | num_ent == 15) %>% arrange(-casos)
top_df <- contagios %>% arrange(nom_ent, -casos) %>% filter(num_ent == 9) %>% slice(1:5) 
top_df <- top_df$nombre
top_df

##### treemap #####
dir.create("graficas/08062020/analisis_alcaldias/matrices")
dir.create("graficas/08062020/analisis_alcaldias/treemaps")

tempo <- data %>% 
        filter(fecha >= as.Date("2020-03-16")) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        mutate(dia = weekdays(fecha),
               percent_change = round(percent_change, 1),
               sem = strftime(fecha, format = "%V"),
        ) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem, hr) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(dia = factor(dia, levels = c("lunes",
                                            "martes",
                                            "miércoles",
                                            "jueves",
                                            "viernes",
                                            "sábado",
                                            "domingo")),
               sem = as.numeric(sem),
               texto_sem = ifelse(sem == 9, "27/feb al\n01/mar",
                                  ifelse(sem == 10, "02/mar al\n08/mar",
                                         ifelse(sem == 11, "09/mar al\n15/mar",
                                                ifelse(sem == 12, "16/mar al\n22/mar",
                                                       ifelse(sem == 13, "23/mar al\n29/mar", 
                                                              ifelse(sem == 14, "30/mar al\n05/abr",
                                                                     ifelse(sem == 15, "06/abr al\n12/abr",
                                                                            ifelse(sem == 16, "13/abr al\n19/abr", 
                                                                                   ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                          ifelse(sem == 18, "27/abr al\n03/may",
                                                                                                 ifelse(sem == 19, "04/may al\n10/may", 
                                                                                                        ifelse(sem == 20, "11/may al\n17/may",
                                                                                                               ifelse(sem == 21, "18/may al\n24/may",
                                                                                                                      ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                             ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                             ))))))))))))))))
alcaldia_tempo <- tempo %>% 
        filter(starting_region_name %in% top_df) %>% 
        group_by(starting_region_name, ending_region_name) %>% 
        dplyr::summarise(n = n()) %>% 
        arrange(starting_region_name, -n) %>% 
        slice(1:10) %>% 
        ungroup()

tempo <- tempo %>% 
        filter(ending_region_name %in% alcaldia_tempo$ending_region_name)


alcaldia_tempo <- tempo %>% 
        filter(starting_region_name %in% top_df) %>% 
        group_by(ending_region_name, starting_region_name) %>% 
        dplyr::summarise(n = n()) %>% 
        group_by(starting_region_name) %>% 
        arrange(starting_region_name, -n) %>% 
        slice(1:10) %>%
        ungroup()

palette <- c(
        "#8dd3c7",
        "#ffffb3",
        "#bebada",
        "#fb8072",
        "#80b1d3",
        "#fdb462",
        "#b3de69",
        "#fccde5",
        "#d9d9d9",
        "#bc80bd"
)

alc <- unique(alcaldia_tempo$starting_region_name)

dir.create("graficas/08062020/analisis_alcaldias/treemaps")
         
for (i in 1:length(alc)) {
        
   alcaldias_tempo_tempo <- alcaldia_tempo %>% 
           filter(starting_region_name == alc[i])

ggplot(alcaldias_tempo_tempo, aes(area = n, fill = palette, label = ending_region_name)) +
        geom_treemap() +
        geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
        scale_fill_manual(values = c(        "#a6cee3",
                                             "#1f78b4",
                                             "#b2df8a",
                                             "#33a02c",
                                             "#fb9a99",
                                             "#e31a1c",
                                             "#fdbf6f",
                                             "#ff7f00",
                                             "#cab2d6",
                                             "#6a3d9a")) +
        guides(fill = FALSE) +
        theme(title = element_text(size = 14)) +
        labs(fill = "",
             title = paste("10 principales destinos de los movimientos\n con origen en ", alcaldias_tempo_tempo$ending_region_name))

ggsave(paste("graficas/08062020/analisis_alcaldias/treemaps/alcaldias_", alc[i], ".jpg", sep=""),  width = 8, height = 4)

}

rm(alcaldias_tempo_tempo, palette, alc, i, alcaldia_tempo)

##### cambios #####

tempo <- data %>% 
        filter(fecha > as.Date("2020-03-15")) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        mutate(dia = weekdays(fecha),
               percent_change = round(percent_change, 1),
               sem = strftime(fecha, format = "%V"),
        ) %>% 
        arrange(ending_region_name,
                starting_region_name,
                dia, sem) %>% 
        #filter(ending_region_name %in% del) %>% 
        left_join(lugares, by = c("starting_region_name" = "name")) %>% 
        dplyr::rename(start_edo = edo) %>% 
        left_join(lugares, by = c("ending_region_name" = "name")) %>% 
        dplyr::rename(end_edo = edo) %>% 
        select(difference, baseline_people_moving, crisis_people_moving, 
               percent_change, starting_region_name, ending_region_name,
               start_edo, end_edo, start_num.x, hora, fecha, dia, sem) %>% 
        dplyr::rename(start_num = start_num.x) %>% 
        mutate(
                sem = as.numeric(sem),
                hr = as.character(substr(hora, 1,2)),
                texto_colores = ifelse(percent_change <= -80, "Menores a -80",
                                       ifelse(percent_change > -80 & percent_change <= -60, "-80 a -60",
                                              ifelse(percent_change > -60 & percent_change <= -40, "-60 a -40",
                                                     ifelse(percent_change > -40 & percent_change <= -20, "-40 a -20",
                                                            ifelse(percent_change > -20 & percent_change <= 0, "-20 a 0",
                                                                   ifelse(percent_change > 0 & percent_change <= 20, "0 a 20",
                                                                          ifelse(percent_change > 20 & percent_change <= 40, "20 a 40", 
                                                                                 ifelse(percent_change > 40, "Mayores a 40","Fallaste")))))))))

tempo$texto_colores <- factor(tempo$texto_colores, levels = c(
        "Menores a -80", "-80 a -60", "-60 a -40", "-40 a -20", "-20 a 0", "0 a 20", "20 a 40", "Mayores a 40"))

palette_perc <- c("#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0","#ffed6f", "#ff7f00", "#e31a1c")

table(unique(tempo$sem))
tempo$ending_region_name <- as.factor(tempo$ending_region_name)
tempo$end_num = as.numeric(tempo$ending_region_name)

expansor_tempo <- expand.grid(starting_region_name = unique(tempo$starting_region_name),
                              ending_region_name = unique(tempo$ending_region_name),
                              fecha = unique(tempo$fecha),
                              hr = unique(tempo$hr))

expansor_tempo <- left_join(expansor_tempo, tempo[c("fecha",
                                                    "hr",
                                                    "starting_region_name", 
                                                    "ending_region_name", 
                                                    "percent_change", 
                                                    "start_edo",
                                                    "end_edo",
                                                    "end_num", 
                                                    "texto_colores",
                                                    "dia")])

expansor_tempo <- expansor_tempo %>% 
        mutate(percent_change_texto = ifelse(is.na(percent_change), "", as.character(percent_change)),
               #end_num = as.numeric(ending_region_name),
               dia = weekdays(fecha),
               sem = strftime(fecha, format = "%V"))

expansor_tempo <- expansor_tempo %>%
        mutate(
                texto_sem = ifelse(sem == 09, "27/feb al\n01/mar",
                                   ifelse(sem == 10, "02/mar al\n08/mar",
                                          ifelse(sem == 11, "09/mar al\n15/mar",
                                                 ifelse(sem == 12, "16/mar al\n22/mar",
                                                        ifelse(sem == 13, "23/mar al\n29/mar", 
                                                               ifelse(sem == 14, "30/mar al\n05/abr",
                                                                      ifelse(sem == 15, "06/abr al\n12/abr", 
                                                                             ifelse(sem == 16, "13/abr al\n19/abr",
                                                                                    ifelse(sem == 17, "20/abr al\n26/abr",
                                                                                           ifelse(sem == 18, "27/abr al\n03/may", 
                                                                                                  ifelse(sem == 19, "04/may al\n10/may",
                                                                                                         ifelse(sem == 20, "11/may al\n17/may",
                                                                                                                ifelse(sem == 21, "18/may al\n24/may",
                                                                                                                       ifelse(sem == 22, "25/may al\n31/may",
                                                                                                                              ifelse(sem == 23, "01/jun al\n07/jun","0"
                                                                                                                              ))))))))))))))))

expansor_tempo$texto_sem = factor(expansor_tempo$texto_sem, 
                                  levels=c("27/feb al\n01/mar",
                                           "02/mar al\n08/mar",
                                           "09/mar al\n15/mar", 
                                           "16/mar al\n22/mar", 
                                           "23/mar al\n29/mar", 
                                           "30/mar al\n05/abr",
                                           "06/abr al\n12/abr",
                                           "13/abr al\n19/abr",
                                           "20/abr al\n26/abr",
                                           "27/abr al\n03/may",
                                           "04/may al\n10/may",
                                           "11/may al\n17/may",
                                           "18/may al\n24/may",
                                           "25/may al\n31/may",
                                           "01/jun al\n07/jun"))


dias <- as.character(unique(tempo$dia))
horas <- unique(tempo$hr)

expansor_tempo <- expansor_tempo %>% 
        filter(starting_region_name %in% top_df)



##### matrices ####

for( j in 1:length(horas)){
        for(i in 1:length(dias)){
        
        dias_tempo <- expansor_tempo %>% 
                filter(fecha >= as.Date("2020-03-16")) %>% 
                #filter(fecha <= as.Date("2020-04-27")) %>% 
                filter(dia == dias[i]  & hr == horas[j]) %>% 
                filter(start_edo == "CDMX" & end_edo == "CDMX") %>% 
                select(dia, starting_region_name, ending_region_name, percent_change, texto_sem, 
                       percent_change_texto, end_num, hr, fecha, sem, texto_colores) %>% 
                arrange(starting_region_name, ending_region_name, dia, texto_sem) %>% 
                distinct() 
        
        ggplot(dias_tempo, aes(x = starting_region_name, y = reorder(ending_region_name, -end_num),
                               fill= texto_colores)) + 
                geom_tile() +
                geom_text(aes(label = percent_change_texto),color = "black", size = 6) + 
                facet_grid(texto_sem ~ .) +
                scale_fill_manual(values = palette_perc, na.value = "grey98") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 26),
                      axis.title.x = element_text(size = 25),
                      axis.text.y = element_text(size = 24),
                      axis.title.y = element_text(size = 25),
                      plot.title = element_text(size = 29, face = "bold"),
                      plot.caption = element_text(size = 18),
                      legend.title=element_text(size=10), 
                      legend.text=element_text(size=20),
                      strip.text = element_text(size=20)) +
                labs(x = "Origen", y = "Destino", 
                     title = paste(capitalize(as.character(dias[i])), 
                                   ".\nVariación porcentual de la movilidad por\norigen-destino por semana"),
                     caption = paste("\nVariaciones correspondientes a las", dias_tempo$hr, "horas")) +
                guides(fill = guide_legend(title = ""))
        
        ggsave(paste("graficas/08062020/analisis_alcaldias/matrices/CDMX_CDMX_", paste(dias[i], horas[j], sep = ""),".jpg", sep=""),  width = 18, height = 25)
        
        }
}

for( j in 1:length(horas)){
        for(i in 1:length(dias)){
                
                dias_tempo <- expansor_tempo %>% 
                        filter(fecha >= as.Date("2020-03-16")) %>% 
                        #filter(fecha <= as.Date("2020-04-19")) %>% 
                        filter(dia == dias[i]  & hr == horas[j]) %>% 
                        filter(start_edo == "CDMX" & end_edo == "Otro") %>% 
                        select(dia, starting_region_name, ending_region_name, percent_change, texto_sem, 
                               percent_change_texto, end_num, hr, fecha, sem, texto_colores) %>% 
                        arrange(starting_region_name, ending_region_name, dia, texto_sem) %>% 
                        distinct() 
                
                ggplot(dias_tempo, aes(x = starting_region_name, y = reorder(ending_region_name, -end_num),
                                       fill= texto_colores)) + 
                        geom_tile() +
                        geom_text(aes(label = percent_change_texto),color = "black", size = 4.5) + 
                        facet_grid(texto_sem ~ .) +
                        scale_fill_manual(values = palette_perc, na.value = "grey98") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 26),
                              axis.title.x = element_text(size = 25),
                              axis.text.y = element_text(size = 20),
                              axis.title.y = element_text(size = 15),
                              plot.title = element_text(size = 29, face = "bold"),
                              plot.caption = element_text(size = 18),
                              legend.title=element_text(size=10), 
                              legend.text=element_text(size=20),
                              strip.text = element_text(size=20)) +
                        labs(x = "Origen", y = "Destino", 
                             title = paste(capitalize(as.character(dias[i])), 
                                           ".\nVariación porcentual de la movilidad por\norigen-destino por semana"),
                             caption = paste("Variaciones correspondientes a las", dias_tempo$hr, "horas")) +
                        guides(fill = guide_legend(title = ""))
                
                ggsave(paste("graficas/08062020/analisis_alcaldias/matrices/EDO_CDMX_", paste(dias[i], horas[j], sep = ""),".jpg", sep=""),  width = 16, height = 31)
                
        }
}

rm(dias_tempo, expansor_tempo, dias, horas, i, j, palette_perc, top_df, tempo, tempo_lag, dtparts, prueba, prueba_f, tempo_prueba_f, thetimes)
##### Sankeys ####
top_df <- contagios %>% arrange(-casos) %>% slice(1:5)
top_df <- top_df %>% 
        mutate(nombre = ifelse(nombre == "Nezahualcoyotl", "Nezahualcóyotl", nombre))
top_df <- top_df$nombre
top_df

alcaldia_tempo <- data %>% #### 10 principales destinos
        filter(starting_region_name %in% top_df) %>% 
        group_by(starting_region_name, ending_region_name) %>% 
        dplyr::summarise(n = n()) %>% 
        arrange(starting_region_name, -n) %>% 
        slice(1:10) %>% 
        ungroup()

tempo <- data %>% # filtro de esos destinos
        filter(starting_region_name %in% top_df) %>% 
        filter(ending_region_name %in% alcaldia_tempo$ending_region_name)

alcaldia_tempo <- tempo %>% 
        filter(starting_region_name != ending_region_name) %>% 
        group_by(starting_region_name, ending_region_name) %>% 
        dplyr::summarise_all("mean") %>% 
        group_by(starting_region_name) %>% 
        arrange(starting_region_name, -percent_change) %>% 
        slice(1:10) %>% 
        ungroup() %>% 
        select(1,2,6) %>% 
        mutate(percent_change = percent_change*-1)

#write_csv(alcaldia_tempo, "out/sankey_diagrama.csv")

subset <- alcaldia_tempo
colnames(subset) <- c("source", "target", "value")
nodes <- data.frame(name=c(as.character(subset$source), as.character(subset$target)) %>% unique())

subset$IDsource=match(subset$source, nodes$name)-1 
subset$IDtarget=match(subset$target, nodes$name)-1

write_csv(subset, paste0("out/sankey_diagrama_", Sys.Date(), ".csv"))

#####

tempo <- data %>% 
        filter(fecha >= (Sys.Date() - 7),
               starting_region_name != ending_region_name,
               starting_region_name %in% del) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        mutate(
               sem = strftime(fecha, format = "%V"),
        ) %>% 
        group_by(starting_region_name, ending_region_name, sem) %>% 
        dplyr::summarise(percent_change_mean = mean(percent_change)) %>% 
        filter(percent_change_mean > -60) %>% 
        ungroup() %>% 
        group_by(starting_region_name) %>% 
        arrange(starting_region_name, -percent_change_mean) %>% 
        slice(1:3) %>% 
        ungroup() %>%
        mutate(edo = ifelse(ending_region_name %in% del, "cdmx",
                            ifelse(ending_region_name %in% mun, "edomex", "NO ESTA")),
               starting_region_name_edo = paste(starting_region_name, edo, sep = "_")) %>%
        group_by(starting_region_name, edo) %>%
        #slice(1:3) %>%
        ungroup() %>%
        arrange(edo)

table(tempo$starting_region_name[tempo$edo == "edomex"])
table(tempo$starting_region_name[tempo$edo == "cdmx"])

write_csv(tempo, paste0("out/dendogram_alcaldia", Sys.Date(), ".csv"))

tempo <- data %>% 
        mutate(sem = strftime(fecha, format = "%V"),
               percent_change = ifelse(is.na(percent_change), 0, percent_change),
               max_sem = as.numeric(max(sem))) %>% 
        filter(sem == max_sem | sem == max_sem - 1 ,
               starting_region_name != ending_region_name,
               starting_region_name %in% del) %>% #### esta filtro fue para el reporte del miercoles 8, se excluyó esa fecha
        group_by(starting_region_name, ending_region_name, sem) %>% 
        dplyr::summarise(percent_change_mean = mean(percent_change)) %>% 
        ungroup() %>% 
        spread(key = sem, value = percent_change_mean) 

colnames(tempo) <- c("starting_region_name", "ending_region_name", "uno", "dos")

tempo_1 <- tempo %>% 
        mutate(percent_change_dif = dos-uno,
               edo = ifelse(ending_region_name %in% del, "cdmx",
                            ifelse(ending_region_name %in% mun, "edomex", "NO ESTA")),
               reducciones = ifelse(percent_change_dif <= 0, "reduccion", "aumento")) %>% 
        filter(reducciones == "reduccion") %>% 
        arrange(starting_region_name, percent_change_dif) %>% 
        group_by(starting_region_name, edo) %>% 
        slice(1:3) %>% 
        ungroup()
        
write_csv(tempo_1, paste0("out/dendogram_reducciones", Sys.Date(), ".csv"))

tempo_1 <- tempo %>% 
        mutate(percent_change_dif = dos-uno,
               edo = ifelse(ending_region_name %in% del, "cdmx",
                            ifelse(ending_region_name %in% mun, "edomex", "NO ESTA")),
               reducciones = ifelse(percent_change_dif <= 0, "reduccion", "aumento")) %>% 
        filter(reducciones == "aumento") %>% 
        group_by(starting_region_name, edo) %>% 
        arrange(-percent_change_dif) %>% 
        slice(1:3) %>% 
        ungroup()

write_csv(tempo_1, paste0("out/dendogram_aumento", Sys.Date(), ".csv"))

rm(tempo_1)

tempo <- data %>%
        filter(starting_region_name == "Ecatepec de Morelos",
               fecha >= as.Date("2020-05-18"),
               starting_region_name != ending_region_name) %>% 
        arrange(fecha) %>% 
        select(starting_region_name, ending_region_name, percent_change, fecha, hr) %>% 
        mutate(dia = weekdays(fecha))
        
dir.create("graficas/08062020/analisis_alcaldias/Sankeys/")


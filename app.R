library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)
library(googledrive)
library(googlesheets4)
library(shiny)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(RColorBrewer) 
library(janitor)
library(mxmaps)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(shinydashboard)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)
library(extrafont)
library(showtext)
library(jsonlite)
library(data.table)
library(shinyjs)
library(leaflet)
library(mxmaps)
library(shinyWidgets)
library(shiny)
library(shiny.router)
library(latex2exp)
library(tibble)
library(tibbletime)
library(lubridate)
library(wesanderson)

# setwd("~/NLMR/avgm_3")


nb.cols.129 <-44
mycolors129 <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols.129)

# Insertar la fuente/font de "Montserrat" para las visualizaciones - - - - - -
sysfonts::font_add_google(name = "Montserrat", family = "Montserrat")
showtext::showtext_auto()

#theme_1------------------------------------------------------------------------

theme_1<-theme_minimal()+
  theme(text=element_text(family = gt::google_font("Montserrat")),
        plot.title = element_text(family = gt::google_font("Montserrat"),
                                  # face = "bold",
                                  size = 16,
                                  hjust = 0),
        plot.subtitle = element_text(family = gt::google_font("Montserrat"),
                                     size = 12,
                                     hjust = 0,
                                     colour = "grey40"),
        plot.caption = element_text(family = gt::google_font("Montserrat"),
                                    size = 12,
                                    colour = "grey40"),
        axis.text.x = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = 12,
                                   colour = "black"),
        axis.text.y = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",   
                                   size = 12,
                                   colour = "black"),
        legend.title = element_text(family = gt::google_font("Montserrat"),
                                    face = "bold",
                                    size = 12,
                                    colour = "black",
                                    hjust = 1),
        legend.title.align = 0.5,
        legend.text = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = "8px",
                                   colour = "black",
                                   hjust = 1),
        legend.text.align = 0.5,
        legend.key.size = unit(25, "pt")) 


data("df_mxmunicipio_2020")
df_mxmunicipio_2020 %>% filter(state_name=="Jalisco")->df_mxmunicipio_2020


############################################################################
#
#                                IJCF
#
#############################################################################

perfil_victima<- read_excel("perfil_victima.xlsx") %>% suppressWarnings()
servicios<- read_excel("servicios.xlsx") %>% suppressWarnings()


# BOX IJCF: - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# perfil_reactive() %>%
perfil_victima %>%
  filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta") %>% 
  summarise(Total=n()) %>%
  mutate(Total=comma(Total)) ->ijcf_box_1

perfil_victima %>% 
  filter(!is.na(fecha)) %>% 
  mutate(fecha=ymd(paste0(fecha, "-01"))) %>% 
  # group_by(fecha) %>% 
  group_by(fecha=floor_date(fecha)) %>%
  summarise(Total=n()) %>%
  filter(Total>0) %>% 
  pull(fecha) %>% 
  max() %>% 
  lubridate::month(abbr = T, label = T)->mes_final_ijcf

# perfil_reactive() %>%
perfil_victima %>% 
  group_by(`Clasificación de causa de muerte (Violenta o Natural)`) %>% 
  summarise(Total=n()) %>%
  mutate(Porcentaje = Total / sum(Total),
         Porcentaje = round((Porcentaje * 100), digits = 2)) %>% 
  arrange(-Total) %>% 
  mutate(Total=comma(Total),
         Porcentaje=paste0(Porcentaje, "%"))->ijcf_box_1_1


# perfil_reactive() %>%
perfil_victima %>%
  filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta") %>% 
  group_by(Año) %>% 
  summarise(Total=n()) %>% 
  mutate(Variación = scales::percent((Total - lag(Total))/lag(Total),0.1))%>%
  mutate(Total=comma(Total)) ->ijcf_box_2


# perfil_reactive() %>%
perfil_victima %>% 
  filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta") %>% 
  group_by(`Municipio de suceso`) %>% 
  summarise(Total=n()) %>%
  arrange(desc(Total)) %>% 
  mutate(Porcentaje = Total / sum(Total),
         Porcentaje = round((Porcentaje * 100), digits = 2),
         Porcentaje =paste0(Porcentaje, "%")) %>% 
  head(n=5)->ijcf_box_3




##############################################################################
#
#                             ORDENES Y MEDIDAS
#
##############################################################################

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQrAC8uJDjHPsFX6cIbh2E52vExKI6hSzLVGMyLVb4AIoxsZnYcyuzz7rAz5m8_u9n-ztjTgnfyRBIL/pub?output=xlsx"
destfile <- "ordenes_medidas_xlsx"
curl::curl_download(url, destfile)


medidas_ordenes_estatal <- read_excel(destfile, sheet = "estatal")
medidas_ordenes_municipal <- read_excel(destfile, sheet = "municipal") %>%
  mutate(mes=factor(mes,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))

# # mapa
# df_mxmunicipio_2020<-data("df_mxmunicipio_2020")
# 
# data("df_mxmunicipio_2020")

medidas_ordenes_municipal %>%
  #filter(año==2019) %>%
  group_by(año, municipio) %>%
  summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
            ordenes=sum(ordenes_aceptadas + ordenes_rechazadas)) %>%
  pivot_longer(cols=c("ordenes","medidas"),
               names_to = "tipo",
               values_to = "total") ->medidas_y_ordenes


medidas_y_ordenes %>%  filter(tipo=="medidas", año==2019)->medidas_2019
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2020)->medidas_2020
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2021)->medidas_2021
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2022)->medidas_2022



# df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")

medidas_2019 <-merge(df_mxmunicipio_2020, medidas_2019, by.x="municipio_name", by.y="municipio")
medidas_2020 <-merge(df_mxmunicipio_2020, medidas_2020, by.x="municipio_name", by.y="municipio")
medidas_2021 <-merge(df_mxmunicipio_2020, medidas_2021, by.x="municipio_name", by.y="municipio")
medidas_2022 <-merge(df_mxmunicipio_2020, medidas_2022, by.x="municipio_name", by.y="municipio")

medidas_2019$value<- medidas_2019$total
medidas_2020$value<- medidas_2020$total
medidas_2021$value<- medidas_2021$total
medidas_2022$value<- medidas_2022$total

medidas_2019<- medidas_2019 %>% filter(state_name=="Jalisco")
medidas_2020<- medidas_2020 %>% filter(state_name=="Jalisco")
medidas_2021<- medidas_2021 %>% filter(state_name=="Jalisco")
medidas_2022<- medidas_2022 %>% filter(state_name=="Jalisco")

#_______________________________________________________________________________#

medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2019)->ordenes_2019
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2020)->ordenes_2020
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2021)->ordenes_2021
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2022)->ordenes_2022

# df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")

ordenes_2019 <-merge(df_mxmunicipio_2020, ordenes_2019, by.x="municipio_name", by.y="municipio")
ordenes_2020 <-merge(df_mxmunicipio_2020, ordenes_2020, by.x="municipio_name", by.y="municipio")
ordenes_2021 <-merge(df_mxmunicipio_2020, ordenes_2021, by.x="municipio_name", by.y="municipio")
ordenes_2022 <-merge(df_mxmunicipio_2020, ordenes_2022, by.x="municipio_name", by.y="municipio")

ordenes_2019$value<- ordenes_2019$total
ordenes_2020$value<- ordenes_2020$total
ordenes_2021$value<- ordenes_2021$total
ordenes_2022$value<- ordenes_2022$total

ordenes_2019<- ordenes_2019 %>% filter(state_name=="Jalisco")
ordenes_2020<- ordenes_2020 %>% filter(state_name=="Jalisco")
ordenes_2021<- ordenes_2021 %>% filter(state_name=="Jalisco")
ordenes_2022<- ordenes_2022 %>% filter(state_name=="Jalisco")

############################################################################
#
#                                FISCALÍA
#
#############################################################################
indicador_16<-read_excel("indicador_16.xlsx")%>% suppressWarnings() %>% 
  mutate(Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
`Rango de edad`=factor(`Rango de edad`, levels = c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                                                   "36 a 45 años", "46 a 59 años", "60 en adelante", "No especifica")))

indicador_27<-read_excel("indicador_27.xlsx")%>% suppressWarnings()
indicador_28<-read_excel("indicador_28.xlsx")%>% suppressWarnings() %>% 
  mutate(Mes=factor(Mes,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                             "Septiembre", "Octubre","Noviembre", "Diciembre")))

indicador_29<-read_excel("indicador_29.xlsx")%>% suppressWarnings() %>% 
  mutate(Mes=factor(Mes,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                             "Septiembre", "Octubre","Noviembre", "Diciembre")))
indicador_30<-read_excel("indicador_30.xlsx")%>% suppressWarnings()
indicador_40<-read_excel("indicador_40.xlsx")%>% suppressWarnings()

############################################################################
#
#                                SALUD
#
#############################################################################

indicador_17<-read_excel("indicador_17.xlsx")%>% suppressWarnings() %>% 
  mutate(Rango=factor(Rango,
               levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                        "36 a 45 años", "46 a 59 años", "60 en adelante")))
indicador_18<-read_excel("indicador_18.xlsx")%>% suppressWarnings()
indicador_19<-read_excel("indicador_19.xlsx")%>% suppressWarnings()
indicador_20<-read_excel("indicador_20.xlsx")%>% suppressWarnings()
indicador_21<-read_excel("indicador_21.xlsx")%>% suppressWarnings()
indicador_22<-read_excel("indicador_22.xlsx")%>% suppressWarnings()
indicador_23<-read_excel("indicador_23.xlsx")%>% suppressWarnings()













# ##############################################################################
# #
# #                                    Data IJCF
# #
# ##############################################################################
# 
# url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRLUJKlsRbvv02Nq6qD6l8ka6XU6Su6j9nvod6lXcL2p6Iz9T22d6rGwkyBkWEqLA/pub?output=xlsx"
# destfile <- "pub_output_ijcf_xlsx"
# curl::curl_download(url, destfile)
# 
# ijcf <- read_excel(destfile, sheet = "Base actualizada", skip = 1,
#                    col_types = c("text", 
#                                  "text", "text", "text", "date", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text", "text", "text", "text", "text", 
#                                  "text")) %>% 
#   select(!c(`ID-SIABA-PF`, Distrito, `Folio identificador único de muer víctima de muerte violenta`, Probable, `edad conf`, Confirmada, Edad, 
#             `Tipo de servicio`, `Procesamiento del Lugar de los hechos (Levantamiento de Cadáver, indicios y fotografía)`)) %>% 
#   select(!4, `Estado del cuerpo`) %>% 
#   mutate(id = row_number(),
#          `Rango de edad`=case_when(
#            is.na(`Rango de edad`) ~ "Sin especificar",
#            T~`Rango de edad`
#          ))
# 
# ijcf$periodo <- as.Date(ijcf$`fecha de suceso \n (dd-mm-aa)`, format="%Y-%m-%d")
# ijcf$fecha <- substr(ijcf$`fecha de suceso \n (dd-mm-aa)`, start = 1, stop = 7)
# # ijcf$Año <- as.factor(ijcf$Año)
# 
# perfil_victima <- ijcf %>% select(id, fecha, Año, 3:8)
# servicios <- ijcf
# 
# servicios <-servicios %>% 
#   # filter(`Municipio de suceso`=="Guadalajara"#,
#   #        #Año=="2024.0"
#   # ) %>%
#   pivot_longer(
#     cols = c(9:32),
#     names_to = "Servicios",
#     values_to = "Total")
# 
# servicios$Año<-substr(servicios$Año, start = 1, stop = 4)
# 
# 
# # BOX IJCF: - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
# # perfil_reactive() %>%
# perfil_victima %>%
#   filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta") %>% 
#   summarise(Total=n()) %>%
#   mutate(Total=comma(Total)) ->ijcf_box_1
# 
# perfil_victima %>% 
#   filter(!is.na(fecha)) %>% 
#   mutate(fecha=ymd(paste0(fecha, "-01"))) %>% 
#   # group_by(fecha) %>% 
#   group_by(fecha=floor_date(fecha)) %>%
#   summarise(Total=n()) %>%
#   filter(Total>0) %>% 
#   pull(fecha) %>% 
#   max() %>% 
#   lubridate::month(abbr = T, label = T)->mes_final_ijcf
# 
# # perfil_reactive() %>%
# perfil_victima %>%
#   # filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta",
#   #        Año==2024) %>% 
#   group_by(`Clasificación de causa de muerte (Violenta o Natural)`) %>% 
#   summarise(Total=n()) %>%
#   mutate(Porcentaje = Total / sum(Total),
#          Porcentaje = round((Porcentaje * 100), digits = 2)) %>% 
#   arrange(-Total) %>% 
#   mutate(Total=comma(Total),
#          Porcentaje=paste0(Porcentaje, "%"))->ijcf_box_1_1
# 
# 
# # perfil_reactive() %>%
# perfil_victima %>%
#   filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta") %>% 
#   group_by(Año) %>% 
#   summarise(Total=n()) %>% 
#   mutate(Variación = scales::percent((Total - lag(Total))/lag(Total),0.1))%>%
#   mutate(Total=comma(Total)) ->ijcf_box_2
# 
# 
# # perfil_reactive() %>%
# perfil_victima %>% 
#   filter(`Clasificación de causa de muerte (Violenta o Natural)`=="Violenta") %>% 
#   group_by(`Municipio de suceso`) %>% 
#   summarise(Total=n()) %>%
#   arrange(desc(Total)) %>% 
#   # mutate(Total=comma(Total)) %>% 
#   mutate(Porcentaje = Total / sum(Total),
#          Porcentaje = round((Porcentaje * 100), digits = 2),
#          Porcentaje =paste0(Porcentaje, "%")) %>% 
#   head(n=5)->ijcf_box_3
# 
# 
# ##############################################################################
# #
# #                             ORDENES Y MEDIDAS
# #
# ##############################################################################
# 
# url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQrAC8uJDjHPsFX6cIbh2E52vExKI6hSzLVGMyLVb4AIoxsZnYcyuzz7rAz5m8_u9n-ztjTgnfyRBIL/pub?output=xlsx"
# destfile <- "ordenes_medidas_xlsx"
# curl::curl_download(url, destfile)
# 
# 
# medidas_ordenes_estatal <- read_excel(destfile, sheet = "estatal")
# medidas_ordenes_municipal <- read_excel(destfile, sheet = "municipal") %>% 
#   mutate(mes=factor(mes,
#                     levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# # mapa
# #df_mxmunicipio_2020<-data("df_mxmunicipio_2020") 
# 
# #data("df_mxmunicipio_2020")
# 
# medidas_ordenes_municipal %>% 
#   #filter(año==2019) %>%
#   group_by(año, municipio) %>% 
#   summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
#             ordenes=sum(ordenes_aceptadas + ordenes_rechazadas)) %>% 
#   pivot_longer(cols=c("ordenes","medidas"),
#                names_to = "tipo",
#                values_to = "total") ->medidas_y_ordenes
# 
# 
# medidas_y_ordenes %>%  filter(tipo=="medidas", año==2019)->medidas_2019
# medidas_y_ordenes %>%  filter(tipo=="medidas", año==2020)->medidas_2020
# medidas_y_ordenes %>%  filter(tipo=="medidas", año==2021)->medidas_2021
# medidas_y_ordenes %>%  filter(tipo=="medidas", año==2022)->medidas_2022
# 
# 
# 
# # df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")
# 
# medidas_2019 <-merge(df_mxmunicipio_2020, medidas_2019, by.x="municipio_name", by.y="municipio")
# medidas_2020 <-merge(df_mxmunicipio_2020, medidas_2020, by.x="municipio_name", by.y="municipio")
# medidas_2021 <-merge(df_mxmunicipio_2020, medidas_2021, by.x="municipio_name", by.y="municipio")
# medidas_2022 <-merge(df_mxmunicipio_2020, medidas_2022, by.x="municipio_name", by.y="municipio")
# 
# medidas_2019$value<- medidas_2019$total
# medidas_2020$value<- medidas_2020$total
# medidas_2021$value<- medidas_2021$total
# medidas_2022$value<- medidas_2022$total
# 
# medidas_2019<- medidas_2019 %>% filter(state_name=="Jalisco")
# medidas_2020<- medidas_2020 %>% filter(state_name=="Jalisco")
# medidas_2021<- medidas_2021 %>% filter(state_name=="Jalisco")
# medidas_2022<- medidas_2022 %>% filter(state_name=="Jalisco")
# 
# #_______________________________________________________________________________#
# 
# medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2019)->ordenes_2019
# medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2020)->ordenes_2020
# medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2021)->ordenes_2021
# medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2022)->ordenes_2022
# 
# # df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")
# 
# ordenes_2019 <-merge(df_mxmunicipio_2020, ordenes_2019, by.x="municipio_name", by.y="municipio")
# ordenes_2020 <-merge(df_mxmunicipio_2020, ordenes_2020, by.x="municipio_name", by.y="municipio")
# ordenes_2021 <-merge(df_mxmunicipio_2020, ordenes_2021, by.x="municipio_name", by.y="municipio")
# ordenes_2022 <-merge(df_mxmunicipio_2020, ordenes_2022, by.x="municipio_name", by.y="municipio")
# 
# ordenes_2019$value<- ordenes_2019$total
# ordenes_2020$value<- ordenes_2020$total
# ordenes_2021$value<- ordenes_2021$total
# ordenes_2022$value<- ordenes_2022$total
# 
# ordenes_2019<- ordenes_2019 %>% filter(state_name=="Jalisco")
# ordenes_2020<- ordenes_2020 %>% filter(state_name=="Jalisco")
# ordenes_2021<- ordenes_2021 %>% filter(state_name=="Jalisco")
# ordenes_2022<- ordenes_2022 %>% filter(state_name=="Jalisco")
# 
# 
# 
# 
# ############################################################################
# #
# #                                FISCALIA
# #
# #############################################################################
# 
# 
# url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRuVRC8FAam8R4KlTPiWkNf-BZsogb1VFesRA0WPV_6usy0b-WSiiWOzeulHD1pOw/pub?output=xlsx"
# destfile <- "pub_output_fiscalia_xlsx"
# curl::curl_download(url, destfile)
# fiscalia <- read_excel(destfile)
# 
# 
# # Indicador 16: ----------------------------------------------------------------
# indicador_16 <- read_excel(destfile, sheet = "Ind 16")%>% suppressWarnings()
# 
# indicador_16 %>%
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12),
#     Mes=factor(Mes,
#                levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                         "Septiembre", "Octubre","Noviembre", "Diciembre")),
#     #  Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
#     `Rango de edad`=factor(`Rango de edad`, levels = c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
#                                                        "36 a 45 años", "46 a 59 años", "60 en adelante", "No especifica")),
#     Periodo = ymd(paste0(Año, "-", Fecha, "-01"))#,
#     #Trimestre = paste0(Año, " ", Trimestre)
#   ) ->indicador_16
# 
# 
# 
# 
# 
# # Indicador 27: ----------------------------------------------------------------#
# indicador_27 <- read_excel(destfile,sheet = "Ind 27")%>% suppressWarnings()
# 
# indicador_27 %>%
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12,
#     
#   ),
#   Mes=factor(Mes,
#              levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                       "Septiembre", "Octubre","Noviembre", "Diciembre")),
#   Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_27
# 
# 
# 
# indicador_27$Fecha   <- format(as.Date(indicador_27$Periodo, format = "%d-%m-%Y"))
# indicador_27$Fecha   <- format(as.Date(indicador_27$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_27$Fecha   <- as.Date(indicador_27$Periodo, format = "%d-%m-%Y")
# 
# 
# 
# # Indicador 28: ----------------------------------------------------------------#
# indicador_28 <- read_excel(destfile, sheet = "Ind 28")%>% suppressWarnings()
# 
# indicador_28 %>%
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12,
#     
#   ),
#   Mes=factor(Mes,
#              levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                       "Septiembre", "Octubre","Noviembre", "Diciembre")),
#   `Mujeres atendidas en el CJM`=as.numeric(`Mujeres atendidas en el CJM`),
#   Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_28
# 
# 
# 
# indicador_28$Fecha   <- format(as.Date(indicador_28$Periodo, format = "%d-%m-%Y"))
# indicador_28$Fecha   <- format(as.Date(indicador_28$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_28$Fecha   <- as.Date(indicador_28$Periodo, format = "%d-%m-%Y")
# 
# 
# 
# 
# 
# # Indicador 29: ----------------------------------------------------------------#
# indicador_29 <- read_excel(destfile,  sheet = "Ind 29")%>% suppressWarnings()
# 
# indicador_29 %>%
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12),
#   Mes=factor(Mes,
#              levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                       "Septiembre", "Octubre","Noviembre", "Diciembre", "Enero a Junio", "Julio a Diciembre"
#              )),
#   Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_29
# 
# 
# 
# indicador_29$Fecha   <- format(as.Date(indicador_29$Periodo, format = "%d-%m-%Y"))
# indicador_29$Fecha   <- format(as.Date(indicador_29$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_29$Fecha   <- as.Date(indicador_29$Periodo, format = "%d-%m-%Y")
# 
# 
# 
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de Contra la Dignidad"] <- "Delitos contra la dignidad"
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de\nCORRUPCIÓN DE MENORES"] <- "Corrupción de menores"
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de CORRUPCIÓN DE MENORES"] <- "Corrupción de menores"
# 
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de\nPROSTITUCIÓN INFANTIL"] <- "Prostitución infantil"
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de PROSTITUCIÓN INFANTIL"] <- "Prostitución infantil"
# 
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de\nULTRAJES A LA MORAL"] <- "Ultrajes a la moral"
# indicador_29$Delito[indicador_29$Delito=="Carpetas de Investigación por el delito de ULTRAJES A LA MORAL"] <- "Ultrajes a la moral"
# 
# 
# indicador_29$Delito[indicador_29$Delito=="Desobediencia o Resistencia de Particulares"] <- "Desobediencia o resistencia de particulares"
# indicador_29$Delito[indicador_29$Delito=="Homicidio (muerte violenta de una mujer)"] <- "Muertes violentas (aún en investigación)"
# indicador_29$Delito[indicador_29$Delito=="Instigación o ayuda al suicidio feminicida"] <- "Suicidio feminicida"
# indicador_29$Delito[indicador_29$Delito=="NAM bajo los protocolos de perspectiva y violencia de género"] <- "Por el delito que resulte"
# 
# 
# # Indicador 30: ----------------------------------------------------------------#
# indicador_30 <- read_excel(destfile,  
#                            sheet = "Ind 30")
# indicador_30 %>%
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12,
#     
#   ),
#   Mes=factor(Mes,
#              levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                       "Septiembre", "Octubre","Noviembre", "Diciembre")),
#   Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) %>% 
#   mutate(`Año de sentencia`=case_when(
#     `Año de sentencia`==2023~1,
#     `Año de sentencia`==2022~1,
#     `Año de sentencia`==2021~1,
#     `Año de sentencia`==2020~1, 
#     T~0))->indicador_30
# 
# 
# 
# indicador_30$Fecha   <- format(as.Date(indicador_30$Periodo, format = "%d-%m-%Y"))
# indicador_30$Fecha   <- format(as.Date(indicador_30$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_30$Fecha   <- as.Date(indicador_30$Periodo, format = "%d-%m-%Y")
# 
# 
# 
# # Indicador 40: ----------------------------------------------------------------#
# indicador_40 <- read_excel(destfile, sheet = "Ind 40")%>% suppressWarnings()
# 
# indicador_40 %>%
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12),
#     Mes=factor(Mes,
#                levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                         "Septiembre", "Octubre","Noviembre", "Diciembre")),
#     Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_40
# 
# 
# 
# indicador_40$Fecha   <- format(as.Date(indicador_40$Periodo, format = "%d-%m-%Y"))
# indicador_40$Fecha   <- format(as.Date(indicador_40$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
# indicador_40$Fecha   <- as.Date(indicador_40$Periodo, format = "%d-%m-%Y")
# 
# 
# ############################################################################
# #
# #                                SALUD
# #
# #############################################################################
# 
# 
# url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSYinHGBlKrVwcjv5LikSGLN_BTtChD81UJA5AlGhNRYPAmbfnECLg-56HrQRXJAQ/pub?output=xlsx"
# destfile <- "pub_output_salud_xlsx"
# curl::curl_download(url, destfile)
# salud <- read_excel(destfile)
# 
# 
# # Indicador 17: ----------------------------------------------------------------
# indicador_17 <- read_excel(destfile, sheet = "Ind 17")%>% suppressWarnings()
# 
# indicador_17 %>% 
#   filter(Año >=2019) %>% 
#   mutate(Fecha=case_when(
#     Mes=="Enero" ~ 1,
#     Mes=="Febrero" ~ 2,
#     Mes=="Marzo" ~ 3,
#     Mes=="Abril" ~ 4,
#     Mes=="Mayo" ~ 5,
#     Mes=="Junio" ~ 6,
#     Mes=="Julio" ~ 7,
#     Mes=="Agosto" ~ 8,
#     Mes=="Septiembre" ~ 9,
#     Mes=="Octubre" ~ 10,
#     Mes=="Noviembre" ~ 11,
#     Mes=="Diciembre" ~ 12,
#     
#   ),
#   Mes=factor(Mes,
#              levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                       "Septiembre", "Octubre","Noviembre", "Diciembre")),
#   Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
#   Rango= case_when(
#     `Edad (0-99)` <= 2 ~ "0 a 2 años",
#     `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
#     `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
#     `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
#     `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
#     `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
#     `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
#     `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
#     `Edad (0-99)` >= 60  ~ "60 en adelante"),
#   Rango=factor(Rango,
#                levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
#                         "36 a 45 años", "46 a 59 años", "60 en adelante"))) %>% 
#   mutate(`Tipo: (abuso sexual infantil / violación)` = case_when(
#     `Edad (0-99)` >= 18 ~ "Violación",
#     `Edad (0-99)` <= 17 ~ "Abuso sexual infantil",
#     TRUE ~ `Tipo: (abuso sexual infantil / violación)`))->indicador_17
# 
# 
# indicador_17$`Tipo: (abuso sexual infantil / violación)`[indicador_17$`Tipo: (abuso sexual infantil / violación)`=="ABUSO SEXUAL INTANTIL"]<- "Abuso sexual infantil"
# indicador_17$`Tipo: (abuso sexual infantil / violación)`[indicador_17$`Tipo: (abuso sexual infantil / violación)`=="VIOLACIÓN"]<- "Violación"      
# 
# 
# # Indicador 18: ----------------------------------------------------------------#
# indicador_18 <- read_excel(destfile, sheet = "Ind 18")%>% suppressWarnings()
# 
# indicador_18 %>% 
#   mutate(
#     Rango= case_when(
#       `Edad (0-99)` <= 2 ~ "0 a 2 años",
#       `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
#       `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
#       `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
#       `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
#       `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
#       `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
#       `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
#       `Edad (0-99)` >= 60  ~ "60 en adelante"),
#     Rango=factor(Rango,
#                  levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
#                           "36 a 45 años", "46 a 59 años", "60 en adelante")),
#     `Canalizadas por fiscalía: (si/no)`= case_when(
#       `Canalizadas por fiscalía: (si/no)`== "No"~0,
#       `Canalizadas por fiscalía: (si/no)`== "Sí"~1),
#     indicador_18 %>% 
#       mutate(
#         Rango= case_when(
#           `Edad (0-99)` <= 2 ~ "0 a 2 años",
#           `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
#           `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
#           `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
#           `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
#           `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
#           `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
#           `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
#           `Edad (0-99)` >= 60  ~ "60 en adelante"),
#         Rango=factor(Rango,
#                      levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
#                               "36 a 45 años", "46 a 59 años", "60 en adelante")),
#         `Canalizadas por fiscalía: (si/no)`= case_when(
#           `Canalizadas por fiscalía: (si/no)`== "No"~0,
#           `Canalizadas por fiscalía: (si/no)`== "Sí"~1),
#         
#         `Solicitud del procedimiento de manera directa a dependencias de salud: (si/no)`= case_when(
#           `Solicitud del procedimiento de manera directa a dependencias de salud: (si/no)`== "No"~0,
#           `Solicitud del procedimiento de manera directa a dependencias de salud: (si/no)`== "Sí"~1))) ->indicador_18
# 
# 
# 
# # Indicador 19: ----------------------------------------------------------------
# indicador_19 <- read_excel(destfile ,sheet = "Ind 19",
#                            col_types = c("text", 
#                                          "date", "numeric", "text", "text", 
#                                          "numeric", "text", "text")) %>% suppressWarnings()
# 
# indicador_19 %>%
#   filter(Año >=2018) %>% 
#   mutate(
#     `Causal: (salud/riesgo)`= case_when(
#       `Causal: (salud/riesgo)`%in% c("Caso IMSS, Grave daño a la salud", "Grave Riesgo a la Salud","Riesgo a la salud","Riesgo de muerte") ~ "Daño a la salud",
#       `Causal: (salud/riesgo)`== "No aplica/ Causal no legal" ~ "Causal no legal",
#       `Causal: (salud/riesgo)`== "Autonomía Reproductiva" ~ "Autonomía reproductiva",
#       `Causal: (salud/riesgo)` %in% c("Violación","Violación Sexual (IVE)") ~ "Causal de violación"),
#     Rango= case_when(
#       `Edad (0-99)` <= 2 ~ "0 a 2 años",
#       `Edad (0-99)` >= 3 & `Edad (0-99)` <= 5 ~ "3 a 5 años",
#       `Edad (0-99)` >= 6 & `Edad (0-99)` <= 12 ~ "6 a 12 años",
#       `Edad (0-99)` >= 13 & `Edad (0-99)` <= 17 ~ "13 a 17 años",
#       `Edad (0-99)` >= 18 & `Edad (0-99)` <= 25 ~ "18 a 25 años",
#       `Edad (0-99)` >= 26 & `Edad (0-99)`  <= 35 ~ "26 a 35 años",
#       `Edad (0-99)` >= 36 & `Edad (0-99)` <= 45 ~ "36 a 45 años",
#       `Edad (0-99)` >= 46 & `Edad (0-99)` <= 59 ~ "46 a 59 años",
#       `Edad (0-99)` >= 60  ~ "60 en adelante"),
#     Rango=factor(Rango,
#                  levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
#                           "36 a 45 años", "46 a 59 años", "60 en adelante")),
#     `¿Se realizó el procedimiento? (sí/no)`=case_when(
#       `¿Se realizó el procedimiento? (sí/no)`== "NA"~0,
#       `¿Se realizó el procedimiento? (sí/no)`== "SI"~1))->indicador_19
# 
# 
# 
# 
# # Indicador 20: ----------------------------------------------------------------
# indicador_20 <- read_excel(destfile, sheet = "Ind 20")
# indicador_20$`Notificadas al mp: (si/no)` <- toupper(indicador_20$`Notificadas al mp: (si/no)`)
# indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`<-toupper(indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)
# indicador_20$`Tipo de violencia: (violencia familiar / sexual)`<- toupper(indicador_20$`Tipo de violencia: (violencia familiar / sexual)`)
# 
# indicador_20 %>% 
#   mutate(
#     Fecha=case_when(
#       Mes=="Enero" ~ 1,
#       Mes=="Febrero" ~ 2,
#       Mes=="Marzo" ~ 3,
#       Mes=="Abril" ~ 4,
#       Mes=="Mayo" ~ 5,
#       Mes=="Junio" ~ 6,
#       Mes=="Julio" ~ 7,
#       Mes=="Agosto" ~ 8,
#       Mes=="Septiembre" ~ 9,
#       Mes=="Octubre" ~ 10,
#       Mes=="Noviembre" ~ 11,
#       Mes=="Diciembre" ~ 12),
#     Mes=factor(Mes,
#                levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                         "Septiembre", "Octubre","Noviembre", "Diciembre")),
#     Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
#     
#     `Notificadas al mp: (si/no)` = case_when(
#       `Notificadas al mp: (si/no)` %in% c("SI","SÍ", "REFER") ~ 1,
#       `Notificadas al mp: (si/no)`== "NO" ~ 0,
#       T~0),
#       str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "psic|PSIC|Psic") ~ "Violencia psicológica", 
#       str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "eco|ECO|Eco") ~ "Violencia económica",
#       str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "fami|FAMI|Fami") ~ "Violencia familiar",
#       str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "física|Física|FÍSICA|FISICA|Fisica|fisica") ~ "Violencia física",
#       str_detect(`Tipo de violencia: (violencia familiar / sexual)`, "sex|SEX|Sex") ~ "Violencia sexual",
#       T~"Otro tipo")) %>% 
#   mutate(`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=case_when(
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="ÁREA DE DEPORTE Y ATLETISMO" ~ "Comunitaria",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="CLUB, CANTINA, BAR" ~ "Comunitaria",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="COMERCIO Y ÁREAS DE SERVICIO" ~ "Comunitaria",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="COMUNITARIA" ~ "Comunitaria",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="VEHÍCULO AUTOMOTOR PÚBLICO" ~ "Comunitaria",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="VÍA PÚBLICA (PEATÓN)" ~ "Comunitaria",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="ESCUELA" ~ "Escolar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="CONYUGE/PAREJA/NOVIA(O)" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="FAMILIAR" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="FAMILIAR, SEXUAL Y ABANDONO" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="MADRE" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="OTRO PARIENTE" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="PADRE" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="SEXUAL Y FAMILIAR" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="SEXUAL, FAMILIAR" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="VIOLENCIA FAMILIAR" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="VIVIENDA" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="FEMINICIDIO" ~ "Feminicida",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="LABORAL Y DOCENTE" ~ "Laboral",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="TRABAJO" ~ "Laboral",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="ABANDONO" ~ "Familiar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="FETAL" ~ "Sin especificar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="INSTITUCIÓN RESIDENCIAL" ~ "Sin especificar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="NO ESPECIFICADO" ~ "Sin especificar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="SEXUAL" ~ "Sin especificar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="VEHÍCULO AUTOMOTOR PRIVADO" ~ "Sin especificar",
#     `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`=="VIOLENCIA_NO_FAMILIAR" ~ "Sin especificar"))-> indicador_20
# 
# 
# 
# # Indicador 21: ----------------------------------------------------------------#
# indicador_21 <- read_excel(destfile, sheet = "Ind 21")%>% suppressWarnings()
# 
# indicador_21 %>% 
#   mutate(
#     Fecha=case_when(
#       Mes=="Enero" ~ 1,
#       Mes=="Febrero" ~ 2,
#       Mes=="Marzo" ~ 3,
#       Mes=="Abril" ~ 4,
#       Mes=="Mayo" ~ 5,
#       Mes=="Junio" ~ 6,
#       Mes=="Julio" ~ 7,
#       Mes=="Agosto" ~ 8,
#       Mes=="Septiembre" ~ 9,
#       Mes=="Octubre" ~ 10,
#       Mes=="Noviembre" ~ 11,
#       Mes=="Diciembre" ~ 12),
#     Mes=factor(Mes,
#                levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                         "Septiembre", "Octubre","Noviembre", "Diciembre")),
#     Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
#     `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)` = case_when(
#       `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)`== "SI" ~ 1,
#       `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)`== "NO" ~ 0,
#       T~0))->indicador_21
# 
# 
# 
# # Indicador 22: ----------------------------------------------------------------#
# indicador_22 <- read_excel(destfile, sheet = "Ind 22")%>% suppressWarnings()
# 
# 
# indicador_22 %>% 
#   mutate(
#     Fecha=case_when(
#       Mes=="Enero" ~ 1,
#       Mes=="Febrero" ~ 2,
#       Mes=="Marzo" ~ 3,
#       Mes=="Abril" ~ 4,
#       Mes=="Mayo" ~ 5,
#       Mes=="Junio" ~ 6,
#       Mes=="Julio" ~ 7,
#       Mes=="Agosto" ~ 8,
#       Mes=="Septiembre" ~ 9,
#       Mes=="Octubre" ~ 10,
#       Mes=="Noviembre" ~ 11,
#       Mes=="Diciembre" ~ 12),
#     Mes=factor(Mes,
#                levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                         "Septiembre", "Octubre","Noviembre", "Diciembre")),
#     Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_22
# 
# indicador_22$Formación <- toupper(indicador_22$Formación)
# 
# 
# # Indicador 23: ----------------------------------------------------------------#
# indicador_23 <- read_excel(destfile, sheet = "Ind 23")%>% suppressWarnings()
# 
# indicador_23 %>% 
#   mutate(
#     Fecha=case_when(
#       Mes=="Enero" ~ 1,
#       Mes=="Febrero" ~ 2,
#       Mes=="Marzo" ~ 3,
#       Mes=="Abril" ~ 4,
#       Mes=="Mayo" ~ 5,
#       Mes=="Junio" ~ 6,
#       Mes=="Julio" ~ 7,
#       Mes=="Agosto" ~ 8,
#       Mes=="Septiembre" ~ 9,
#       Mes=="Octubre" ~ 10,
#       Mes=="Noviembre" ~ 11,
#       Mes=="Diciembre" ~ 12),
#     Mes=factor(Mes,
#                levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                         "Septiembre", "Octubre","Noviembre", "Diciembre")),
#     Periodo = ymd(paste0(Año, "-", Fecha, "-01"))) %>% 
#   mutate(`Objetor de conciencia: (SI/NO)` =case_when(
#     `Objetor de conciencia: (SI/NO)`=="SI"~0,
#     `Objetor de conciencia: (SI/NO)`=="NO"~1)) %>% 
#   mutate(Función =toupper(Función)) %>% 
#   mutate(Función=case_when(
#     str_detect(`Función`, "MEDIC|ADSC|GINE")~"Personal médico",
#     str_detect(`Función`, "ENFER")~"Personal de enfermería",
#     T~"No se especifica"))->indicador_23
# 
# indicador_23$Formación <- toupper(indicador_23$Formación)
# unique(indicador_23$Función)















# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

header_img <- div(
  # img(src="https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png", height="45px"),
  div(
    class = "my-title",
    h4('Title'), h5('Subtitle'),
    tags$style(".my-title :is(h4, h5){color: white; font-weight: bold;}")
  ),
  style = "display: flex;"
)


###############################################################################
#                                         
#
#                                      UI
###############################################################################

ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(
      class = 'p-2',
      tags$head(
        tags$style(HTML("
        
  .navbar .navbar-header {float: left; }
  .navbar .navbar-nav {float: right;}
  .container {min-width: 2250px}
          
  .nav.navbar-nav .form-group.shiny-input-container {margin-bottom: 0; height: 0px;}
  .nav.navbar-nav .form-group.shiny-input-container > label {display: inline;}                      
                        
  .p{                    
  font-family: Nutmeg-Regular;
  white-space: normal;
  color: #ffffff;
  }
  

  .dropdown-menu > .active > a {background-color:#742484;}


  .navbar-static-top {
   font-family: Nutmeg-Regular;
   background-color:#AA86CA;
   
   }
                                       
   .navbar > .active {
   color: white;
   font-family: Nutmeg-Regular;
   background-color:#AA86CA;!important;},
   
   .navbar-default .navbar-brand{
    color: white;
    font-family: Nutmeg-Regular;
    background-color:#AA86CA;
   }
  
  .navbar-nav li a:hover, .navbar-nav > .active > a {
    color: white;
    font-family: Nutmeg-Regular;
    background-color:#C5B6CA!important;}

      
  .p-2 {
   padding: 0px!important;
  
   }
   .small-box h3 {
    font-size: 38px;
    font-weight: 700;
    margin: 0 0 10px 0;
    white-space: normal!important;
    padding: 0;
  }
    @media (min-width: 768px) {
  .d-flex {
    display: flex;
  }
    }
    .small-box{
    border-radius: 2px;
    position: relative;
    display: block;
    margin-bottom: 20px;
    box-shadow: 0 1px 1px rgb(0 0 0 / 10%);
    height: calc(100% - 20px);
    }
    # .html-widget{min-width: 300px;
    # }
    .mb-2{ 
    margin-bottom:20px;
    }
    .p-2{ 
    padding: 20px;     
    }x|
    #table_muertes{overflow: scroll; 
    }   
    
  .small-box.bg-fuchsia {
   background-color: #564d79 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-purple {
   background-color: #6e6491 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }

   .small-box.bg-maroon {
     background-color: #857baa !important; 
   color: white !important;       
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-light-blue {
   background-color: #9d91c2 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
   }
    
                        ")),
        tags$script(HTML("window.addEventListener('message', event => {
    // IMPORTANT: check the origin of the data!
    console.log('recibi un mensaje', event);
    if (event.origin.includes('https://igualdad.jalisco.gob.mx')) {
        // The data was sent from your site.
        // Data sent with postMessage is stored in event.data:

        let height = 0;
        if (document.body) {
            if (document.body.scrollHeight) {
                height= document.body.scrollHeight;
            }
        }

        event.source.postMessage(height, event.origin);
    } 

    return;
});")),
        tags$script('
           var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
      
      
      
      add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E34F70"),
      
      navbarPage(title=div(img(src="https://raw.githubusercontent.com/nancymanzo/Micrositio-de-datos-AVGM/main/Logo_AVGM_2%20-%20copia.jpg", width = "60", height="30"), "Micrositio de datos", style ="font-weight:bold; color: #ffffff;"),
                 header= busy_start_up(
                   loader = spin_epic("flower", color = "#8F5199"),
                   text = "Cargando",
                   timeout = 1500,
                   color = "#8F5199",
                   background = " white"),
                 useShinydashboard(),
                 
                 # Inicio -----------------------------------------------------------------------
                 
                 tabPanel(#icon = icon("home"),
                   title=p("Inicio", style ="font-weight:bold; color: #ffffff;"), id="Inicio",
                   div(
                     class="row d-flex", #Replicar
                     valueBox("Micrositio de datos de los Indicadores de la AVGM", "Visualiza, descarga y monitorea los indicadores de la Alerta de Violencia", width = 12, color="maroon")),
                    div(
                     class="row d-flex", #Replicar
                     valueBox("Visualiza", "Cada indicador incluye una tabla de resumen y un gráfico que presenta el avance y comportamiento mensual", icon=icon("chart-simple"),color="fuchsia", width = 4), # actualizar
                     valueBox("Descarga", "De manera periodica podrá descargar en formato de excel, los datos con los cuales se calculan los indicadores", icon=icon("file-excel"),color="purple", width = 4), # actualizar
                     valueBox("Monitorea", "Utiliza y conoce el avance de los indicadores en cada uno de sus objetivos",  icon=icon("binoculars"), color="maroon", width = 4)),
                   column(width = 7,
                   p("Micrositio de datos de la AVGM:", style = "color:black; font-weight:bold; font-size: 20px; text-align: justify"),
                   
                   p(style="color:black; font-weight:regular; font-size:15px; text-align: justify",
                     "Aquí podrá encontrar los datos que alimentan los indicadores de la AVGM. En cada apartado de arriba se presentan las distintas temáticas que se monitorean en la Alerta."),
                   p(style="color:black; font-weight:regular; font-size:15px; text-align: justify", "Asimismo, en cada apartado puede localizar una sección de filtros, donde se incluyen variables para desagregar la información, la cual se refleja en las visualizaciones en forma de gráficos, tablas y mapas."),
                   br(),
                   p("Sobre la información presentada", style = "color:black; font-weight:bold; font-size:15px; text-align: justify"),
                   # Lista con bullets
                   tags$ul(
                     tags$li(style = "text-align: justify", "Servicios Forense: En este apartado encuentra la información del perfil de las muertes de mujeres así como los dictámenes y servicios forenses aplicados"),
                     br(),
                     tags$li(style = "text-align: justify", "Órdenes y medidas de protección: Aqui encontraras los datos desagregados por municipios y su evolución en el tiempo"),
                     br(),
                     tags$li(style = "text-align: justify", "Salud ILE/IVE: Se tiene información de las atenciones brindadas por servicios sexual, así como la información de las solicitudes del procedimiento de la Interrupción del Embarazo, además, de la información del personal y establecimientos capacitados para la atención ILE-IVE"),
                     br(),
                     tags$li(style = "text-align: justify", "Feminicidios: Información de la incidencia delictiva por caso relacionadas con las muertes violentas de mujeres por feminicidios, así como accidentes, suicidios, homicidio doloso y culposo. Además encontraras información sobre el estatus de la sentencias emitidas por el delito de de feminicidio y el estatus de la judicialización de otros delitos por razón de género"),
                     br(),
                     tags$li(style = "text-align: justify", "Protocolo ALBA: En este apartado encontrarás información relacionada con la busqueda y localización de niñas, adolescentes y mujeres reportadas como desaparecidas en la entidad, así como el total de casos de búsqueda y localización")
                     )),
                   
                   # Hipervínculo
                   box(width = 5,
                   p(style = "color:black; font-weight:regular; font-size:15px; text-align: justify", "Recuerda que, además de descargar los datos y las visualizaciones en cada sección del micrositio, también puedes acceder al código y a los resultados del cálculo de los principales indicadores. Asimismo, tienes acceso al código abierto utilizado en esta plataforma:"),
                   br(),   
                   tags$ul(
                     tags$li("Reporte de los principales indicadores de la AVGM",tags$a(href = "//rstudio-pubs-static.s3.amazonaws.com/1198090_bd70156662bb4056afaab86f565c149e.html", "click aquí"))
                   ))),
                 
                 # uiOutput('box'),
                 # actionLink("ling_indicador_1", "Indicador 1: Porcentaje de servicios forenses en casos de muertes violentas de mujeres, provistos conforme a la debida diligencia y perspectiva de género"), hr(),
                 # actionLink("link_indicadores", "Volver al inicio")),
                 
                 
                 # Servicios forenses------------------------------------------------------------
                 
                 tabPanel(title = p("Servicios forenses",  style ="font-weight:bold; color: #ffffff;"), 
                          box(
                            width=12,
                            div(class="row d-flex", #Replicar
                                valueBox(
                                  value = paste(ijcf_box_1[1,1], "mujeres víctimas"),
                                  subtitle = paste0("de muertes violentas en el período de 2020 a 2024 (",  tolower(mes_final_ijcf[1]),  "). La mayor proporción es por muertes clasificadas como ", tolower(ijcf_box_1_1[1,1]), " con el ", ijcf_box_1_1[1,3], " (", ijcf_box_1_1[1,2],"), seguido de muerte de tipo ", tolower(ijcf_box_1_1[2,1]), " con el ", ijcf_box_1_1[2,3], " (", ijcf_box_1_1[2,2],")."),
                                  icon=icon("wave-square"), color="fuchsia", width = 4),
                                
                                valueBox(
                                  value = paste("De enero a", tolower(mes_final_ijcf[1]), "del", as.numeric(ijcf_box_2[5,1])),
                                  subtitle =  paste0("Se registran mujeres víctimas de muertes violentas ", as.numeric(ijcf_box_2[5,2]), ", una variación anual de ",  ijcf_box_2[5,3], ". El año anterior, ", as.numeric(ijcf_box_2[4,1]), " se presenta una variación de ",ijcf_box_2[4,3], " (", as.numeric(ijcf_box_2[4,2]), ")."),
                                  icon=icon("chart-area"),color="purple", width = 4),
                                
                                valueBox(
                                  value = "Resaltan los siguientes municipios",
                                  subtitle =  paste("donde se registran la mayor proporción de víctimas mujeres de muertes violentas", ijcf_box_3[1,1], " con el ", ijcf_box_3[1,3], ", seguido de", 
                                                    ijcf_box_3[2,1], "con el", ijcf_box_3[2,3], "y",ijcf_box_3[3,1], "con el", ijcf_box_3[3,3], "."),
                                  icon=icon("equals"), color="maroon", width = 4))),
                          
                          tabsetPanel(
                            tabPanel("Perfil de las víctimas",
                                     
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(width = 3, "\nSeleccione algunas características",
                                                    selectInput(
                                                      inputId = "perfil_fecha",
                                                      label = "Seleccione el año",
                                                      choices = unique(sort((perfil_victima$Año))),
                                                      multiple = T 
                                                    ),
                                                    selectInput(
                                                      inputId = "perfil_municipio",
                                                      label = "Seleccione el municipio",
                                                      choices = c("Jalisco", unique(sort(perfil_victima$`Municipio de suceso`))),
                                                      multiple = F,
                                                      selected = "Jalisco"
                                                    ),
                                                    selectInput(
                                                      inputId = "perfil_rango",
                                                      label = "Selecciona el rango de edad",
                                                      choices = unique(sort(perfil_victima$`Rango de edad`)),
                                                      multiple = TRUE
                                                    ),
                                                    selectInput(
                                                      inputId = "perfil_causa",
                                                      label = "Seleccione la causa",
                                                      choices = unique(sort(perfil_victima$`Posible causa de muerte (y/o mecanismo de muerte)`)),
                                                      multiple = T 
                                                    ),
                                                    selectInput(
                                                      inputId = "perfil_clasificacion",
                                                      label = "Seleccione el tipo de muerte (violenta o natural)",
                                                      choices = unique(sort(perfil_victima$`Clasificación de causa de muerte (Violenta o Natural)`)),
                                                      multiple = TRUE,
                                                      selected = "Violenta"
                                                    ),
                                                    
                                                    selectInput("value_total_g1", 
                                                                "Seleccione una opción de leyenda" , 
                                                                choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                                                selected = NULL ,  multiple = FALSE, selectize = TRUE),
                                                    downloadButton(
                                                      outputId = "download_gr1",
                                                      label = "Descarga el gráfico"
                                                    ),
                                                    downloadButton(
                                                      "download_d1",
                                                      "Descarga los datos")
                                       ),
                                       mainPanel(width=9,
                                                 plotOutput("gr1"), br(),br(),br(),
                                                 
                                                 fluidRow(
                                                   splitLayout(cellWidths = c("50%", "50%"),
                                                               plotOutput("mapa_ijcf"),
                                                               dataTableOutput("t1")
                                                   )),
                                                 h6("Fuente: Datos proporcionados por IJCF.")))),
                            tabPanel("Servicios forenses",
                                     br(),
                                     
                                     sidebarPanel(width = 3, "\nSeleccione algunas características",
                                                  selectInput(
                                                    inputId = "servicio_fecha",
                                                    label = "Seleccione el año",
                                                    choices = unique(sort(as.factor(servicios$Año))),
                                                    multiple = T 
                                                  ),
                                                  selectInput(
                                                    inputId = "servicio_municipio",
                                                    label = "Seleccione el municipio",
                                                    choices = c("Jalisco", unique(sort(servicios$`Municipio de suceso`))),
                                                    multiple = F,
                                                    selected = "Jalisco"
                                                  ),
                                                  selectInput(
                                                    inputId = "servicio_servicios",
                                                    label = "Seleccione los servicios",
                                                    choices = unique(sort(servicios$Servicios)),
                                                    multiple = TRUE,
                                                    selected = c("Balística", "Búsqueda de pelos y fibras",
                                                                 "Delitos sexuales", "Dictamen Psicosocial (servicio)")
                                                  ),
                                                  selectInput(
                                                    inputId = "servicio_rango",
                                                    label = "Selecciona el rango de edad",
                                                    choices = unique(sort(servicios$`Rango de edad`)),
                                                    multiple = TRUE
                                                  ),
                                                  selectInput(
                                                    inputId = "servicio_causa",
                                                    label = "Seleccione la causa",
                                                    choices = unique(sort(servicios$`Posible causa de muerte (y/o mecanismo de muerte)`)),
                                                    multiple = T 
                                                  ),
                                                  selectInput(
                                                    inputId = "servicio_clasificacion",
                                                    label = "Seleccione el tipo de muerte (violenta o natural)",
                                                    choices = unique(sort(servicios$`Clasificación de causa de muerte (Violenta o Natural)`)),
                                                    multiple = TRUE,
                                                    selected = "Violenta"
                                                  ),
                                                  
                                                  selectInput("value_total_g2",
                                                              "Seleccione una opción de leyenda" ,
                                                              choices = c("Sin etiqueta de datos", "Con etiqueta de datos"),
                                                              selected = "Con etiqueta de datos" ,  multiple = FALSE, selectize = TRUE),
                                                  downloadButton(
                                                    outputId = "download_gr2",
                                                    label = "Descarga el gráfico"
                                                  ),
                                                  downloadButton(
                                                    outputId = "download_d2",
                                                    label = "Descarga los datos")
                                                  # downloadButton(
                                                  #   outputId = "download_gr1",
                                                  #   label = "Descarga el gráfico"
                                                  # ),
                                                  # downloadButton(
                                                  #   outputId = "download_d1",
                                                  #   label = "Descarga los datos")
                                     ),
                                     mainPanel(width=9,
                                               plotOutput("gr2"),
                                               dataTableOutput("t2"))
                                     
                            ))),
                 
                 tabPanel(title = p("Órdenes y medidas", style ="font-weight:bold; color: #ffffff;"),
                          box(
                            width=12,  
                            div(class="row d-flex", #Replicar
                                valueBox("2023", "Se otorgaron 436 órdenes de protección, una aumento del 78% con respecto al año anterior.", icon=icon("equals"), color="fuchsia", width = 3),
                                valueBox("2022", "Se otorgaron 245 órdenes de protección, una reducción del 32% con respecto al año anterior.", icon=icon("equals"), color="purple", width = 3),
                                valueBox("2021", "Se otorgaron 359 órdenes de protección, un aumento del 81% con respecto al año anterior.", icon=icon("wave-square"), color="maroon", width = 3),
                                valueBox("2020", "Se otorgaron 198 órdenes de protección, un aumento del 607% con respecto al año anterior.", icon=icon("signal"), color="light-blue", width = 3))),
                          
                          # tabsetPanel(
                          #   tabPanel("Total de órdenes",  class="mb-2",
                          #            sidebarPanel("Seleccione algunas características", class=".mb-2",
                          #                         selectInput(
                          #                           inputId = "ordenes_año",
                          #                           label = "Año",
                          #                           choices = unique(sort(medidas_ordenes_municipal$año)),
                          #                           multiple = TRUE),
                          #                         selectInput(
                          #                           inputId = "ordenes_mes",
                          #                           label = "Mes",
                          #                           choices = unique(sort(medidas_ordenes_municipal$mes)),
                          #                           multiple = TRUE
                          #                         ),
                          #                         selectInput(
                          #                           inputId = "ordenes_municipio",
                          #                           label = "Municipio",
                          #                           choices = unique(sort(medidas_ordenes_municipal$municipio)),
                          #                           multiple = TRUE
                          #                         ),
                          #                         downloadButton("downloadData_ordenes", "Descarga (.csv)")),
                          #            mainPanel(h3("Total de órdenes de protección emitidas", align="center"),
                          #                      plotlyOutput("grafico_ordenes", height = "auto", width = "auto"),
                          #                      h6("Fuente: Elaborado con datos de la Fiscalía Estatal a diciembre 2023."))),
                          # 
                          #   tabPanel("Mapa de órdenes de protección", class="p-2",
                          #            column(12, align="center",
                          #                   h2(""),
                          # 
                          #                   #h2("Total de ordenes trabajadas en el estado de Jalisco, 2019 a 2022"),
                          #                   #h6("Datos de la Fiscalía del Estado"),
                          #                   selectInput("mapa_ordenes", "Seleccione el año" ,
                          #                               choices = c("Año 2019", "Año 2020",
                          #                                           "Año 2021", "Año 2022", "Año 2023", "Año 2024"),
                          #                               selected = "Año 2024",  multiple = FALSE,
                          #                               selectize = TRUE),
                          #                   # h3(text=paste0("Total de ordenes de protección otorgadas: ", input$mapa_ordenes ,
                          #                   #           '<br>','<sup>',
                          #                   #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                          # 
                          #                   fluidRow(
                          #                     splitLayout(cellWidths = c("50%", "50%"),
                          #                                 dataTableOutput("table_ordenes"),
                          #                                 plotlyOutput("mapa_2",height = "auto", width = "auto")
                          #                     )),
                          # 
                          #                   h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                          #                   h5("*La etiqueta del mapa 'value' hace referencia al valor total de órdenes de protección.",
                          #                      align="left", face="italic"))))
                          ),

                 tabPanel(title = p("Salud (ILE / IVE)", style ="font-weight:bold; color: #ffffff;"),
                          box(
                            width=12,
                            div(class="row d-flex", #Replicar
                                valueBox("46.4%", "Atenciones por violación en 2023", icon=icon("equals"),color="fuchsia", width = 3), # actualizar
                                valueBox("100%", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 3), # actualizar
                                valueBox("46.4%", "Atenciones por violación en 2022",  icon=icon("ellipsis"), color="maroon", width = 3), # actualizar
                                valueBox("100%", "Indicador 2019",  icon=icon("ellipsis"), color="light-blue", width = 3))), # actualizar
                          
                          tabsetPanel(
                            tabPanel(title = h6("Atenciones por delitos sexual", style="break-spaces: pre-line", class="p-2"),
                                     box(
                                       width=12,
                                       sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                                    selectInput(
                                                      inputId = "ind_17_año",
                                                      label = "Seleccione el año",
                                                      choices = unique(sort(indicador_17$Año)),
                                                      multiple = T),
                                                    selectInput(
                                                      inputId = "ind_17_mes",
                                                      label = "Seleccione el mes",
                                                      choices = unique(sort(indicador_17$Mes)),
                                                      multiple = TRUE
                                                    ),
                                                    selectInput(
                                                      inputId = "ind_17_edad",
                                                      label = "Selecciona el rango de edad",
                                                      choices = unique(sort(indicador_17$Rango)),
                                                      multiple = TRUE
                                                    ),
                                                    downloadButton(
                                                      outputId = "download_gr17",
                                                      label = "Descarga el gráfico"
                                                    ),
                                                    downloadButton(
                                                      outputId = "download_d17",
                                                      label = "Descarga los datos")),
                                       mainPanel(width=9,
                                                 plotOutput("gr17"),
                                                 br(),br(),br(),
                                                 dataTableOutput("t_17"),
                                                 
                                                 h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br()))), 
                            
                            tabPanel(title =h6("Atenciones por violencia familiar y/o sexual",
                                               style="break-spaces: pre-line", class="p-2"),
                                     br(),
                                     # h3(align="center","Indicador 20:", style="color:black"),
                                     # h4(p(align="center", "Porcentaje de casos atendidos en el Sector Salud por violencia familiar y/o sexual que se notifican al Ministerio Público de Fiscalía.")),
                                     # box(width=12,
                                     sidebarPanel(width = 3,"Seleccione algunas características", class=".mb-2",
                                                  selectInput(
                                                    inputId = "ind_20_año",
                                                    label = "Seleccione el año",
                                                    choices = unique(sort(indicador_20$Año)),
                                                    multiple = T, selected = c(2020,2021,2022,2023)
                                                  ),
                                                  selectInput(
                                                    inputId = "ind_20_tipo",
                                                    label = "Selecciona el tipo de violencia",
                                                    choices = unique(sort(indicador_20$`Tipo de violencia: (violencia familiar / sexual)`)),
                                                    multiple = TRUE
                                                  ),
                                                  
                                                  selectInput(
                                                    inputId = "ind_20_modalidad",
                                                    label = "Selecciona el tipo de modalidad de violencia",
                                                    choices = unique(sort(indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)),
                                                    multiple = TRUE
                                                  ),
                                                  downloadButton(
                                                    outputId = "download_gr20",
                                                    label = "Descarga el gráfico"
                                                  ),
                                                  downloadButton(
                                                    outputId = "download_20",
                                                    label = "Descarga los datos")),
                                     mainPanel(width=9,
                                               plotOutput("gr20"),
                                               br(),br(),br(),
                                               dataTableOutput("t_20"),
                                               h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())
                            ),
                            
                            
                            
                            tabPanel(title = h6("Solicitudes de IVE por violación", style="break-spaces: pre-line", class="p-2"),
                                     box(
                                       width=12,
                                       sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                                    selectInput(
                                                      inputId = "ind_18_año",
                                                      label = "Seleccione el año",
                                                      choices = unique(sort(indicador_18$Año)),
                                                      multiple = T),
                                                    selectInput(
                                                      inputId = "ind_18_mes",
                                                      label = "Seleccione el mes",
                                                      choices = unique(sort(indicador_18$Mes)),
                                                      multiple = TRUE
                                                    ),
                                                    selectInput(
                                                      inputId = "ind_18_edad",
                                                      label = "Selecciona el rango de edad",
                                                      choices = unique(sort(indicador_18$Rango)),
                                                      multiple = TRUE
                                                    ),
                                                    downloadButton(
                                                      outputId = "download_gr18",
                                                      label = "Descarga el gráfico"
                                                    ),
                                                    downloadButton(
                                                      outputId = "download_18",
                                                      label = "Descarga los datos")),
                                       mainPanel(width=9,
                                                 plotOutput("gr18"),
                                                 br(),br(),br(),
                                                 dataTableOutput("t_18"),
                                                 h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br()))),
                            
                            tabPanel(title = h6("Procedimientos de ILE", style="break-spaces: pre-line", class="p-2"),
                                     box(
                                       width=12,
                                       br(),
                                       sidebarLayout(
                                         sidebarPanel(width = 3,"Seleccione algunas características",
                                                      selectInput(
                                                        inputId = "ind_19_año",
                                                        label = "Seleccione el año",
                                                        choices = unique(sort(indicador_19$Año)),
                                                        multiple = T
                                                      ),
                                                      selectInput(
                                                        inputId = "ind_19_causal",
                                                        label = "Selecciona la causal",
                                                        choices = unique(sort(indicador_19$`Causal: (salud/riesgo)`)),
                                                        multiple = TRUE
                                                      ),
                                                      selectInput(
                                                        inputId = "ind_19_edad",
                                                        label = "Selecciona el rango de edad",
                                                        choices = unique(sort(indicador_19$Rango)),
                                                        multiple = TRUE
                                                      ),
                                                      downloadButton(
                                                        outputId = "download_gr19",
                                                        label = "Descarga el gráfico"
                                                      ),
                                                      downloadButton(
                                                        outputId = "download_19",
                                                        label = "Descarga los datos")),
                                         mainPanel(width=9,
                                                   plotOutput("gr19"),
                                                   br(),br(),br(),
                                                   dataTableOutput("t_19"),
                                                   h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())))),
                            
                            
                            tabPanel(title = h6("Personal y establecimientos ILE/IVE", style="break-spaces: pre-line", class="p-2"),
                                     box(width = 12,
                                         br(),
                                         sidebarLayout(
                                           sidebarPanel(width=3,"Seleccione algunas características", class=".mb-2",
                                            selectInput(
                                              inputId = "ind_22_año",
                                              label = "Seleccione el año",
                                              choices = unique(sort(indicador_22$Año)),
                                              multiple = T
                                              ),
                                     selectInput(
                                       inputId = "ind_22_formación",
                                       label = "Seleccione la formación del personal",
                                       choices = unique(sort(indicador_22$Formación)),
                                       multiple = TRUE
                                       )),
                                     mainPanel(width = 9, 
                                               h4("Personal de salud relacionado al procedimiento ILE/IVE, capacitado en el Programa ILE y NOM 046"),
                                               dataTableOutput("t_22"),
                                               h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")))),
                                     box(width=12,
                                         br(),
                                         sidebarLayout(
                                           sidebarPanel(width=3, "Seleccione algunas características", class=".mb-2",
                                              selectInput(
                                                inputId = "ind_21_año",
                                                label = "Seleccione el año",
                                                choices = unique(sort(indicador_21$Año)),
                                                multiple = T
                                                ),
                                        selectInput(
                                             inputId = "ind_21_establecimiento",
                                             label = "Seleccione el hospital",
                                             choices = unique(sort(indicador_21$Establecimiento)),
                                             multiple = TRUE)
                                        ),
                                        mainPanel(width=9,
                                                  h4("Establecimientos estatales proveedores de servicios de salud en condiciones óptimas para realizar un procedimiento ILE/IVE"),
                                                  dataTableOutput("t_21"),
                                                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())))))),
                            
                            
# Delitos por razón de género---------------------------------------------------
            tabPanel(title = p("Feminicidio", style ="font-weight:bold; color: #ffffff;"),
                     box(width=12,
                        div(class="row d-flex", #Replicar
                            valueBox("974", "Indicador 2022", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
                            valueBox("523", "Indicador 2021", icon=icon("wave-square"),color="purple", width = 4), # actualizar
                            valueBox("392","Indicador 2020",  icon=icon("ellipsis"), color="maroon", width = 4))), # actualizar
                        br(),
                     tabsetPanel(
                       tabPanel(title = h6("Opiniones técnicas", style="break-spaces: pre-line", class="p-2"),
                                box(
                                  width=12,
                                  sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                               selectInput(
                                               inputId = "ind_27_año",
                                               label = "Seleccione el año",
                                               choices = unique(sort(indicador_27$Año)),
                                               multiple = T
                                             ),
                                             selectInput(
                                              inputId = "ind_27_mes",
                                              label = "Seleccione el mes",
                                              choices = unique(sort(indicador_27$Mes)),
                                              multiple = TRUE
                                              ),
                                             downloadButton(
                                               outputId = "download_gr27",
                                               label = "Descarga el gráfico"
                                             ),
                                             downloadButton(
                                               outputId = "download_d27",
                                               label = "Descarga los datos")),
                                  mainPanel(width=9,
                                            plotOutput("gr27"),
                                            br(),br(),br(),
                                            dataTableOutput("t_27"),
                                            h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br())),

           tabPanel(title = h6("Denuncias por violación y violencia familiar", style="break-spaces: pre-line", class="p-2"),
                    box(
                      width=12,
                      sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                   selectInput(
                                     inputId = "ind_28_año",
                                     label = "Seleccione el año",
                                     choices = unique(sort(indicador_28$Año)),
                                     multiple = T
                                   ),
                                   selectInput(
                                     inputId = "ind_28_mes",
                                     label = "Seleccione el mes",
                                     choices = unique(sort(indicador_28$Mes)),
                                     multiple = TRUE
                                   ),
                                   downloadButton(
                                     outputId = "download_gr28",
                                     label = "Descarga el gráfico"
                                   ),
                                   downloadButton(
                                     outputId = "download_d28",
                                     label = "Descarga los datos")),
                      mainPanel(width=9,
                                plotOutput("gr28"),
                                br(),br(),br(),
                                dataTableOutput("t_28"),
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br())),
           
           
           
           tabPanel(title = h6("Delitos judicializados", style="break-spaces: pre-line", class="p-2"),
                    box(br(),
                      width=12,
                      sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                   selectInput(
                                     inputId = "ind_29_año",
                                     label = "Seleccione el año",
                                     choices = unique(sort(indicador_29$Año)),
                                     multiple = T
                                   ),
                                   selectInput(
                                     inputId = "ind_29_mes",
                                     label = "Seleccione el mes",
                                     choices = unique(sort(indicador_29$Mes)),
                                     multiple = TRUE
                                   ),
                                  selectInput(
                                    inputId = "ind_29_municipio",
                                    label = "Selecciona el municipio",
                                    choices = unique(sort(indicador_29$Municipio)),
                                    multiple = TRUE
                                  ),
                                 selectInput(
                                   inputId = "ind_29_delito",
                                   label = "Selecciona el delito",
                                   choices = unique(sort(indicador_29$Delito)),
                                   multiple = TRUE,
                                   selected = c("Abuso sexual", "Feminicidio",
                                                "Tentativa de feminicidio", "Violación",
                                                "Violación equiparada")
                                  ),
                                  downloadButton(
                                    outputId = "download_gr29",
                                    label = "Descarga el gráfico"
                                  ),
                                  downloadButton(
                                    outputId = "download_d29",
                                    label = "Descarga los datos"
                                 )),
                      mainPanel(width=9,
                                plotOutput("gr29"),
                                br(),br(),br(),
                                dataTableOutput("t_29"),
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br())),
           
           
           
           
           tabPanel(title = h6("Sentencias por feminicidio", style="break-spaces: pre-line", class="p-2"),
                    box(
                      width=12,
                      sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                   selectInput(
                                     inputId = "ind_30_año",
                                     label = "Seleccione el año",
                                     choices = unique(sort(indicador_30$Año)),
                                     multiple = T
                                   ),
                                   selectInput(
                                     inputId = "ind_30_mes",
                                     label = "Seleccione el mes",
                                     choices = unique(sort(indicador_30$Mes)),
                                     multiple = TRUE
                                   ),
                                   selectInput(
                                     inputId = "ind_30_condena",
                                     label = "Selecciona el tipo de condena",
                                     choices = unique(sort(indicador_30$`Tipo de sentencia (absolutoria, condenatoria y en proceso)`)),
                                     multiple = TRUE
                                   ),
                                   downloadButton(
                                     outputId = "download_gr30",
                                     label = "Descarga el gráfico"
                                   ),
                                   downloadButton(
                                     outputId = "download_d30",
                                     label = "Descarga los datos"
                                   )),
                      mainPanel(width=9,
                                plotOutput("gr30"),
                                br(),br(),br(),
                                dataTableOutput("t_30"),
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br())),
           
           
           tabPanel(title = h6("Muertes violentas", style="break-spaces: pre-line", class="p-2"),
                    box(
                      width=12,
                      sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
                                   selectInput(
                                     inputId = "ind_40_año",
                                     label = "Seleccione el año",
                                     choices = unique(sort(indicador_40$Año)),
                                     multiple = T
                                   ),
                                   selectInput(
                                     inputId = "ind_40_mes",
                                     label = "Seleccione el mes",
                                     choices = unique(sort(indicador_40$Mes)),
                                     multiple = TRUE
                                   ),
                                   selectInput("value_total_g40", 
                                               "Seleccione una opción de periodo" , 
                                               choices = c("Anual", "Mensual"), 
                                               selected = NULL ,  multiple = FALSE, selectize = TRUE
                                   ),
                                   downloadButton(
                                     outputId = "download_gr40",
                                     label = "Descarga el gráfico"
                                   ),
                                   downloadButton(
                                     outputId = "download_d40",
                                     label = "Descarga los datos"
                                   )),
                      mainPanel(width=9,
                                plotOutput("gr40"),
                                br(),br(),br(),
                                dataTableOutput("t_40"),
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br())),
           
           )),


                 tabPanel(title = p("Protocolo Alba", style ="font-weight:bold; color: #ffffff;"))
                 
      
))))

###############################################################################
#
#                             SERVER
#
################################################################################
server <- function(input, output, session) {
  
  output$filecontainer <- renderUI({
    tags$iframe(src="//rstudio-pubs-static.s3.amazonaws.com/1083902_d0b2a08d8b944627aed0720cb7a08291.html",
                style="border: 0px solid white; width: 1400px; height: 700px;")
  })
  
  
  
  # IJCF PERFIL: -----------------------------------------------------------------  
  
  output$perfil_fecha <- renderUI({
    selectInput("perfil_fecha",
                label =  "Seleccione el año",
                choices = sort(unique(perfil_victima$Año)),
                multiple = T)
  })
  
  
  output$perfil_municipio<- renderUI({
    selectInput("perfil_municipio",
                label =  "Seleccione el municipio",
                choices = sort(unique(perfil_victima$`Municipio de suceso`)),
                multiple = T)
  })
  
  
  output$perfil_rango <- renderUI({
    selectInput("perfil_rango",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(perfil_victima$`Rango de edad`)),
                multiple = T)
  })
  
  output$perfil_causa <- renderUI({
    selectInput("perfil_causa",
                label =  "Selecciona la causa de muerte",
                choices = sort(unique(perfil_victima$`Posible causa de muerte (y/o mecanismo de muerte)`)),
                multiple = T)
  })
  
  output$perfil_clasificacion <- renderUI({
    selectInput("perfil_clasificacion",
                label =  "Selecciona la clasificación de la muerte",
                choices = sort(unique(perfil_victima$`Clasificación de causa de muerte (Violenta o Natural)`)),
                multiple = T)
  })
  
  output$perfil_periodo <- renderUI({
    selectInput("perfil_periodo",
                label =  "Selecciona la fecha",
                choices = sort(unique(perfil_victima$fecha)),
                multiple = T)
  })
  
  
  perfil_reactive <- reactive({
    
    perfil_victima %>%
      filter(
        if(!is.null(input$perfil_periodo))                                                          fecha %in% input$perfil_periodo           else fecha != "",
        if(!is.null(input$perfil_fecha))                                                              Año %in% input$perfil_fecha             else Año != "",
        # if(!is.null(input$perfil_municipio))                                        `Municipio de suceso` %in% input$perfil_municipio         else `Municipio de suceso` != "",
        if(input$perfil_municipio=="Jalisco")                        `Municipio de suceso`!= ""            else `Municipio de suceso` %in% input$perfil_municipio,
        if(!is.null(input$perfil_rango))                                                  `Rango de edad` %in% input$perfil_rango             else `Rango de edad` != "",
        if(!is.null(input$perfil_causa))              `Posible causa de muerte (y/o mecanismo de muerte)` %in% input$perfil_causa             else `Posible causa de muerte (y/o mecanismo de muerte)` != "",
        if(!is.null(input$perfil_clasificacion))  `Clasificación de causa de muerte (Violenta o Natural)` %in% input$perfil_clasificacion     else `Clasificación de causa de muerte (Violenta o Natural)` != ""
        
      )
    
  })
  
  
  output$gr1 <-renderPlot ({
    
    if (input$value_total_g1 == "Con etiqueta de datos") {
      
      perfil_reactive() %>%
        
        filter(`Municipio de suceso`==perfil_reactive()$`Municipio de suceso`[1]) %>% 
        group_by(fecha, `Municipio de suceso`) %>% 
        summarise(Total=n()) %>% 
        ggplot() +
        aes(x = fecha, y = Total, group=1) +
        geom_line(fill = "#857baa", colour = "#857baa", size=2) +
        geom_point(fill = "#857baa", colour = "#857baa", size=8) +
        geom_text(aes(label=comma(Total)), size=3.5, vjust = 0.5, label.size = .1, colour="white")+
        scale_y_continuous(labels = scales::comma) +     
        labs(x="", y="", fill="", color="",
             title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de muertes de mujeres en", input$perfil_municipio, "}")),# perfil_reactive()$`Municipio de suceso`[1], "}")),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
             caption="\n\nElaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH.")+
        theme_minimal()+
        theme_1 +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->gr1
      
      
    }  else { 
      
      perfil_reactive() %>%
        # perfil_victima %>% 
        filter(`Municipio de suceso`==perfil_reactive()$`Municipio de suceso`[1]) %>% 
        group_by(fecha, `Municipio de suceso`) %>% 
        summarise(Total=n()) %>% 
        ggplot() +
        aes(x = fecha, y = Total, group=1) +
        geom_line(fill = "#857baa", colour = "#857baa", size=2) +
        geom_point(fill = "#857baa", colour = "#857baa", size=8) +
        # geom_text(aes(label=comma(Total)), size=2.5, vjust = 0.5, label.size = .1, colour="white")+
        scale_y_continuous(labels = scales::comma) +     
        labs(x="", y="", fill="", color="",
             title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de muertes de mujeres en", input$perfil_municipio, "}")),# perfil_reactive()$`Municipio de suceso`[1], "}")),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
             caption="\n\nElaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH.")+
        theme_minimal()+
        theme_1 +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->gr1
    }
    gr1
  })
  
  
  # MAPA IJCF ANUAL . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  
  output$mapa_ijcf <- renderPlot ({
    
    
    perfil_reactive() %>%
      # perfil_victima %>%
      group_by(`Municipio de suceso`) %>%
      summarise(Total=n()) %>%
      mutate(municipio_name=`Municipio de suceso`) %>%
      merge(df_mxmunicipio_2020, by = "municipio_name", all = T) %>%
      mutate(value=Total) %>%
      mutate(value = if_else(is.na(value), 0, value)) %>% 
      mxmunicipio_choropleth(num_colors = 1,
                             zoom = subset(., state_name %in% c("Jalisco"))$region,
                             show_states = FALSE, legend = "") +
      labs(caption=NULL, fill="Total", x=NULL, y=NULL,
           title = latex2exp::TeX(paste("Mapa: \\textbf{Total de mujeres víctimas de muertes}")),
           subtitle = paste0("Datos correspondientes a ", input$perfil_municipio, ", ", min(perfil_reactive()$fecha), " a ", max(perfil_reactive()$fecha))) +
      scale_fill_gradient(
        low = "#e9e8eb",
        high = "#857baa",
        guide = "colourbar",
        label=comma)+ theme_1
    
  })
  
  
  output$download_gr1 <- downloadHandler(
    filename = function() {
      paste0("IJCF_muertes_de_mujeres_", tolower(perfil_reactive()$`Municipio de suceso`[1]),".jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        perfil_reactive() %>%
          # perfil_victima %>% 
          group_by(fecha, `Municipio de suceso`) %>% 
          summarise(Total=n()) %>% 
          ggplot() +
          aes(x = fecha, y = Total, group=1) +
          geom_line(fill = "#857baa", colour = "#857baa", size=2) +
          geom_point(fill = "#857baa", colour = "#857baa", size=8) +
          geom_text(aes(label=comma(Total)), size=3.5, vjust = 0.5, label.size = .1, colour="white")+
          scale_y_continuous(labels = scales::comma) +     
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de mujeres víctimas de muertes registradas en el IJCF:", perfil_reactive()$`Municipio de suceso`[1], "}")),
               subtitle = paste("Datos correspondientes a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
               caption="\n\nElaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH.")+
          theme_minimal()+
          theme_1 +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      ) 
      dev.off()
    })
  
  
  output$download_d1<- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(perfil_victima, file, row.names = F)
    }
  )
  
  # Tabla IJCF -------------------------------------------------------------
  output$t1 <- renderDataTable ({
    # width <- session$clientData$output_plot_responsive_width
    # height <- session$clientData$output_plot_responsive_height
    
    perfil_reactive() %>%
      # perfil_victima %>% 
      group_by(Año, `Municipio de suceso`) %>% 
      summarise(total=n()) %>%
      select(Año, `Municipio de suceso`, total) %>% 
      mutate(total = if_else(is.na(total), 0, total),
             Año  = as.numeric(Año)) %>% 
      datatable(
        caption = htmltools::tags$caption(paste0("Total de mujeres víctimas de muertes registradas en el IJCF:", 
                                                 # "\n Valores correspondiente a",tolower(perfil_reactive()$`Municipio de suceso`[1]), "del tota de muertes ", perfil_reactive()$`Clasificación de causa de muerte (Violenta o Natural)`[1], 
                                                 "."), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c('Año','Municipio','Total'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, "All"),
                                         c(5,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
  })
  
  
  ################################################################################
  #                           S E R V I C I O S
  ################################################################################  
  
  # IJCF PERFIL: -----------------------------------------------------------------  
  
  output$servicio_fecha <- renderUI({
    selectInput("servicio_fecha",
                label =  "Seleccione el año",
                choices = sort(unique(servicios$Año)),
                multiple = T)
  })
  
  
  output$servicio_municipio<- renderUI({
    selectInput("servicio_municipio",
                label =  "Seleccione el municipio",
                choices = sort(unique(servicios$`Municipio de suceso`)),
                multiple = T)
  })
  
  
  output$servicio_rango <- renderUI({
    selectInput("servicio_rango",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(servicios$`Rango de edad`)),
                multiple = T)
  })
  
  output$servicio_causa <- renderUI({
    selectInput("servicio_causa",
                label =  "Selecciona la causa de muerte",
                choices = sort(unique(servicios$`Posible causa de muerte (y/o mecanismo de muerte)`)),
                multiple = T)
  })
  
  output$servicio_clasificacion <- renderUI({
    selectInput("servicio_clasificacion",
                label =  "Selecciona la clasificación de la muerte",
                choices = sort(unique(servicios$`Clasificación de causa de muerte (Violenta o Natural)`)),
                multiple = T)
  })
  
  output$servicio_periodo <- renderUI({
    selectInput("servicio_periodo",
                label =  "Selecciona la fecha",
                choices = sort(unique(servicios$fecha)),
                multiple = T)
  })
  
  output$servicio_servicios <- renderUI({
    selectInput("servicio_servicios",
                label =  "Seleccione los servicios",
                choices = sort(unique(servicios$Servicios)),
                multiple = T)
  })
  
  output$servicio_total <- renderUI({
    selectInput("servicio_total",
                label =  "Seleccione el total",
                choices = sort(unique(servicios$Total)),
                multiple = T)
  })
  
  output$servicio_id <- renderUI({
    selectInput("servicio_id",
                label =  "Seleccione el id",
                choices = sort(unique(servicios$id)),
                multiple = T)
  })
  
  
  servicio_reactive <- reactive({
    
    servicios %>%
      filter(
        if(!is.null(input$servicio_periodo))                                                          fecha %in% input$servicio_periodo           else fecha != "",
        if(!is.null(input$servicio_fecha))                                                              Año %in% input$servicio_fecha             else Año != "",
        # if(!is.null(input$perfil_municipio))                                        `Municipio de suceso` %in% input$perfil_municipio         else `Municipio de suceso` != "",
        if(input$servicio_municipio=="Jalisco")                        `Municipio de suceso`!= ""            else `Municipio de suceso` %in% input$servicio_municipio,
        if(!is.null(input$servicio_rango))                                                  `Rango de edad` %in% input$servicio_rango             else `Rango de edad` != "",
        if(!is.null(input$servicio_causa))              `Posible causa de muerte (y/o mecanismo de muerte)` %in% input$servicio_causa             else `Posible causa de muerte (y/o mecanismo de muerte)` != "",
        if(!is.null(input$servicio_clasificacion))  `Clasificación de causa de muerte (Violenta o Natural)` %in% input$servicio_clasificacion     else `Clasificación de causa de muerte (Violenta o Natural)` != "",
        if(!is.null(input$servicio_servicios))                                                    Servicios %in% input$servicio_servicios           else Servicios != ""
        
        
        
      )
    
  })
  
  
  
  
  output$gr2 <-renderPlot ({
    
    if (input$value_total_g2 == "Con etiqueta de datos") {
      
      servicio_reactive() %>% 
        mutate(Total=as.numeric(Total),
               Total = if_else(is.na(Total), 0, Total)) %>% 
        group_by(Año, Servicios) %>% 
        summarise(victimas = n_distinct(id, na.rm=T),
                  total_servicios=sum(Total)) %>% 
        ggplot() +
        aes(x = Año, y = total_servicios, group=Servicios, fill = Servicios, color=Servicios) +
        geom_col() +
        geom_line(aes(x=Año, y=victimas, group = Servicios), colour = "purple")+
        geom_label(aes(label=total_servicios), vjust = 0.5, label.size = .1, color="white",size = 4)+
        facet_wrap(~str_wrap(Servicios, width = 40), ncol = 2)+
        scale_y_continuous(labels = scales::comma) +     
        scale_fill_manual(values = mycolors129) +
        scale_color_manual(values = mycolors129)+
        labs(x="", y="", fill=NULL, color=NULL,
             title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de servicios forenses en muertes de mujeres en", input$servicio_municipio, "}")),# perfil_reactive()$`Municipio de suceso`[1], "}")),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(servicio_reactive()$fecha), "a ", max(servicio_reactive()$fecha)),
             caption="\n\nLa línea morada representa al total de víctimas mujeres de muertes violentas
           Elaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH.")+
        theme_minimal()+
        theme_1 + theme(legend.position = "none",
                        strip.text = element_text(face="bold", size=11,lineheight=0.8),
                        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))->gr2
      
      
    }  else { 
      
      servicio_reactive() %>% 
        mutate(Total=as.numeric(Total),
               Total = if_else(is.na(Total), 0, Total)) %>% 
        group_by(Año, Servicios) %>% 
        summarise(victimas = n_distinct(id, na.rm=T),
                  total_servicios=sum(Total)) %>% 
        ggplot() +
        aes(x = Año, y = total_servicios, 
            group=Servicios, fill = Servicios, color=Servicios) +
        # geom_col() +aes(x = as.factor(Año), y = total_servicios, group=Servicios, fill = Servicios, color=Servicios) +
        geom_col() +
        geom_line(aes(x=Año, y=victimas, group = Servicios), colour = "purple")+
        # geom_label(aes(label=total_servicios), vjust = 0.5, label.size = .1, color="white",size = 4)+
        facet_wrap(~str_wrap(Servicios, width = 40), ncol = 2)+
        scale_y_continuous(labels = scales::comma) +     
        scale_fill_manual(values = mycolors129) +
        scale_color_manual(values = mycolors129)+
        labs(x="", y="", fill=NULL, color=NULL,
             title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de servicios forenses en muertes de mujeres en", input$servicio_municipio, "}")),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(servicio_reactive()$fecha), "a ", max(servicio_reactive()$fecha)),
             caption="\n\nLa línea morada representa al total de víctimas mujeres de muertes violentas.
             Elaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH.")+
        theme_minimal()+
        theme_1 + theme(legend.position = "none",
                        strip.text = element_text(face="bold", size=11,lineheight=0.8),
                        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))->gr2
    }
    gr2
  })
  
  # Tabla IJCF -------------------------------------------------------------
  output$t2 <- renderDataTable ({
    # width <- session$clientData$output_plot_responsive_width
    # height <- session$clientData$output_plot_responsive_height
    
    servicio_reactive() %>%
      # servicios %>% 
      mutate(Total=as.numeric(Total),
             Total = if_else(is.na(Total), 0, Total)) %>% 
      group_by(Año, , Servicios) %>% 
      summarise(victimas = n_distinct(id, na.rm=T),
                total_servicios=sum(Total)) %>% 
      datatable(
        caption = htmltools::tags$caption(paste0("Total de mujeres víctimas de muertes registradas en el IJCF y los servicios aplicados:", 
                                                 # "\n Valores correspondiente a",tolower(servicios_reactive()$`Municipio de suceso`[1]), "del tota de muertes ", perfil_reactive()$`Clasificación de causa de muerte (Violenta o Natural)`[1], 
                                                 "."), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c('Año','Servicios','Víctimas','Total de servicios'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, "All"),
                                         c(5,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:4)))) %>%  
      formatStyle(
        columns = c(1:4),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })
  
  
  output$download_gr2 <- downloadHandler(
    filename = function() {
      paste0("IJCF_servicios_a_muertes_de_mujeres_", tolower(input$servicio_municipio),".jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        servicio_reactive() %>% 
          mutate(Total=as.numeric(Total),
                 Total = if_else(is.na(Total), 0, Total)) %>% 
          group_by(Año, Servicios) %>% 
          summarise(victimas = n_distinct(id, na.rm=T),
                    total_servicios=sum(Total)) %>% 
          ggplot() +
          aes(x = Año, y = total_servicios, group=Servicios, fill = Servicios, color=Servicios) +
          geom_col() +
          geom_line(aes(x=Año, y=victimas, group = Servicios), colour = "purple")+
          geom_label(aes(label=total_servicios), vjust = 0.5, label.size = .1, color="white",size = 4)+
          facet_wrap(~str_wrap(Servicios, width = 40), ncol = 2)+
          scale_y_continuous(labels = scales::comma) +     
          scale_fill_manual(values = mycolors129) +
          scale_color_manual(values = mycolors129)+
          labs(x="", y="", fill=NULL, color=NULL,
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de servicios forenses en muertes de mujeres en", input$servicio_municipio, "}")),# perfil_reactive()$`Municipio de suceso`[1], "}")),
               subtitle = paste("Datos correspondientes a la fecha del período:", min(servicio_reactive()$fecha), "a ", max(servicio_reactive()$fecha)),
               caption="\n\nLa línea morada representa al total de víctimas mujeres de muertes violentas
           Elaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH.")+
          theme_minimal()+
          theme_1 + theme(legend.position = "none",
                          strip.text = element_text(face="bold", size=11,lineheight=0.8),
                          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
      ) 
      dev.off()
    })
  
  
  output$download_d2<- downloadHandler(
    filename = function() {paste0(input$dataset, "servicios_periciales.xlsx", sep="")},
    content = function(file) {openxlsx::write.xlsx(servicios, file, row.names = F)}
  )
  
  
  # # Indicador 16: -----------------------------------------------------------------  
  
  output$ind_16_año <- renderUI({
    selectInput("ind_16_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_16$Año)),
                multiple = T)
  })
  
  output$ind_16_mes<- renderUI({
    selectInput("ind_16_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_16$Mes)),
                multiple = T)
  })
  
  
  output$ind_16_edad <- renderUI({
    selectInput("ind_16_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_16$`Rango de edad`)),
                multiple = T)
  })
  
  
  ind_16_reactive <- reactive({
    
    indicador_16 %>%
      filter(
        if(!is.null(input$ind_16_año))               Año %in% input$ind_16_año      else Año != "",
        if(!is.null(input$ind_16_mes))               Mes %in% input$ind_16_mes      else Mes != "",
        if(!is.null(input$ind_16_edad))  `Rango de edad` %in% input$ind_16_edad    else `Rango de edad` != ""
      )
    
  })
  
  
  
  output$gr16 <-renderPlotly ({
    
    ind_16_reactive() %>%
      # indicador_16 %>%
      filter(!is.na(`Rango de edad`)) %>% 
      group_by(Año,# Mes, Periodo, 
               `Rango de edad`) %>%
      summarise(`Total de NA que denuncian ASI`=sum(`Total de mujeres que denuncian abuso sexual infantil`),
                
                `Total de NA canalizadas a Salud para atención integral por ASI`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                
                `Total de mujeres que denuncian violación`=sum(`Total de mujeres que denuncian violación`),
                
                `Total de mujeres canalizadas a salud para atención integral por violación`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por violación`)) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de NA que denuncian ASI",
                          "Total de mujeres que denuncian violación",
                          "Total de NA canalizadas a Salud para atención integral por ASI",
                          "Total de mujeres canalizadas a salud para atención integral por violación")) %>%
      
      mutate(text = paste("Año: ", Año,
                          # "\nMes: ", Mes,
                          "\nTotal : ", scales::comma(Total), sep="")) %>%
      filter(!Total==0) %>%
      ggplot() +
      aes(x = Año, y =`Rango de edad`, size=Total,
          fill = Total, group=Clasificación, text=text) +
      # geom_line(size = 1.5) + geom_point()+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      labs(x="", y="", title = "Indicador 16:",
           color = "Total", fill= "Total", size=NULL) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=2, color="ghostwhite")+
      scale_size_continuous(range = c(3,15))+
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      # guides(size = FALSE)+
      
      facet_wrap(~ Clasificación, ncol = 2,
                 labeller = label_wrap_gen(width = 40, multi_line = TRUE)) +
      theme_minimal()+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light",  hjust=.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr16
    
    
    
    ggplotly(gr16, tooltip = "text") %>%
      layout(#title = list(text = paste0(" Indicador 16: "#,ind_16_reactive()$`Rango de edad`
        # )),
        legend = list(orientation = 'v',  x = 0, y = -1),
        xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
    #
    # ggplotly(gr16, tooltip = "text") %>%
    #   layout(title = "Indicador 16",
    #          #legend = list(orientation = 'v', x = 0, y = -.1),
    #
    #          legend = list(orientation = "h", x = 0, y=-.50, font = list(size = 10, bgcolor = 'rgb(251)')),
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_16 <- renderDataTable ({
    
    ind_16_reactive() %>%
      # indicador_16 %>%
      rename(`Total de niñas que denuncian abuso sexual infantil`=`Total de mujeres que denuncian abuso sexual infantil`, 
             `Total de adolescentes canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`=`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`) %>% 
      group_by(Año) %>%
      summarise(`Total de NA que denuncian ASI`=sum(round(`Total de niñas que denuncian abuso sexual infantil`, digits = 2)),
                
                `Total de NA canalizadas a Salud para atención integral por ASI`=sum(`Total de adolescentes canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                
                `Total de mujeres que denuncian violación`=sum(`Total de mujeres que denuncian violación`),
                
                `Total de mujeres canalizadas a salud para atención integral por violación`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por violación`),
                
                `% Abuso sexual infantil`=scales::percent(sum((`Total de NA canalizadas a Salud para atención integral por ASI`)/(`Total de NA que denuncian ASI`)), 0.1),
                `% Violación`=scales::percent(sum((`Total de mujeres canalizadas a salud para atención integral por violación`)/(`Total de mujeres que denuncian violación`)), 0.1)) -> tabla_16
    
    
    tabla_16 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo")),
                                           headerCallback = DT::JS(
                                             "function(thead) {",
                                             "  $(thead).css('font-size', '0.8em');",
                                             "}"
                                           ))) %>% 
      formatCurrency(c(2:4),currency = "", interval = 3, mark = ",", digits = 0) %>% 
      DT::formatStyle(columns = colnames(.), fontSize = '70%')
    
  })
  
  
  
  # Indicador 17: -----------------------------------------------------------------
  
  output$ind_17_año <- renderUI({
    selectInput("ind_17_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_17$Año)),
                multiple = T)
  })
  
  output$ind_17_mes<- renderUI({
    selectInput("ind_17_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_17$Month)),
                multiple = T)
  })
  
  
  output$ind_17_edad <- renderUI({
    selectInput("ind_17_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_17$Rango)),
                multiple = T)
  })
  
  
  ind_17_reactive <- reactive({
    
    indicador_17 %>%
      filter(
        if(!is.null(input$ind_17_año))     Año %in% input$ind_17_año    else Año != "",
        if(!is.null(input$ind_17_mes))     Mes %in% input$ind_17_mes    else Mes != "",
        if(!is.null(input$ind_17_edad))  Rango %in% input$ind_17_edad   else Rango != ""
      )
    
  })
  
  ind_16_reactive <- reactive({
    
    indicador_16 %>%
      filter(
        if(!is.null(input$ind_16_año))               Año %in% input$ind_16_año      else Año != "",
        if(!is.null(input$ind_16_mes))               Mes %in% input$ind_16_mes      else Mes != "",
        if(!is.null(input$ind_16_edad))  `Rango de edad` %in% input$ind_16_edad    else `Rango de edad` != ""
      )
    
  })
  
  output$gr17 <-renderPlot ({
    
    ind_17_reactive() %>%
      # indicador_17 %>% 
      filter(!is.na(Rango)) %>% 
      group_by(Año,# Mes, Periodo, 
               `Tipo: (abuso sexual infantil / violación)`, Rango) %>%
      summarise(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`= n()) %>%
      mutate(text = paste("Año: ", Año,
                          # "\nMes: ",  Mes,
                          "\nRango de edad: ", Rango,
                          "\nTotal: ", scales::comma(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`), sep=""))%>%
      ggplot() +
      aes(x = Año, y =Rango, text=text,
          size =`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`,
          colour = `Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`)+
      geom_point(mapping=aes(colour=`Tipo: (abuso sexual infantil / violación)`, group=`Tipo: (abuso sexual infantil / violación)`))+
      geom_text(aes(label=comma(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=5, color="ghostwhite")+
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de atenciones por violación y abuso sexual infantil en el Sector Salud}")),
           subtitle = paste("Datos correspondientes al total de mujeres y niñas atendidas que son referidas a la Fiscalía del Estado"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
      
      scale_size_continuous(range = c(8,17))+
      scale_color_manual(
        values = c(
          `Abuso sexual infantil` = "#7e3794",
          `Violación` = "#c91682"))+
      guides(size = FALSE)+
      theme_minimal()+ theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) ->gr17
    gr17
    
  })
  
  
  output$download_gr17 <- downloadHandler(
    filename = function() {
      paste0("Salud_mujeres_atendidas_violación_abusosexual", tolower(ind_17_reactive()$Año[1]),".jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_17_reactive() %>%
          # indicador_17 %>% 
          filter(!is.na(Rango)) %>% 
          group_by(Año,# Mes, Periodo, 
                   `Tipo: (abuso sexual infantil / violación)`, Rango) %>%
          summarise(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`= n()) %>%
          mutate(text = paste("Año: ", Año,
                              # "\nMes: ",  Mes,
                              "\nRango de edad: ", Rango,
                              "\nTotal: ", scales::comma(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`), sep=""))%>%
          ggplot() +
          aes(x = Año, y =Rango, text=text,
              size =`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`,
              colour = `Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`)+
          geom_point(mapping=aes(colour=`Tipo: (abuso sexual infantil / violación)`, group=`Tipo: (abuso sexual infantil / violación)`))+
          geom_text(aes(label=comma(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=3, color="ghostwhite")+
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de atenciones por violación y abuso sexual infantil en el Sector Salud}")),
               subtitle = paste("Datos correspondientes al total de mujeres y niñas atendidas que son referidas a la Fiscalía del Estado"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
          
          scale_size_continuous(range = c(8,17))+
          scale_size_continuous(range = c(8,17))+
          scale_color_manual(
            values = c(
              `Abuso sexual infantil` = "#7e3794",
              `Violación` = "#c91682"))+
          guides(size = FALSE)+
          theme_minimal()+ theme_1+
          theme(legend.position = "bottom")+
          theme(text=element_text(size=10, family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      ) 
      dev.off()
    })
  
  
  output$download_d17<- downloadHandler(
    filename = function() {
      paste0(input$dataset, "salud_atenciones_violacion_abusosexual.xlsx", 
             sep="")
    },
    content = function(file) {
      openxlsx::write.xlsx(ind_17_reactive(), file,row.names=F)
    }
  )
  
  output$t_17 <- DT::renderDataTable ({
    
    ind_16_reactive <- reactive({
      
      indicador_16 %>%
        filter(
          if(!is.null(input$ind_16_año))               Año %in% input$ind_16_año      else Año != "",
          # if(!is.null(input$ind_16_mes))               Mes %in% input$ind_16_mes      else Mes != "",
          if(!is.null(input$ind_16_edad))  `Rango de edad` %in% input$ind_16_edad    else `Rango de edad` != ""
        )
      
    })
    
    indicador_16 %>%
    # ind_16_reactive() %>%
      filter(Año %in% c(2019,2020,2021,2022,2023)) %>% 
      group_by(Año) %>%
      summarise(`Referidas por abuso sexual`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                `Referidas por violación`=sum(`Total de mujeres que denuncian violación`))-> total_referida
    
    
    
    
    ind_17_reactive() %>%
      # indicador_17 %>%
      filter(!Año== 2019, !Año== 2024,
             `Tipo: (abuso sexual infantil / violación)` %in% c("Violación", "Abuso sexual infantil")) %>% 
      group_by(Año,`Tipo: (abuso sexual infantil / violación)`) %>%
      summarise(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`= n()) %>% 
      pivot_wider(names_from = "Tipo: (abuso sexual infantil / violación)", 
                  values_from = "Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud") %>% 
      cbind(total_referida) %>% 
      select(1:3,5:6) %>% 
      mutate(Año=`Año...1`,
             `% de atención por abuso sexual`= scales::percent(`Abuso sexual infantil`/`Referidas por abuso sexual`),
             `% de atención por violación`= scales::percent(`Violación`/`Referidas por violación`)) %>% 
      select(Año,`Referidas por abuso sexual`,`Referidas por violación`, `Abuso sexual infantil`,`Violación`, `% de atención por abuso sexual`,`% de atención por violación`)->tabla_17
    
    
    
    
    tabla_17%>%  
      # datatable(filter="top", extensions = 'Buttons',
      #                      options = list(dom = 'Blfrtip',
      #                                     buttons = c('copy', 'excel', 'print'),
      #                                     lengthMenu = list(c(6,10,20, -1),
      #                                                       c(6,10,20,"Todo")),
      #                                     headerCallback = DT::JS(
      #                                       "function(thead) {",
      #                                       "  $(thead).css('font-size', '0.8em');",
      #                                       "}"
      #                                     ))) %>% 
      # formatCurrency(c(2:4),currency = "", interval = 3, mark = ",", digits = 0) %>% 
      # DT::formatStyle(columns = colnames(.), fontSize = '90%')
      
      datatable(
        caption = htmltools::tags$caption(paste0("Total de atenciones por violación y abuso sexual"), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c("Año","Referidas por abuso sexual","Referidas por violación", "Abuso sexual infantil","Violación", "% de atención por abuso sexual","% de atención por violación"),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,5,10,20, "All"),
                                         c(4,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:7)))) %>%  
      formatStyle(
        columns = c(2:7),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })
  
  
  # Indicador 18: -----------------------------------------------------------------
  
  output$ind_18_año <- renderUI({
    selectInput("ind_18_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_18$Año)),
                multiple = T)
  })
  
  output$ind_18_mes<- renderUI({
    selectInput("ind_18_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_18$Mes)),
                multiple = T)
  })
  
  
  output$ind_18_edad <- renderUI({
    selectInput("ind_18_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_18$Rango)),
                multiple = T)
  })
  
  
  ind_18_reactive <- reactive({
    
    indicador_18 %>%
      filter(
        if(!is.null(input$ind_18_año))     Año %in% input$ind_18_año    else Año != "",
        if(!is.null(input$ind_18_mes))     Mes %in% input$ind_18_mes    else Mes != "",
        if(!is.null(input$ind_18_edad))  Rango %in% input$ind_18_edad   else Rango != ""
      )
    
  })
  
  
  
  output$gr18 <-renderPlot ({
    
    ind_18_reactive() %>%
      # indicador_18 %>%
      filter(!is.na(Año)) %>%
      group_by(Año, Rango, `Causal: (violacion/ salud/ riesgo)`) %>%
      summarise(`Total de mujeres víctimas de violación que recibieron el procedimiento`= n()) %>%
      mutate(text = paste("Año: ", Año,
                          "\nRango de edad: ", Rango,
                          "\nTotal: ", scales::comma(`Total de mujeres víctimas de violación que recibieron el procedimiento`), sep="")) %>%
      ggplot() +
      aes(x = as.factor(Año), y =Rango,
          size =`Total de mujeres víctimas de violación que recibieron el procedimiento`,
          colour = `Causal: (violacion/ salud/ riesgo)`, text=text) +
      geom_point(mapping=aes(colour=`Total de mujeres víctimas de violación que recibieron el procedimiento`, group=`Causal: (violacion/ salud/ riesgo)`))+
      #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
      geom_text(aes(label=comma(`Total de mujeres víctimas de violación que recibieron el procedimiento`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=4, color="ghostwhite")+
      scale_size_continuous(range = c(7,15))+
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      scale_colour_gradient(low = "#dba9c8", high = "#7e3794") +
      facet_wrap(~`Causal: (violacion/ salud/ riesgo)`, ncol = 1) +
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{Total mujeres solicitantes de IVE por violación que reciben el procedimiento.}")),
           subtitle = paste("Datos correspondientes al total de mujeres que solicitan el procedimiento de manera directa"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
      guides(size=F)+
      theme_minimal()+
      theme_1+
      theme(legend.position = "right")+
      theme(text=element_text(size=10, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr18
    
    gr18
    
  })
  
  output$download_gr18 <- downloadHandler(
    filename = function() {
      paste0("Salud_total_solicitantes_violacion.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_18_reactive() %>%
          # indicador_18 %>%
          filter(!is.na(Año)) %>%
          group_by(Año, Rango, `Causal: (violacion/ salud/ riesgo)`) %>%
          summarise(`Total de mujeres víctimas de violación que recibieron el procedimiento`= n()) %>%
          mutate(text = paste("Año: ", Año,
                              "\nRango de edad: ", Rango,
                              "\nTotal: ", scales::comma(`Total de mujeres víctimas de violación que recibieron el procedimiento`), sep="")) %>%
          ggplot() +
          aes(x = as.factor(Año), y =Rango,
              size =`Total de mujeres víctimas de violación que recibieron el procedimiento`,
              colour = `Causal: (violacion/ salud/ riesgo)`, text=text) +
          geom_point(mapping=aes(colour=`Total de mujeres víctimas de violación que recibieron el procedimiento`, group=`Causal: (violacion/ salud/ riesgo)`))+
          theme(panel.grid.major = element_line(colour = "grey"))+
          #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
          geom_text(aes(label=comma(`Total de mujeres víctimas de violación que recibieron el procedimiento`, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=4, color="ghostwhite")+
          scale_size_continuous(range = c(7,15))+
          scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
          scale_colour_gradient(low = "#dba9c8", high = "#7e3794") +
          facet_wrap(~`Causal: (violacion/ salud/ riesgo)`, ncol = 1) +
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Total mujeres solicitantes de IVE por violación que reciben el procedimiento}")),
               subtitle = paste("Datos correspondientes al total de mujeres que solicitan el procedimiento de manera directa"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
          guides(size=F)+
          theme_minimal()+
          theme_1+
          theme(legend.position = "right")+
          theme(text=element_text(size=10, family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      )
      dev.off()
    })
  
  
  
  
  
  output$t_18 <- DT::renderDataTable ({
   
  
    ind_18_reactive() %>%
    # tabla_18 %>%  
      datatable(
      caption = htmltools::tags$caption("Total mujeres solicitantes de IVE por violación que reciben el procedimiento", 
                                        style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold'),
    # colnames = c('Año', "Total de mujeres víctimas de violación que recibieron el procedimiento", "Total de canalización por Fiscalía","Total de solicitudes directas"),
    extensions = 'Buttons',
    options = list(dom = 'Blfrtip',
                   buttons = c('copy', 'excel', 'print'),
                   language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                   initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                   dom = "tip",
                   buttons = c('copy', 'excel', 'print'),
                   lengthMenu = list(c(3,1,3,10,20, "All"),
                                     c(3,1,3,10,20, "All")),
                   columnDefs = list(list(className = 'dt-center', targets = 1:10)))) %>%  
    formatStyle(
      columns = c(1:10),
      fontFamily = "Nutmeg-Light",
      fontSize = "11px",
      fontWeight = 'plain',
      borderRightWidth = "1px",
      borderRightStyle = "solid",
      borderRightColor = "white",
      borderBottomColor = "#ffffff",
      borderBottomStyle = "solid",
      borderBottomWidth = "0.5px",
      verticalAlign = "middle",
      textAlign = "center",
      wordWrap = "break-word")
  

  })
  
  
  # Indicador 19: -----------------------------------------------------------------
  
  output$ind_19_año <- renderUI({
    selectInput("ind_19_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_19$Año)),
                multiple = T)
  })
  
  # output$ind_19_mes<- renderUI({
  #   selectInput("ind_19_mes",
  #               label =  "Seleccione el mes",
  #               choices = sort(unique(indicador_19$Mes)),
  #               multiple = T)
  # })
  
  output$ind_19_edad <- renderUI({
    selectInput("ind_19_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_19$Rango)),
                multiple = T)
  })
  output$ind_19_causal <- renderUI({
    selectInput("ind_19_causal",
                label =  "Selecciona la causal",
                choices = sort(unique(indicador_19$`Causal: (salud/riesgo)`)),
                multiple = T)
  })
  
  
  ind_19_reactive <- reactive({
    
    indicador_19 %>%
      filter(
        if(!is.null(input$ind_19_año))                           Año %in% input$ind_19_año     else Año != "",
        # if(!is.null(input$ind_19_mes))                           Mes %in% input$ind_19_mes     else Mes != "",
        if(!is.null(input$ind_19_causal))   `Causal: (salud/riesgo)` %in% input$ind_19_causal  else `Causal: (salud/riesgo)` != "",
        if(!is.null(input$ind_19_edad))                        Rango %in% input$ind_19_edad    else Rango != ""
      )
    
  })
  
  
  
  output$gr19 <-renderPlot ({
    
    ind_19_reactive() %>%
      # indicador_19 %>%
      filter(!is.na(Año),
             !is.na(Rango),
             Año >= 2021,
             !is.na(`Causal: (salud/riesgo)`)) %>%
      group_by(Año, Rango, `Causal: (salud/riesgo)`) %>%
      summarise(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`= n()) %>%
      mutate(text = paste("Año: ", Año,
                          # "\nMes: ", Mes,
                          "\nCausal: ", `Causal: (salud/riesgo)`,
                          "\nRango de edad: ", Rango,
                          "\nTotal: ", scales::comma(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`), sep="")) %>% 
      ggplot() +
      aes(x =as.factor(Año) , y = Rango, text=text,
          fill = `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
          colour = `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
          size=`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`) +
      geom_point()+
      geom_text(aes(label=comma(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite", angle=0)+
      facet_wrap(~`Causal: (salud/riesgo)`) +
      scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
      scale_color_gradient(low = "#dba9c8", high = "#7e3794") +
      scale_size_continuous(range = c(7,15))+
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{Total mujeres que recibieron el procedimiento de ILE}")),
           subtitle = paste("Datos correspondientes al total de mujeres que reciben el procedimiento de interrupción legal del embarazo \nconforme al Programa Estatal para la Interrupción Legal del Embarazo (Programa ILE) en los Servicios de Salud del Estado de Jalisco"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
      guides(size=F)+
      theme_minimal()+ theme_1+
      theme(legend.position = "right")+
      theme(strip.text = element_text(face="bold", size=9,lineheight=1.0))->gr19
    
    gr19
    
    
  })
  
  
  output$download_gr19 <- downloadHandler(
    filename = function() {
      paste0("SALUD_procedimientos_ile.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        
        ind_19_reactive() %>%
          # indicador_19 %>%
          filter(!is.na(Año),
                 !is.na(Rango),
                 Año >= 2021,
                 !is.na(`Causal: (salud/riesgo)`)) %>%
          group_by(Año, Rango, `Causal: (salud/riesgo)`) %>%
          summarise(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`= n()) %>%
          mutate(text = paste("Año: ", Año,
                              # "\nMes: ", Mes,
                              "\nCausal: ", `Causal: (salud/riesgo)`,
                              "\nRango de edad: ", Rango,
                              "\nTotal: ", scales::comma(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`), sep="")) %>% 
          ggplot() +
          aes(x =as.factor(Año) , y = Rango, text=text,
              fill = `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
              colour = `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`,
              size=`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`) +
          geom_point()+
          geom_text(aes(label=comma(`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=3, color="ghostwhite", angle=0)+
          facet_wrap(~`Causal: (salud/riesgo)`) +
          scale_fill_gradient(low = "#dba9c8", high = "#7e3794") +
          scale_color_gradient(low = "#dba9c8", high = "#7e3794") +
          scale_size_continuous(range = c(7,15))+
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Total mujeres que recibieron el procedimiento de ILE}")),
               subtitle = paste("Datos correspondientes al total de mujeres que reciben el procedimiento de interrupción legal del embarazo \nconforme al Programa Estatal para la Interrupción Legal del Embarazo (Programa ILE) en los Servicios de Salud del Estado de Jalisco"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
          guides(size=F)+
          theme_minimal()+ theme_1+
          theme(legend.position = "right")+
          theme(strip.text = element_text(face="bold", size=9,lineheight=1.0))
      )
      dev.off()
    })
  
  output$t_19 <- DT::renderDataTable ({
    
    ind_19_reactive() %>%
      # indicador_19 %>%
      group_by(Año) %>%
      summarise(`Total de mujeres que solicitaron el procedimiento de interrupción legal del embarazo`= n(),
                `Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`= sum(`¿Se realizó el procedimiento? (sí/no)`),
                # `Indicador`=scales::percent((`Total de mujeres que de conformidad con las causales legales recibieron el procedimiento de ile`)/(`Total de mujeres que solicitaron el procedimiento de interrupción legal del embarazo`), 0.1)
      )->tabla_19
    
    
    tabla_19 %>% 
      # datatable(filter="top", extensions = 'Buttons',
      #                      options = list(dom = 'Blfrtip',
      #                                     buttons = c('copy', 'excel', 'print'),
      #                                     lengthMenu = list(c(6,10,20, -1),
      #                                                       c(6,10,20,"Todo"))))
      datatable(
        caption = htmltools::tags$caption("Total mujeres solicitantes de IVE por violación que reciben el procedimiento", 
                                          style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold'),
        # colnames = c('Año', "Total de mujeres víctimas de violación que recibieron el procedimiento", "Total de canalización por Fiscalía","Total de solicitudes directas"),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(3,1,3,10,20, "All"),
                                         c(3,1,3,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
    
  })
  
  # Indicador 20: -----------------------------------------------------------------  
  
  output$ind_20_año <- renderUI({
    selectInput("ind_20_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_20$Año)),
                multiple = T)
  })
  
  output$ind_20_tipo<- renderUI({
    selectInput("ind_20_tipo",
                label =  "Seleccione el tipo de violencia",
                choices = sort(unique(indicador_20$`Tipo de violencia: (violencia familiar / sexual)`)),
                multiple = T)
  })
  
  output$ind_20_modalidad <- renderUI({
    selectInput("ind_20_modalidad",
                label =  "Selecciona la modalidad de violencia",
                choices = sort(unique(indicador_20$`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)),
                multiple = T)
  })
  
  
  ind_20_reactive <- reactive({
    
    indicador_20 %>%
      filter(
        if(!is.null(input$ind_20_año))               Año %in% input$ind_20_año      else Año != "",
        if(!is.null(input$ind_20_tipo))       `Tipo de violencia: (violencia familiar / sexual)` %in% input$ind_20_tipo     else `Tipo de violencia: (violencia familiar / sexual)` != "",
        if(!is.null(input$ind_20_modalidad))  `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)` %in% input$ind_20_modalidad  else`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)` != ""
      )
    
  })
  
  
  
  output$gr20 <-renderPlot ({
    
    ind_20_reactive() %>%
      # indicador_20 %>%
      filter(!is.na(`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)) %>%
      group_by(Año, `Tipo de violencia: (violencia familiar / sexual)`, 
               `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`
      ) %>% 
      summarise(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`= sum(`Notificadas al mp: (si/no)`, na.rm = T)) %>%  
      
      
      filter(!`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`==0) %>% 
      mutate(text = paste("Año: ", Año,
                          #"Periodo: ",  format(as_date(PERIODO), "%B de %Y"),
                          "\nModalidad de violencia: ", `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`,
                          "\nTipo de violencia: ", `Tipo de violencia: (violencia familiar / sexual)`,
                          "\nTotal: ", scales::comma(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`), sep=""))%>%
      ggplot() +
      aes(x = as.factor(Año), y = `Tipo de violencia: (violencia familiar / sexual)`,
          # size =`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`,
          fill = `Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`, text=text) +
      geom_tile()+
      geom_text(aes(label=comma(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite")+
      labs(x="",
           title = latex2exp::TeX("Gráfico: \\textbf{Total de casos atendidos en el Sector Salud por violencia familiar y/o sexual}"),
           subtitle = paste("Datos correspondientes al total decasos atendidos en el Sector Salud por violencia familiar y/o sexual que se notifican al Ministerio Público de Fiscalía"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
      scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 17)) + 
      scale_fill_gradient(low = "#8a84a3", high = "#7e3794", 
                          label=comma, name=" ") +
      facet_wrap(~ `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`, ncol = 5, 
                 scales = "free_x",
                 labeller = label_wrap_gen(width = 5, multi_line = TRUE)) +
      theme_minimal()+theme_1+
      theme(legend.position = "right",
            strip.text = element_text(face="bold", size=11,lineheight=1)) -> gr20
    gr20
    
  })
  
  
  output$download_gr20 <- downloadHandler(
    filename = function() {
      paste0("SALUD_casos_atendidos_violencia_familiar_sexual.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_20_reactive() %>%
          # indicador_20 %>%
          filter(!is.na(`Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`)) %>%
          group_by(Año, `Tipo de violencia: (violencia familiar / sexual)`, 
                   `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`
          ) %>% 
          summarise(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`= sum(`Notificadas al mp: (si/no)`, na.rm = T)) %>%  
          
          
          filter(!`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`==0) %>% 
          mutate(text = paste("Año: ", Año,
                              #"Periodo: ",  format(as_date(PERIODO), "%B de %Y"),
                              "\nModalidad de violencia: ", `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`,
                              "\nTipo de violencia: ", `Tipo de violencia: (violencia familiar / sexual)`,
                              "\nTotal: ", scales::comma(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`), sep=""))%>%
          ggplot() +
          aes(x = as.factor(Año), y = `Tipo de violencia: (violencia familiar / sexual)`,
              # size =`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`,
              fill = `Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`, text=text) +
          geom_tile()+
          geom_text(aes(label=comma(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=3, color="ghostwhite")+
          labs(x="", 
               title = latex2exp::TeX("Gráfico: \\textbf{Total de casos atendidos en el Sector Salud por violencia familiar y/o sexual}"),
               subtitle = paste("Datos correspondientes al total decasos atendidos en el Sector Salud por violencia familiar y/o sexual que se notifican al Ministerio Público de Fiscalía"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | Micrositio de datos de la AVGM, SISEMH.")+
          scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 17)) + 
          scale_fill_gradient(low = "#8a84a3", high = "#7e3794", 
                              label=comma, name=" ") +
          facet_wrap(~ `Modalidad de violencia (familiar/ laboral y docente/ comunitaria/ institucional/ feminicida)`, ncol = 5, 
                     scales = "free_x",
                     labeller = label_wrap_gen(width = 5, multi_line = TRUE)) +
          theme_minimal()+theme_1+
          theme(legend.position = "right",
                strip.text = element_text(face="bold", size=9,lineheight=1))
      ) 
      dev.off()
    })
  
  
  # output$download_d1<- downloadHandler(
  #   filename = function() { "salud_Casos_atendidos_violencia_fam_sexuales.xlsx"},
  #   content = function(file) {write_xlsx(ind_20_reactive(), path = file)}
  # )
  
  
  
  output$t_20 <- DT::renderDataTable ({
    
    ind_20_reactive() %>% 
      # indicador_20 %>% 
      group_by(Año) %>%
      summarise(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`= n(),
                `Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía`= sum(`Notificadas al mp: (si/no)`),
                # `Indicador`=scales::percent((`Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía`)/(`Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud`), 0.1)
      )->tabla_20
    
    
    tabla_20%>%  
      datatable(
        caption = htmltools::tags$caption(paste0("Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud"), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c('Año','Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud','Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, "All"),
                                         c(4,1,4,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatCurrency('Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud',currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatCurrency('Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía',currency = "", interval = 3, mark = ",", digits = 0) %>%
      
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
    # 
    # datatable(filter="top", extensions = 'Buttons',
    #                      options = list(dom = 'Blfrtip',
    #                                     buttons = c('copy', 'excel', 'print'),
    #                                     lengthMenu = list(c(6,10,20, -1),
    #                                                       c(6,10,20,"Todo")))) %>% 
    # formatCurrency('Total de mujeres víctimas de violencia sexual y/o familiar atendidas en el sector salud',currency = "", interval = 3, mark = ",", digits = 0) %>%          formatCurrency('Total de mujeres atendidas por violencia sexual y/o familiar notificadas al mp/fiscalía',currency = "", interval = 3, mark = ",", digits = 0)
    # 
    # 
  })  
  
  
  
  # Indicador 21: -----------------------------------------------------------------  
  
  output$ind_21_año <- renderUI({
    selectInput("ind_21_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_21$Año)),
                multiple = T)
  })
  
  output$ind_21_establecimiento<- renderUI({
    selectInput("ind_21_establecimiento",
                label =  "Seleccione el hospital",
                choices = sort(unique(indicador_21$Establecimiento)),
                multiple = T)
  })
  
  ind_21_reactive <- reactive({
    
    indicador_21 %>%
      filter(
        if(!is.null(input$ind_21_año))                          Año %in% input$ind_21_año              else Año != "",
        if(!is.null(input$ind_21_establecimiento))  Establecimiento %in% input$ind_21_establecimiento  else Establecimiento != ""
      )
    
  })
  
  
  
  
  output$t_21 <- DT::renderDataTable ({
    
    ind_21_reactive() %>% 
      # indicador_21 %>% 
      group_by(Año) %>%
      summarise(`Total de unidades en condiciones óptimas para realizar ILE/IVE` = sum(`Se cuenta con equipo y material para procedimiento ile/ive: (si/no)`),
                `Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile`= n(),
                # `Indicador`=scales::percent((`Total de unidades en condiciones óptimas para realizar ILE/IVE`)/(`Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile`), 0.1)
      )->tabla_21
    
    tabla_21%>% 
      # datatable(filter="top", options = list(pageLength = 6))
      datatable(
        caption = htmltools::tags$caption(paste0("Establecimientos estatales proveedores para procedimientos ILE/IVE"), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c('Año','Total de unidades en condiciones óptimas para realizar ILE/IVE',
                     'Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, "All"),
                                         c(4,1,4,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })  
  
  
  
  
  # Indicador 22: -----------------------------------------------------------------  
  
  output$ind_22_año <- renderUI({
    selectInput("ind_22_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_22$Año)),
                multiple = T)
  })
  
  output$ind_22_formación<- renderUI({
    selectInput("ind_22_formación",
                label =  "Seleccione la formación del personal",
                choices = sort(unique(indicador_22$Formación)),
                multiple = T)
  })
  
  ind_22_reactive <- reactive({
    
    indicador_22 %>%
      filter(
        if(!is.null(input$ind_22_año))              Año %in% input$ind_22_año         else Año != "",
        if(!is.null(input$ind_22_formación))  Formación %in% input$ind_22_formación   else Formación != ""
      )
    
  })
  
  output$gr22 <-renderPlotly ({
    
    ind_22_reactive() %>% 
      # indicador_22 %>% 
      # group_by(Periodo, Formación) %>% 
      group_by(Formación) %>% 
      summarise(`Total de personal de salud que atiende ILE/IVE capacitado`= n()) %>% 
      mutate(text = paste(#"año: ", periodo,
        # "Periodo: ",  format(as_date(Periodo), "%b de %y"),
        "\nFormación del personal: ", Formación,
        "\nTotal: ", scales::comma(`Total de personal de salud que atiende ILE/IVE capacitado`), sep=""))%>%
      ggplot() +
      aes(x = reorder(Formación, -`Total de personal de salud que atiende ILE/IVE capacitado`),
          y =`Total de personal de salud que atiende ILE/IVE capacitado`,
          colour = Formación, group=Formación, text=text) +
      geom_line(size = 1.5) + 
      geom_point(size = 2) +
      # scale_fill_manual(values = mycolors) +
      # scale_color_manual(values=mycolors)+      
      # scale_x_discrete(breaks = c("enero", "febrero", "marzo","abril", "mayo", "junio","julio", "agosto",
      #                            "septiembre", "octubre","noviembre", "diciembre"))+
      #scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 25)) + 
      # scale_color_manual(
      #     values = c(
      #       `daño a la salud` = "#d98cbc",
      #       `peligro de muerte` = "#c91682",
      #       `causal de violación` = "#7e3794"))+
      labs(x="", y="", title = "Indicador 22")+
      theme_minimal()+
      theme(legend.position = "none")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            strip.text = element_text(size = 9, family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.y = element_text(size=7, family="Nutmeg-Light"))->gr22
    
    
    
    ggplotly(gr22, tooltip = "text") %>% 
      layout(title = "Indicador 22",
             legend = list(orientation = 'h', 
                           x = 0, y = -1.4), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
  })
  
  
  output$t_22 <- DT::renderDataTable ({
    
    ind_22_reactive() %>% 
      # indicador_22 %>% 
      group_by(Año) %>%
      summarise(`Total de personal de salud que atiende ILE/IVE capacitado`= n(),
                `Total de personal de salud que atiende ILE/IVE`= n(),
                # `Indicador`=scales::percent((`Total de personal de salud que atiende ILE/IVE capacitado`)/(`Total de personal de salud que atiende ILE/IVE`), 0.1)
      )->tabla_22
    
    
    tabla_22%>% 
      # datatable(filter="top", options = list(pageLength = 6))
      datatable(
        caption = htmltools::tags$caption(paste0("Personal capacitado"), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c('Año','Total de personal de salud relacionado al procedimiento ILE/IVE, capacitado en el Programa ILE y NOM 046',
                     'Total de personal de salud que atiende ILE/IVE'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, "All"),
                                         c(4,1,4,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
  })  
  
  
  
  # Indicador 23: -----------------------------------------------------------------  
  
  output$ind_23_año <- renderUI({
    selectInput("ind_23_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_23$Año)),
                multiple = T)
  })
  
  output$ind_23_mes<- renderUI({
    selectInput("ind_23_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_23$Mes)),
                multiple = T)
  })
  
  
  output$ind_23_Función <- renderUI({
    selectInput("ind_23_formación",
                label =  "Selecciona la función del personal medicx",
                choices = sort(unique(indicador_23$Función)),
                multiple = T)
  })
  
  
  ind_23_reactive <- reactive({
    
    indicador_23 %>%
      filter(
        if(!is.null(input$ind_23_año))               Año %in% input$ind_23_año       else Año != "",
        if(!is.null(input$ind_23_mes))               Mes %in% input$ind_23_mes       else Mes != "",
        if(!is.null(input$Función))              Función %in% input$ind_23_Función   else Función != ""
      )
    
  })
  
  
  
  output$gr23 <-renderPlotly ({
    
    ind_23_reactive() %>%
      #indicador_23 %>% 
      group_by(Año, Mes, Periodo, Función) %>% 
      summarise(`Personal médico no objetor de conciencia`=sum(`Objetor de conciencia: (SI/NO)`, na.rm = T),
                `Total de personal médico debidamente capacitado`= n()) %>% 
      
      # mutate(Función=case_when(
      #   Función=="ADSCRITO"~"Medicx adscritx",
      #   Función=="Medico Adscrito "~"Médicx Adscritx",
      #   Función=="Enfermería"~"Enfermera",
      #   T~"Otro")) %>% 
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nFunción del personal : ", Función, sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = `Personal médico no objetor de conciencia`, 
          colour = Función, group=Función, 
          size=`Personal médico no objetor de conciencia`, text=text) +
      #  geom_line(size = 1.5) + 
      geom_point()+
      geom_text(aes(label=comma(`Personal médico no objetor de conciencia`, accuracy = 1)), size=3, color="ghostwhite")+
      scale_size_continuous(range = c(3,10))+
      labs(x="", y="", title = "Indicador 23") +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Personal médico` = "#D98CBC",
          `Personal de enfermería` = "#7E3794",
          `No se especifica` = "#C91682"))+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr23
    
    
    
    ggplotly(gr23, tooltip = "text") %>% 
      layout(title = list(text = paste0(" Indicador 23 ")),
             legend = list(orientation = 'v',  x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  output$t_23 <- renderDataTable ({
    
    ind_23_reactive() %>%
      #indicador_23 %>%
      group_by(Año) %>% 
      summarise(`Personal médico no objetor de conciencia`=sum(`Objetor de conciencia: (SI/NO)`, na.rm = T),
                `Total de personal médico debidamente capacitado`= n(),
                # `Indicador`=scales::percent(sum(`Personal médico no objetor de conciencia`/`Total de personal médico debidamente capacitado`), 0.1)
      ) -> tabla_23
    
    
    tabla_23 %>%  datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(6,10,20, -1),
                                                             c(6,10,20,"Todo"))))
  })  
  
  
  
  
  
  
  # Indicador 27: -----------------------------------------------------------------  
  
  output$ind_27_año <- renderUI({
    selectInput("ind_27_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_27$Año)),
                multiple = T)
  })
  
  output$ind_27_mes<- renderUI({
    selectInput("ind_27_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_27$Mes)),
                multiple = T)
  })
  
  
  output$ind_27_delito <- renderUI({
    selectInput("ind_27_municipio",
                label =  "Selecciona el delito",
                choices = sort(unique(indicador_27$Delito)),
                multiple = T)
  })
  
  
  ind_27_reactive <- reactive({
    
    indicador_27 %>%
      filter(
        if(!is.null(input$ind_27_año))         Año %in% input$ind_27_año     else Año != "",
        if(!is.null(input$ind_27_mes))         Mes %in% input$ind_27_mes     else Mes != "",
        if(!is.null(input$ind_27_delito))   Delito %in% input$ind_27_delito  else Delito != ""
      )
    
  })
  
  
  
  output$gr27 <-renderPlot ({

    ind_27_reactive() %>%
    # indicador_27 %>%
      group_by(Año, #Mes,Periodo,
               Delito) %>% 
      summarise(`Número de casos analizados`=sum(`Número de casos analizados`),
                `Total opiniones técnicas`=sum(`Total opiniones técnicas`),
                # `Indicador`=scales::percent(sum(`Número de casos analizados`/`Total opiniones técnicas`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Número de casos analizados",
                          "Total opiniones técnicas")) %>% 
      mutate(text = paste("Año: ", Año,
                          #"\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nTotal : ", scales::comma(Total), sep="")) %>%
      filter(!Total ==0) %>% 
      ggplot() +
      aes(x = Año , y = Total, fill = Delito,
          colour = Delito, group=Delito,
          text=text) +
      geom_line(size = 2) + geom_point(size = 4)+
      
      labs(x="", y="",
           title = latex2exp::TeX("Gráfico: \\textbf{Opiniones técnicas y casos analizados en muertes violentas y desaparición de mujeres}"),
           subtitle = paste("Datos correspondientes al total de Opiniones técnicas y casos analizados por las Unidades de Análisis y Contexto, 
en los delitos que involucran muertes violentas y desaparición de niñas, adolescentes y mujeres"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco | 
Micrositio de datos de la AVGM, SISEMH.")+
      facet_wrap(~ Clasificación) +
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(
        values = c("#C91682","#7E3794"))+
      scale_colour_manual(
        values = c("#C91682","#7E3794"))+
      theme_minimal()+ theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            strip.text = element_text(face="bold", size=11,lineheight=0.8),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr27
    gr27
  })
  
  output$t_27 <- renderDataTable ({
    
    ind_27_reactive() %>%
      # indicador_27 %>% 
      group_by(Año, Delito) %>% 
      summarise(`Opiniones técnicas`=sum(`Total opiniones técnicas`),
                `Casos analizados`=sum(`Número de casos analizados`)) %>% 
      pivot_wider(names_from = "Delito",
                  values_from = c(`Casos analizados`, `Opiniones técnicas`)) %>% 
      select(Año, `Casos analizados_Desaparición`,   `Opiniones técnicas_Desaparición`, `Casos analizados_Feminicidio`, `Opiniones técnicas_Feminicidio`)-> tabla_27
    
    names(tabla_27)[names(tabla_27) == "Casos analizados_Desaparición"] <- "Casos analizados desaparición"
    names(tabla_27)[names(tabla_27) == "Opiniones técnicas_Desaparición"] <- "Opiniones técnicas desaparición"
    
    names(tabla_27)[names(tabla_27) == "Casos analizados_Feminicidio"] <- "Casos analizados feminicidio"
    names(tabla_27)[names(tabla_27) == "Opiniones técnicas_Feminicidio"] <- "Opiniones técnicas feminicidio"
    
    
    tabla_27 %>%  
      # datatable(filter="top", extensions = 'Buttons',
      #                       options = list(dom = 'Blfrtip',
      #                                      buttons = c('copy', 'excel', 'print'),
      #                                      lengthMenu = list(c(6,10,20, -1),
      #                                                        c(6,10,20,"Todo")))) 
      datatable(filter="top", extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                               lengthMenu = list(c(5,10,20, -1),
                                                 c(5,10,20,"Todo")),
                               buttons = c('copy', 'excel', 'print'),
                               language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                               initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                               dom = "tip",
                               columnDefs = list(list(className = 'dt-center', targets = 1:5))))%>%
      # formatCurrency('Mujeres atendidas en el CJM',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      # formatCurrency('Mujeres que realizan una denuncia por el delito de violencia familiar',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      
      formatStyle(
        columns = c(1:5),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
   })  
  
  
  output$download_gr27 <- downloadHandler(
    filename = function() {
      paste0("FISCALIA_opiones_técnicas_muertes_violentas.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_27_reactive() %>%
          # indicador_27 %>%
          group_by(Año, #Mes,Periodo,
                   Delito) %>% 
          summarise(`Número de casos analizados`=sum(`Número de casos analizados`),
                    `Total opiniones técnicas`=sum(`Total opiniones técnicas`),
                    # `Indicador`=scales::percent(sum(`Número de casos analizados`/`Total opiniones técnicas`), 0.1)
          ) %>% 
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=c("Número de casos analizados",
                              "Total opiniones técnicas")) %>% 
          mutate(text = paste("Año: ", Año,
                              #"\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                              "\nTotal : ", scales::comma(Total), sep="")) %>%
          filter(!Total ==0) %>% 
          ggplot() +
          aes(x = Año , y = Total, fill = Delito,
              colour = Delito, group=Delito,
              text=text) +
          geom_line(size = 2) + geom_point(size = 4)+
          
          labs(x="", y="",
               title = latex2exp::TeX("Gráfico: \\textbf{Opiniones técnicas y casos analizados en muertes violentas y desaparición de mujeres}"),
               subtitle = paste("Datos correspondientes al total de Opiniones técnicas y casos analizados por las Unidades de Análisis y Contexto, 
en los delitos que involucran muertes violentas y desaparición de niñas, adolescentes y mujeres"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
          facet_wrap(~ Clasificación) +
          theme_minimal()+   
          scale_y_continuous(labels = scales::comma) +
          scale_fill_manual(
            values = c("#C91682","#7E3794"))+
          scale_colour_manual(
            values = c("#C91682","#7E3794"))+
          theme_minimal()+ theme_1+
          theme(legend.position = "bottom")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                strip.text = element_text(face="bold", size=11,lineheight=0.8),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      ) 
      dev.off()
    })
  
  
  output$download_d27<- downloadHandler(
    filename = function() {
      paste(input$dataset, "opiniones_técnicas_muertes_violentas_mujeres.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_27, file, row.names = F)
    }
  )
  
  
  
  # Indicador 28: -----------------------------------------------------------------  
  
  output$ind_28_año <- renderUI({
    selectInput("ind_28_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_28$Año)),
                multiple = T)
  })
  
  output$ind_28_mes<- renderUI({
    selectInput("ind_28_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_28$Mes)),
                multiple = T)
  })
  
  
  output$ind_28_municipio <- renderUI({
    selectInput("ind_28_municipio",
                label =  "Selecciona el delito",
                choices = sort(unique(indicador_28$Municipio)),
                multiple = T)
  })
  
  
  ind_28_reactive <- reactive({
    
    indicador_28 %>%
      filter(
        if(!is.null(input$ind_28_año))               Año %in% input$ind_28_año        else Año != "",
        if(!is.null(input$ind_28_mes))               Mes %in% input$ind_28_mes        else Mes != "",
        if(!is.null(input$ind_28_municipio))   Municipio %in% input$ind_28_municipio  else Municipio != ""
      )
    
  })
  
  
  
  output$gr28 <-renderPlot ({

    ind_28_reactive() %>%
      # indicador_28 %>%
      group_by(Año, Municipio) %>% 
      summarise(`Mujeres que realizan una denuncia por el delito de violacion`=sum(`Mujeres que realizan una denuncia por el delito de violacion`, na.rm=T),
                `Mujeres atendidas en el CJM`=sum(`Mujeres atendidas en el CJM`, na.rm=T),
                `Mujeres que realizan una denuncia por el delito de violencia familiar`=sum(`Mujeres que realizan una denuncia por el delito de violencia familiar`, na.rm=T),
                # `Indicador`=scales::percent(sum((`Mujeres que realizan una denuncia por el delito de violacion`+ `Mujeres que realizan una denuncia por el delito de violencia familiar`)/`Mujeres atendidas en el CJM`), 0.1)
      ) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Mujeres atendidas en el CJM",
                          "Mujeres que realizan una denuncia por el delito de violacion",
                          "Mujeres que realizan una denuncia por el delito de violencia familiar")) %>% 
      mutate(text = paste("Año: ", Año,
                          #"\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                          "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = as.factor(Año), y = Total, 
          colour = Clasificación, group=Clasificación) +
      geom_line(size = 2) + geom_point(size = 4)+
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX("Gráfico: \\textbf{Víctimas de violación y violencia familiar que denuncian en los CJM}"),
           subtitle = paste("Datos correspondientes al total de mujeres víctimas de violación y violencia familiar que son
atendidas y proceden a realizar una denuncia en los Centros de Justicia para las Mujeres."),
           caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(.~Municipio, scales = "free") +
      scale_color_manual(
        values = c(
          `Mujeres atendidas en el CJM` = "#D98CBC",
          `Mujeres que realizan una denuncia por el delito de violacion` = "#C91682",
          `Mujeres que realizan una denuncia por el delito de violencia familiar` = "#7E3794"))+
      guides(colour = guide_legend(label.hjust = 0, label.position = "right", nrow = 3))+       
      theme_minimal()+
      theme_1+
      theme(legend.position = "right")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            strip.text = element_text(face="bold", size=11,lineheight=0.8),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr28
    
    
  gr28
  })
  
  output$t_28 <- renderDataTable ({
    
    ind_28_reactive() %>%
      # indicador_28 %>% 
      group_by(Año) %>% 
      summarise(`Mujeres atendidas en el CJM`=sum(`Mujeres atendidas en el CJM`, na.rm=T),
                `Mujeres que realizan una denuncia por el delito de violacion`=sum(`Mujeres que realizan una denuncia por el delito de violacion`, na.rm=T),
                `Mujeres que realizan una denuncia por el delito de violencia familiar`=sum(`Mujeres que realizan una denuncia por el delito de violencia familiar`, na.rm=T),
                # `Indicador`=scales::percent(sum((`Mujeres que realizan una denuncia por el delito de violacion`+ `Mujeres que realizan una denuncia por el delito de violencia familiar`)/`Mujeres atendidas en el CJM`), 0.1, na.rm=T)
      ) -> tabla_28
    
    
    
    
    tabla_28 %>%  
      datatable(filter="top", extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           lengthMenu = list(c(5,10,20, -1),
                                                             c(5,10,20,"Todo")),
                                           buttons = c('copy', 'excel', 'print'),
                                           language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                                           initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                                           dom = "tip",
                                           columnDefs = list(list(className = 'dt-center', targets = 1:4))))%>%
      formatCurrency('Mujeres atendidas en el CJM',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Mujeres que realizan una denuncia por el delito de violencia familiar',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      
  formatStyle(
    columns = c(1:4),
    fontFamily = "Nutmeg-Light",
    fontSize = "11px",
    fontWeight = 'plain',
    borderRightWidth = "1px",
    borderRightStyle = "solid",
    borderRightColor = "white",
    borderBottomColor = "#ffffff",
    borderBottomStyle = "solid",
    borderBottomWidth = "0.5px",
    verticalAlign = "middle",
    textAlign = "center",
    wordWrap = "break-word")


    
    
  })
  
  
  output$download_gr28 <- downloadHandler(
    filename = function() {
      paste0("FISCALIA_denuncias_violación_violecia_familiar_CJM.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_28_reactive() %>%
          # indicador_28 %>%
          group_by(Año, Municipio) %>% 
          summarise(`Mujeres que realizan una denuncia por el delito de violacion`=sum(`Mujeres que realizan una denuncia por el delito de violacion`, na.rm=T),
                    `Mujeres atendidas en el CJM`=sum(`Mujeres atendidas en el CJM`, na.rm=T),
                    `Mujeres que realizan una denuncia por el delito de violencia familiar`=sum(`Mujeres que realizan una denuncia por el delito de violencia familiar`, na.rm=T),
                    # `Indicador`=scales::percent(sum((`Mujeres que realizan una denuncia por el delito de violacion`+ `Mujeres que realizan una denuncia por el delito de violencia familiar`)/`Mujeres atendidas en el CJM`), 0.1)
          ) %>% 
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=c("Mujeres atendidas en el CJM",
                              "Mujeres que realizan una denuncia por el delito de violacion",
                              "Mujeres que realizan una denuncia por el delito de violencia familiar")) %>% 
          mutate(text = paste("Año: ", Año,
                              #"\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                              "\nTotal: ", scales::comma(Total), sep="")) %>% 
          ggplot() +
          aes(x = as.factor(Año), y = Total, 
              colour = Clasificación, group=Clasificación, text=text) +
          geom_line(size = 2) + geom_point(size = 4)+
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX("Gráfico: \\textbf{Víctimas de violación y violencia familiar que denuncian en los CJM}"),
               subtitle = paste("Datos correspondientes al total de mujeres víctimas de violación y violencia familiar que son
atendidas y proceden a realizar una denuncia en los Centros de Justicia para las Mujeres."),
               caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
          scale_y_continuous(labels = scales::comma) +
          facet_wrap(.~Municipio, scales = "free") +
          scale_color_manual(
            values = c(
              `Mujeres atendidas en el CJM` = "#D98CBC",
              `Mujeres que realizan una denuncia por el delito de violacion` = "#C91682",
              `Mujeres que realizan una denuncia por el delito de violencia familiar` = "#7E3794"))+
          guides(colour = guide_legend(label.hjust = 0, label.position = "right", nrow = 3))+       
          theme_minimal()+
          theme_1+
          theme(legend.position = "right")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                strip.text = element_text(face="bold", size=11,lineheight=0.8),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) 
      
      ) 
      dev.off()
    })
  
  
  output$download_d28<- downloadHandler(
    filename = function() {
      paste(input$dataset, "denuncias_por_violacion_violencia_familiar_CJM.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_28, file, row.names = F)
    }
  )
  
  
  # Indicador 29: -----------------------------------------------------------------  
  
  output$ind_29_año <- renderUI({
    selectInput("ind_29_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_29$Año)),
                multiple = T)
  })
  
  output$ind_29_mes<- renderUI({
    selectInput("ind_29_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_29$Mes)),
                multiple = T)
  })
  
  output$ind_29_delito <- renderUI({
    selectInput("ind_29_delito",
                label =  "Selecciona el delito",
                choices = sort(unique(indicador_29$Delito)),
                multiple = T)
  })
  
  output$ind_29_municipio <- renderUI({
    selectInput("ind_29_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_29$Municipio)),
                multiple = T)
  })
  
  
  # output$ind_29_carpeta <- renderUI({
  #   selectInput("ind_29_carpeta",
  #               label =  "Selecciona el tipo de carpeta",
  #               choices = sort(unique(indicador_29$Carpeta)),
  #               multiple = T)
  # })
  
  
  ind_29_reactive <- reactive({
    
    indicador_29 %>%
      filter(
        if(!is.null(input$ind_29_año))               Año %in% input$ind_29_año       else Año != "",
        if(!is.null(input$ind_29_mes))               Mes %in% input$ind_29_mes       else Mes != "",
        if(!is.null(input$ind_29_municipio))   Municipio %in% input$ind_29_municipio else Municipio != "",
        if(!is.null(input$ind_29_delito))         Delito %in% input$ind_29_delito    else Delito != "",
        if(!is.null(input$ind_29_carpeta))       Carpeta %in% input$ind_29_carpeta    else Carpeta != ""
        
        
      )
    
  })
  
  
  output$gr29 <-renderPlot ({
    
    ind_29_reactive() %>%
      # indicador_29 %>% 
      filter(Carpeta=="Judicializada") %>% 
      group_by(Año, Delito) %>% 
      summarise(`Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`=sum(Registro, na.rm=T)) %>%   
      mutate(text = paste("Año: ", Año,
                          # "\nMes: ", Mes,
                          "\n`Casos denunciados que llegan a la etapa de judicialización` : ",  `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`  ,
                          "\nDelito : ",  Delito, 
                          sep="")) %>%
      ggplot() +
      aes(x = Año, y = `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`, 
          colour = Delito, group=Delito, 
          text=text) +
      geom_line(linewidth = 2) + geom_point(size = 4)+
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX("Gráfico: \\textbf{Carpetas de investigación judicializadas por delitos en razón de género}"),
           subtitle = paste("Datos correspondientes al total carpetas de investigación iniciadas y judicializadas por delitos 
diversos contra mujeres que involucran razones de género"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
      scale_fill_manual(values = mycolors129) +
      scale_color_manual(values = mycolors129)+
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +   
      theme_1 +
      theme(legend.position = "bottom") +
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr29
    gr29
    
  })
  
  output$t_29 <- renderDataTable ({
    
    ind_29_reactive() %>%
      # indicador_29 %>% 
      mutate(row = row_number()) %>%
      pivot_wider(names_from = "Carpeta",
                  values_from = "Registro") %>% 
      mutate(Iniciada=as.numeric(Iniciada),
             Judicializada=as.numeric(Judicializada),
             Total=Iniciada+Judicializada) %>% 
      group_by(Año) %>%
      summarise(Iniciada=sum(Iniciada, na.rm=T),
                Judicializada=sum(Judicializada, na.rm=T),
                `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`=sum(Judicializada, na.rm = T),
                `Total de casos por violencia por razón de género denunciados`=sum(Iniciada + Judicializada),
                # `Indicador`=scales::percent(sum((`Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`)/
                #                                    `Total de casos por violencia por razón de género denunciados`), 0.1)
      ) %>% 
      select(!c(Iniciada, Judicializada))->tabla_29
    
    
    
    tabla_29 %>%
      datatable(filter="top", extensions = 'Buttons',
      options = list(dom = 'Blfrtip',
                     buttons = c('copy', 'excel', 'print'),
                     language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                     initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                     dom = "tip",
                     buttons = c('copy', 'excel', 'print'),
                     lengthMenu = list(c(5,1,5,10,20, "All"),
                                       c(5,1,5,10,20, "All")),
                     columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
      
      # datatable(filter="top", extensions = 'Buttons',
      #                       options = list(dom = 'Blfrtip',
      #                                      buttons = c('copy', 'excel', 'print'),
      #                                      lengthMenu = list(c(6,10,20, -1),
      #                                                        c(6,10,20,"Todo")))) %>% 
      formatCurrency('Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatCurrency('Total de casos por violencia por razón de género denunciados',currency = "", interval = 3, mark = ",", digits = 0) %>% 
      formatStyle(
        columns = c(1:4),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
    
    
  })  
  
  
  output$download_gr29 <- downloadHandler(
    filename = function() {
      paste0("FISCALIA_judicializadas.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_29_reactive() %>%
          # indicador_29 %>% 
          filter(Carpeta=="Judicializada") %>% 
          group_by(Año, Delito) %>% 
          summarise(`Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`=sum(Registro, na.rm=T)) %>%   
          mutate(text = paste("Año: ", Año,
                              # "\nMes: ", Mes,
                              "\n`Casos denunciados que llegan a la etapa de judicialización` : ",  `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`  ,
                              "\nDelito : ",  Delito, 
                              sep="")) %>%
          ggplot() +
          aes(x = Año, y = `Casos denunciados por violencia por razón de género que llegan a la etapa de judicialización`, 
              colour = Delito, group=Delito, 
              text=text) +
          geom_line(linewidth = 2) + geom_point(size = 4)+
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX("Gráfico: \\textbf{Carpetas de investigación judicializadas por delitos en razón de género}"),
               subtitle = paste("Datos correspondientes al total carpetas de investigación iniciadas y judicializadas por delitos 
diversos contra mujeres que involucran razones de género"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
        scale_fill_manual(values = mycolors129) +
          scale_color_manual(values = mycolors129)+
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +   
          theme_1 +
          theme(legend.position = "bottom") +
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))

      ) 
      dev.off()
    })
  
  
  output$download_d29<- downloadHandler(
    filename = function() {
      paste(input$dataset, "carpetas_delitos_judicializados.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_29, file, row.names = F)
    }
  )
  
  # Indicador 30: -----------------------------------------------------------------  
  
  output$ind_30_año <- renderUI({
    selectInput("ind_30_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_30$Año)),
                multiple = T)
  })
  
  output$ind_30_mes<- renderUI({
    selectInput("ind_30_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_30$Mes)),
                multiple = T)
  })
  
  
  output$ind_30_condena <- renderUI({
    selectInput("ind_30_condena",
                label =  "Selecciona el tipo de condena",
                choices = sort(unique(indicador_30$`Tipo de sentencia (absolutoria, condenatoria y en proceso)`)),
                multiple = T)
  })
  
  
  ind_30_reactive <- reactive({
    
    indicador_30 %>%
      filter(
        if(!is.null(input$ind_30_año))               Año %in% input$ind_30_año       else Año != "",
        if(!is.null(input$ind_30_mes))               Mes %in% input$ind_30_mes       else Mes != "",
        if(!is.null(input$ind_30_condena))  `Tipo de sentencia (absolutoria, condenatoria y en proceso)` %in% input$ind_30_condena    else `Tipo de sentencia (absolutoria, condenatoria y en proceso)` != ""
        
      )
    
  })
  
  
  
  output$gr30 <-renderPlot ({
    
    ind_30_reactive() %>%
      #indicador_30 %>% 
      group_by(Periodo, `Tipo de sentencia (absolutoria, condenatoria y en proceso)`) %>% 
      summarise(`Total de sentencias`=n(),
                `Indicador`=scales::percent(sum(`Total de sentencias`/n()))) %>% 
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de sentencias"))%>% 
      # mutate(text = paste("Año: ", Año,
      #                     "\nMes: ",  Mes,
      #                     "\nTotal: ", scales::comma(Total), sep="")) %>% 
      ggplot() +
      aes(x = Periodo, y = Total, 
          colour = `Tipo de sentencia (absolutoria, condenatoria y en proceso)`, group=`Tipo de sentencia (absolutoria, condenatoria y en proceso)`) +
      geom_line(size = 2) + geom_point(size = 4)+
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX("Gráfico: \\textbf{Total de sentencias emitidas por el delito de feminicidio}"),
           subtitle = paste("Datos correspondientes al total de vinculaciones al proceso por el delito de feminicidio"),
           caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
      theme_minimal()+   
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Total de sentencia` = "#C91682"))+
      scale_color_manual(
        values = c(
          `En proceso` = "#b58cd9",
          `Condenatoria` = "#d98cbc",
          `Absolutoria` = "#7e3794",
          `Juicio` = "#c91682"))+
      theme_minimal()+
      theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr30
    
    
    gr30

  })
  
  output$t_30 <- renderDataTable ({
    
    # ind_30_reactive() %>%
    indicador_30 %>%
      group_by(Año) %>% 
      summarise(`Total de sentencias`=sum(`Año de sentencia`, na.rm = T),           
                `Total de casos vinculados a procesos`=n(),
                # `Indicador`=scales::percent(sum(`Total de sentencias`/n()), 0.1)
      ) -> tabla_30
    
    
    tabla_30 %>%  
      # datatable(filter="top", extensions = 'Buttons',
      #                       options = list(dom = 'Blfrtip',
      #                                      buttons = c('copy', 'excel', 'print'),
      #                                      lengthMenu = list(c(6,10,20, -1),
      #                                                        c(6,10,20,"Todo")))) 
      datatable(
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, "All"),
                                         c(5,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })  
  
  
  
  output$download_gr30 <- downloadHandler(
    filename = function() {
      paste0("FISCALIA_sentencia_por_femicidio.jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_30_reactive() %>%
          #indicador_30 %>% 
          group_by(Mes, Periodo, `Tipo de sentencia (absolutoria, condenatoria y en proceso)`) %>% 
          summarise(`Total de sentencias`=n(),
                    `Indicador`=scales::percent(sum(`Total de sentencias`/n()))) %>% 
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=c("Total de sentencias"))%>% 
          # mutate(text = paste("Año: ", Año,
          #                     "\nMes: ",  Mes,
          #                     "\nTotal: ", scales::comma(Total), sep="")) %>% 
          ggplot() +
          aes(x = Periodo, y = Total, 
              colour = `Tipo de sentencia (absolutoria, condenatoria y en proceso)`, group=`Tipo de sentencia (absolutoria, condenatoria y en proceso)`) +
          geom_line(size = 2) + geom_point(size = 4)+
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX("Gráfico: \\textbf{Total de sentencias emitidas por el delito de feminicidio}"),
               subtitle = paste("Datos correspondientes al total de vinculaciones al proceso por el delito de feminicidio"),
               caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
          scale_y_continuous(labels = scales::comma) +
          scale_color_manual(
            values = c(`Total de sentencia` = "#C91682"))+
          scale_color_manual(
            values = c(
              `En proceso` = "#b58cd9",
              `Condenatoria` = "#d98cbc",
              `Absolutoria` = "#7e3794",
              `Juicio` = "#c91682"))+
          theme_minimal()+
          theme_1+
          theme(legend.position = "bottom")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
        ) 
      dev.off()
    })
  
  
  output$download_d30<- downloadHandler(
    filename = function() {
      paste(input$dataset, "sentencias_por_delito_feminicidio.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_30, file, row.names = F)
    }
  )
  
  
  
  
  output$ind_40_año <- renderUI({
    selectInput("ind_40_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_40$Año)),
                multiple = T)
  })
  
  output$ind_40_mes<- renderUI({
    selectInput("ind_40_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_40$Mes)),
                multiple = T)
  })
  
  
  ind_40_reactive <- reactive({
    
    indicador_40 %>%
      filter(
        if(!is.null(input$ind_40_año))     Año %in% input$ind_40_año               else Año != "",
        if(!is.null(input$ind_40_mes))     Mes %in% input$ind_40_mes               else Mes != ""
      )
    
  })
  
  

  
  output$gr40 <-renderPlot ({
    
    if (input$value_total_g40 == "Anual") {
      
      ind_40_reactive() %>%
        #indicador_40 %>% 
        group_by(Año) %>% 
        summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
                  `Homicidio doloso`=sum(`Homicidio doloso`),
                  `Accidente`=sum(`Accidente`),
                  `Feminicidio`=sum(`Feminicidio`),
                  `Suicidio`=sum(`Suicidio`),
                  `El que resulte`=sum(`El que resulte`),
                  `Indicador`=sum(`Homicidio culposo`+ `Homicidio doloso`+
                                    `Accidente`+ `Feminicidio`+
                                    `Suicidio`+ `El que resulte`)) %>% 
        ungroup() %>% 
        pivot_longer(names_to = "Clasificación",
                     values_to = "Total",
                     cols=c("Homicidio culposo",
                            "Homicidio doloso",
                            "Accidente",
                            "Feminicidio",
                            "Suicidio",
                            "El que resulte")) %>% 
        # mutate(text = paste("Año: ", Año,
        #                     "\nPeriodo: ",  format(as_date(Periodo), "%b de %y"),
        #                     "\nTotal: ", scales::comma(Total), sep="")) %>% 
        ggplot() +
        aes(x = Año, y = Total, 
            colour = Clasificación, group=Clasificación) +
        geom_line(size = 1) + geom_point(size = 4)+
        labs(x="", y="", fill="", color="",
             title = latex2exp::TeX("Gráfico: \\textbf{Carpetas de investigación de muertes violentas de mujeres iniciadas por diversos delitos}"),
             subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
             caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
        # facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(
          values = c(`Homicidio culposo` = "#c91682",
                     `Homicidio doloso` = "#7e3794",
                     `Accidente` = "#597dff",
                     `Feminicidio`= "#b58cd9",
                     `Suicidio`= "#a544ff",
                     `El que resulte`= "#d98cbc")) +
        theme_minimal()+   
        theme_1+
        theme(legend.position = "bottom")+
        theme(text=element_text(size=12,  family="Nutmeg-Light"),
              plot.title = element_text(family="Nutmeg-Light"),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr40
      
      
    }  else { 
      
      
      
      ind_40_reactive() %>%
        #indicador_40 %>% 
        group_by(Año, Mes, Periodo) %>% 
        summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
                  `Homicidio doloso`=sum(`Homicidio doloso`),
                  `Accidente`=sum(`Accidente`),
                  `Feminicidio`=sum(`Feminicidio`),
                  `Suicidio`=sum(`Suicidio`),
                  `El que resulte`=sum(`El que resulte`),
                  `Indicador`=sum(`Homicidio culposo`+ `Homicidio doloso`+
                                    `Accidente`+ `Feminicidio`+
                                    `Suicidio`+ `El que resulte`)) %>% 
        pivot_longer(names_to = "Clasificación",
                     values_to = "Total",
                     cols=c("Homicidio culposo",
                            "Homicidio doloso",
                            "Accidente",
                            "Feminicidio",
                            "Suicidio",
                            "El que resulte")) %>% 
        mutate(text = paste("Año: ", Año,
                            "\nPeriodo: ",  format(as_date(Periodo), "%b de %y"),
                            "\nTotal: ", scales::comma(Total), sep="")) %>% 
        ggplot() +
        aes(x = Periodo, y = Total, 
            colour = Clasificación, group=Clasificación, text=text) +
        geom_line(size = 1) + geom_point(size = 2)+
        labs(x="", y="", fill="", color="",
             title = latex2exp::TeX("Gráfico: \\textbf{Carpetas de investigación de muertes violentas de mujeres iniciadas por diversos delitos}"),
             subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
             caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
        # facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
        scale_y_continuous(labels = scales::comma, limits = c(0,30)) +
        scale_color_manual(
          values = c(`Homicidio culposo` = "#c91682",
                     `Homicidio doloso` = "#7e3794",
                     `Accidente` = "#597dff",
                     `Feminicidio`= "#b58cd9",
                     `Suicidio`= "#a544ff",
                     `El que resulte`= "#d98cbc")) +
        theme_minimal()+   
        theme_1+
        theme(legend.position = "bottom")+
        theme(text=element_text(size=12,  family="Nutmeg-Light"),
              plot.title = element_text(family="Nutmeg-Light"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))->gr40
    }
    gr40
  })
  
  
  
  
  
  output$t_40 <- renderDataTable ({
    
    ind_40_reactive() %>%
      #indicador_40 %>% 
      group_by(Año) %>% 
      summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
                `Homicidio doloso`=sum(`Homicidio doloso`),
                `Accidente`=sum(`Accidente`),
                `Feminicidio`=sum(`Feminicidio`),
                `Suicidio`=sum(`Suicidio`),
                `El que resulte`=sum(`El que resulte`),
                Total=sum(`Homicidio culposo`+ `Homicidio doloso`+
                            `Accidente`+ `Feminicidio`+
                            `Suicidio`+ `El que resulte`)) -> tabla_40# %>% 
    # mutate(`Indicador` = scales::percent((Total - lag(Total))/lag(Total),0.1))-> tabla_40
    
    
    
    tabla_40 %>%  
      datatable(
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, "All"),
                                         c(5,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:8)))) %>%  
      formatStyle(
        columns = c(1:8),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
    
  })  
  
  
  
    output$download_gr40 <- downloadHandler(
      filename = function() {
        paste0("FISCALIA_carpetas_investigacion_delito.jpeg")},
      
      content = function(file) {
        jpeg(file, width = 1500, height = 800)
        print(
          ind_40_reactive() %>%
            #indicador_40 %>% 
            group_by(Año, Mes, Periodo) %>% 
            summarise(`Homicidio culposo`=sum(`Homicidio culposo`),
                      `Homicidio doloso`=sum(`Homicidio doloso`),
                      `Accidente`=sum(`Accidente`),
                      `Feminicidio`=sum(`Feminicidio`),
                      `Suicidio`=sum(`Suicidio`),
                      `El que resulte`=sum(`El que resulte`),
                      `Indicador`=sum(`Homicidio culposo`+ `Homicidio doloso`+
                                        `Accidente`+ `Feminicidio`+
                                        `Suicidio`+ `El que resulte`)) %>% 
            pivot_longer(names_to = "Clasificación",
                         values_to = "Total",
                         cols=c("Homicidio culposo",
                                "Homicidio doloso",
                                "Accidente",
                                "Feminicidio",
                                "Suicidio",
                                "El que resulte")) %>% 
            mutate(text = paste("Año: ", Año,
                                "\nPeriodo: ",  format(as_date(Periodo), "%b de %y"),
                                "\nTotal: ", scales::comma(Total), sep="")) %>% 
            ggplot() +
            aes(x = Periodo, y = Total, 
                colour = Clasificación, group=Clasificación, text=text) +
            geom_line(size = .8) + geom_point(size = 1.3)+
            labs(x="", y="", fill="", color="",
                 title = latex2exp::TeX("Gráfico: \\textbf{Carpetas de investigación de muertes violentas de mujeres iniciadas por diversos delitos}"),
                 subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
                 caption="\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado | Micrositio de datos de la AVGM, SISEMH")+
            # facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
            scale_y_continuous(labels = scales::comma, limits = c(0,30)) +
            scale_color_manual(
              values = c(`Homicidio culposo` = "#c91682",
                         `Homicidio doloso` = "#7e3794",
                         `Accidente` = "#597dff",
                         `Feminicidio`= "#b58cd9",
                         `Suicidio`= "#a544ff",
                         `El que resulte`= "#d98cbc")) +
            theme_minimal()+   
            theme_1+
            theme(legend.position = "bottom")+
            theme(text=element_text(size=12,  family="Nutmeg-Light"),
                  plot.title = element_text(family="Nutmeg-Light"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
        ) 
        dev.off()
      })
  
  
  output$download_d40<- downloadHandler(
    filename = function() { "carpetas_investigación_por_muertes_violentas.xlsx"},
    content = function(file) {write_xlsx(indicador_40, path = file)}
  )
  
  
}

shinyApp(ui, server)


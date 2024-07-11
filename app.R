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
sysfonts::font_add_google(name = "Poppins", family = "Poppins")
showtext::showtext_auto()

# font_add("Nutmeg-Light", "Nutmeg-Light.ttf")
# font_families()

#theme_1------------------------------------------------------------------------

theme_1<-theme_minimal()+
  theme(text=element_text(family = gt::google_font("Poppins")),
        plot.title = element_text(family = gt::google_font("Poppins"),
                                  face = "bold",
                                  size = 15,
                                  hjust = 0),
        plot.subtitle = element_text(family = gt::google_font("Poppins"),
                                     size = 12,
                                     hjust = 0,
                                     colour = "grey40"),
        plot.caption = element_text(family = gt::google_font("Poppins"),
                                    size = 10,
                                    colour = "grey40"),
        axis.text.x = element_text(family = gt::google_font("Poppins"),
                                   # face = "bold",
                                   size = 12,
                                   colour = "black"),
        axis.text.y = element_text(family = gt::google_font("Poppins"),
                                   # face = "bold",
                                   size = 12,
                                   colour = "black"),
        legend.title = element_text(family = gt::google_font("Poppins"),
                                    face = "bold",
                                    size = 12,
                                    colour = "black",
                                    hjust = 1),
        legend.title.align = 0.5,
        legend.text = element_text(family = gt::google_font("Poppins"),
                                   # face = "bold",
                                   size = "8px",
                                   colour = "black",
                                   hjust = 1),
        legend.text.align = 0.5,
        legend.key.size = unit(25, "pt"))

# theme_1<-theme_minimal()+
#   theme(text=element_text(family = "Nutmeg-Light"),
#         plot.title = element_text(family = "Nutmeg-Light",
#                                   face = "bold",
#                                   size = 16,
#                                   hjust = 0),
#         plot.subtitle = element_text(family = "Nutmeg-Light",
#                              size = 12,
#                              hjust = 0,
#                              colour = "grey40"),
#         plot.caption = element_text(family = "Nutmeg-Light",
#                             size = 12,
#                             colour = "grey40"),
#         axis.text.x = element_text(family ="Nutmeg-Light",
#                            # face = "bold",
#                            size = 12,
#                            colour = "black"),
#         axis.text.y = element_text(family = "Nutmeg-Light",
#                            # face = "bold",
#                            size = 12,
#                            colour = "black"),
#         legend.title = element_text(family = "Nutmeg-Light",
#                             face = "bold",
#                             size = 12,
#                             colour = "black",
#                             hjust = 1),
#         legend.title.align = 0.5,
#         legend.text = element_text(family = "Nutmeg-Light",
#                            # face = "bold",
#                            size = "8px",
#                            colour = "black",
#                            hjust = 1),
#         legend.text.align = 0.5,
#         legend.key.size = unit(25, "pt"))


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

medidas_ordenes_municipal <- read_excel("medidas_ordenes_municipal.xlsx") %>%
  mutate(mes=factor(mes,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio", "Julio",
                             "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))

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
indicador_30<-read_excel("indicador_30.xlsx", col_types = c("numeric","text", "text",
                                                            "text", "date", "numeric","text", "text"))%>% suppressWarnings()
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

indicador_18<-read_excel("indicador_18.xlsx")%>% suppressWarnings() %>%
  mutate(Rango=factor(Rango,
                      levels=c("0 a 2 años", "3 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 25 años", "26 a 35 años",
                               "36 a 45 años", "46 a 59 años", "60 en adelante")))

indicador_19<-read_excel("indicador_19.xlsx")%>% suppressWarnings()
indicador_20<-read_excel("indicador_20.xlsx")%>% suppressWarnings()
indicador_21<-read_excel("indicador_21.xlsx")%>% suppressWarnings() %>% 
  mutate(Establecimiento=case_when(
    grepl("CH CIUDAD GUZMAN", Establecimiento) ~ "Clínica Hospital ISSSTE Cd. Guzmán", 
    grepl("CH PUERTO VALLARTA", Establecimiento) ~ "Clínica Hospital ISSSTE Puerto Vallarta", 
    grepl("110", Establecimiento) ~ "Hospital General de Zona No. 110 Guadalajara",
    grepl("46", Establecimiento) ~ "Hospital General Regional No. 46 Guadalajara",
    grepl("42", Establecimiento) ~ "Hospital General de Zona No. 42 Puerto Vallarta",
    grepl("Mateos", Establecimiento) ~ "Hospital Materno Infantil Esperanza Lopez Mateos",
    grepl("Far", Establecimiento) ~ "Hospital Regional Dr. Valentín Gómez Farías - ISSSTE",T ~ Establecimiento
  ), 
  tipo=case_when(
    grepl("General", Establecimiento) ~ "IMSS", 
    grepl("Clínica|Valentín|", Establecimiento) ~ "IMSS", 
    T ~ "OPDSSJ")) 
indicador_22<-read_excel("indicador_22.xlsx")%>% suppressWarnings() %>% 
  mutate(Formación=case_when(
    grepl("ABOG|DERE|JUEZ", Formación) ~ "DERECHO", 
    grepl("PSI", Formación) ~ "PSICOLOGÍA", grepl("ENF", Formación) ~ "ENFERMERÍA",
    grepl("QU|\\Q.F", Formación) ~ "QUÍMICA", grepl("ADMI", Formación) ~ "ADMINISTRACIÓN",
    grepl("ODO|DENT", Formación) ~ "ODONTOLOGÍA", grepl("TRAB", Formación) ~ "TRABAJO SOCIAL",
    grepl("BACH|PREP", Formación) ~ "BACHILLERATO", grepl("PAS", Formación) ~ "PASANTE",
    grepl("GIN", Formación) ~ "GINECOLOGÍA", grepl("PROMO", Formación) ~ "PROMOTOR DE SALUD",
    grepl("CIR", Formación) ~ "MÉDICO CIRUJANO", grepl("CONTA", Formación) ~ "CONTADURÍA",
    grepl("MÉDI|MEDI", Formación) ~ "MEDICINA",
    grepl("MERCA", Formación) ~ "MERCADOTECNIA",
    T ~ Formación
  ))
indicador_23<-read_excel("indicador_23.xlsx")%>% suppressWarnings()

################################################################################
#
#                              PROTOCOLO ALBA
#
################################################################################

# setwd("~/NLMR/avgm_3_data")

# Indicador 32: ----------------------------------------------------------------#
indicador_32 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 32")%>% suppressWarnings()

indicador_32 %>%
  mutate(Fecha=case_when(
    Mes=="Enero" ~ 1,
    Mes=="Febrero" ~ 2,
    Mes=="Marzo" ~ 3,
    Mes=="Abril" ~ 4,
    Mes=="Mayo" ~ 5,
    Mes=="Junio" ~ 6,
    Mes=="Julio" ~ 7,
    Mes=="Agosto" ~ 8,
    Mes=="Septiembre" ~ 9,
    Mes=="Octubre" ~ 10,
    Mes=="Noviembre" ~ 11,
    Mes=="Diciembre" ~ 12,

  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_32



indicador_32$Fecha   <- format(as.Date(indicador_32$Periodo, format = "%d-%m-%Y"))
indicador_32$Fecha   <- format(as.Date(indicador_32$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_32$Fecha   <- as.Date(indicador_32$Periodo, format = "%d-%m-%Y")



indicador_32 %>%
  group_by(Año, Mes, Fecha) %>%
  summarise(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(
    `0 a 2 años`+
      `3 a 5 años`+
      `6 a 12 años`+
      `13 a 17 años`+
      `18 a 25 años`+
      `26 a 35 años`+
      `36 a 45 años`+
      `46 a 59 años`+
      `60  años en adelante`+
      `Sin datos`, na.rm = T),
    `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
    `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
    `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T))->estatal_32

indicador_32 %>%
  group_by(Año, Mes, Fecha, Municipio) %>%
  summarise(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(
    `0 a 2 años`+
      `3 a 5 años`+
      `6 a 12 años`+
      `13 a 17 años`+
      `18 a 25 años`+
      `26 a 35 años`+
      `36 a 45 años`+
      `46 a 59 años`+
      `60  años en adelante`+
      `Sin datos`, na.rm = T),
    `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
    `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
    `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T))->municipal_32


entidad<- c("Estado de Jalisco")
cbind(entidad, estatal_32)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipal_32)->indicador_32


# Indicador 33: ----------------------------------------------------------------#
indicador_33 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 33")%>% suppressWarnings()

indicador_33 %>%
  mutate(Fecha=case_when(
    Mes=="Enero" ~ 1,
    Mes=="Febrero" ~ 2,
    Mes=="Marzo" ~ 3,
    Mes=="Abril" ~ 4,
    Mes=="Mayo" ~ 5,
    Mes=="Junio" ~ 6,
    Mes=="Julio" ~ 7,
    Mes=="Agosto" ~ 8,
    Mes=="Septiembre" ~ 9,
    Mes=="Octubre" ~ 10,
    Mes=="Noviembre" ~ 11,
    Mes=="Diciembre" ~ 12,

  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_33



indicador_33$Fecha   <- format(as.Date(indicador_33$Periodo, format = "%d-%m-%Y"))
indicador_33$Fecha   <- format(as.Date(indicador_33$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_33$Fecha   <- as.Date(indicador_33$Periodo, format = "%d-%m-%Y")


# Indicador 34: ----------------------------------------------------------------#
indicador_34 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 34")%>% suppressWarnings()

indicador_34 %>%
  mutate(
    Fecha=case_when(
      Mes=="Enero" ~ 1,
      Mes=="Febrero" ~ 2,
      Mes=="Marzo" ~ 3,
      Mes=="Abril" ~ 4,
      Mes=="Mayo" ~ 5,
      Mes=="Junio" ~ 6,
      Mes=="Julio" ~ 7,
      Mes=="Agosto" ~ 8,
      Mes=="Septiembre" ~ 9,
      Mes=="Octubre" ~ 10,
      Mes=="Noviembre" ~ 11,
      Mes=="Diciembre" ~ 12),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    Trimestre = case_when(
      Mes == "Enero" ~ "ene - mar",
      Mes == "Febrero" ~ "ene - mar",
      Mes == "Marzo" ~ "ene - mar",
      Mes == "Abril" ~ "abr - jun",
      Mes == "Mayo" ~ "abr - jun",
      Mes == "Junio" ~ "abr - jun",
      Mes == "Julio" ~ "jul - sep",
      Mes == "Agosto" ~ "jul - sep",
      Mes == "Septiembre" ~ "jul - sep",
      Mes == "Octubre" ~ "oct - dic",
      Mes == "Noviembre" ~ "oct - dic",
      Mes == "Diciembre" ~ "oct - dic"),
    Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
    Trimestre = paste0(Año, " ", Trimestre))->indicador_34



# Indicador 35: ----------------------------------------------------------------#
indicador_35 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 35")%>% suppressWarnings()

indicador_35 %>%
  mutate(Fecha=case_when(
    Mes=="Enero" ~ 1,
    Mes=="Febrero" ~ 2,
    Mes=="Marzo" ~ 3,
    Mes=="Abril" ~ 4,
    Mes=="Mayo" ~ 5,
    Mes=="Junio" ~ 6,
    Mes=="Julio" ~ 7,
    Mes=="Agosto" ~ 8,
    Mes=="Septiembre" ~ 9,
    Mes=="Octubre" ~ 10,
    Mes=="Noviembre" ~ 11,
    Mes=="Diciembre" ~ 12,

  ),
  Mes=factor(Mes,
             levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                      "Septiembre", "Octubre","Noviembre", "Diciembre")),
  Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_35



indicador_35$Fecha   <- format(as.Date(indicador_35$Periodo, format = "%d-%m-%Y"))
indicador_35$Fecha   <- format(as.Date(indicador_35$Periodo, format = "%d-%m-%Y"),  "%Y-%m")
indicador_35$Fecha   <- as.Date(indicador_35$Periodo, format = "%d-%m-%Y")

# Indicador 36: ----------------------------------------------------------------#
indicador_36 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 36")%>% suppressWarnings()

indicador_36 %>%
  mutate(
    Fecha=case_when(
      Mes=="Enero" ~ 1,
      Mes=="Febrero" ~ 2,
      Mes=="Marzo" ~ 3,
      Mes=="Abril" ~ 4,
      Mes=="Mayo" ~ 5,
      Mes=="Junio" ~ 6,
      Mes=="Julio" ~ 7,
      Mes=="Agosto" ~ 8,
      Mes=="Septiembre" ~ 9,
      Mes=="Octubre" ~ 10,
      Mes=="Noviembre" ~ 11,
      Mes=="Diciembre" ~ 12),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")),
    Trimestre = case_when(
      Mes == "Enero" ~ "ene - mar",
      Mes == "Febrero" ~ "ene - mar",
      Mes == "Marzo" ~ "ene - mar",
      Mes == "Abril" ~ "abr - jun",
      Mes == "Mayo" ~ "abr - jun",
      Mes == "Junio" ~ "abr - jun",
      Mes == "Julio" ~ "jul - sep",
      Mes == "Agosto" ~ "jul - sep",
      Mes == "Septiembre" ~ "jul - sep",
      Mes == "Octubre" ~ "oct - dic",
      Mes == "Noviembre" ~ "oct - dic",
      Mes == "Diciembre" ~ "oct - dic"),
    Trimestre=factor(Trimestre, levels = c("ene - mar", "abr - jun", "jul - sep", "oct - dic")),
    Trimestre = paste0(Año, " ", Trimestre),
    Trimestre=factor(Trimestre, levels = c("2020 ene - mar", "2020 abr - jun", "2020 jul - sep", "2020 oct - dic",
                                           "2021 ene - mar", "2021 abr - jun", "2021 jul - sep", "2021 oct - dic",
                                           "2022 ene - mar", "2022 abr - jun", "2022 jul - sep", "2022 oct - dic")),
    `Rango de edad`=factor(`Rango de edad`,
                           levels=c("0 a 2 años", "3 a 5 años", "6  a 12 años","13 a 17 años", "Sin datos")))->indicador_36


# Indicador 37: ----------------------------------------------------------------#
indicador_37 <- read_excel("indicadores_fiscalia.xlsx", sheet = "Ind 37")%>% suppressWarnings()

indicador_37 %>%
  mutate(
    Fecha=case_when(
      Mes=="Enero" ~ 1,
      Mes=="Febrero" ~ 2,
      Mes=="Marzo" ~ 3,
      Mes=="Abril" ~ 4,
      Mes=="Mayo" ~ 5,
      Mes=="Junio" ~ 6,
      Mes=="Julio" ~ 7,
      Mes=="Agosto" ~ 8,
      Mes=="Septiembre" ~ 9,
      Mes=="Octubre" ~ 10,
      Mes=="Noviembre" ~ 11,
      Mes=="Diciembre" ~ 12),
    Mes=factor(Mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                        "Septiembre", "Octubre","Noviembre", "Diciembre")),
    Periodo = ymd(paste0(Año, "-", Fecha, "-01")))->indicador_37


###############################################################################
#                           
#                            D I C C I O N A R I O
#
###############################################################################

texto_ijcf <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Este apartado muestra el total de registros administrativos de las intervenciones que hace el IJCF directamente a las mujeres víctimas de muertes en Jalisco desde el año 2020 y que se plantea actualizar de manera periodica cada trimestre.</p>
      <p>La sección divide en dos los datos, <b>1) Perfil de las víctimas</b>, que refiere a las características sociodemográficas de cada mujer:</p>
      <ul>
        <li><b>Año:</b> Año en el que sucedió la muerte.</li>
        <li><b>Municipio:</b> Municipio donde sucedió la muerte.</li>
        <li><b>Rango de edad:</b> Grupo de edad de la mujer.</li>
        <li><b>Causa:</b> Posible causa de muerte (y/o mecanismo de muerte), esta clasificación es definida por el IJCF.</li>
        <li><b>Tipo de muerte:</b> Refiere a que si fue de tipo violento, natural, o ‘No aplica’ que registras los casos distintos a los dos tipos anteriores.</li></ul>
        
       
      <p>La segunda sección <b>2) Servicios forenses</b>, incluye los servicios que reportó el IJCF en la atención de las mujeres víctimas de muertes:</p>
        <li><b>Servicios forenses:</b> Conjunto de estudios multidisciplinarios técnicos científicos, realizado por expertos en diversas especialidades en un caso en particular, en el que se realizan procesamiento del lugar de los hechos, el análisis genético, químico, toxicológico, balístico, psicológico, etc. para conocer la reconstrucción del hecho y él y/o presuntos responsables del mismo, así como las implicaciones legales. Esta información puede ser crucial tanto para la defensa como para los argumentos de la fiscalía.</li>
        </ul>
    
    ")
}


texto_medidas_ordenes <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Este apartado muestra el total de órdenes y medidas de protección emitidas en la entidad. Se actualiza cada mes por parte de la Fiscalía Estatal de Jalisco.</p>
      <p>Las <b>órdenes y medidas de protección</b> son un mecanismo de protección para las mujeres víctimas de violencia, en las cuales se puede brindar los siguientes apoyos:</p>
      <ul>
      <li>Rondines policiales en la casa, trabajo o escuela (de la víctima) para disuadir al agresor.</li>
      <li>Auxilio inmediato y un dispositivo de geolocalización para emergencias.</li>
      <li>Separación del agresor del domicilio, incluso si la propiedad está a su nombre.</li>
      <li>Desarme del agresor si pertenece a una corporación de seguridad pública o privada.</li>
      <li>Recuperación de tus pertenencias de la casa común, acompañada de una autoridad.</li>
      <li>Prohibición al agresor de molestarte, intimidarte o comunicarse contigo.</li>
      <li>Tratamiento psicológico/reeducativo integral y especializado para el agresor.</li>
      
      </ul>
      <p>Las <b>medidas de protección</b> tiene una temporalidad de 1,440 horas (60 días) y pueden ser emitidas por Agentes del Ministerio Público.
      Las <b>órdenes de protección</b> tiene una temporalidad de 72 horas (3 días) y pueden ser emitidas por Agentes del Ministerio Público, síndicas o síndicos, juezas o jueces municipales y juezas y jueces de primera instancia.<br>
      Si la orden expiró se puede acudir con el Ministerio Público, jueces/zas municipales o síndico/as para que la amplíen.</p>
      
      <p>Las variables de <b>año</b>, <b>mes</b> y <b>municipio</b> refieren a la temporalidad de emisión de las órdenes y medidas, así como, del lugar.</p>
    ")
}

#Cocca
texto_salud_17 <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección muestra el total de <b>mujeres atendidas por violación y abuso sexual infantil</b> en el Sector Salud referidas por la Fiscalía del Estado.</p>
      <ul>
      <li><b>Año:</b> Indica el año en que fue referida por la Fiscalía.</li>
      <li><b>Mes:</b> corresponde al mes en que fue referida por la Fiscalía.</li>
      <li><b>Rango de edad:</b> Especifica el intervalo de edad.</li>
      </ul>
    ")
} 

texto_salud_20 <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección muestra el total de <b>casos atendidos</b> por violencia sexual y/o familiar, notificadas mediante aviso al Ministerio Público de la Fiscalía del Estado.</p>
      <ul>
      <li><b>Año:</b> Indica el año en que atendido el caso.</li>
      <li><b>Tipo de violencia:</b> Forma en la que se ejerce la violencia.</li>
      <li><b>Modalidad de violencia:</b> Lugar, ambiente o situación donde se ejerce la violencia.</li>

      </ul>
    ")
} 

texto_salud_18 <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección muestra el total de mujeres víctimas de violación que recibieron el procedimiento de Interrupción Vontuaria del Embarazo de conformidad con la NOM 046 y el Programa Estatal de Interrupción Legal del Embarazo</p>
      <ul>
      <li><b>Año:</b> Indica el año en que se solicitó el procedimiento.</li>
      <li><b>Mes:</b> corresponde al mes en que se solicitó el procedimeinto.</li>
      <li><b>Rango de edad:</b> Especifica el intervalo de la edad de la mujer solicitante.</li>

      </ul>
    ")
} 


texto_salud_19 <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección muestra el total de mujeres que de conformidad con las causale legales en el estado, recibieron el procedimiento de Interrupción Legal del Embarazo conforme al Programa ILE en los servicios de salud del Estado de Jalisco</p>
      <ul>
      <li><b>Año:</b> Indica el año en que se llevó a cabo el procedimiento.</li>
      <li><b>Causal:</b> corresponde a las causales legales para interrumpir el embarazo.</li>
      <li><b>Rango de edad:</b> Especifica el intervalo de la edad de la mujer solicitante.</li>

      </ul>
    ")
} 


texto_salud_22 <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección muestra el total de personal de salud que atiende ILE/IVE y que está capacitado en el Programa ILE y la NOM 046</p>
      <ul>
      <li><b>Año:</b> Indica el año que corresponde con el personal registrado.</li>
      <li><b>Formación del personal:</b> corresponde a la educación formal del personal que atiende, puede ser personal médico, así como, personal urídico y se asistencia social.</li>

      </ul>
    ")
} 

texto_salud_21 <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección muestra el total de unidades de médidcas del segundo y tercer nivel del sector salud en condiciones óptimas para realizar un procedimiento ILE/IVE</p>
      <ul>
      <li><b>Año:</b> Indica el año que corresponde con las unidades médicas registradas.</li>
      <li><b>Hospital:</b> hace referencias las unidades médicas y hospitales que cuentan con las condiciones para realizar ILE/IVE.</li>

      </ul>
    ")
} 

texto_feminicidio <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Esta sección se muestran diversos datos que sitúan el contexto de los feminicidios contabilizados en la entidad.</p>
      <ul>
      <li><b>Opiniones técnicas: </b> Son el mecanismo que realiza la Dirección de Análisis y Contexto con la finalidad de orientar la investigación en casos de víctimas de feminicidios y desapariciones de niñas, adolescentes y mujeres.</li>
      <li><b>Denuncias en los CJM: </b> Refiere a las mujeres víctimas de los delitos de violación y violencia familiar que son atendidas, y a su vez realizaron una denuncia en los Centros de Justicia para las Mujeres de Jalisco.</li>
      <li><b>Delitos judicializados: </b>Total de casos denunciados en la Fiscalía por delito en razón de género que llegan a la etapa de judicialización.</li>
      <li><b>Sentencias por feminicidio: </b> Refleja el estatus de la sentencia (absolutoria, condenatoria, en proceso) de los casos vinculados a proceso por el delito de feminicidio, .</li>
      <li><b>Muertes violentas: </b> Entendida como muertes violentas de mujer incluye homicidios, accidentes, feminicidios y suicidios, así como los que resulte.</li>

      </ul>
    ")
} 


texto_protocolo <- function() {
  HTML("
      <p><b>Diccionario de variables</b></p>
      <p>Este apartado presenta información relacionada a los casos de niñas, adolescentes y mujeres desaparecidas y no localizadas. Así como, información relacionada a los protocolos Alba y Amber</p>
      <ul>
        <li><b>Cédulas únicas de difusión: </b> Formato general que contiene fotografía reciente de la Niña, Adolescente o Mujer reportada o denunciada por desconocerse su paradero o ubicación, así como sus datos  generales, filiación descriptiva y breves referencias de la desaparición.</li>
        <li><b>Cédulas únicas remitidas: </b> Refiere a las cédulas únicas que son remitidas al Comité Técnico de Colaboración para su difusión.</li>
        <li><b>Casos de búsqueda y localización: </b> Casos de búsquedas y localización de niñas, adolescentes y mujeres donde se aplicó a cabalidad el Protocolo Alba de manera inmediata y diferenciada.</li>
        <li><b>Cédulas de difusión activas: </b> Cédulas únicas de difusión que se mantienen activas.</li>
        <li><b>Casos de Reporte Amber: </b> Casos de desaparición de niñas y adolescentes en los que se activa el Reporte Amber.</li></ul>
        <li><b>Personal capacitado en el Protocolo Alba: </b> Total del personal de la Fiscalía del Estado de Jalisco que se encuentra debidamente formado en el programa de capacitación continua y permanente en el funcionamiento del Protocolo Alba, de conformidad a la normativa estatal y los estándares internaciones en la búsqueda y localización de mujeres, adolescentes y niñas.</li>

       
      <p>La segunda sección <b>2) Servicios forenses</b>, incluye los servicios que reportó el IJCF en la atención de las mujeres víctimas de muertes:</p>
        <li><b>Servicios forenses:</b> Conjunto de estudios multidisciplinarios técnicos científicos, realizado por expertos en diversas especialidades en un caso en particular, en el que se realizan procesamiento del lugar de los hechos, el análisis genético, químico, toxicológico, balístico, psicológico, etc. para conocer la reconstrucción del hecho y él y/o presuntos responsables del mismo, así como las implicaciones legales. Esta información puede ser crucial tanto para la defensa como para los argumentos de la fiscalía.</li>
        </ul>
    
    ")
}
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
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
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
   background-color: #867191 !important;
   color: white !important;
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-purple {
   background-color: #917180 !important;
   color: white !important;
   font-family: Nutmeg-Light !important;

   }

   .small-box.bg-maroon {
     background-color: #718f91   !important;
   color: white !important;
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-light-blue {
   background-color: #767191 !important;
   color: white !important;
   font-family: Nutmeg-Light !important;
   }
   .small-box.bg-black {
   background-color: #9388cf !important;
   color: white !important;
   font-family: Nutmeg-Light !important;
   }

                        ")),
        tags$footer(style = "
              position:fixed;
              bottom:0px;
              width:100%;
              height:20px;   /* Height of the footer */
              color: black;
              padding: 0px;
              background-color: #a87bc9;
              z-index: 100;"),
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

      navbarPage(
      # title=img(src="https://raw.githubusercontent.com/Igualdad-Jalisco/Indicadores_AVGM/main/Logo%20AVGM.png", width = "300", height="40"),

                 header= busy_start_up(
                   loader = spin_epic("flower", color = "#8F5199"),
                   text = "Cargando",
                   timeout = 1500,
                   color = "#8F5199",
                   background = " white"),
                 useShinydashboard(),


                 # Inicio -----------------------------------------------------------------------

                 tabPanel(title=p("Inicio", style ="font-weight:bold; color: #ffffff;"), id="Inicio",
                   div(class="row d-flex", #Replicar
                     valueBox("Micrositio de datos de los Indicadores de la AVGM",
                              subtitle = HTML(
                                # <div style='font-size: 16px; color: #333; line-height: 1.5;'>
                                "<ul>
                                 <li>Visualiza: Cada indicador incluye una tabla de resumen y un gráfico que presenta el avance y comportamiento</li>
                                 <li>Descarga: De manera periodica podrá descargar en formato de excel, los datos con los cuales se calculan los indicadores</li>
                                 <li>Monitorea: Utiliza y conoce el avance de los indicadores en cada uno de sus objetivos </li>
                               </ul>"
                              ), width = 12, color="black")),
                   column(width = 7,
                   p("Micrositio de datos de la AVGM:", style = "color:black; font-weight:bold; font-size: 18px; text-align: justify"),

                   p(style="color:black; font-weight:regular; font-size:13px; text-align: justify",
                     "Aquí se encuentran los datos que alimentan los indicadores de la AVGM. En cada apartado de arriba se presentan las distintas temáticas que se monitorea en la Alerta."),
                   tags$ul(
                     tags$li(style = "text-align: justify; font-size:13px", "Servicios Forense"),
                     tags$li(style = "text-align: justify; font-size:13px", "Órdenes y medidas de protección"),
                     tags$li(style = "text-align: justify; font-size:13px", "Salud ILE/IVE"),
                     tags$li(style = "text-align: justify; font-size:13px", "Feminicidios"),
                     tags$li(style = "text-align: justify; font-size:13px", "Protocolo ALBA")
                   ),
                   p(style="color:black; font-weight:regular; font-size:13px; text-align: justify", "Asimismo, en cada apartado puede localizar una sección de filtros, donde se incluyen variables para desagregar la información, la cual se refleja en las visualizaciones."),
                   br(),
                   p("Sobre los insumos que se localizan en cada sección:", style = "color:black; font-weight:bold; font-size:13px; text-align: justify"),
                   # Lista con bullets
                   tags$ul(
                     tags$li(style = "text-align: justify; font-size:13px", "Gráficos descargables en formato jpg"),
                     tags$li(style = "text-align: justify; font-size:13px", "Tabulados que se exportan a excel"),
                     tags$li(style = "text-align: justify; font-size:13px", "Mapas de la entidad con desagregación municipal"),
                     br()
                     )),

                   # Hipervínculo
                   box(width = 5, style='font-size: 13px; color: #333; line-height: 1.5;',status = "primary", solidHeader = TRUE,
                       br(),
                       br(),

                   p(style = "color:black; font-weight:regular; font-size:13px; text-align: justify", "Recuerda que, además de descargar los datos y las visualizaciones en cada sección del micrositio, también puedes acceder al código abierto y a los resultados de los principales indicadores:"),
                   br(),
                   p(style = "color:black; font-weight:bold; font-size:16px; text-align: justify", "Descarga:"),
                   tags$ul(tags$li(tags$a(href = "//rstudio-pubs-static.s3.amazonaws.com/1198090_bd70156662bb4056afaab86f565c149e.html", "Reporte de los principales indicadores de la AVGM"))),
                   tags$ul(tags$li(tags$a(href = "https://github.com/Igualdad-Jalisco/Indicadores_AVGM", "Código para replicar los cálculos de los principales indicadores"))),
                   tags$ul(tags$li(tags$a(href = "https://github.com/Igualdad-Jalisco/App-AVGM", "Código abierto del micrositio"))),
                   br()

                   )),


                 # Servicios forenses------------------------------------------------------------

                 tabPanel(title = p("Servicios forenses",  style ="font-weight:bold; color: #ffffff;"),
                          box(
                            width=12,
                            div(class="row d-flex", #Replicar
                                valueBox(
                                  value = paste(ijcf_box_1[1,1], "mujeres víctimas"),
                                  subtitle = paste0("de muertes violentas en el período de 2020 a 2024 (",  tolower(mes_final_ijcf[1]),  "). La mayor proporción es por muertes clasificadas como ", tolower(ijcf_box_1_1[1,1]), " con el ", ijcf_box_1_1[1,3], " (", ijcf_box_1_1[1,2],")."),#, seguido de muerte de tipo ", tolower(ijcf_box_1_1[2,1]), " con el ", ijcf_box_1_1[2,3], " (", ijcf_box_1_1[2,2],")."),
                                  icon=icon("wave-square"), color="fuchsia", width = 4),

                                valueBox(
                                  value = paste("De enero a", tolower(mes_final_ijcf[1]), "del", as.numeric(ijcf_box_2[5,1])),
                                  subtitle =  paste0("Se registran mujeres víctimas de muertes violentas ", as.numeric(ijcf_box_2[5,2]), ", una variación anual de ",  ijcf_box_2[5,3], "."),# El año anterior, ", as.numeric(ijcf_box_2[4,1]), " se presenta una variación de ",ijcf_box_2[4,3], " (", as.numeric(ijcf_box_2[4,2]), ")."),
                                  icon=icon("chart-area"),color="purple", width = 4),

                                valueBox(
                                  value = "Los municipios",
                                  subtitle =  paste("donde se registran la mayor proporción de víctimas mujeres de muertes violentas", ijcf_box_3[1,1], " con el ", ijcf_box_3[1,3], ", seguido de",
                                                    ijcf_box_3[2,1], "con el", ijcf_box_3[2,3], "y",ijcf_box_3[3,1], "con el", ijcf_box_3[3,3], "."),
                                  icon=icon("equals"), color="maroon", width = 4))),
                          

                          conditionalPanel(condition = "input.toggle1.length > 0 || input.toggle2.length > 0",htmlOutput("texto_ijcf_1")),
                          
                          hr(),
                          
                          tabsetPanel(
                            tabPanel("Perfil de las víctimas",

                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(width = 3, 
                                                    
                                                    div(
                                                      style = "margin-bottom: 10px;",
                                                      span(HTML("<b>Diccionario de la sección:</b>"))
                                                    ),
                                                    awesomeCheckboxGroup(
                                                      inputId = "toggle1",
                                                      label = NULL,
                                                      choices = list("Mostrar/Ocultar" = 1)
                                                    ),
                                                    
                                                    "\nSeleccione variables a filtrar",
                                                    
                                                    selectInput(
                                                      inputId = "perfil_fecha",
                                                      label = "Seleccione el año",
                                                      choices = unique(sort(as.factor(perfil_victima$Año))),
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
                                                      choices = unique(sort(str_to_sentence(servicios$`Posible causa de muerte (y/o mecanismo de muerte)`))),
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
                                                 
                                                 plotOutput("gr1"), br(), hr(),

                                                 fluidRow(
                                                   splitLayout(cellWidths = c("50%", "50%"),
                                                               plotOutput("mapa_ijcf"),
                                                               dataTableOutput("t1")
                                                   )),
                                                 # h6("Fuente: Datos proporcionados por IJCF." ,br())
                                                 ))),
                            tabPanel("Servicios forenses",
                                     br(),

                                     sidebarPanel(width = 3, 
                                                  
                                                  div(
                                                    style = "margin-bottom: 10px;",
                                                    span(HTML("<b>Diccionario de la sección: </b>"))
                                                  ),
                                                  awesomeCheckboxGroup(
                                                    inputId = "toggle2",
                                                    label = NULL,
                                                    choices = list("Mostrar/Ocultar" = 1)
                                                  ),
                                                  
                                                  "\nSeleccione variables a filtrar",
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
                                                    choices = unique(sort(str_to_sentence(servicios$`Posible causa de muerte (y/o mecanismo de muerte)`))),
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
                                               br(),
                                               dataTableOutput("t2"),
                                               h5("Fuente: Datos proporcionados por IJCF."))

                            ))),

                 tabPanel(title = p("Órdenes y medidas", style ="font-weight:bold; color: #ffffff;"),
                          box(
                            width=12,
                            div(class="row d-flex", #Replicar
                                valueBox("2024", "Se otorgaron 162 órdenes y 10,200 órdenes de protección.", icon=icon("equals"), color="fuchsia", width = 3),
                                valueBox("2023", "Se otorgaron 436 órdenes y 33,111 órdenes de protección.", icon=icon("signal"), color="purple", width = 3),
                                valueBox("2022", "Se otorgaron 244 órdenes y 26,630 órdenes de protección.", icon=icon("wave-square"), color="maroon", width = 3),
                                valueBox("2021", "Se otorgaron 359 órdenes y 21,452 órdenes de protección.", icon=icon("signal"), color="light-blue", width = 3)),
                            
                            conditionalPanel(condition = "input.toggle_medidas_ordenes.length > 0",htmlOutput("texto_medidas_ordenes_1")),
                            hr(),
                            tabPanel("Órdenes y medidas de protección",  class="mb-2",
                                
                                     sidebarPanel(width=3, 
                                                  
                                                  div(
                                                    style = "margin-bottom: 10px;",
                                                    span(HTML("<b>Diccionario de la sección:</b>"))
                                                  ),
                                                  awesomeCheckboxGroup(
                                                    inputId = "toggle_medidas_ordenes",
                                                    label = NULL,
                                                    choices = list("Mostrar/Ocultar" = 1)
                                                  ),
                                                  
                                                  "Seleccione algunas variables a filtrar", class=".mb-2",
                                                  selectInput(
                                                    inputId = "medidas_año",
                                                    label = "Año",
                                                    choices = unique(sort(medidas_ordenes_municipal$año)),
                                                    multiple =T,
                                                    selected = c(2020,2021,2022,2023,2024)
                                                  ),
                                                  selectInput(
                                                    inputId = "medidas_mes",
                                                    label = "Mes",
                                                    choices = unique(sort(medidas_ordenes_municipal$mes)),
                                                    multiple = TRUE
                                                  ),
                                                  selectInput(
                                                    inputId = "medidas_municipio",
                                                    label = "Municipio",
                                                    choices = c("Jalisco", unique(sort(medidas_ordenes_municipal$municipio))),
                                                    multiple = F,
                                                    selected = "Jalisco"
                                                  ),
                                                  selectInput(
                                                    inputId = "medidas_tipo",
                                                    label = "Órdenes o medidas de protección",
                                                    choices = unique(sort(medidas_ordenes_municipal$tipo)),
                                                    multiple = F,
                                                    selected = "Medidas"
                                                  ),
                                                  downloadButton(
                                                    outputId = "download_gr_medidas",
                                                    label = "Descarga el gráfico"
                                                  ),
                                                  downloadButton(
                                                    "download_d_medidas",
                                                    "Descarga los datos")
                                     ),
                                     mainPanel(width=9,
                                               plotOutput("grafico_medidas"),
                                               hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("60%", "40%"),
                                                             plotOutput("mapa_medidas"),
                                                             dataTableOutput("tabla_medidas")
                                                 )))
                            ))),


                 tabPanel(title = p("Salud (ILE / IVE)", style ="font-weight:bold; color: #ffffff;"),
                          box(
                            width=12,
                            div(class="row d-flex", #Replicar
                                valueBox("Durante 2024", "Se registran 1,159 niñas y adolescentes que denuncian abuso sexual, de las cuales 310 son remitidas a Salud",  icon=icon("wave-square"), color="fuchsia", width = 4), # actualizar
                                valueBox("104 víctimas de violación", "han sido remitidas para atención integran conforme a la NOM 046 durante 2024", icon=icon("chart-area"),color="purple", width = 4), # actualizar
                                valueBox("Al cierre de 2023", "se registran 504 mujeres y 816 niñas y adolescentes víctimas de violación y abuso sexual remitidas a salud", icon=icon("equals"), color="maroon", width = 4))), # actualizar
                          hr(),

                          tabsetPanel(
                            tabPanel(title = "Atenciones por delitos sexual",
                                     br(),
                                     conditionalPanel(condition = "input.toggle17.length > 0", htmlOutput("texto_salud_17_1")),
                                     br(),
                                     
                                       sidebarPanel(width = 3, 
                                                    
                                                    div(
                                                      style = "margin-bottom: 10px;",
                                                      span(HTML("<b>Diccionario de la sección:</b>"))
                                                    ),
                                                    awesomeCheckboxGroup(
                                                      inputId = "toggle17",
                                                      label = NULL,
                                                      choices = list("Mostrar/Ocultar" = 1)
                                                    ),
                                                    
                                                    "Seleccione algunas variables a filtrar", class=".mb-2",
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

                                                 h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())),

                            tabPanel(title="Atenciones por violencia familiar y/o sexual",
                                     br(),
                                     conditionalPanel(condition = "input.toggle20.length > 0", htmlOutput("texto_salud_20_1")),
                                     br(),
                                     sidebarPanel(width = 3, 
                                                  
                                                  div(
                                                    style = "margin-bottom: 10px;",
                                                    span(HTML("<b>Diccionario de la sección:</b>"))
                                                  ),
                                                  awesomeCheckboxGroup(
                                                    inputId = "toggle20",
                                                    label = NULL,
                                                    choices = list("Mostrar/Ocultar" = 1)
                                                  ),
                                                  
                                                  "Seleccione algunas variables a filtrar", class=".mb-2",
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



                            tabPanel(title = "Solicitudes de IVE por violación",
                                     br(),
                                     conditionalPanel(condition = "input.toggle18.length > 0", htmlOutput("texto_salud_18_1")),
                                     br(),
                                     sidebarPanel(width=3,
                                                  div(
                                                    style = "margin-bottom: 10px;",
                                                    span(HTML("<b>Diccionario de la sección:</b>"))
                                                  ),
                                                  awesomeCheckboxGroup(
                                                    inputId = "toggle18",
                                                    label = NULL,
                                                    choices = list("Mostrar/Ocultar" = 1)
                                                  ),
                                                  
                                                  "Seleccione algunas variables a filtrar", class=".mb-2",
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
                                                 h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())),

                            tabPanel(title = "Procedimientos de ILE", 
                                     br(),
                                     conditionalPanel(condition = "input.toggle19.length > 0", htmlOutput("texto_salud_19_1")),
                                     br(),
                                     sidebarPanel(width=3,
                                                  div(
                                                    style = "margin-bottom: 10px;",
                                                    span(HTML("<b>Diccionario de la sección:</b>"))
                                                  ),
                                                  awesomeCheckboxGroup(
                                                    inputId = "toggle19",
                                                    label = NULL,
                                                    choices = list("Mostrar/Ocultar" = 1)
                                                  ),
                                                  
                                                  "Seleccione algunas variables a filtrar", class=".mb-2",
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
                                                   h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())),


                            tabPanel(title = "Personal y establecimientos ILE/IVE", 
                                     box(width = 12,
                                     conditionalPanel(condition = "input.toggle22.length > 0", htmlOutput("texto_salud_22_1")),
                                     # br(),
                                     sidebarLayout(
                                     sidebarPanel(width = 3, 
                                                  
                                                  div(
                                                    style = "margin-bottom: 10px;",
                                                    span(HTML("<b>Diccionario de la sección:</b>"))
                                                  ),
                                                  awesomeCheckboxGroup(
                                                    inputId = "toggle22",
                                                    label = NULL,
                                                    choices = list("Mostrar/Ocultar" = 1)
                                                  ),
                                                  
                                                  "Seleccione algunas variables a filtrar", class=".mb-2",
                                            selectInput(
                                              inputId = "ind_22_año",
                                              label = "Seleccione el año",
                                              choices = unique(sort(indicador_22$Año)),
                                              multiple = T
                                              ),
                                     selectInput(
                                       inputId = "ind_22_formación",
                                       label = "Seleccione la formación del personal",
                                       choices = unique(sort(str_to_title(indicador_22$Formación))),
                                       multiple = TRUE
                                       )),
                                     mainPanel(width = 9,
                                               h4("Personal de salud relacionado al procedimiento ILE/IVE, capacitado en el Programa ILE y NOM 046", style ="font-weight:bold;"),
                                               dataTableOutput("t_22"),
                                               h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco.")))),
                                     
                                     box(width=12,
                                         # br(),
                                         # hr(),
                                       conditionalPanel(condition = "input.toggle21.length > 0", htmlOutput("texto_salud_21_1")),
                                       # br(),
                                     sidebarLayout(
                                       sidebarPanel(width = 3, 
                                                    div(
                                                      style = "margin-bottom: 10px;",
                                                      span(HTML("<b>Diccionario de la sección:</b>"))
                                                    ),
                                                    awesomeCheckboxGroup(
                                                      inputId = "toggle21",
                                                      label = NULL,
                                                      choices = list("Mostrar/Ocultar" = 1)
                                                    ),
                                                    
                                                    "Seleccione algunas variables a filtrar", class=".mb-2",
                                       
                                        selectInput(
                                          inputId = "ind_21_año",
                                          label = "Seleccione el año",
                                          choices = unique(sort(indicador_21$Año)),
                                          multiple = T
                                          ),
                                        selectInput(
                                             inputId = "ind_21_establecimiento",
                                             label = "Seleccione el hospital",
                                             choices = unique(sort(str_to_title(indicador_21$Establecimiento))),
                                             multiple = TRUE)
                                        ),
                                        mainPanel(width=9,
                                                  h4("Establecimientos estatales proveedores de servicios de salud en condiciones óptimas para realizar un procedimiento ILE/IVE", style ="font-weight:bold;"),
                                                  dataTableOutput("t_21"),
                                                  h6("Fuente: Datos proporcionados por Secretaría de Salud y OPD Servicios de Salud Jalisco."), br())))))),


# Delitos por razón de género---------------------------------------------------
            tabPanel(title = p("Feminicidio", style ="font-weight:bold; color: #ffffff;"),
                     box(width=12,
                        div(class="row d-flex", #Replicar
                            valueBox("6,028 atenciones", "realizadas a mujeres en los CJM durante 2024", icon=icon("equals"),color="fuchsia", width = 4), # actualizar
                            valueBox("3,123 denuncias", "por violencia familiar realizadas por mujeres en los distintos CJM durante 2024", icon=icon("wave-square"),color="purple", width = 4), # actualizar
                            valueBox("141 denuncias por violación","realizadas por mujeres en los CJM durante 2024",  icon=icon("ellipsis"), color="maroon", width = 4))), # actualizar
                        
                     conditionalPanel(condition = "input.toggle_feminicidio.length > 0",htmlOutput("texto_feminicidio_1")),
                     
                     hr(),
                     
                     tabsetPanel(
                       tabPanel(title = "Opiniones técnicas",
                                br(),
                                # sidebarLayout(
                                  sidebarPanel(width = 3, 
                                               
                                               div(
                                                 style = "margin-bottom: 10px;",
                                                 span(HTML("<b>Diccionario de la sección:</b>"))
                                               ),
                                               awesomeCheckboxGroup(
                                                 inputId = "toggle_feminicidio",
                                                 label = NULL,
                                                 choices = list("Mostrar/Ocultar" = 1)
                                               ),
                                               
                                               "\nSeleccione variables a filtrar",
                                               
                                  # sidebarPanel(width = 3, "Seleccione algunas características", class=".mb-2",
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
                                            h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()),

           tabPanel(title = "Denuncias en los CJM",
                    br(),
                    # sidebarLayout(
                    sidebarPanel(width = 3, 
                                 
                                 div(
                                   style = "margin-bottom: 10px;",
                                   span(HTML("<b>Diccionario de la sección:</b>"))
                                 ),
                                 awesomeCheckboxGroup(
                                   inputId = "toggle_feminicidio",
                                   label = NULL,
                                   choices = list("Mostrar/Ocultar" = 1)
                                 ),
                                 
                                 "\nSeleccione variables a filtrar",
                                 
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
                                   selectInput(
                                     inputId = "ind_28_municipio",
                                     label = "Seleccione el municipio sede del CJF",
                                     choices = unique(sort(indicador_28$Municipio)),
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
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()),



           tabPanel(title = "Delitos judicializados",
                    br(),
                    # sidebarLayout(
                    sidebarPanel(width = 3, 
                                 
                                 div(
                                   style = "margin-bottom: 10px;",
                                   span(HTML("<b>Diccionario de la sección:</b>"))
                                 ),
                                 awesomeCheckboxGroup(
                                   inputId = "toggle_feminicidio",
                                   label = NULL,
                                   choices = list("Mostrar/Ocultar" = 1)
                                 ),
                                 
                                 "\nSeleccione variables a filtrar",
                                 
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
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()),




           tabPanel(title = "Sentencias por feminicidio",
                    br(),
                    # sidebarLayout(
                    sidebarPanel(width = 3, 
                                 
                                 div(
                                   style = "margin-bottom: 10px;",
                                   span(HTML("<b>Diccionario de la sección:</b>"))
                                 ),
                                 awesomeCheckboxGroup(
                                   inputId = "toggle_feminicidio",
                                   label = NULL,
                                   choices = list("Mostrar/Ocultar" = 1)
                                 ),
                                 
                                 "\nSeleccione variables a filtrar",
                                 
                                   selectInput(
                                     inputId = "ind_30_año",
                                     label = "Seleccione el año",
                                     choices = unique(sort(indicador_30$`Año de origen de la carpeta de investigación`)),
                                     multiple = T
                                   ),
                                   # selectInput(
                                   #   inputId = "ind_30_mes",
                                   #   label = "Seleccione el mes",
                                   #   choices = unique(sort(indicador_30$Mes)),
                                   #   multiple = TRUE
                                   # ),
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
                                # dataTableOutput("t_30"),
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()),


           tabPanel(title = "Muertes violentas",
                    br(),
                    # sidebarLayout(
                    sidebarPanel(width = 3, 
                                 
                                 div(
                                   style = "margin-bottom: 10px;",
                                   span(HTML("<b>Diccionario de la sección:</b>"))
                                 ),
                                 awesomeCheckboxGroup(
                                   inputId = "toggle_feminicidio",
                                   label = NULL,
                                   choices = list("Mostrar/Ocultar" = 1)
                                 ),
                                 
                                 "\nSeleccione variables a filtrar",
                                 
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
                                h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco.")), br()))),


           # - Protocolo alba---------------------------------------------------

           tabPanel(title = p("Protocolo Alba", style ="font-weight:bold; color: #ffffff;"),
                    box(
                      width=12,
                      div(class="row d-flex", #Replicar
                          valueBox("484 desaparciones", "de niñas, adolescentes y mujeres en el año 2023",  icon=icon("wave-square"), color="fuchsia", width = 4), # actualizar
                          valueBox("Durante 2023", "Se emitieron 30 cédulas de difusión y 16 en estatus de activas", icon=icon("chart-area"),color="purple", width = 4), # actualizar
                          valueBox("173 casos activados", " de conformodida con la normatividad con el Protocolo de la Alerta Amber, en 2023", icon=icon("equals"), color="maroon", width = 4))), # actualizar
                    
                    conditionalPanel(condition = "input.toggle_protocolo.length > 0",htmlOutput("texto_protocolo_1")),
                    
                    hr(),
                    tabsetPanel(
                      tabPanel(title = "Cédulas únicas de difusión",

                               br(),
                               # sidebarLayout(
                               sidebarPanel(width = 3, 
                                            
                                            div(
                                              style = "margin-bottom: 10px;",
                                              span(HTML("<b>Diccionario de la sección:</b>"))
                                            ),
                                            awesomeCheckboxGroup(
                                              inputId = "toggle_protocolo",
                                              label = NULL,
                                              choices = list("Mostrar/Ocultar" = 1)
                                            ),
                                            
                                            "\nSeleccione variables a filtrar",
                                            
                                                selectInput(
                                                  inputId = "ind_32_año",
                                                  label = "Seleccione el año",
                                                  choices = unique(sort(indicador_32$Año)),
                                                  multiple = T
                                                ),
                                                selectInput(
                                                  inputId = "ind_32_mes",
                                                  label = "Seleccione el mes",
                                                  choices = unique(sort(indicador_32$Mes)),
                                                  multiple = TRUE
                                                ),
                                                selectInput(
                                                  inputId = "ind_32_municipio",
                                                  label = "Selecciona el municipio",
                                                  choices = unique(sort(indicador_32$Municipio)),
                                                  multiple = FALSE,
                                                  selected = "Estado de Jalisco"
                                                ),
                                                downloadButton(
                                                  outputId = "download_gr32",
                                                  label = "Descarga el gráfico"
                                                ),
                                                downloadButton(
                                                  outputId = "download_d32",
                                                  label = "Descarga los datos")),

                                   mainPanel(width=9,
                                             plotOutput("gr32"),
                                             br(),br(),br(),
                                             dataTableOutput("t_32"),
                                             h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."), br())),



                      tabPanel(title = "Cédulas únicas remitidas",
                               br(),
                               # sidebarLayout(
                               sidebarPanel(width = 3, 
                                            
                                            div(
                                              style = "margin-bottom: 10px;",
                                              span(HTML("<b>Diccionario de la sección:</b>"))
                                            ),
                                            awesomeCheckboxGroup(
                                              inputId = "toggle_protocolo",
                                              label = NULL,
                                              choices = list("Mostrar/Ocultar" = 1)
                                            ),
                                            
                                            "\nSeleccione variables a filtrar",
                                                selectInput(
                                                  inputId = "ind_33_año",
                                                  label = "Seleccione el año",
                                                  choices = unique(sort(indicador_33$Año)),
                                                  multiple = T
                                                ),
                                                selectInput(
                                                  inputId = "ind_33_mes",
                                                  label = "Seleccione el mes",
                                                  choices = unique(sort(indicador_33$Mes)),
                                                  multiple = TRUE
                                                ),
                                                selectInput(
                                                  inputId = "ind_33_edad",
                                                  label = "Selecciona el rango de edad",
                                                  choices = unique(sort(indicador_33$Edad)),
                                                  multiple = TRUE),
                                                downloadButton(
                                                  outputId = "download_gr33",
                                                  label = "Descarga el gráfico"
                                                ),
                                                downloadButton(
                                                  outputId = "download_d33",
                                                  label = "Descarga los datos")
                                   ),
                                   mainPanel(width=9,
                                             plotOutput("gr33"),
                                             br(),br(),br(),
                                             dataTableOutput("t_33"),
                                             h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."), br())),



                      tabPanel(title = "Casos de búsqueda y localización",
                               br(),
                               # sidebarLayout(
                               sidebarPanel(width = 3, 
                                            
                                            div(
                                              style = "margin-bottom: 10px;",
                                              span(HTML("<b>Diccionario de la sección:</b>"))
                                            ),
                                            awesomeCheckboxGroup(
                                              inputId = "toggle_protocolo",
                                              label = NULL,
                                              choices = list("Mostrar/Ocultar" = 1)
                                            ),
                                            
                                            "\nSeleccione variables a filtrar",
                                                selectInput(
                                                  inputId = "ind_34_año",
                                                  label = "Seleccione el año",
                                                  choices = unique(sort(indicador_34$Año)),
                                                  multiple = T
                                                ),
                                                selectInput(
                                                  inputId = "ind_34_mes",
                                                  label = "Seleccione el mes",
                                                  choices = unique(sort(indicador_34$Mes)),
                                                  multiple = TRUE
                                                ),
                                                selectInput(
                                                  inputId = "ind_34_edad",
                                                  label = "Selecciona el rango de edad",
                                                  choices = unique(sort(indicador_34$`Rango de edad`)),
                                                  multiple = TRUE
                                                ),
                                                downloadButton(
                                                  outputId = "download_gr34",
                                                  label = "Descarga el gráfico"
                                                ),
                                                downloadButton(
                                                  outputId = "download_d34",
                                                  label = "Descarga los datos")),
                                   mainPanel(width=9,
                                             plotOutput("gr34"),
                                             br(),br(),br(),
                                             dataTableOutput("t_34"),
                                             h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."), br())),

                      tabPanel(title = "Cédulas de Difusión activas",
                               br(),
                               # sidebarLayout(
                               sidebarPanel(width = 3, 
                                            
                                            div(
                                              style = "margin-bottom: 10px;",
                                              span(HTML("<b>Diccionario de la sección:</b>"))
                                            ),
                                            awesomeCheckboxGroup(
                                              inputId = "toggle_protocolo",
                                              label = NULL,
                                              choices = list("Mostrar/Ocultar" = 1)
                                            ),
                                            
                                            "\nSeleccione variables a filtrar",
                                                selectInput(
                                                  inputId = "ind_35_año",
                                                  label = "Seleccione el año",
                                                  choices = unique(sort(indicador_35$Año)),
                                                  multiple = T
                                                ),
                                                selectInput(
                                                  inputId = "ind_35_mes",
                                                  label = "Seleccione el mes",
                                                  choices = unique(sort(indicador_35$Mes)),
                                                  multiple = TRUE
                                                ),
                                                downloadButton(
                                                  outputId = "download_gr35",
                                                  label = "Descarga el gráfico"
                                                ),
                                                downloadButton(
                                                  outputId = "download_d35",
                                                  label = "Descarga los datos")),

                                   mainPanel(width=9,
                                             plotOutput("gr35"),
                                             br(),br(),br(),
                                             dataTableOutput("t_35"),
                                             h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."), br())),


                      tabPanel(title = "Casos de Reporte Amber",
                               br(),
                               # sidebarLayout(
                               sidebarPanel(width = 3, 
                                            
                                            div(
                                              style = "margin-bottom: 10px;",
                                              span(HTML("<b>Diccionario de la sección:</b>"))
                                            ),
                                            awesomeCheckboxGroup(
                                              inputId = "toggle_protocolo",
                                              label = NULL,
                                              choices = list("Mostrar/Ocultar" = 1)
                                            ),
                                            
                                            "\nSeleccione variables a filtrar",
                                                selectInput(
                                                  inputId = "ind_36_año",
                                                  label = "Seleccione el año",
                                                  choices = unique(sort(indicador_36$Año)),
                                                  multiple = T
                                                ),
                                                selectInput(
                                                  inputId = "ind_36_mes",
                                                  label = "Seleccione el mes",
                                                  choices = unique(sort(indicador_36$Mes)),
                                                  multiple = TRUE
                                                ),
                                                selectInput(
                                                  inputId = "ind_36_edad",
                                                  label = "Selecciona el rango de edad",
                                                  choices = unique(sort(indicador_36$`Rango de edad`)),
                                                  multiple = TRUE),
                                                downloadButton(
                                                  outputId = "download_gr36",
                                                  label = "Descarga el gráfico"
                                                ),
                                                downloadButton(
                                                  outputId = "download_d36",
                                                  label = "Descarga los datos")
                                   ),

                                   mainPanel(width=9,
                                             plotOutput("gr36"),
                                             br(),br(),br(),
                                             dataTableOutput("t_36"),
                                             h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."), br())),


                      tabPanel(title = "Personal capacitado en el Protocolo Alba",
                               br(),
                               # sidebarLayout(
                               sidebarPanel(width = 3, 
                                            
                                            div(
                                              style = "margin-bottom: 10px;",
                                              span(HTML("<b>Diccionario de la sección:</b>"))
                                            ),
                                            awesomeCheckboxGroup(
                                              inputId = "toggle_protocolo",
                                              label = NULL,
                                              choices = list("Mostrar/Ocultar" = 1)
                                            ),
                                            
                                            "\nSeleccione variables a filtrar",
                                                selectInput(
                                                  inputId = "ind_37_año",
                                                  label = "Seleccione el año",
                                                  choices = unique(sort(indicador_37$Año)),
                                                  multiple = T
                                                ),
                                                selectInput(
                                                  inputId = "ind_37_mes",
                                                  label = "Seleccione el mes",
                                                  choices = unique(sort(indicador_37$Mes)),
                                                  multiple = TRUE
                                                ),
                                                selectInput(
                                                  inputId = "ind_37_personal",
                                                  label = "Seleccione la función del personal",
                                                  choices = unique(sort(indicador_37$Personal)),
                                                  multiple = TRUE
                                                ),
                                                downloadButton(
                                                  outputId = "download_d37",
                                                  label = "Descarga los datos")),
                                   mainPanel(width=9,
                                             dataTableOutput("t_37"),
                                             h6("Fuente: Datos proporcionados por Fiscalía del Estado de Jalisco."), br())),

           )))


)))


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


 # DICCIONARIO-------------------------------------------------------------
  output$texto_ijcf_1 <- renderUI({
    texto_ijcf()
  })
  
  
  output$texto_medidas_ordenes_1 <- renderUI({
    texto_medidas_ordenes()
  })
  
  output$texto_salud_17_1 <- renderUI({
    texto_salud_17()
  })
  output$texto_salud_20_1 <- renderUI({
    texto_salud_20()
  })  
  
  output$texto_salud_18_1 <- renderUI({
    texto_salud_18()
  })  
  
  output$texto_salud_19_1 <- renderUI({
    texto_salud_19()
  })  
  
  output$texto_salud_22_1 <- renderUI({
    texto_salud_22()
  })  
  
  output$texto_salud_21_1 <- renderUI({
    texto_salud_21()
  })  
  
  output$texto_feminicidio_1 <- renderUI({
    texto_feminicidio()
  })  
  
  output$texto_protocolo_1<- renderUI({
    texto_protocolo()
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
             title = paste("Total de muertes de mujeres en", input$perfil_municipio, ""),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
             caption=paste("\nFecha de consulta", Sys.Date(), ".",
                           "\n\nElaboración propia con base a los datos recopilados por el IJCF con corte a febrero 2024. | Micrositio de datos de la AVGM, SISEMH."))+
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
             title = paste("Total de muertes de mujeres en", input$perfil_municipio, ""),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
             caption=paste("\nFecha de consulta", Sys.Date(), ".",
             "\n\nElaboración propia con base a los datos recopilados por el IJCF con corte a febrero 2024. | Micrositio de datos de la AVGM, SISEMH."))+
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
      labs(x="", y="", fill="", color="",
           title = paste("Total de muertes de mujeres en", input$perfil_municipio, ""),
           subtitle = paste("Datos a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
           caption=paste("\nFecha de consulta", Sys.Date(), ".",
                         "\nElaboración propia con base a los datos 
                         recopilados por el IJCF con corte a febrero 2024 | Micrositio de datos de la AVGM, SISEMH."))+
      # labs(caption=NULL, fill="Total", x=NULL, y=NULL,
      #      title = latex2exp::TeX(paste("Mapa: \\textbf{Total de mujeres víctimas de muertes}")),
      #      subtitle = paste0("Datos correspondientes a ", input$perfil_municipio, ", ", min(perfil_reactive()$fecha), " a ", max(perfil_reactive()$fecha))) +
      scale_fill_gradient(
        low = "#e9e8eb",
        high = "#857baa",
        guide = "colourbar",
        label=comma)+ theme_1+
      theme(text=element_text(family = gt::google_font("Poppins")),
           plot.title = element_text(family = gt::google_font("Poppins"),
                                     face = "bold",
                                     size = 11,
                                     hjust = 0),
           plot.subtitle = element_text(family = gt::google_font("Poppins"),
                                        size = 9,
                                        hjust = 0,
                                        colour = "grey40"),
           plot.caption = element_text(family = gt::google_font("Poppins"),
                                       size = 8,
                                       colour = "grey40"),
           legend.title = element_text(family = gt::google_font("Poppins"),
                                       face = "bold",
                                       size = 8,
                                       colour = "black",
                                       hjust = 1),
           legend.title.align = 0.5,
           legend.text = element_text(family = gt::google_font("Poppins"),
                                      # face = "bold",
                                      size = 7,
                                      colour = "black",
                                      hjust = 1),
           legend.text.align = 0.5,
           legend.key.size = unit(15, "pt"))

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
               title = paste("Total de muertes de mujeres en", input$perfil_municipio, ""),
               subtitle = paste("Datos correspondientes a la fecha del período:", min(perfil_reactive()$fecha), "a ", max(perfil_reactive()$fecha)),
               caption=paste("\nFecha de consulta", Sys.Date(), ".",
                             "\nElaboración propia con base a los datos 
                         recopilados por el IJCF con corte a febrero 2024 | Micrositio de datos de la AVGM, SISEMH."))+
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
                       lengthMenu = list(c(5,1,5,10,20, 100),
                                         c(5,1,5,10,20, 100)),
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
             title = paste("Total de servicios forenses en muertes de mujeres en", input$servicio_municipio),# perfil_reactive()$`Municipio de suceso`[1], "}")),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(servicio_reactive()$fecha), "a ", max(servicio_reactive()$fecha),
                              "\nLa línea morada representa al total de víctimas mujeres de muertes."),
             caption=paste("\nFecha de consulta ", Sys.Date(),
           "\nElaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH."))+
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
             title = paste("Total de servicios forenses en muertes de mujeres en", input$servicio_municipio),# perfil_reactive()$`Municipio de suceso`[1], "}")),
             subtitle = paste("Datos correspondientes a la fecha del período:", min(servicio_reactive()$fecha), "a ", max(servicio_reactive()$fecha),
                              "\nLa línea morada representa al total de víctimas mujeres de muertes."),
             caption=paste("\nFecha de consulta ", Sys.Date(),
                           "\nElaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH."))+
        theme_minimal()+
        theme_1 + 
        theme(legend.position = "none",
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
      group_by(Año,  Servicios) %>%
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
                       lengthMenu = list(c(5,1,5,10,20, 100),
                                         c(5,1,5,10,20, 100)),
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
               title = paste("Total de servicios forenses en muertes de mujeres en", input$servicio_municipio),# perfil_reactive()$`Municipio de suceso`[1], "}")),
               subtitle = paste("Datos correspondientes a la fecha del período:", min(servicio_reactive()$fecha), "a ", max(servicio_reactive()$fecha),
                                "\nLa línea morada representa al total de víctimas mujeres de muertes."),
               caption=paste("\nFecha de consulta ", Sys.Date(),
                             "\nElaboración propia con base a los datos recopilados por el IJCF | Micrositio de datos de la AVGM, SISEMH."))+
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
      scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
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
           title = paste("Total de atenciones por violación y abuso sexual infantil en el Sector Salud"),
           subtitle = paste("Datos correspondientes al total de mujeres y niñas atendidas que son referidas a la Fiscalía del Estado"),
           caption=paste("\nFecha de consulta: ", Sys.Date(),
                         "\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco a marzo 2024| Micrositio de datos de la AVGM, SISEMH."))+
      scale_size_continuous(range = c(8,15))+
      scale_color_manual(
        values = c(
          `Abuso sexual infantil` = "#857baa",
          `Violación` = "#8c5991"))+
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
               title = paste("Total de atenciones por violación y abuso sexual infantil en el Sector Salud"),
               subtitle = paste("Datos correspondientes al total de mujeres y niñas atendidas que son referidas a la Fiscalía del Estado"),
               caption=paste("\nFecha de consulta: ", Sys.Date(),
                             "\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco a marzo 2024| Micrositio de datos de la AVGM, SISEMH."))+
          
          scale_size_continuous(range = c(8,15))+
          # scale_size_continuous(range = c(8,17))+
          scale_color_manual(
            values = c(
              `Abuso sexual infantil` = "#857baa",
              `Violación` = "#8c5991"))+
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
      filter(Año %in% c(2019,2020,2021,2022,2023,2024)) %>%
      group_by(Año) %>%
      summarise(`Referidas por abuso sexual`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por abuso sexual infantil`),
                `Referidas por violación`=sum(`Total de mujeres canalizadas para atención integral de la salud (nom 46) por violación`))-> total_referida




    ind_17_reactive() %>%
    # indicador_17 %>%
      filter(!Año== 2019,
             `Tipo: (abuso sexual infantil / violación)` %in% c("Violación", "Abuso sexual infantil")) %>%
      group_by(Año,`Tipo: (abuso sexual infantil / violación)`) %>%
      summarise(`Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud`= n()) %>%
      pivot_wider(names_from = "Tipo: (abuso sexual infantil / violación)",
                  values_from = "Total de mujeres víctimas de violación o abuso sexual canalizadas al sector salud") %>%
      cbind(total_referida) %>%
      select(1:3,5:6) %>%
      mutate(Año=`Año...1`) %>%
      select(Año,`Referidas por abuso sexual`,`Referidas por violación`, `Abuso sexual infantil`,`Violación`)->tabla_17




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
        colnames = c("Año","Referidas por abuso sexual","Referidas por violación", "Abuso sexual infantil","Violación"),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, 100),
                                         c(5,1,5,10,20, 100)),
                       columnDefs = list(list(className = 'dt-center', targets = 1:5)))) %>%
      formatStyle(
        columns = c(2:5),
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
      scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
      scale_colour_gradient(low = "#dba9c8", high = "#857baa") +
      # facet_wrap(~`Causal: (violacion/ salud/ riesgo)`, ncol = 1) +
      labs(x="", y="", fill="", color="",
           title = "Total mujeres solicitantes de IVE por violación que reciben el procedimiento",
           subtitle = paste("Datos correspondientes al total de mujeres que solicitan el procedimiento de manera directa"),
           caption=paste("\nFecha de consulta: ", Sys.Date(),
                         "\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco a marzo 2024| Micrositio de datos de la AVGM, SISEMH."))+
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
          scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
          scale_colour_gradient(low = "#dba9c8", high = "#857baa") +
          # facet_wrap(~`Causal: (violacion/ salud/ riesgo)`, ncol = 1) +
          labs(x="", y="", fill="", color="",
               title = "Total mujeres solicitantes de IVE por violación que reciben el procedimiento",
               subtitle = paste("Datos correspondientes al total de mujeres que solicitan el procedimiento de manera directa"),
               caption=paste("\nFecha de consulta: ", Sys.Date(),
                             "\n\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco a marzo 2024| Micrositio de datos de la AVGM, SISEMH."))+
          
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

# indicador_18 %>%
    ind_18_reactive() %>%
      filter(!is.na(Año)) %>%
      group_by(Año, Rango, `Causal: (violacion/ salud/ riesgo)`) %>%
      summarise(`Total de mujeres víctimas de violación que recibieron el procedimiento`= n()) %>%
      datatable(
      caption = htmltools::tags$caption("Total mujeres solicitantes de IVE por violación que reciben el procedimiento",
                                        style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold'),
    extensions = 'Buttons',
    options = list(dom = 'Blfrtip',
                   buttons = c('copy', 'excel', 'print'),
                   language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                   initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                   dom = "tip",
                   buttons = c('copy', 'excel', 'print'),
                   lengthMenu = list(c(3,1,3,10,20, 100),
                                     c(3,1,3,10,20, 100)),
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
      scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
      scale_color_gradient(low = "#dba9c8", high = "#857baa") +
      scale_size_continuous(range = c(7,13))+
      labs(x="", y="", fill="", color="",
           title = "Total mujeres que recibieron el procedimiento de ILE",
           subtitle = paste("Datos correspondientes al total de mujeres que reciben el procedimiento de interrupción legal del embarazo \nconforme al Programa Estatal para la Interrupción Legal del Embarazo (Programa ILE) en los Servicios de Salud del Estado de Jalisco"),
           caption=paste("Fecha de consulta:", Sys.Date(),
           "\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y 
           OPD Servicios de Salud Jalisco a marzo de 2024 | Micrositio de datos de la AVGM, SISEMH."))+
      guides(size=F)+
      theme_minimal()+ theme_1+
      theme(legend.position = "right")+
      theme(strip.text = element_text(face="bold", size=10,lineheight=1.0))->gr19

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
          scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
          scale_color_gradient(low = "#dba9c8", high = "#857baa") +
          scale_size_continuous(range = c(7,13))+
          labs(x="", y="", fill="", color="",
               title = "Total mujeres que recibieron el procedimiento de ILE",
               subtitle = paste("Datos correspondientes al total de mujeres que reciben el procedimiento de interrupción legal del embarazo \nconforme al Programa Estatal para la Interrupción Legal del Embarazo (Programa ILE) en los Servicios de Salud del Estado de Jalisco"),
               caption=paste("Fecha de consulta:", Sys.Date(),
                             "\nElaboración propia con base a los datos recopilados por la Secretaría de Salud y 
           OPD Servicios de Salud Jalisco a marzo de 2024 | Micrositio de datos de la AVGM, SISEMH."))+
          guides(size=F)+
          theme_minimal()+ theme_1+
          theme(legend.position = "right")+
          theme(strip.text = element_text(face="bold", size=10,lineheight=1.0))
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
      arrange(Año) %>% 
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
                       lengthMenu = list(c(3,1,3,10,20, 100),
                                         c(3,1,3,10,20, 100)),
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
           title = "Total de casos atendidos en el Sector Salud por violencia familiar y/o sexual",
           subtitle = paste("Datos correspondientes al total de casos que se notifican al Ministerio Público"),
           caption=paste("Fecha de consulta: ", Sys.Date(),
                         "\n\nElaborado con base a los datos recopilados por la Secretaría de Salud y 
                             OPD Servicios de Salud Jalisco a marzo 2024 | Micrositio de datos de la AVGM, SISEMH."))+
      scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 17)) +
      scale_fill_gradient(low = "#ac9bc4", high = "#45375c",
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
               title = "Total de casos atendidos en el Sector Salud por violencia familiar y/o sexual",
               subtitle = paste("Datos correspondientes al total de casos que se notifican al Ministerio Público"),
               caption=paste("Fecha de consulta: ", Sys.Date(),
                             "\n\nElaborado con base a los datos recopilados por la Secretaría de Salud y OPD Servicios de Salud Jalisco a marzo 2024 | Micrositio de datos de la AVGM, SISEMH."))+
          scale_y_discrete(limits = rev,labels = function(x) str_wrap(x, width = 17)) +
          scale_fill_gradient(low = "#ac9bc4", high = "#45375c",
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
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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
      mutate(
        `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)` = case_when(
          `Se cuenta con equipo y material para procedimiento ile/ive: (si/no)` =="SI" ~ 1,
          T~0)) %>% 
      mutate(
        `Nivel de unidad: (2°/ 3°)`= case_when(
          `Nivel de unidad: (2°/ 3°)` %in% c("2°","3°")~1,
            T~0)) %>% 
       group_by(Año) %>%
      summarise(`Número del personal médico no objetor de conciencia` = sum(`Número del personal médico no objetor de conciencia`, na.rm = T),
                `Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile`= sum(`Nivel de unidad: (2°/ 3°)`)
      )->tabla_21

    tabla_21%>%
      # datatable(filter="top", options = list(pageLength = 6))
      datatable(
        caption = htmltools::tags$caption(paste0("Establecimientos estatales proveedores para procedimientos ILE/IVE"), style = 'caption-side: top; text-align: center; color:black;  font-size:90%; font-weight: bold;'),
        colnames = c('Año','Número del personal médico no objetor de conciencia',
                     'Total de unidades de segundo y tercer nivel en condiciones óptimas para realizar el procedimiento ive e ile'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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
      #       `peligro de muerte` = "#8c5991",
      #       `causal de violación` = "#857baa"))+
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
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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
          `Personal de enfermería` = "#857baa",
          `No se especifica` = "#8c5991"))+
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
          colour = Delito, group=Delito, text=text) +
      geom_line(size = 2) + geom_point(size = 4)+

      labs(x="", y="",
           title = "Opiniones técnicas y casos analizados en muertes violentas y desaparición de mujeres",
           subtitle = paste("Datos correspondientes al total de Opiniones técnicas y casos analizados por las Unidades de Análisis y Contexto,
en los delitos que involucran muertes violentas y desaparición de niñas, adolescentes y mujeres"),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
           "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024|
Micrositio de datos de la AVGM, SISEMH."))+
      facet_wrap(~ Clasificación) +
      theme_minimal()+
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(
        values = c("#8c5991","#857baa"))+
      scale_colour_manual(
        values = c("#8c5991","#857baa"))+
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
               title = "Opiniones técnicas y casos analizados en muertes violentas y desaparición de mujeres",
               subtitle = paste("Datos correspondientes al total de Opiniones técnicas y casos analizados por las Unidades de Análisis y Contexto,
en los delitos que involucran muertes violentas y desaparición de niñas, adolescentes y mujeres"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024|
Micrositio de datos de la AVGM, SISEMH."))+
          facet_wrap(~ Clasificación) +
          theme_minimal()+
          scale_y_continuous(labels = scales::comma) +
          scale_fill_manual(
            values = c("#8c5991","#857baa"))+
          scale_colour_manual(
            values = c("#8c5991","#857baa"))+
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
           title ="Víctimas de violación y violencia familiar que denuncian en los CJM",
           subtitle = paste("Datos correspondientes al total de mujeres víctimas de violación y violencia familiar que son
atendidas y proceden a realizar una denuncia en los Centros de Justicia para las Mujeres."),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(.~Municipio, scales = "free") +
      scale_color_manual(
        values = c(
          `Mujeres atendidas en el CJM` = "#D98CBC",
          `Mujeres que realizan una denuncia por el delito de violacion` = "#8c5991",
          `Mujeres que realizan una denuncia por el delito de violencia familiar` = "#857baa"))+
      guides(colour = guide_legend(label.hjust = 0, label.position = "right", nrow = 3))+
      theme_minimal()+
      theme_1+
      theme(legend.position = "bottom")+
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
               title ="Víctimas de violación y violencia familiar que denuncian en los CJM",
               subtitle = paste("Datos correspondientes al total de mujeres víctimas de violación y violencia familiar que son
atendidas y proceden a realizar una denuncia en los Centros de Justicia para las Mujeres."),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          scale_y_continuous(labels = scales::comma) +
          facet_wrap(.~Municipio, scales = "free") +
          scale_color_manual(
            values = c(
              `Mujeres atendidas en el CJM` = "#D98CBC",
              `Mujeres que realizan una denuncia por el delito de violacion` = "#8c5991",
              `Mujeres que realizan una denuncia por el delito de violencia familiar` = "#857baa"))+
          guides(colour = guide_legend(label.hjust = 0, label.position = "right", nrow = 3))+
          theme_minimal()+
          theme_1+
          theme(legend.position = "bottom")+
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
           title = "Carpetas de investigación judicializadas por delitos en razón de género",
           subtitle = paste("Datos correspondientes al total carpetas de investigación iniciadas y judicializadas por delitos
diversos contra mujeres que involucran razones de género"),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
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
                     lengthMenu = list(c(5,1,5,10,20, 100),
                                       c(5,1,5,10,20, 100)),
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
               title = "Carpetas de investigación judicializadas por delitos en razón de género",
               subtitle = paste("Datos correspondientes al total carpetas de investigación iniciadas y judicializadas por delitos
diversos contra mujeres que involucran razones de género"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
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
                choices = sort(unique(indicador_30$`Año de origen de la carpeta de investigación`)),
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
        # if(!is.null(input$ind_30_mes))               Mes %in% input$ind_30_mes       else Mes != "",
        if(!is.null(input$ind_30_condena))  `Tipo de sentencia (absolutoria, condenatoria y en proceso)` %in% input$ind_30_condena    else `Tipo de sentencia (absolutoria, condenatoria y en proceso)` != ""

      )

  })



  output$gr30 <-renderPlot ({

    ind_30_reactive() %>%
      #indicador_30 %>%
      filter(Año%in%c(2016,2017,2018,2019,2020,2021,2022,2023,2024)) %>%
      group_by(`Año de origen de la carpeta de investigación`, `Tipo de sentencia (absolutoria, condenatoria y en proceso)`) %>%
      summarise(`Total de sentencias`=n(),
                `Indicador`=scales::percent(sum(`Total de sentencias`/n()))) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Total de sentencias"))%>%
      # mutate(text = paste("Año: ", Año,
      #                     "\nMes: ",  Mes,
      #                     "\nTotal: ", scales::comma(Total), sep="")) %>%
      ggplot() +
      aes(x = `Año de origen de la carpeta de investigación`, y = Total,
          colour = `Tipo de sentencia (absolutoria, condenatoria y en proceso)`, group=`Tipo de sentencia (absolutoria, condenatoria y en proceso)`) +
      geom_line(size = 2) + geom_point(size = 4)+
      labs(x="", y="", fill="", color="",
           title = "Total de sentencias emitidas por el delito de feminicidio",
           subtitle = paste("Datos correspondientes al total de vinculaciones al proceso por el delito de feminicidio"),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
      theme_minimal()+
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(`Total de sentencia` = "#8c5991"))+
      scale_color_manual(
        values = c(
          `En proceso` = "#b58cd9",
          `Condenatoria` = "#d98cbc",
          `Absolutoria` = "#857baa",
          `Juicio` = "#8c5991"))+
      theme_minimal()+
      theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))-> gr30


    gr30

  })

  output$t_30 <- renderDataTable ({

    ind_30_reactive() %>%
    # indicador_30 %>%
      rename(ano_ini=`Año de origen de la carpeta de investigación`) %>%
      mutate(ano_ini=ifelse(
        nchar(ano_ini)>6,
        substr(ano_ini,
               nchar(ano_ini)-3,nchar(ano_ini)), ano_ini
      ),
      vinculacion=ifelse(is.na(`Fecha de vinculación a proceso`), 1, 0),
      `Año de sentencia`=case_when(
        `Año de sentencia`==2023~1,
        `Año de sentencia`==2022~1,
        `Año de sentencia`==2021~1,
        `Año de sentencia`==2020~1,
        T~0))
      group_by("ano"=as.integer(ano_ini)) %>%
      summarise(Sentencias=sum(`Año de sentencia`),
                Vinculaciones=sum(vinculacion, na.rm = T),
                Carpetas=n()
      ) ->tabla_30


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
                       lengthMenu = list(c(5,1,5,10,20, 100),
                                         c(5,1,5,10,20, 100)),
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
          filter(Año%in%c(2016,2017,2018,2019,2020,2021,2022,2023,2024)) %>%
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
          aes(x = as.factor(Periodo), y = Total,
              colour = `Tipo de sentencia (absolutoria, condenatoria y en proceso)`, group=`Tipo de sentencia (absolutoria, condenatoria y en proceso)`) +
          geom_line(size = 2) + geom_point(size = 4)+
          labs(x="", y="", fill="", color="",
               title = "Total de sentencias emitidas por el delito de feminicidio",
               subtitle = paste("Datos correspondientes al total de vinculaciones al proceso por el delito de feminicidio"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          scale_y_continuous(labels = scales::comma) +
          scale_color_manual(
            values = c(`Total de sentencia` = "#8c5991"))+
          scale_color_manual(
            values = c(
              `En proceso` = "#b58cd9",
              `Condenatoria` = "#d98cbc",
              `Absolutoria` = "#857baa",
              `Juicio` = "#8c5991"))+
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
             title = "Carpetas de investigación de muertes violentas de mujeres iniciadas por diversos delitos",
             subtitle = paste("Datos correspondientes al total de las carpetas de investigación de 
muertes violentas de mujeres iniciadas en la Fiscalía"),
             caption=paste("Fecha de consulta: ", Sys.Date(), 
                           "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
        # facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(
          values = c(`Homicidio culposo` = "#8c5991",
                     `Homicidio doloso` = "#857baa",
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
             title = "Carpetas de investigación de muertes violentas de mujeres iniciadas por diversos delitos",
             subtitle = paste("Datos correspondientes al total de las carpetas de investigación de 
muertes violentas de mujeres iniciadas en la Fiscalía"),
             caption=paste("Fecha de consulta: ", Sys.Date(), 
                           "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
        # facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
        scale_y_continuous(labels = scales::comma, limits = c(0,30)) +
        scale_color_manual(
          values = c(`Homicidio culposo` = "#8c5991",
                     `Homicidio doloso` = "#857baa",
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
                       lengthMenu = list(c(5,1,5,10,20, 100),
                                         c(5,1,5,10,20, 100)),
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
                 title = "Carpetas de investigación de muertes violentas de mujeres iniciadas por diversos delitos",
                 subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
                 caption=paste("Fecha de consulta: ", Sys.Date(), 
                               "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
            # facet_wrap(.~Clasificación, scales = "free_y", ncol = 1) +
            scale_y_continuous(labels = scales::comma, limits = c(0,30)) +
            scale_color_manual(
              values = c(`Homicidio culposo` = "#8c5991",
                         `Homicidio doloso` = "#857baa",
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


  output$medidas_año <- renderUI({
    selectInput("medidas_año",
                label =  "Seleccione el año",
                choices = sort(unique(medidas_ordenes_municipal$año)),
                multiple = T)
  })

  output$medidas_mes <- renderUI({
    selectInput("medidasr_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(medidas_ordenes_municipal$mes)),
                multiple = T,
                selected = "Jalisco")
  })


  output$medidas_municipio <- renderUI({
    selectInput("medidas_municipio",
                label =  "Selecciona el municipio",
                choices = c("Jalisco", sort(unique(medidas_ordenes_municipal$municipio))),
                multiple = F)
  })

  output$medidas_tipo <- renderUI({
    selectInput("medidas_tipo",
                label =  "órdenes y medidas",
                choices = sort(unique(medidas_ordenes_municipal$tipo)),
                multiple = F,
                selected = "Medidas")
  })



  medidas_reactive <- reactive({

    medidas_ordenes_municipal %>%
      filter(
        if(!is.null(input$medidas_año))             año %in% input$medidas_año               else año != "",
        if(!is.null(input$medidas_mes))             mes %in% input$medidas_mes               else mes != "",
        if(input$medidas_municipio=="Jalisco")     municipio!= ""else municipio %in% input$medidas_municipio,
        # if(!is.null(input$medidas_municipio))       municipio %in% input$medidas_municipio   else municipio != "",
        if(!is.null(input$medidas_tipo))             tipo %in% input$medidas_tipo               else tipo != "",

      )

  })






  output$grafico_medidas <- renderPlot ({

    medidas_reactive() %>%
      # medidas_ordenes_municipal %>%
      # filter(año==2023) %>%
      group_by(año, mes, tipo) %>%
      summarise(total=sum(total)) %>%
      mutate(fecha=case_when(
        mes=="Enero"~ "01",
        mes=="Febrero"~"02",
        mes=="Marzo"~"03",
        mes=="Abril"~"04",
        mes=="Mayo"~"05",
        mes=="Junio"~"06",
        mes=="Julio"~"07",
        mes=="Agosto"~"08",
        mes=="Septiembre"~"09",
        mes=="Octubre"~"10",
        mes=="Noviembre"~"11",
        mes=="Diciembre"~"12"),
        fecha=paste0(año,"-", fecha)) %>%
      filter(tipo==input$medidas_tipo) %>%
      ggplot() +
      aes(x =fecha, y = total, color="#de1065") +
      geom_point(color="#857baa", size=4, alpha=0.7) +
      geom_segment(aes(x=fecha, xend=fecha, y=0, yend=total))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-12","2022-03","2022-06", "2022-09",
                                  "2023-03","2023-06","2023-09", "2023-12",
                                  "2024-03"))+
      labs(x="", y="", fill="", color="",
          title = paste0("Total de ",tolower(medidas_reactive()$tipo[1])," de protección emitidas en " , input$medidas_municipio),
          subtitle = paste("Datos correspondientes a la fecha del período:", min(medidas_reactive()$año), "a ", max(medidas_reactive()$año)),
          caption=paste("\nFecha de consulta", Sys.Date(),
          "\n\nElaboración propia con base a los datos recopilados por Fiscalia del Estado a abril 2024 | Micrositio de datos de la AVGM, SISEMH."),
          x="", y="", fill=NULL, color=NULL)+
      theme_minimal() + theme_1+
      theme(legend.position = "none")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico_medidas

    grafico_medidas

  })

  #-----------------------------------------------------------------
  output$mapa_medidas <- renderPlot ({

    medidas_reactive() %>%
      #medidas_ordenes_municipal %>%
      # filter(año==año[i]) %>%
      group_by(municipio) %>%
      summarise(total=sum(total))%>%
      mutate(municipio_name=municipio) %>%
      merge(df_mxmunicipio_2020, by = "municipio_name", all = T) %>%
      mutate(value=total) %>%
      mutate(value = if_else(is.na(value), 0, value)) %>%
      filter(
        !municipio_name%in%c("Sin dato", "Sin datos")) %>%
      mxmunicipio_choropleth(num_colors = 1,
                             zoom = subset(., state_name %in% c("Jalisco"))$region,
                             show_states = FALSE, legend = "")  +
      labs(x="", y="", fill="", color="",
           title = paste0("Total de ",tolower(medidas_reactive()$tipo[1])," de protección emitidas en " , input$medidas_municipio),
           subtitle = paste("Datos correspondientes a la fecha:", min(medidas_reactive()$año), "a ", max(medidas_reactive()$año)),
           caption=paste("\nFecha de consulta", Sys.Date(),
                         "\n\nElaboración propia con base a los datos recopilados 
                         por Fiscalia del Estado a abril 2024 |
                         Micrositio de datos de la AVGM, SISEMH."),
           x="", y="", fill=NULL, color=NULL)+
      scale_fill_gradient(
        low = "#e9e8eb",
        high = "#857baa",
        guide = "colourbar",
        label=comma)+
      theme_minimal()+
      theme_1+
      theme(text=element_text(family = gt::google_font("Poppins")),
            plot.title = element_text(family = gt::google_font("Poppins"),
                                      face = "bold",
                                      size = 11,
                                      hjust = 0),
            plot.subtitle = element_text(family = gt::google_font("Poppins"),
                                         size = 9,
                                         hjust = 0,
                                         colour = "grey40"),
            plot.caption = element_text(family = gt::google_font("Poppins"),
                                        size = 8,
                                        colour = "grey40"),
            legend.title = element_text(family = gt::google_font("Poppins"),
                                        face = "bold",
                                        size = 8,
                                        colour = "black",
                                        hjust = 1),
            legend.title.align = 0.5,
            legend.text = element_text(family = gt::google_font("Poppins"),
                                       # face = "bold",
                                       size = 7,
                                       colour = "black",
                                       hjust = 1),
            legend.text.align = 0.5,
            legend.key.size = unit(15, "pt"))->mapa_medidas

    mapa_medidas

  })


  output$tabla_medidas <- renderDataTable ({

    medidas_reactive() %>%
      #medidas_ordenes_municipal %>%
      # filter(año==año[i]) %>%
      group_by(municipio) %>%
      summarise(total=sum(total)) %>%
      datatable(
        caption = htmltools::tags$caption(paste0("Total de ",tolower(medidas_reactive()$tipo[1])," emitidas en " , input$medidas_municipio),
                                          style = 'caption-side: top; text-align: center; color:black;  font-size:70%; font-weight: bold;'),
        colnames = c('Municipio','Total'),
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
                       columnDefs = list(list(className = 'dt-center', targets = 1:2)))) %>%
      formatStyle(
        columns = c(1:2),
        fontFamily = "Nutmeg-Light",
        fontSize = "10px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")->tabla_medidas

    tabla_medidas

  })
  
  output$download_d_medidas<- downloadHandler(
    filename = function() {
      paste(input$dataset, "datos_medidas_ordenes.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(medidas_ordenes_municipal, file, row.names = F)
    }
  )
  
  output$download_gr_medidas <- downloadHandler(
    filename = function() {
      paste0("grafico_", tolower(input$medidas_tipo), ".jpeg")},
    
    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        medidas_reactive() %>%
          # medidas_ordenes_municipal %>%
          # filter(año==2023) %>%
          group_by(año, mes, tipo) %>%
          summarise(total=sum(total)) %>%
          mutate(fecha=case_when(
            mes=="Enero"~ "01",
            mes=="Febrero"~"02",
            mes=="Marzo"~"03",
            mes=="Abril"~"04",
            mes=="Mayo"~"05",
            mes=="Junio"~"06",
            mes=="Julio"~"07",
            mes=="Agosto"~"08",
            mes=="Septiembre"~"09",
            mes=="Octubre"~"10",
            mes=="Noviembre"~"11",
            mes=="Diciembre"~"12"),
            fecha=paste0(año,"-", fecha)) %>%
          filter(tipo==input$medidas_tipo) %>%
          ggplot() +
          aes(x =fecha, y = total, color="#de1065") +
          geom_point(color="#857baa", size=4, alpha=0.7) +
          geom_segment(aes(x=fecha, xend=fecha, y=0, yend=total))+
          scale_y_continuous(labels = scales::comma) +
          scale_x_discrete(breaks = c("2020-12","2020-03","2020-06","2020-09",
                                      "2021-12","2021-03","2021-06","2021-09",
                                      "2022-12","2022-03","2022-06", "2022-09",
                                      "2023-03","2023-06","2023-09", "2023-12",
                                      "2024-03"))+
          labs(x="", y="", fill="", color="",
               title = paste0("Total de ",tolower(medidas_reactive()$tipo[1])," de protección emitidas en " , input$medidas_municipio),
               subtitle = paste("Datos correspondientes a la fecha del período:", min(medidas_reactive()$año), "a ", max(medidas_reactive()$año)),
               caption=paste("\nFecha de consulta", Sys.Date(),
                             "\n\nElaboración propia con base a los datos recopilados por Fiscalia del Estado a abril 2024 |
Micrositio de datos de la AVGM, SISEMH."),
               x="", y="", fill=NULL, color=NULL)+
          theme_minimal() + theme_1+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      )
      dev.off()
    })
  

  # Indicador 32: -----------------------------------------------------------------

  output$ind_32_año <- renderUI({
    selectInput("ind_32_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_32$Año)),
                multiple = T)
  })

  output$ind_32_mes<- renderUI({
    selectInput("ind_32_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_32$Mes)),
                multiple = T)
  })


  output$ind_32_municipio <- renderUI({
    selectInput("ind_32_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(indicador_32$Municipio)),
                multiple = T)
  })


  ind_32_reactive <- reactive({

    indicador_32 %>%
      filter(
        if(!is.null(input$ind_32_año))               Año %in% input$ind_32_año       else Año != "",
        if(!is.null(input$ind_32_mes))               Mes %in% input$ind_32_mes       else Mes != "",
        if(!is.null(input$ind_32_municipio))   Municipio %in% input$ind_32_municipio else Municipio != ""
      )

  })



  output$gr32 <-renderPlot ({

    ind_32_reactive() %>%
      # indicador_32 %>%
      group_by(Año, Mes, Fecha) %>%
      summarise(
        `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`, na.rm = T),
        `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
        `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
        `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T)) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=4:7) %>%
      mutate(text = paste("Año: ", Año,
                          "\nPeriodo: ",  Mes,
                          "\nTotal: ", scales::comma(Total), sep="")) %>%
      ggplot() +
      aes(x = Fecha, y = Total,
          colour = Clasificación, group=Clasificación, text=text) +
      geom_line(size = 1.5) + geom_point(size = 2)+
      labs(x="", y="", fill="", color="",
           title = "Total de  casos de denuncias de desaparición de niñas, adolescentes y mujeres",
           # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
      facet_wrap(.~Clasificación, scales = "free") +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas` = "#b58cd9",
          `Número de cédulas de Alerta Amber emitidas` = "#d98cbc",
          `Número de cédulas de Protocolo Alba emitidas`= "#857baa",
          `Número de informes de factor de riesgo elaborados` = "#8c5991"))+
      theme_minimal()+ theme_1+
      theme(legend.position = "none") +
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            strip.text = element_text(face="bold", size=11,lineheight=0.8),

            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr32
    gr32
  })

  output$t_32 <- renderDataTable ({

    ind_32_reactive() %>%
      #indicador_32 %>%
      group_by(Año) %>%
      summarise(
        `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`, na.rm = T),
        `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
        `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
        `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T)) -> tabla_32



    tabla_32 %>%
      datatable(
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
                       columnDefs = list(list(className = 'dt-center', targets = 1:5)))) %>%
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

    # datatable(filter="top", extensions = 'Buttons',
    #     options = list(dom = 'Blfrtip',
    #                    buttons = c('copy', 'excel', 'print'),
    #                    lengthMenu = list(c(6,10,20, -1),
    #                                      c(6,10,20,"Todo"))))


  })


  output$download_d32<- downloadHandler(
    filename = function() {
      paste(input$dataset, "total_de_casos_denunciados_desaparicion.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_32, file, row.names = F)
    }
  )

  output$download_gr32 <- downloadHandler(
    filename = function() {
      paste0("total_de_casos_denunciados_desaparicion.jpeg")},

    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_32_reactive() %>%
          # indicador_32 %>%
          group_by(Año, Mes, Fecha) %>%
          summarise(
            `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`= sum(`Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas`, na.rm = T),
            `Número de cédulas de Alerta Amber emitidas` = sum(`Número de cédulas de Alerta Amber emitidas`, na.rm = T),
            `Número de cédulas de Protocolo Alba emitidas` =sum(`Número de cédulas de Protocolo Alba emitidas`, na.rm = T),
            `Número de informes de factor de riesgo elaborados` =sum(`Número de informes de factor de riesgo elaborados`, na.rm = T)) %>%
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=4:7) %>%
          mutate(text = paste("Año: ", Año,
                              "\nPeriodo: ",  Mes,
                              "\nTotal: ", scales::comma(Total), sep="")) %>%
          ggplot() +
          aes(x = Fecha, y = Total,
              colour = Clasificación, group=Clasificación, text=text) +
          geom_line(size = 1.5) + geom_point(size = 2)+
          labs(x="", y="", fill="", color="",
               title = "Total de casos de denuncias de desaparición de niñas, adolescentes y mujeres",
               # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          facet_wrap(.~Clasificación, scales = "free") +
          scale_y_continuous(labels = scales::comma) +
          scale_color_manual(
            values = c(
              `Total de número de denuncias de niñas, adolescentes y mujeres desaparecidas` = "#b58cd9",
              `Número de cédulas de Alerta Amber emitidas` = "#d98cbc",
              `Número de cédulas de Protocolo Alba emitidas`= "#857baa",
              `Número de informes de factor de riesgo elaborados` = "#8c5991"))+
          theme_minimal()+ theme_1+
          theme(legend.position = "none") +
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                strip.text = element_text(face="bold", size=11,lineheight=0.8),

                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      )
      dev.off()
    })




  # Indicador 33: -----------------------------------------------------------------

  output$ind_33_año <- renderUI({
    selectInput("ind_33_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_33$Año)),
                multiple = T)
  })

  output$ind_33_mes<- renderUI({
    selectInput("ind_33_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_33$Mes)),
                multiple = T)
  })


  output$ind_33_edad <- renderUI({
    selectInput("ind_33_municipio",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_33$Edad)),
                multiple = T)
  })


  ind_33_reactive <- reactive({

    indicador_33 %>%
      filter(
        if(!is.null(input$ind_33_año))     Año %in% input$ind_33_año    else Año != "",
        if(!is.null(input$ind_33_mes))     Mes %in% input$ind_33_mes    else Mes != "",
        if(!is.null(input$ind_33_edad))   Edad %in% input$ind_33_edad   else Edad != ""
      )

  })



  output$gr33 <-renderPlot ({

    ind_33_reactive() %>%
      #indicador_33 %>%
      group_by(Año,Mes, Fecha, Edad) %>%
      summarise(`Total de cédulas únicas de difusión emitidas`=sum(`Total de cédulas únicas de difusión emitidas`),
                `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`=sum(`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`)) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=5:6) %>%
      mutate(text = paste("Año: ", Año,
                          "\nMes: ",  Mes,
                          "\nTotal: ", scales::comma(Total), sep="")) %>%
      filter(!Total==0) %>%

      ggplot() +
      aes(x = Fecha, y = Edad,
          size =Total,
          colour = Clasificación, group=Clasificación, text=text) +
      geom_point()+
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite")+# ggplot() +
      scale_size_continuous(range = c(5,10))+
      facet_wrap(.~ Clasificación, labeller = label_wrap_gen(width = 70, multi_line = TRUE)) +
      labs(x="", y="", fill="", color="",
           title = "Total de cédulas únicas de difusión que son remitidas al Comité Técnico de Colaboración",
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
      scale_color_manual(
        values = c(`Total de cédulas únicas de difusión emitidas` = "#8c5991",
                   `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración` = "#857baa"))+
      guides(size = FALSE)+
      theme_minimal()+ theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            strip.text = element_text(face="bold", size=11,lineheight=0.8),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr33
    gr33

  })

  output$t_33 <- renderDataTable ({

    ind_33_reactive() %>%
      #indicador_33 %>%
      group_by(Año) %>%
      summarise(`Total de cédulas únicas de difusión emitidas`=sum(`Total de cédulas únicas de difusión emitidas`),
                `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`=sum(`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`),
                # `Indicador`=scales::percent(sum((`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`)/`Total de cédulas únicas de difusión emitidas`), 0.1)
      ) -> tabla_33



    tabla_33 %>%
      datatable(
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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
    # datatable(filter="top", extensions = 'Buttons',
    #                       options = list(dom = 'Blfrtip',
    #                                      buttons = c('copy', 'excel', 'print'),
    #                                      lengthMenu = list(c(6,10,20, -1),
    #                                                        c(6,10,20,"Todo"))))


  })


  output$download_d33<- downloadHandler(
    filename = function() {
      paste(input$dataset, "cedulas_unicas_remitidas.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_33, file, row.names = F)
    }
  )

  output$download_gr33 <- downloadHandler(
    filename = function() {
      paste0("cedulas_unicas_remitidas.jpeg")},

    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_33_reactive() %>%
          #indicador_33 %>%
          group_by(Año,Mes, Fecha, Edad) %>%
          summarise(`Total de cédulas únicas de difusión emitidas`=sum(`Total de cédulas únicas de difusión emitidas`),
                    `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`=sum(`Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración`)) %>%
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=5:6) %>%
          mutate(text = paste("Año: ", Año,
                              "\nMes: ",  Mes,
                              "\nTotal: ", scales::comma(Total), sep="")) %>%
          filter(!Total==0) %>%

          ggplot() +
          aes(x = Fecha, y = Edad,
              size =Total,
              colour = Clasificación, group=Clasificación#, text=text
              ) +
          geom_point()+
          geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=3, color="ghostwhite")+# ggplot() +
          scale_size_continuous(range = c(5,10))+
          facet_wrap(.~ Clasificación, labeller = label_wrap_gen(width = 70, multi_line = TRUE)) +
          labs(x="", y="", fill="", color="",
               title = "Total de cédulas únicas de difusión que son remitidas al Comité Técnico de Colaboración",
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          scale_color_manual(
            values = c(`Total de cédulas únicas de difusión emitidas` = "#8c5991",
                       `Cédulas únicas de difusión emitidas que son remitidas al comité técnico de colaboración` = "#857baa"))+
          guides(size = FALSE)+
          theme_minimal()+ theme_1+
          theme(legend.position = "bottom")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                strip.text = element_text(face="bold", size=11,lineheight=0.8),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      )
      dev.off()
    })



  # Indicador 34: -----------------------------------------------------------------

  output$ind_34_año <- renderUI({
    selectInput("ind_34_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_34$Año)),
                multiple = T)
  })

  output$ind_34_mes<- renderUI({
    selectInput("ind_34_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_34$Mes)),
                multiple = T)
  })


  output$ind_34_edad <- renderUI({
    selectInput("ind_34_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_34$`Rango de edad`)),
                multiple = T)
  })


  ind_34_reactive <- reactive({

    indicador_34 %>%
      filter(
        if(!is.null(input$ind_34_año))     Año %in% input$ind_34_año               else Año != "",
        if(!is.null(input$ind_34_mes))     Mes %in% input$ind_34_mes               else Mes != "",
        if(!is.null(input$ind_34_edad))   `Rango de edad` %in% input$ind_34_edad   else `Rango de edad` != ""
      )

  })



  output$gr34 <-renderPlot ({

    ind_34_reactive() %>%
      #indicador_34 %>%
      group_by(Año, `Rango de edad`) %>%
      summarise(`Número de casos de localización`=sum(`Número de casos de localización`),
                `Número de casos de búsqueda`=sum(`Número de casos de búsqueda`),
                `Número de casos de aplicación a cabalidad del protocolo alba`=sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`),
                `Indicador`=scales::percent(sum((`Número de casos de localización`)/`Número de casos de aplicación a cabalidad del Protocolo Alba`), 0.1)
      ) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=4:5) %>%
      # mutate(text = paste("Año: ", Año,
      #                     "\nPeriodo: ",  Trimestre, "%B de %Y"),
      #        "\nTotal: ", scales::comma(Total), sep="") %>%
      filter(!Total==0) %>%
      ggplot() +
      aes(x =Año , y = `Rango de edad`,# text=text,
          fill = Total) +
      #geom_raster()+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=3, color="ghostwhite", angle=0)+
      labs(x="", y="", fill="", color="",
           title = "Casos de búsqueda y localización realizados de forma inmediata y diferenciada",
           # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
    caption=paste("Fecha de consulta: ", Sys.Date(), 
                  "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
      facet_grid(.~ Clasificación,
                 labeller = label_wrap_gen(width = 40, multi_line = TRUE)
      ) +
      scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
      theme_minimal()+   theme_1+
      theme(legend.position = "right")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            strip.text = element_text(face="bold", size=11,lineheight=0.8),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9, family="Nutmeg-Light")) -> gr34
    gr34

  })

  output$t_34 <- renderDataTable ({

    ind_34_reactive() %>%
      #indicador_33 %>%
      group_by(Año) %>%
      summarise(`Número de casos de localización`=sum(`Número de casos de localización`),
                `Número de casos de búsqueda`=sum(`Número de casos de búsqueda`),
                `Número de casos de aplicación a cabalidad del protocolo alba`=sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`),
                # `Indicador`=scales::percent(sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`/(`Número de casos de localización`+ `Número de casos de búsqueda`)), 0.1)
      ) -> tabla_34

    tabla_34 %>%
      datatable(
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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
    # datatable(filter="top", extensions = 'Buttons',
    #                      options = list(dom = 'Blfrtip',
    #                                     buttons = c('copy', 'excel', 'print'),
    #                                     lengthMenu = list(c(6,10,20, -1),
    #                                                       c(6,10,20,"Todo"))))

  })



  output$download_d34<- downloadHandler(
    filename = function() {
      paste(input$dataset, "total_de_casos_busqueda_localizacion.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_34, file, row.names = F)
    }
  )

  output$download_gr34 <- downloadHandler(
    filename = function() {
      paste0("total_de_casos_busqueda_localizacion.jpeg")},

    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_34_reactive() %>%
          #indicador_34 %>%
          group_by(Año, `Rango de edad`) %>%
          summarise(`Número de casos de localización`=sum(`Número de casos de localización`),
                    `Número de casos de búsqueda`=sum(`Número de casos de búsqueda`),
                    `Número de casos de aplicación a cabalidad del protocolo alba`=sum(`Número de casos de aplicación a cabalidad del Protocolo Alba`),
                    `Indicador`=scales::percent(sum((`Número de casos de localización`)/`Número de casos de aplicación a cabalidad del Protocolo Alba`), 0.1)
          ) %>%
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=4:5) %>%
          # mutate(text = paste("Año: ", Año,
          #                     "\nPeriodo: ",  Trimestre, "%B de %Y"),
          #        "\nTotal: ", scales::comma(Total), sep="") %>%
          filter(!Total==0) %>%
          ggplot() +
          aes(x =Año , y = `Rango de edad`,# text=text,
              fill = Total) +
          #geom_raster()+
          geom_tile(color = "white",
                    lwd = 1,
                    linetype = 1) +
          geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=3, color="ghostwhite", angle=0)+
          labs(x="", y="", fill="", color="",
               title = "Casos de búsqueda y localización realizados de forma inmediata y diferenciada",
               # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          facet_grid(.~ Clasificación,
                     labeller = label_wrap_gen(width = 40, multi_line = TRUE)
          ) +
          scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
          theme_minimal()+   theme_1+
          theme(legend.position = "right")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                strip.text = element_text(face="bold", size=11,lineheight=0.8),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9, family="Nutmeg-Light"))
      )
      dev.off()
    })

  # Indicador 35: -----------------------------------------------------------------

  output$ind_35_año <- renderUI({
    selectInput("ind_35_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_35$Año)),
                multiple = T)
  })

  output$ind_35_mes<- renderUI({
    selectInput("ind_35_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_35$Mes)),
                multiple = T)
  })


  ind_35_reactive <- reactive({

    indicador_35 %>%
      filter(
        if(!is.null(input$ind_35_año))     Año %in% input$ind_35_año    else Año != "",
        if(!is.null(input$ind_35_mes))     Mes %in% input$ind_35_mes    else Mes != ""
      )

  })



  output$gr35 <-renderPlot ({

    ind_35_reactive() %>%
      # indicador_35 %>%
      group_by(Año) %>%
      summarise(`Cédulas de difusión activas`=sum(`Cédulas de difusión activas`),
                `Total de cédulas de difusión emitidas`=sum(`Total de cédulas de difusión emitidas`),
                # `Indicador`=scales::percent(sum((`Cédulas de difusión activas`)/`Total de cédulas de difusión emitidas`), 0.1)
      ) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=c("Cédulas de difusión activas",
                          "Total de cédulas de difusión emitidas")) %>%
      # mutate(text = paste("Año: ", Año,
      #                     "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
      #                     "\nTotal: ", scales::comma(Total), sep="")) %>%
      ggplot() +
      aes(x = Año , y = Total,
          fill = Clasificación#, group=Clasificación
          ) +
      # geom_line(size = 1.5) + geom_point(size = 2)+
      geom_col(position = "dodge")+
      geom_label(aes(label = Total), size = 4, vjust = -0.1, color="white",
                position = position_dodge(width=.9), show_guide = F)+
      # facet_wrap(~Clasificación)+
      labs(x="", y="", fill="", color="",
           title = "Total de Cédulas de Difusión activas en relación al total de Cédulas de Difusión emitidas",
           # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(
        values = c(`Cédulas de difusión activas` = "#8c5991",
                   `Total de cédulas de difusión emitidas` = "#857baa"))+
      theme_minimal() +theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr35


    gr35

  })

  output$t_35 <- renderDataTable ({

    ind_35_reactive() %>%
      #indicador_35 %>%
      group_by(Año) %>%
      summarise(`Cédulas de difusión activas`=sum(`Cédulas de difusión activas`),
                `Total de cédulas de difusión emitidas`=sum(`Total de cédulas de difusión emitidas`),
                # `Indicador`=scales::percent(sum((`Cédulas de difusión activas`)/`Total de cédulas de difusión emitidas`), 0.1)
      ) -> tabla_35


    tabla_35 %>%
      datatable(
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#857baa', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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
    # datatable(filter="top", extensions = 'Buttons',
    #                       options = list(dom = 'Blfrtip',
    #                                      buttons = c('copy', 'excel', 'print'),
    #                                      lengthMenu = list(c(6,10,20, -1),
    #                                                        c(6,10,20,"Todo"))))

  })


  output$download_d35<- downloadHandler(
    filename = function() {
      paste(input$dataset, "cédulas_difusión_emitidas.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_35, file, row.names = F)
    }
  )

  output$download_gr35 <- downloadHandler(
    filename = function() {
      paste0("cédulas_difusión_emitidas.jpeg")},

    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_35_reactive() %>%
          # indicador_35 %>%
          group_by(Año) %>%
          summarise(`Cédulas de difusión activas`=sum(`Cédulas de difusión activas`),
                    `Total de cédulas de difusión emitidas`=sum(`Total de cédulas de difusión emitidas`),
                    # `Indicador`=scales::percent(sum((`Cédulas de difusión activas`)/`Total de cédulas de difusión emitidas`), 0.1)
          ) %>%
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=c("Cédulas de difusión activas",
                              "Total de cédulas de difusión emitidas")) %>%
          # mutate(text = paste("Año: ", Año,
          #                     "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
          #                     "\nTotal: ", scales::comma(Total), sep="")) %>%
          ggplot() +
          aes(x = Año , y = Total,
              fill = Clasificación#, group=Clasificación
          ) +
          # geom_line(size = 1.5) + geom_point(size = 2)+
          geom_col(position = "dodge")+
          geom_label(aes(label = Total), size = 4, vjust = -0.1, color="white",
                     position = position_dodge(width=.9), show_guide = F)+
          # facet_wrap(~Clasificación)+
          labs(x="", y="", fill="", color="",
               title = "Total de Cédulas de Difusión activas en relación al total de Cédulas de Difusión emitidas",
               # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          scale_y_continuous(labels = scales::comma) +
          scale_fill_manual(
            values = c(`Cédulas de difusión activas` = "#8c5991",
                       `Total de cédulas de difusión emitidas` = "#857baa"))+
          theme_minimal() +theme_1+
          theme(legend.position = "bottom")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      )
      dev.off()
    })



  # Indicador 36: -----------------------------------------------------------------

  output$ind_36_año <- renderUI({
    selectInput("ind_36_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_36$Año)),
                multiple = T)
  })

  output$ind_36_mes<- renderUI({
    selectInput("ind_36_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_36$Mes)),
                multiple = T)
  })


  output$ind_36_edad <- renderUI({
    selectInput("ind_36_edad",
                label =  "Selecciona el rango de edad",
                choices = sort(unique(indicador_36$`Rango de edad`)),
                multiple = T)
  })


  ind_36_reactive <- reactive({

    indicador_36 %>%
      filter(
        if(!is.null(input$ind_36_año))     Año %in% input$ind_36_año               else Año != "",
        if(!is.null(input$ind_36_mes))     Mes %in% input$ind_36_mes               else Mes != "",
        if(!is.null(input$ind_36_edad))   `Rango de edad` %in% input$ind_36_edad   else `Rango de edad` != ""
      )

  })



  output$gr36 <-renderPlot ({

    ind_36_reactive() %>%
      #indicador_36 %>%
      group_by(Año, `Rango de edad`) %>%
      summarise(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`=sum(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`),
                `Total de casos de desaparición de niñas y adolescentes`=sum(`Total de casos de desaparición de niñas y adolescentes`),
                # `Indicador`=scales::percent(sum((`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`)/`Total de casos de desaparición de niñas y adolescentes`), 0.1)
      ) %>%
      pivot_longer(names_to = "Clasificación",
                   values_to = "Total",
                   cols=3:4) %>%
      # mutate(text = paste("\nPeriodo: ",  Trimestre,
      #                     "\nTotal: ", scales::comma(Total), sep="")) %>%
      filter(!Total==0) %>%
      ggplot() +
      aes(x =Año , y = `Rango de edad`, #text=text,
          fill = Total) +
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=5, color="ghostwhite", angle=0)+
      labs(x="", y="", fill="", color="",
           title = "Casos de investigación de desaparición de niñas y adolescentes en los que se activa reporte Amber",
           # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
           caption=paste("Fecha de consulta: ", Sys.Date(), 
                         "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+      facet_wrap(.~ Clasificación, scales = "free",
                 labeller = label_wrap_gen(width = 70, multi_line = TRUE)
      ) +
      scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
      theme_minimal()+ theme_1+
      theme(legend.position = "bottom")+
      theme(text=element_text(size=12,  family="Nutmeg-Light"),
            plot.title = element_text(family="Nutmeg-Light"),
            strip.text = element_text(face="bold", size=11,lineheight=0.8),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light")) -> gr36
    gr36
  })

  output$t_36 <- renderDataTable ({

    ind_36_reactive() %>%
      #indicador_33 %>%
      group_by(Año) %>%
      summarise(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`=sum(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`),
                `Total de casos de desaparición de niñas y adolescentes`=sum(`Total de casos de desaparición de niñas y adolescentes`),
                # `Indicador`=scales::percent(sum((`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`)/`Total de casos de desaparición de niñas y adolescentes`), 0.1)
      ) -> tabla_36

    tabla_36 %>%
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
                       lengthMenu = list(c(4,1,4,10,20, 100),
                                         c(4,1,4,10,20, 100)),
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



  output$download_gr36 <- downloadHandler(
    filename = function() {
      paste0("casos_reporte_amber.jpeg")},

    content = function(file) {
      jpeg(file, width = 1500, height = 800)
      print(
        ind_36_reactive() %>%
          #indicador_36 %>%
          group_by(Año, `Rango de edad`) %>%
          summarise(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`=sum(`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`),
                    `Total de casos de desaparición de niñas y adolescentes`=sum(`Total de casos de desaparición de niñas y adolescentes`),
                    # `Indicador`=scales::percent(sum((`Número de casos en los que se activa, de conformidad con la normatividad y en el tiempo establecido el protocolo de la Alerta Amber`)/`Total de casos de desaparición de niñas y adolescentes`), 0.1)
          ) %>%
          pivot_longer(names_to = "Clasificación",
                       values_to = "Total",
                       cols=3:4) %>%
          # mutate(text = paste("\nPeriodo: ",  Trimestre,
          #                     "\nTotal: ", scales::comma(Total), sep="")) %>%
          filter(!Total==0) %>%
          ggplot() +
          aes(x =Año , y = `Rango de edad`, #text=text,
              fill = Total) +
          geom_tile(color = "white",
                    lwd = 1,
                    linetype = 1) +
          geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                    size=5, color="ghostwhite", angle=0)+
          labs(x="", y="", fill="", color="",
               title = "Casos de investigación de desaparición de niñas y adolescentes en los que se activa reporte Amber",
               # subtitle = paste("Datos correspondientes al total de las carpetas de investigación de muertes violentas de mujeres iniciadas en la Fiscalía"),
               caption=paste("Fecha de consulta: ", Sys.Date(), 
                             "\n\nElaboración propia con base a los datos recopilados por la Fiscalía del Estado de Jalisco a mayo 2024 |
Micrositio de datos de la AVGM, SISEMH."))+
          facet_wrap(.~ Clasificación, scales = "free",
                     labeller = label_wrap_gen(width = 70, multi_line = TRUE)) +
          scale_fill_gradient(low = "#dba9c8", high = "#857baa") +
          theme_minimal()+ theme_1+
          theme(legend.position = "bottom")+
          theme(text=element_text(size=12,  family="Nutmeg-Light"),
                plot.title = element_text(family="Nutmeg-Light"),
                strip.text = element_text(face="bold", size=11,lineheight=0.8),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))
      )
      dev.off()
    })


  output$download_d36<- downloadHandler(
    filename = function() {
      paste(input$dataset, "casos_reporte_amber.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_36, file, row.names = F)
    }
  )



  # Indicador 37: -----------------------------------------------------------------

  output$ind_37_año <- renderUI({
    selectInput("ind_37_año",
                label =  "Seleccione el año",
                choices = sort(unique(indicador_37$Año)),
                multiple = T)
  })

  output$ind_37_mes<- renderUI({
    selectInput("ind_37_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(indicador_37$Mes)),
                multiple = T)
  })

  output$ind_37_personal<- renderUI({
    selectInput("ind_37_personal",
                label =  "Seleccione la función del personal",
                choices = sort(unique(indicador_37$Personal)),
                multiple = T)
  })


  ind_37_reactive <- reactive({

    indicador_37 %>%
      filter(
        if(!is.null(input$ind_35_año))             Año %in% input$ind_35_año         else Año != "",
        if(!is.null(input$ind_35_mes))             Mes %in% input$ind_35_mes         else Mes != "",
        if(!is.null(input$ind_37_personal))   Personal %in% input$ind_37_personal    else Personal != ""

      )

  })



  output$t_37 <- renderDataTable ({

    ind_37_reactive() %>%
      # indicador_37 %>%
      group_by(Año, Personal) %>%
      summarise(`Personal de la fiscalía debidamente formado en el funcionamiento del Protocolo Alba`=sum(`Personal de la fiscalía debidamente formado en el funcionamiento del Protocolo Alba`),
                `Total personal de la fiscalía`=sum(`Total personal de la fiscalía`),
                # `Indicador`=scales::percent(sum((`Personal de la fiscalía debidamente formado en el funcionamiento del Protocolo Alba`)/`Total personal de la fiscalía`), 0.1)
      ) -> tabla_37
    tabla_37 %>%
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
                       lengthMenu = list(c(5,1,5,10,30, 100),
                                         c(5,1,5,10,30, 100)),
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


  output$download_d37<- downloadHandler(
    filename = function() {
      paste(input$dataset, "personal_protocolo_alba.xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(indicador_77, file, row.names = F)
    }
  )


}

shinyApp(ui, server)

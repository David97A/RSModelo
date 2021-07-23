##Shiny
library(shiny)
library(shinydashboard)
library(shinyjs)

##Data Mining
library(rtweet)
library(tm)
library(readxl)

##Tidy
library(scales)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidytext)

##Graficación
library(plotly)
library(ggplot2)
library(ggthemes)
library(hwordcloud)
library(bsplus)


#Extracción de Datos

## Actividad

tablaactividadModeloSemestral <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Actividad de Modelo en Twitter.xlsx", 
                                            sheet = "Total", range = "B1:F295")
tablaactividadModeloEnero <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Actividad de Modelo en Twitter.xlsx", 
                                        sheet = "Enero", range = "B1:F16")
tablaactividadModeloFebrero <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Actividad de Modelo en Twitter.xlsx", 
                                          sheet = "Febrero", range = "B1:F40")
tablaactividadModeloJunio <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Actividad de Modelo en Twitter.xlsx", 
                                        sheet = "Junio", range = "B1:F47")

## Métricas

metricasprincipalesModelo <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Seguimiento Modelo Facebook.xlsx", 
                                        sheet = "Info General", range = "A2:B4")
metricasprincipalesTecate <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Seguimiento Modelo Facebook.xlsx", 
                                        sheet = "Info General", range = "D2:E4")

## Reacciones, sentimientos y menciones

reaccionesModeloJunio <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Seguimiento Modelo Facebook.xlsx", 
                                    sheet = "Comparación", range = "B2:C9")
reaccionesModeloMayo <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Seguimiento Modelo Facebook.xlsx", 
                                   sheet = "Comparación", range = "B11:C18")

emocionesModeloJulio <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Emociones.xlsx",
                                    sheet = "Julio", range = "B1:C9")
polaridadModeloJulio <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Polaridad de Reacciones.xlsx",
                                   sheet = "Julio", range = "B1:C3")



reaccionesTimeLineModeloJulio <- read_excel( "/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Timeline de Reacciones.xlsx",
                                             sheet = "Julio", range = "B1:D35")
tokensModeloJulio <- read_excel("/Users/davidaguirrecrespo/Desktop/SBR/Empresas/Cerveza Modelo/ShinyApp/RSModelo/Tokens de Menciones Julio.xlsx", 
                                range = "B1:C41")


#Tidy

## Crear DataFrame de Métricas Principales

metricasprincipalesDF <- data.frame(metricasprincipalesModelo$Métrica,
                                    metricasprincipalesModelo$Total,
                                    metricasprincipalesTecate$Total)
names(metricasprincipalesDF) <- c("Métrica", "Cerveza Modelo", "Cerveza Tecate")


## Formato de Fecha

tablaactividadModeloEnero <- tablaactividadModeloEnero %>%
    mutate(Fecha = as.POSIXct(Fecha))

tablaactividadModeloFebrero <- tablaactividadModeloFebrero %>%
    mutate(Fecha = as.POSIXct(Fecha))

tablaactividadModeloJunio <- tablaactividadModeloJunio%>%
    mutate(Fecha = as.POSIXct(Fecha))

## DataFrame de Reacciones por Mes para Modelo

reaccionesDF <- data.frame(reaccionesModeloJunio$Reacción,
                           reaccionesModeloJunio$Total,
                           reaccionesModeloMayo$Total)
names(reaccionesDF) <- c("Reacción", "Junio 2021", "Mayo 2021")


# Graficación de Datos

## Actividad de @CervezaModeloMX en Twitter

### Semestral

plotactividadModeloSemestral <- ggplot(data = tablaactividadModeloSemestral, 
                                       aes(x = month(Fecha, label = TRUE))) +
    geom_bar(fill = "#B8B525") +
    geom_density(aes(x = month(Fecha),
                     y = ..count..)) +
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white", size = 8) +
    labs(title = "Numero de Tweets por mes",
         subtitle = "Primer Semestre 2021",
         x=" ",
         y=" ",
         caption = "Fuente: Twitter") +
    scale_x_discrete(labels = c("Enero", "Febrero","Marzo","Abril","Mayo","Junio","Julio")) +
    theme_tufte(base_size = 20) 



### Por Día

actividaddiariaEnero <- ggplot(tablaactividadModeloEnero) + 
    geom_bar(aes(x = wday(Fecha, week_start = getOption("lubridate.week.start", 1), label = TRUE, abbr = FALSE)),
             colour = "#000000",
             fill = "#B8B525") +
    labs(title="Tweets por día de la semana",
         subtitle = "Enero",
         caption = "Fuente: Twitter") +
    theme_tufte()

actividaddiariaFebrero <- ggplot(tablaactividadModeloFebrero) + 
    geom_bar(aes(x = wday(Fecha, week_start = getOption("lubridate.week.start", 1), label = TRUE, abbr = FALSE)),
             colour = "#000000",
             fill = "#B8B525") +
    labs(title="Tweets por día de la semana",
         subtitle = "Febrero",
         caption = "Fuente: Twitter") +
    theme_tufte()

actividaddiariaJunio <- ggplot(tablaactividadModeloJunio) + 
    geom_bar(aes(x = wday(Fecha, week_start = getOption("lubridate.week.start", 1), label = TRUE, abbr = FALSE)),
             colour = "#000000",
             fill = "#B8B525") +
    labs(title="Tweets por día de la semana",
         subtitle = "Junio",
         caption = "Fuente: Twitter") +
    theme_tufte()


### Por Hora

tablaactividadModeloJunio$Hora <- hms::hms(second(tablaactividadModeloJunio$Fecha),
                                           minute(tablaactividadModeloJunio$Fecha),
                                           hour(tablaactividadModeloJunio$Fecha))
tablaactividadModeloJunio$Hora <- as.POSIXct(tablaactividadModeloJunio$Hora)

actividadhoraModeloJunio <- ggplot(data = tablaactividadModeloJunio)+
    geom_density(aes(x = Hora, y = ..scaled..),
                 fill="#B8B525", alpha=0.3) +
    xlab(" ") + ylab("Densidad de Actividad") +
    scale_x_datetime(breaks = date_breaks("2 hours"),
                     labels = date_format("%H:%M")) +
    theme_tufte()



## KPIs

### Métricas Principales

metricasprincipalesPlot <- plot_ly(data = metricasprincipalesDF,
                                   x = ~Métrica) %>%
    add_trace(y = ~`Cerveza Modelo`,
              type = 'bar',
              name = 'Cerveza Modelo',
              marker = list(color = "#B8B525")) %>%
    add_trace(y = ~`Cerveza Tecate`,
              type = 'bar',
              name = 'Cerveza Tecate',
              marker = list(color = "#E34229")) %>%
    layout(title = 'Métricas de Facebook Modelo vs Tecate',
           xaxis = list(title = 'Métricas'),
           yaxis = list(title = 'Totales'),
           barmode = 'group')

###Seguimiento de las Reacciones en Facebook por mes


#### Reacciones de Junio

reaccionesJunioPlot <- plot_ly()
reaccionesJunioPlot <- reaccionesJunioPlot %>% add_pie(data = reaccionesModeloJunio, labels = ~Reacción, values = ~Total,
                         name = "Reacciones Junio", domain = list(row = 0, column = 0))


#### Comparación de reacciones Mayo - Junio

reaccionesPlot <- plot_ly(data = reaccionesDF,
                          x = ~Reacción) %>%
    add_trace(y = ~`Mayo 2021`,
              type = 'bar',
              name = 'Reacciones del Mes de Mayo',
              marker = list(color = "#B8B525")) %>% 
    add_trace(y = ~`Junio 2021`,
              type = 'bar',
              name = 'Reacciones del Mes de Junio',
              marker = list(color = "#1F76EC")) %>% 
    layout(title = 'Reacciones a las Publicaciones Mayo - Junio',
           xaxis = list(title = 'Reacciones'),
           yaxis = list(title = 'Total de Reacciones'),
           barmode = 'group')

### WordClouds

wordcloudMenciones <- hwordcloud(text = tokensModeloJulio$Palabras,
                                 size = tokensModeloJulio$n,
                                 theme = "sunset")


### Sentiment Analysis

#### Emociones

emocionesColores <-  c("#B53810", "#05BF3E","#B53810","#B53810","#05BF3E","#B53810","#05BF3E","#05BF3E")

emocionesplotModeloJulio <- ggplot(data = emocionesModeloJulio, aes(x = Sentimiento, y = Total)) +
    geom_bar(fill = emocionesColores, stat = "identity") +
    geom_text(aes(label = Total), vjust = 1.5, colour = "white", size = 4) +
    labs(title = "Emociones de los Usuarios",
         subtitle = "Julio 2021",
         x=" ",
         y=" ",
         caption = "Fuente: Twitter") +
    theme_tufte()

#### Polaridad

polaridadColores <- c("#B53810", "#05BF3E")

polaridadplotModeloJulio <- ggplot(data = polaridadModeloJulio, aes(x = Reacción, y = Total)) +
    geom_bar(fill = polaridadColores, 
             stat = "identity") +
    geom_text(aes(label = Total), vjust = 2, colour = "white", size = 5.5) +
    labs(title = "Polaridad de las Reacciones",
         subtitle = "Julio 2021",
         x=" ",
         y=" ",
         caption = "Fuente: Twitter") +
    theme_tufte()


#### Timeline de la Polaridad a través del Tiempo

coloresTimeline <- c("#B53810","#05BF3E")

timelinereaccionesModelo <- ggplot(data = reaccionesTimeLineModeloJulio, 
                                   aes(x = as.Date(Fecha), y = Promedio, group = Reacción)) +
    geom_line(size = 0.8, alpha = 0.7, aes(color = Reacción)) +
    geom_point(size = 0.3) +
    ylim(-5, 5) + 
    scale_colour_manual(values = coloresTimeline) +
    theme(legend.title = element_blank(), axis.title.x = element_blank()) +
    scale_x_date(breaks = date_breaks("1 day"),
                 labels = date_format("%b-%d")) +
    ylab("Calificación Promedio de Sentimientos") + 
    xlab("") +
    ggtitle("Reacciones a través del Tiempo") +
    theme_tufte()






#ShinyDashboard

ui <- dashboardPage(
    skin = 'yellow',
    
    dashboardHeader(title = 'Cerveza Modelo'),
    
    dashboardSidebar(collapsed = TRUE,
                     width = 250,
                                sidebarMenu(id = 'sidebar',
                                            menuItem("Actividad en Twitter",
                                                     tabName = 'actividad',
                                                     icon = icon('twitter')),
                                            
                                            menuItem("Social Listening Analysis",
                                                     tabName = "sentiment",
                                                     icon = icon('stats', lib = "glyphicon")))),
                                            
    dashboardBody(
        
       tabItems( 
           
           tabItem(tabName = "actividad",
                   
               fluidRow(
                box(title = 'Actividad',
                id = 'box1',
                solidHeader = TRUE,
                status = 'warning',
                width = 12,
                plotOutput('actividadSemestral'))),
        
               fluidRow(
                valueBox(129.6,
                     "Población en México (Millones)",
                     color = 'yellow',
                     width = 4,
                     icon = icon("fas fa-globe-americas", lib = 'font-awesome')),
            
                valueBox(100,
                         "Usuarios de Redes Sociales (Millones)",
                         color = 'navy',
                         width = 4,
                         icon = icon("fab fa-facebook", lib = 'font-awesome')),
                
                valueBox(92.01,
                         "Usuarios de Internet (Millones)",
                         color = 'yellow',
                         width = 4,
                         icon = icon("fas fa-wifi", lib = 'font-awesome'))),
        
                fluidRow(
                        valueBox(4.27,
                                 "Likes en Facebook (Millones)",
                                 color = 'navy',
                                 width = 4,
                                 icon = icon("fas fa-globe-americas", lib = 'font-awesome')),
                        
                        valueBox(4.25,
                                 "Seguidores en Facebook (Millones)",
                                 color = 'yellow',
                                 width = 4,
                                 icon = icon("fab fa-facebook", lib = 'font-awesome')),
                        
                        valueBox(181.7,
                                 "Seguidores en Twitter (Miles)",
                                 color = 'navy',
                                 width = 4,
                                 icon = icon("fas fa-wifi", lib = 'font-awesome'))),
        
                fluidRow(
                    tabBox(title = 'Actividad',
                           id = 'tabbox1',
                           height = '460px',
                           tabPanel('Actividad Enero', plotlyOutput('actividadporDiaEnero')),
                           tabPanel('Actividad Febrero', plotlyOutput('actividadporDiaFebrero')),
                           tabPanel('Actividad Junio', plotlyOutput('actividadporDiaJunio'))),
                    
                    box(title = 'Actividad',
                        id = 'box1',
                        solidHeader = TRUE,
                        status = 'warning',
                        width = 6,
                        plotlyOutput('actividadHoraria')))),
           
           tabItem(tabName = 'sentiment',
                  
                  fluidRow(
                      box(title = "KPIs",
                          id = "kpis1",
                          solidHeader = TRUE,
                          status = "primary",
                          width = 6,
                          plotlyOutput("metricasPrincipales")),
                      tabBox(title = 'Reacciones en Facebook',
                             id = 'tabboxreacciones',
                             height = '460px',
                             tabPanel('Reacciones en Junio', plotlyOutput('reaccionesJulioFacebook')),
                             tabPanel('Comparación Mayo - Junio', plotlyOutput("reaccionesPrincipales")))),
                    
                  fluidRow(
                      box(title = "WordCloud",
                          id = "nube",
                          solidHeader = TRUE,
                          status = 'warning',
                          width = 12,
                          hwordcloudOutput("wordcloudreferencias"))),
                  
                  fluidRow(
                      box(title = "Emociones",
                          id = "emociones",
                          solidHeader = TRUE,
                          status = "warning",
                          width = 6,
                          plotOutput("emocionesJulio")),
                      box(title = "Polaridad",
                          id = "polaridad",
                          solidHeader = TRUE,
                          status = "primary",
                          width = 6,
                          plotOutput("polaridadJulio"))),
                  
                  fluidRow(
                      box(title = "Timeline",
                          id = "timelinereacciones",
                          solidHeader = TRUE,
                          status = 'primary',
                          width = 12,
                          plotOutput("timelineReacciones"))))
          )
    )
)


server <- function(input, output) {
    
    output$actividadSemestral <- renderPlot({print(plotactividadModeloSemestral)})
    output$actividadporDiaEnero <- renderPlotly({print(ggplotly(actividaddiariaEnero))})
    output$actividadporDiaFebrero <- renderPlotly({print(ggplotly(actividaddiariaFebrero))})
    output$actividadporDiaJunio <- renderPlotly({print(ggplotly(actividaddiariaJunio))})
    output$actividadHoraria <- renderPlotly({print(ggplotly(actividadhoraModeloJunio))})
    
    output$wordcloudreferencias <- renderHwordcloud({print(wordcloudMenciones)})
    output$metricasPrincipales <- renderPlotly({print(metricasprincipalesPlot)})
    output$reaccionesPrincipales <- renderPlotly({print(reaccionesPlot)})
    output$reaccionesJulioFacebook <- renderPlotly({print(reaccionesJunioPlot)})
    output$emocionesJulio <- renderPlot({print(emocionesplotModeloJulio)})
    output$polaridadJulio <- renderPlot({print(polaridadplotModeloJulio)})
    output$timelineReacciones <- renderPlot({print(timelinereaccionesModelo)})
}


shinyApp(ui = ui, server = server)

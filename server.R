library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(haven)
library(scales)
library(dygraphs)
library(sp)
library(sf)
library(readr)
library(stats)
library(ggrepel)
library(DT)
library(leaflet)
library(pals)


function(input, output){
  
  base <- read_rds("data/Estatal-Delitos-2015-2020_ago2020.rds") %>%
    tbl_df() %>%
    gather(Mes, Carpetas, Enero:Diciembre) %>%
    select(-Total) %>%
    setNames(c("Year", "Clave_Ent", "Entidad", "Bien afectado", "Tipo de delito",
               "Subtipo de delito", "Modalidad", "Mes", "Carpetas")) %>%
    mutate(Carpetas = str_replace(Carpetas, pattern = ",", replacement = ""),
           Carpetas = as.numeric(Carpetas),
           Periodo = ymd(paste0(Year, "-", Mes, "-01")),
           Entidad = ifelse(Entidad == "Coahuila de Zaragoza", "Coahuila", 
                            ifelse(Entidad == "Michoacán de Ocampo", "Michoacán", 
                                   ifelse(Entidad == "Veracruz de Ignacio de la Llave", "Veracruz", 
                                          ifelse(Entidad == "México", "Estado de México", Entidad)))))
  
  shp <- st_read("data/Mexico_Estados.shp") %>%
    mutate(ESTADO = as.character(ESTADO),
           ESTADO = ifelse(ESTADO == "México", "Estado de México", ESTADO))
  names(shp) <- c("Código", "Entidad", "geometry")
  
  conapo_est <- reactive({
    read_rds("data/conapo_estatal.rds") %>%
      tbl_df() %>%
      setNames(c("N", "Year", "ENTIDAD", "Clave_Ent", "EDAD", "SEXO", "POBLACION")) %>%
      filter(Year == input$year, ENTIDAD != "República Mexicana") %>%
      group_by(Clave_Ent, ENTIDAD, .drop = FALSE) %>%
      summarise(Poblacion = sum(POBLACION)) %>%
      select(Clave_Ent, Poblacion)
  })
  
  estatal1 <- reactive({
    
    a <- switch(input$year,
                "2020" = 2020,
                "2019" = 2019,
                "2018" = 2018,
                "2017" = 2017,
                "2016" = 2016)
    
    if(a < 2020){
      m <- switch(input$month,
                  "Enero" = 1, "Febrero" = 2, "Marzo" = 3,
                  "Abril" = 4, "Mayo" = 5, "Junio" = 6,
                  "Julio" = 7, "Agosto" = 8, "Septiembre" = 9,
                  "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12,
                  "Total" = 13)} else {
                    m <- switch(input$month2,
                                "Enero" = 1,
                                "Febrero" = 2,
                                "Marzo" = 3,
                                "Abril" = 4,
                                "Mayo" = 5,
                                "Junio" = 6,
                                "Julio"  = 7,
                                "Agosto" = 8)                            
                  }
    
    
    if(m == 13){
      
      if(input$crime != "Total"){
        
        estatal1_2 <- base %>%
          filter(`Subtipo de delito` == input$crime,
                 Year == a) %>%
          group_by(Clave_Ent, Entidad,.drop = FALSE) %>%
          summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
          inner_join(conapo_est(), by = "Clave_Ent") %>%
          mutate(Tasa = Carpetas*100000/Poblacion) %>%
          arrange(Carpetas)
        
      } else {
        
        estatal1_2 <- base %>%
          filter(Year == a) %>%
          group_by(Clave_Ent, Entidad,.drop = FALSE) %>%
          summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
          inner_join(conapo_est(), by = "Clave_Ent") %>%
          mutate(Tasa = Carpetas*100000/Poblacion) %>%
          arrange(Carpetas)
        
      }
      
    } else {
      
      if(input$crime != "Total"){
        
        estatal1_2 <- base %>%
          filter(`Subtipo de delito` == input$crime,
                 Periodo == ymd(paste0(a,"-",m,"-01"))) %>%
          group_by(Clave_Ent, Entidad, .drop = FALSE) %>%
          summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
          inner_join(conapo_est(), by = "Clave_Ent") %>%
          mutate(Tasa = Carpetas*100000/Poblacion) %>%
          arrange(Carpetas)
      } else {
        
        estatal1_2 <- base %>%
          filter(Periodo == ymd(paste0(a,"-",m,"-01"))) %>%
          group_by(Clave_Ent, Entidad, .drop = FALSE) %>%
          summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
          inner_join(conapo_est(), by = "Clave_Ent") %>%
          mutate(Tasa = Carpetas*100000/Poblacion) %>%
          arrange(Carpetas)  
        
      }
      
    }
    
    
    estatal1_2
  })
  

  
  output$g1 <- renderPlot({
    
    
    ggplot(estatal1()) +
      geom_bar(stat = 'identity', 
               aes(reorder(Entidad, Carpetas), Carpetas,
                   fill = Entidad), alpha = .7) +
      geom_text(aes(reorder(Entidad, Carpetas),Carpetas+max(Carpetas)/9),
                label = estatal1()$Carpetas, color = ifelse(estatal1()$Entidad == "Jalisco",
                                                            "#d87a0f", "black"), size = 4) +
      geom_point(aes(Entidad, y = max(Carpetas)*1.4, 
                     size = Tasa, color = Entidad), alpha = .7)+
      geom_text(aes(Entidad, y = max(Carpetas)*1.575,
                    label = round(Tasa,1)), color = ifelse(estatal1()$Entidad == "Jalisco",
                                                           "#d87a0f", "black"), size = 4) +
      
      coord_flip() +
      labs(x = '', y = 'Registros',
           size = 'Tasa')+
      scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10))+
      scale_size(breaks = trans_breaks(identity, identity, n = 4), range = c(3, 10))+
      scale_fill_manual(values = as.vector(glasbey(32)))+
      scale_color_manual(values = as.vector(glasbey(32)))+
      guides(color = FALSE,
             fill = FALSE,
             size=guide_legend(override.aes=list(colour="grey"))) +
      theme(legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            axis.text.x = element_text(size = 11, angle = 90),
            axis.text.y = element_text(size = 11,
                                       face = ifelse(estatal1()$Entidad == "Jalisco",
                                                     "bold", "plain"),
                                       colour = ifelse(estatal1()$Entidad == "Jalisco",
                                                       "#d87a0f", "black")),
            axis.title = element_text(size = 13, color = 'black', face = 'bold'),
            panel.grid.major.y  = element_line(colour = "grey", size = .3, 
                                               linetype = 'dashed'),
            panel.grid.minor.y  = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(fill = 'white'),
            legend.key = element_rect(fill = 'white', color = 'white'),
            legend.title = element_text(size = 15, 
                                        color = 'black',
                                        face = 'bold',
                                        hjust = .5),
            legend.text = element_text(size = 11, 
                                       color = 'black'),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = 'black'), 
            axis.ticks = element_line(colour = 'black'),
            plot.title = element_text(hjust = .5),
            plot.caption = element_text(hjust =0)
      )
  })
  
  output$g4 <- renderPlot({
    
    estatal1.1 <- estatal1() %>%
      mutate(Registros = Carpetas,
             delito = input$crime,
             ent = case_when(Entidad == "Estado de México" ~ "EDOMEX",
                             Entidad == "Ciudad de México" ~ "CDMX",
                             Entidad == "Jalisco" ~ "JAL",
                             Entidad == "Guanajuato" ~ "GTO",
                             Entidad == "Baja California" ~ "BC",
                             Entidad == "Veracruz" ~ "VER",
                             Entidad == "Chihuahua" ~ "CHIH",
                             Entidad == "Nuevo León" ~ "NL",
                             Entidad == "Puebla" ~ "PUE",
                             Entidad == "Coahuila" ~ "COAH",
                             Entidad == "Querétaro" ~ "QRO",
                             Entidad == "San Luis Potosí" ~ "SLP",
                             Entidad == "Tabasco" ~ "TAB",
                             Entidad == "Michoacán" ~ "MICH",
                             Entidad == "Morelos" ~ "MOR",
                             Entidad == "Quintana Roo" ~ "QROO",
                             Entidad == "Oaxaca" ~ "OAX",
                             Entidad == "Hidalgo" ~ "HGO",
                             Entidad == "Aguascalientes" ~ "AGS",
                             Entidad == "Sonora" ~ "SON",
                             Entidad == "Durango" ~ "DGO",
                             Entidad == "Tamaulipas" ~ "TAMPS",
                             Entidad == "Colima" ~ "COL",
                             Entidad == "Zacatecas" ~ "ZAC",
                             Entidad == "Guerrero" ~ "GRO",
                             Entidad == "Sinaloa" ~ "SIN",
                             Entidad == "Baja California Sur" ~ "BCS",
                             Entidad == "Chiapas" ~ "CHIS",
                             Entidad == "Yucatán" ~ "YUC",
                             Entidad == "Tlaxcala" ~ "TLAX",
                             Entidad == "Nayarit" ~ "NAY",
                             Entidad == "Campeche" ~ "CAMP")
             )
    
   ggplot(estatal1.1)+
      geom_boxplot(aes(delito, Tasa), fill = "#CBCBCB") +
      geom_point(aes(delito, Tasa, color = Entidad), size = 4, alpha = .7, position = "jitter")+
      # geom_text(aes(delito, Tasa, label = ent), color = ifelse(estatal1()$Entidad == "Jalisco",
      #                                                                    "#c6004c", "#393d3f"),
      #            size = 4, angle = 90)+
      labs(x = "", y = "Registros por cada 100 mil habitantes")+
      scale_y_continuous(breaks = trans_breaks(identity, identity, n = 10))+
     scale_color_manual(values = as.vector(glasbey(32)))+ 
     coord_flip()+
      theme(legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_blank(),
            axis.title = element_text(size = 13, color = 'black', face = 'bold'),
            panel.grid.major.x  = element_line(colour = "#C3C2C2", size = .2, 
                                               linetype = 'dashed'),
            panel.grid.minor.x  = element_line(colour = "#C3C2C2", size = .2, 
                                               linetype = 'dashed'),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.background = element_rect(fill = '#E4E4E4'),
            legend.key = element_rect(fill = 'white', color = 'white'),
            legend.title = element_text(size = 15, 
                                        color = 'black',
                                        face = 'bold',
                                        hjust = .5),
            legend.text = element_text(size = 11, 
                                       color = 'black'),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = 'black'), 
            axis.ticks = element_line(colour = 'black'),
            plot.title = element_text(hjust = .5),
            plot.caption = element_text(hjust =0),
            strip.text.y = element_text(
              size = 13),
            strip.background = element_rect(
              color="black", fill="#7c878f", size=2, linetype="solid"
            ),
            panel.border = element_rect(color = "black", fill = NA, size = .7),
            legend.position = "none"
      )
  })
  
  
  estatal2 <- reactive({
    
    
    a <- switch(input$year,
                "2020" = 2020,
                "2019" = 2019,
                "2018" = 2018,
                "2017" = 2017,
                "2016" = 2016)
    
    if(a < 2020){
      m <- switch(input$month,
                  "Enero" = 1, "Febrero" = 2, "Marzo" = 3,
                  "Abril" = 4, "Mayo" = 5, "Junio" = 6,
                  "Julio" = 7, "Agosto" = 8, "Septiembre" = 9,
                  "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12,
                  "Total" = 13)
    } else {
      m <- switch(input$month2,
                  "Enero" = 1,
                  "Febrero" = 2,
                  "Marzo" = 3,
                  "Abril" = 4,
                  "Mayo" = 5,
                  "Junio" = 6,
                  "Julio"  = 7,
                  "Agosto" = 8)
      
    }
    
    if(m != 13){
      
      if(input$period == "Mismo mes, año anterior"){
        
        if(input$crime != "Total"){
          
          estatal2_2 <- base %>%
            filter(`Subtipo de delito` == input$crime,
                   Periodo %in% c(ymd(paste0(a,"-",m,"-01")), 
                                  ymd(paste0(a-1,"-",m,"-01")))) %>%
            group_by(Entidad, Periodo, .drop = FALSE) %>%
            summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
            spread(Periodo, Carpetas) %>%
            setNames(c("Entidad", "Previo", "Actual")) %>%
            mutate(Cambio = ifelse(Previo == 0, Actual,Actual/Previo - 1))
          
        } else {
          
          estatal2_2 <- base %>%
            filter(Periodo %in% c(ymd(paste0(a,"-",m,"-01")), 
                                  ymd(paste0(a-1,"-",m,"-01")))) %>%
            group_by(Entidad, Periodo, .drop = FALSE) %>%
            summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
            spread(Periodo, Carpetas) %>%
            setNames(c("Entidad", "Previo", "Actual")) %>%
            mutate(Cambio = ifelse(Previo == 0, Actual,Actual/Previo - 1))
          
        }
        
      } else {
        
        if(input$crime != "Total"){
          
          estatal2_2 <- base %>%
            filter(`Subtipo de delito` == input$crime,
                   Periodo %in% c(ymd(paste0(a,"-",m,"-01")), 
                                  ymd(paste0(a,"-",m,"-01"))%m-% months(1))) %>%
            group_by(Entidad, Periodo, .drop = FALSE) %>%
            summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
            spread(Periodo, Carpetas) %>%
            setNames(c("Entidad", "Previo", "Actual")) %>%
            mutate(Cambio = ifelse(Previo == 0, Actual,Actual/Previo - 1))
          
        } else {
          
          estatal2_2 <- base %>%
            filter(Periodo %in% c(ymd(paste0(a,"-",m,"-01")), 
                                  ymd(paste0(a,"-",m,"-01"))%m-% months(1))) %>%
            group_by(Entidad, Periodo, .drop = FALSE) %>%
            summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
            spread(Periodo, Carpetas) %>%
            setNames(c("Entidad", "Previo", "Actual")) %>%
            mutate(Cambio = ifelse(Previo == 0, Actual,Actual/Previo - 1))
          
        }
      }
      
    } else {
      
      if(input$crime != "Total"){
        
        estatal2_2 <- base %>%
          filter(`Subtipo de delito` == input$crime,
                 Year %in% c(a, a-1)) %>%
          group_by(Entidad, Year, .drop = FALSE) %>%
          summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
          spread(Year, Carpetas) %>%
          setNames(c("Entidad", "Previo", "Actual")) %>%
          mutate(Cambio = ifelse(Previo == 0, Actual,Actual/Previo - 1))
        
      } else {
        
        estatal2_2 <- base %>%
          filter(Year %in% c(a, a-1)) %>%
          group_by(Entidad, Year, .drop = FALSE) %>%
          summarise(Carpetas = sum(as.numeric(Carpetas))) %>%
          spread(Year, Carpetas) %>%
          setNames(c("Entidad", "Previo", "Actual")) %>%
          mutate(Cambio = ifelse(Previo == 0, Actual,Actual/Previo - 1))
        
      }
    }
    
    
    estatal2_2
  })
  
  output$g2 <- renderLeaflet({
    
    paleta <- colorNumeric("viridis", NULL)
    datosMapa <- inner_join(shp, estatal2(), by = "Entidad")
    leaflet(datosMapa) %>%
      addPolygons(color = "black", opacity = .3, weight = .4,
                  fillColor = ~paleta(Cambio),fillOpacity = .57,
                  label = ~paste0(Entidad, ": ", formatC(round(100*Cambio,2), big.mark = ","),"%"),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      
      addLegend("bottomleft",
                pal = paleta,
                values = ~Cambio,
                labFormat = labelFormat(suffix = "%", transform = function(x) 100*x)) %>%
      addTiles()
      })
  
  
  output$g3 <- renderDygraph({
    
    if(input$state2 != "Promedio nacional"){
    
    if(input$crime != "Total"){
      
      delitos <- base %>%
        filter(`Subtipo de delito` == input$crime,
               Entidad %in% c(input$state1, input$state2),
               Periodo >= ymd("2015-01-01"),
               Periodo <= ymd(paste0(2020,"-",8,"-01"))) %>%
        group_by(Entidad, Periodo,.drop = FALSE) %>%
        summarise(Carpetas = sum(Carpetas))
      
    } else {
      
      delitos <- base %>%
        filter(Entidad %in% c(input$state1, input$state2),
               Periodo >= ymd("2015-01-01"),
               Periodo <= ymd(paste0(2020,"-",8,"-01"))) %>%
        group_by(Entidad, Periodo,.drop = FALSE) %>%
        summarise(Carpetas = sum(Carpetas))
      
    }
    
    
    
    est1 <- ts(filter(delitos, Entidad == input$state1)$Carpetas, start = 2015, frequency = 12)
    est2 <- ts(filter(delitos, Entidad == input$state2)$Carpetas, start = 2015, frequency = 12)
    ts <- cbind(est1, est2)
    dimnames(ts)[[2]] <- c(input$state1, input$state2)
    
    
    dygraph(ts)  %>% 
      dyRangeSelector() %>% 
      dyOptions(fillGraph = T, fillAlpha = .05, colors = c("darkblue", "#FBBB27"))
  } else {
    
    if(input$crime != "Total"){
      
      delitos <- base %>%
        filter(`Subtipo de delito` == input$crime,
               #Entidad == input$state1,
               Periodo >= ymd("2015-01-01"),
               Periodo <= ymd(paste0(2020,"-",8,"-01"))) %>%
        mutate(Entidad = ifelse(Entidad == input$state1, input$state1, "Resto")) %>%
        group_by(Entidad, Periodo,.drop = FALSE) %>%
        summarise(Carpetas = sum(Carpetas))
      
    } else {
      
      delitos <- base %>%
        filter(#Entidad == input$state1,
               Periodo >= ymd("2015-01-01"),
               Periodo <= ymd(paste0(2020,"-",8,"-01"))) %>%
        mutate(Entidad = ifelse(Entidad == input$state1, input$state1, "Resto")) %>%
        group_by(Entidad, Periodo,.drop = FALSE) %>%
        summarise(Carpetas = sum(Carpetas))
      
    }
    
    
    est1 <- ts(filter(delitos, Entidad == input$state1)$Carpetas, start = 2015, frequency = 12)
    est2 <- ts(filter(delitos, Entidad == "Resto")$Carpetas/31, start = 2015, frequency = 12)
    ts <- cbind(est1, est2)
    dimnames(ts)[[2]] <- c(input$state1, "Promedio nacional")
    
    
    dygraph(ts)  %>% 
      dyRangeSelector() %>% 
      dyOptions(fillGraph = T, fillAlpha = .05, colors = c("darkblue", "#FBBB27"))
  }
    
  })
  
  
  datos <- reactive({
    base %>%
    filter(Periodo <= ymd(paste0(2020,"-",8,"-01"))) %>%
    mutate(`Periodo anual` = Year) %>%
    select(`Periodo anual`, Mes, Entidad, Clave=Clave_Ent, `Bien afectado`,
          `Tipo de delito`, `Subtipo de delito`, Modalidad,
          Carpetas)
  })
  
  output$download <- downloadHandler(
    filename = "incidencia_delictiva.csv",
    content = function(file){
      write.csv(datos(), file, row.names = FALSE)
    }
  )
  
   output$dtable <-  DT::renderDataTable({
     datos()
   })
   
   output$download2 <- downloadHandler(
     filename = "proyecciones_conapo.csv",
     content = function(file){
       write.csv(read_rds("data/conapo_estatal.rds") %>%
                   tbl_df() %>%
                   setNames(c("n", "Periodo anual", "Entidad", "Clave", "Edad", "Sexo", "Población")) %>%
                   select(-n), file, row.names = FALSE)
     }
   )
   
   output$dtable2 <-  DT::renderDataTable({
     read_rds("data/conapo_estatal.rds") %>%
       tbl_df() %>%
       setNames(c("n", "Periodo anual", "Entidad", "Clave", "Edad", "Sexo", "Población")) %>%
       select(-n)
   })
  
  output$totalcrimes <- renderText({
    estatal1()$Carpetas[which(estatal1()$Entidad == "Jalisco")]
  })
  
  output$crimerate <- renderText({
    round(estatal1()$Tasa[which(estatal1()$Entidad == "Jalisco")],1)
  })
  
  output$crimechange <- renderText({
    paste0(round(100*estatal2()$Cambio[which(estatal2()$Entidad == "Jalisco")],1),"%")
  })
  

  
  output$title1 <- renderText({
    ifelse(input$crime != "Total",
           paste0("Registros de ", tolower(input$crime), " por entidad"),
           "Registro total de delitos por entidad")
  })
 
  output$title2 <- renderText({
    ifelse(input$crime != "Total",
           paste0("Cambio porcentual en registros de ", tolower(input$crime)),
           "Cambio porcentual en registros de incidencia total")
  })
  
  
   
  output$title3 <- renderText({
    ifelse(input$crime != "Total",
           paste0("Distribución estatal de la tasa delictiva por ", tolower(input$crime)),
           "Distribución estatal de la tasa delictiva por el total de registros")
  })
  
  output$title4 <- renderText({
    ifelse(input$crime != "Total",
           paste0("Registro mensual de ", tolower(input$crime)),
           "Registro mensual total de delitos")
  })
}

#remotes::install_github('JonasStage/FluxSeparator')
library(shiny);library(dplyr);library(lubridate)


#This app was orignially created by Kenneth Thorø Martinsen, but further developed by Methane Insight to incorporate the Methane Insight sensor
#FluxBandit
#Shiny app for interactive processing and calculation of greenhouse gas emissions using commercial and DIY type sensor systems
#Kenneth Thorø Martinsen
#https://github.com/KennethTM/FluxBandit

options(shiny.maxRequestSize = 100*1024^2)

read_csv("Data/all_sensor_model_coef.csv") -> model_coef

name <- "Methane sensor calculations"

ui <- fluidPage(
  
  titlePanel(name),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$b("Upload data file"),
      
      fileInput("file", "",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values", 
                           ".csv")),
      
      textInput("sensor_name","Input sensor name for calibration"),
      
      tags$b("Timezone of measurement (UTC)"), numericInput("timezone",
                                                      label = "",
                                                      value = 0,
                                                      min = -12,
                                                      max = 14),
      
      tags$b("Download methane data as csv"),
      
      tags$br(),
      
      #Button to save the file
      downloadButton('ch4_download', 'Download'),
      
      tags$hr(style = "border-top: 3px solid #000000;"),
      
      tags$b("Plotting options"),
      
      tags$p("Adjust time range"),
      
      sliderInput("range", "",
                  ymd_hm("2024-01-01 12:00"),
                  ymd_hm("2024-12-31 12:00"),
                  c(ymd_hm("2024-01-01 12:00"), 
                    ymd_hm("2024-12-31 12:00")),
                  60*60*24, 
                  timezone="+0000"),
      
      tags$p("Adjust methane range"),
      
      sliderInput("ch4_range", "",
                  min = 0, max = 10000, value = c(0, 10000)),
      
      tags$hr(),
      
      tags$p(HTML(paste0("Adjust CO",tags$sub("2")," range"))),
      
      sliderInput("co2_range", "",
                  min = 0, max = 10000, value = c(0, 10000)),
      
      tags$hr(style = "border-top: 3px solid #000000;"),
      
      tags$b("Enter chamber metadata"),
      fluidRow(
        column(4,
          tags$p("Chamber volume (L):")),
        column(4,
          tags$p(HTML(paste0("Chamber area (m",tags$sup(2),"):")))),
        column(4,
          tags$p("Atmos. pressure (atm):"))),
      
      fluidRow(
        column(4,
          numericInput("chamber_vol", NULL, 10, min = 0)),
        column(4,
               numericInput("chamber_area", NULL, 0.5, min = 0)),
        column(4,
               numericInput("atm_pres", NULL, 1, min = 0))),
      
    ),
    
    mainPanel(
      
      tags$b("Main plot"),
      p("Use slider (step 4) to adjust the x-axis"),
      
      plotOutput("plot",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 )),
      
      tags$hr(style = "border-top: 3px solid #000000;"),
      
      tags$b("Zoom plot"),
      p("Use mouse to select measurements in the main plot"),
      
      plotOutput("plot_zoom"),
      
      htmlOutput("result_string"), 
      
      tags$hr(style = "border-top: 3px solid #000000;"),
      
      tags$b("Save flux of zoom plot"),
      
      tags$p("Flux ID (optional):"), 
      
      fluidRow(
        column(4,
               textInput("sample_id", NULL, "ID", width = "200px")),
        column(2,
                actionButton("save", "Save"))),
      
      tags$b("Saved data"),
      p("Table with saved data, export table as '.csv' file using the download button"),
      tableOutput("results"),
      
      tags$b("Download flux data as csv"),
      
      tags$br(),
      
      #Button to save the file
      downloadButton('download', 'Download'),
      
      tags$hr(style = "border-top: 3px solid #000000;"),
      
      tags$i("We thank Kenneth T. Martinsen for help developing this app")
      
    )
    
  )
)

server <- function(input, output, session){
  data <- reactive({
    
    req(input$file)
    req(input$sensor_name)
    req(input$timezone)
    
    model_coef %>% 
      mutate(sensor == str_to_upper(sensor)) %>% 
      filter(sensor == str_to_upper(input$sensor_name)) -> calibration_constants
    
    lookup <- c(rh = "RH")
    
    df <- read_csv(input$file$datapath, skip = 4, col_names = F,
                   col_types = cols(X1 = col_integer(),
                                    X2 = col_integer(),
                                    X3 = col_character(), 
                                    X4 = col_double(), 
                                    X5 = col_double(), 
                                    X6 = col_double(), 
                                    X7 = col_double(), 
                                    X8 = col_double(), 
                                    X9 = col_double(), 
                                    X10 = col_double(), 
                                    X11 = col_double(), 
                                    X12 = col_integer(),
                                    X13 = col_integer())) %>% 
      rename(millis = X1, 
             stampunix = X2,
             datetime = X3,
             RH = X4,
             tempC = X5,
             CH4smV = X6,
             CH4rmV = X7,
             VbatmV = X8,
             K33_RH = X9,
             K33_Temp = X10,
             K33_CO2 = X11,
             SampleNumber = X12,
             PumpCycle = X13) %>% 
      rename(rh = starts_with("RH"), 
             ch4_smv=CH4smV) %>% 
      cbind(calibration_constants) %>% 
      filter(!is.na(datetime),
             between(rh, 0,100)) %>%  
      filter(lead(!is.na(SampleNumber)), !is.na(SampleNumber)) %>% 
      mutate(datetime = ymd_hms(datetime),
             airt = as.numeric(tempC),
             abs_H = (6.112*exp((17.67*airt)/(airt+243.5))*rh*18.02)/((273.15+airt)*100*0.08314),
             ppm_H20 = 1358.326542*abs_H,
             co2 = (K33_CO2/(1-(ppm_H20/10^6))),
             V0 = abs_H*5.160442+268.39739,
             RsR0 = ((5000/ch4_smv)-1)/((5000/V0)-1),
             ch4 = a*(RsR0^b)+c*abs_H*(a*RsR0^b) + K,
             datetime = datetime+(input$timezone-1)*3600) %>% 
      rename(water = ppm_H20) %>% 
      group_by(datetime) %>% 
      summarise_at(vars(rh, airt, co2, ch4, water,PumpCycle), list(mean)) %>% 
      select(datetime, rh, airt, co2, ch4, water,PumpCycle) 
    
    time_start <- min(df$datetime, na.rm =T)
    time_end <- max(df$datetime, na.rm =T)
    
    ch4_start <- floor(min(df$ch4, na.rm=T))
    ch4_end <- ceiling(max(df$ch4, na.rm=T))
    
    co2_start <- floor(min(df$co2, na.rm =T))
    co2_end <- ceiling(max(df$co2, na.rm =T))
    
    updateSliderInput(session, "range", value = c(time_start, time_end),
                      min = time_start, max = time_end, step = 60)
    
    updateSliderInput(session, "ch4_range", value = c(ch4_start, ch4_end),
                      min = ch4_start, max = ch4_end, step = 1)
    
    updateSliderInput(session, "co2_range", value = c(co2_start, co2_end),
                      min = co2_start, max = co2_end, step = 1)
    
    return(df)
    
    
  })
  
  data_out <- data.frame()
  
  output$plot <- renderPlot({
    
    data_subset <- data() %>% 
      filter(between(datetime, input$range[1], input$range[2]),
             between(ch4, input$ch4_range[1],input$ch4_range[2]),
             between(co2, input$co2_range[1],input$co2_range[2]))
    
    #par(mar = c(5,4,4,4) + 0.1)
    
    # plot(x = data_subset$datetime,
    #      y = data_subset$ch4,
    #      ylab=expression("CH"[4]*" (ppm)"), 
    #      xlab="Datetime",
    #      main = "Overview plot",
    #      col = "darkorange")
    
    data() %>% 
      reframe(across(co2, c(mean = mean,var = var))) -> co2_status
    
    ggplot() + 
      geom_point(data = data_subset, aes(datetime, ch4, col = "CH4")) + 
      labs(x = "Datetime",
           y = bquote("CH"[4]*" (ppm)"),
           col = "") + 
      scale_color_manual(limits = c("CH4"),
                         labels = c(expression("CH"[4])),
                         values = c("darkorange")) + 
      scale_x_datetime(date_breaks = "10 min",
                       date_minor_breaks = "1 min",
                       date_labels = "%R") + 
      theme_bw() + 
      theme(legend.position = "bottom") -> op1
    
    if (co2_status$co2_mean == 0 & co2_status$co2_var == 0) {
      # legend("topright", 
      #        c(expression("CH"[4])), 
      #        col = "darkorange", pch=19)
      op1
    } else {
      
      #mtext(expression("CO"[2]*" (ppm)"), side = 4, line = 3, col="forestgreen")
      
      co2_min = min(data_subset$co2)
      co2_max = max(data_subset$co2)
      ch4_min = min(data_subset$ch4)
      ch4_max = max(data_subset$ch4)
      
      ch4_scaled = (co2_max - co2_min)*((data_subset$ch4-ch4_min)/(ch4_max - ch4_min))+co2_min
      ch4_labels = pretty(data_subset$ch4)
      ch4_at = (co2_max - co2_min)*((ch4_labels-ch4_min)/(ch4_max - ch4_min))+co2_min
      
      co2_scaled = (ch4_max - ch4_min)*((data_subset$co2-co2_min)/(co2_max - co2_min))+ch4_min
      co2_labels = pretty(data_subset$co2)
      co2_at = (ch4_max - ch4_min)*((co2_labels-co2_min)/(co2_max - co2_min))+ch4_min
      
      # points(x = data_subset$datetime, y = co2_scaled, col="forestgreen")
      # axis(4, at = co2_at, labels = co2_labels, col="forestgreen", col.ticks="forestgreen")
      # 
      # legend("topright", 
      #        c(expression("CH"[4]), expression("CO"[2])), 
      #        col = c("darkorange","forestgreen"), pch=19)
      
      op1 + 
        geom_point(data = data_subset, aes(datetime, co2_scaled, col = "CO2")) + 
        labs(x = "Datetime",
             y = bquote("CH"[4]*" (ppm)"),
             col = "") + 
        scale_y_continuous(sec.axis = sec_axis(trans=~., name = bquote("CO"[2]*" (ppm)"),
                                               breaks = co2_at, labels = co2_labels)) +
        scale_color_manual(limits = c("CH4","CO2"),
                           labels = c(expression("CH"[4]),expression("CO"[2])),
                           values = c("darkorange", "forestgreen")) +
        scale_x_datetime(date_breaks = "1 hour",
                         date_minor_breaks = "30 min",
                         date_labels = "%R")
    }
    
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  data_subset <- reactive({
    req(input$file)
    
    data() %>% 
      reframe(across(co2, c(mean = mean,var = var))) -> co2_status
    
    if (!is.null(ranges2$x)) {
      ranges2$x <- as_datetime(ranges2$x)
      
      data_subset <- data() %>%
        filter(between(datetime, ranges2$x[1], ranges2$x[2])) %>%
        mutate(sec = cumsum(c(0, diff(as.numeric(datetime))))) 
      
      lm_model_ch4 <- lm(ch4~sec, data = data_subset)
      slope_ch4 <- coef(lm_model_ch4)[2]
      intercept_ch4 <- coef(lm_model_ch4)[1]
      r2_ch4 <- summary(lm_model_ch4)$r.squared
      
      
      mean_temp <- mean(data_subset$airt)
      
      R <- 0.08206 #L atm K^-1 mol^-1
      
      ch4_flux <- (slope_ch4*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)*input$chamber_area)
      
      # legend("topright", 
      #        c(expression("CH"[4])), 
      #        col = "darkorange", pch=19)
      
      if (co2_status$co2_mean == 0 & co2_status$co2_var == 0) {
        
        
        results_string <- paste0("<b>CH<sub>4</sub></b>: slope = ", round(slope_ch4*3600, 2), " (ppm h<sup>-1</sup>)",
                                 ", flux = ", round(ch4_flux*3600, 2), " (µmol m<sup>-2</sup> h<sup>-1</sup>)",
                                 ", R<sup>2</sup> = ", round(r2_ch4, 2))
        
        results <- data.frame("processing_date" = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "id" = as.character(input$sample_id),
                              "start" = strftime(ranges2$x[1], "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "end" = strftime(ranges2$x[2], "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "CH4_slope" = slope_ch4*3600,
                              "CH4_intercept" = intercept_ch4,
                              "CH4_R2" = r2_ch4,
                              "temperature" = mean_temp, 
                              "chamber_volume" = input$chamber_vol,
                              "chamber_area" = input$chamber_area,
                              "CH4_flux_umol_m2_h" = ch4_flux*3600)
        
      } else {
        
        lm_model_co2 <- lm(co2~sec, data = data_subset)
        slope_co2 <- coef(lm_model_co2)[2]
        intercept_co2 <- coef(lm_model_co2)[1]
        r2_co2 <- summary(lm_model_co2)$r.squared
        
        co2_flux <- (slope_co2*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)*input$chamber_area)
        
        results_string <- paste0("<b>CH<sub>4</sub></b>: slope = ", round(slope_ch4*3600, 2), " (ppm h<sup>-1</sup>)",
                                 ", flux = ", round(ch4_flux*3600, 2), " (µmol m<sup>-2</sup> h<sup>-1</sup>)",
                                 ", R<sup>2</sup> = ", round(r2_ch4, 2),
                                 "<br>", 
                                 "<b>CO<sub>2</sub></b>: slope = ", round(slope_co2*3600, 2), " (ppm h<sup>-1</sup>)", 
                                 ", flux = ", round(co2_flux*3600, 2), " (µmol m<sup>-2</sup> h<sup>-1</sup>)", 
                                 ", R<sup>2</sup> = ", round(r2_co2, 2))
        
        results <- data.frame("processing_date" = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "id" = as.character(input$sample_id),
                              "start" = strftime(ranges2$x[1], "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "end" = strftime(ranges2$x[2], "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "CH4_slope" = slope_ch4*3600,
                              "CH4_intercept" = intercept_ch4,
                              "CH4_R2" = r2_ch4,
                              "CO2_slope" = slope_co2*3600,
                              "CO2_intercept" = intercept_co2,
                              "CO2_R2" = r2_co2,
                              "temperature" = mean_temp, 
                              "chamber_volume" = input$chamber_vol,
                              "chamber_area" = input$chamber_area,
                              "CH4_flux_umol_m2_h" = ch4_flux*3600,
                              "CO2_flux_umol_m2_h" = co2_flux*3600)
      }  
          }else{
            data_subset <- data() %>% 
              mutate(sec = cumsum(c(0, diff(as.numeric(datetime))))) 
            
            results <- data.frame()
            results_string <- ""
          }
    
    return(list("df" = data_subset, 
                "results" = results, 
                "results_string" = results_string,
                "co2_status" = co2_status))
    
              })
  
  output$plot_zoom <- renderPlot({
    
    zoom_data <- data_subset() 
    
    zoom_plot_data <- zoom_data$df %>% 
      filter(between(ch4,input$ch4_range[1], input$ch4_range[2]),
             between(co2,input$co2_range[1],input$co2_range[2]))
    print(zoom_plot_data)
    
    par(mar = c(5,4,4,4) + 0.1)
    
    # plot(x = zoom_data$df$sec,
    #      y = zoom_data$df$ch4,
    #      ylab=expression("CH"[4]*" (ppm)"), 
    #      xlab="Time steps",
    #      main= "Zoom plot",
    #      col = "darkorange")
    
    
    water_min = min(zoom_plot_data$water)
    ch4_min = min(zoom_plot_data$ch4)
    ch4_max = max(zoom_plot_data$ch4)
    water_max = max(zoom_plot_data$water)
    water_scaled = (ch4_max - ch4_min)*((zoom_plot_data$water-water_min)/(water_max - water_min))+ch4_min
    # points(x = zoom_data$df$sec, y = water_scaled, col="lightblue", type="l")
    
    ggplot() + 
      geom_point(data = zoom_plot_data, aes(sec, ch4, col = "CH4")) +
      geom_smooth(data = zoom_plot_data, aes(sec, ch4, col = "CH4"), method = "lm", se = F, formula = 'y ~ x') +
      geom_line(data = zoom_plot_data, aes(sec, water_scaled, col = "H2O")) +
      labs(x = "Time steps",
           y = bquote("CH"[4]*" (ppm)"),
           col = "") + 
      scale_color_manual(limits = c("CH4","H2O"),
                         labels = c(expression("CH"[4]),expression("H"[2]*"O")),
                         values = c("darkorange", "lightblue")) + 
      theme_bw() + 
      theme(legend.position = "bottom") -> p1
    
    if (zoom_data$co2_status$co2_mean == 0 & zoom_data$co2_status$co2_var == 0) {
      print(p1)
      if (!is.null(ranges2$x)){
        output$result_string <- renderText(zoom_data$results_string)
      }
      
      # abline(zoom_data$results$CH4_intercept,
      #        zoom_data$results$CH4_slope/3600,
      #        col = "darkorange", lwd = 4)
      
      
      # 
      # legend("topright", 
      #        c(expression("CH"[4]), expression("H"[2]*"O")), 
      #        col = c("darkorange", "lightblue"), pch=19)
      
    } else {
      
      # mtext(expression("CO"[2]*" (ppm)"), side = 4, line = 3, col="forestgreen")
      co2_min = min(zoom_plot_data$co2)
      co2_max = max(zoom_plot_data$co2)
      
      co2_scaled = (ch4_max - ch4_min)*((zoom_plot_data$co2-co2_min)/(co2_max - co2_min))+ch4_min
      co2_labels = pretty(zoom_plot_data$co2)
      co2_at = (ch4_max - ch4_min)*((co2_labels-co2_min)/(co2_max - co2_min))+ch4_min
      
      ch4_scaled = (co2_max - co2_min)*((zoom_plot_data$ch4-ch4_min)/(ch4_max - ch4_min))+co2_min
      ch4_labels = pretty(zoom_plot_data$ch4)
      ch4_at = (co2_max - co2_min)*((ch4_labels-ch4_min)/(ch4_max - ch4_min))+co2_min
      
      # points(x = zoom_data$df$sec, y = co2_scaled, col="forestgreen")
      # axis(4, at = co2_at, labels = co2_labels, col="forestgreen", col.ticks="forestgreen") 
      
      if (!is.null(ranges2$x)){
        
        output$result_string <- renderText(zoom_data$results_string)
      }
      
      # abline(zoom_data$results$CH4_intercept,
      #        zoom_data$results$CH4_slope/3600,
      #        col = "darkorange", lwd = 4)
      
      lm_model_co2_scaled <- lm(co2_scaled~zoom_plot_data$sec,na.action=na.exclude)
      slope_co2_scaled <- coef(lm_model_co2_scaled)[2]
      intercept_co2_scaled <- coef(lm_model_co2_scaled)[1]
      
      # abline(intercept_co2_scaled,
      #        slope_co2_scaled,
      #        col = "forestgreen", lwd = 4)
      
      
      # legend("topright", 
      #      c(expression("CH"[4]),expression("CO"[2]), expression("H"[2]*"O")), 
      #      col = c("darkorange", "forestgreen", "lightblue"), pch=19)
      
      p1 + 
        geom_point(data = zoom_plot_data, aes(sec, co2_scaled, col = "CO2")) +
        geom_smooth(data = zoom_plot_data, aes(sec, co2_scaled, col = "CO2"), method = "lm", se = F,formula = 'y ~ x') + 
        #geom_abline(slope = slope_co2_scaled, intercept = intercept_co2_scaled, aes(col = "CO2")) +
        scale_color_manual(limits = c("CH4","CO2","H2O"),
                           labels = c(expression("CH"[4]),expression("CO"[2]),expression("H"[2]*"O")),
                           values = c("darkorange","forestgreen", "lightblue")) +
        scale_y_continuous(sec.axis = sec_axis(trans=~., name = bquote("CO"[2]*" (ppm)"),
                                               breaks = co2_at, labels = co2_labels)) 
    }
  })
  
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  observeEvent(input$save,{
    data_out <<- rbind(data_out, data_subset()$results)
    output$results <- renderTable(data_out[, c("id", "start", "end", 
                                               "CH4_R2", "CH4_flux_umol_m2_h",
                                               "CO2_R2", "CO2_flux_umol_m2_h")])
    
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(version, "-", Sys.Date(), "_fluxdata.csv")
    },
    
    content = function(file) {
      write.csv(data_out, file, row.names = FALSE)
    })  
  
  output$ch4_download <- downloadHandler(
    
    filename = function() {
      paste0(version, "-", Sys.Date(), "_ch4data.csv")
    },
    
    content = function(file) {
      data() %>% 
        rename('rh (%)' = rh, 'airt (°C)' = airt, 'CO2 (ppm)' = co2, 'CH4 (ppm)' = ch4, "H2O (ppm)" = water) ->data_write
      write.csv(data_write, file, row.names = FALSE)
    })  
  
}

shinyApp(ui, server)
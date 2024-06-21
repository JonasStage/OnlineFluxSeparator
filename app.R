#remotes::install_github('JonasStage/FluxSeparator')
library(shiny);library(dplyr);library(lubridate);library(FluxSeparator)


#This app was orignially created by Kenneth Thorø Martinsen, but further developed by Methane Insight to incorporate the Methane Insight sensor
#FluxBandit
#Shiny app for interactive processing and calculation of greenhouse gas emissions using commercial and DIY type sensor systems
#Kenneth Thorø Martinsen
#https://github.com/KennethTM/FluxBandit

calibration_values <- tibble(a = c(15.4768338368636,20.8996128188715,12.0845179165111,12.2594795072657,13.9216687951681,49.0404640934359,11.7011477005422,9.6180853386033,13.1916048746485,11.7554751062074,31.0227651438435,29.1574861544143,26.6193278389359,23.8469981981315,29.9795114367573,29.2294056064029,28.0100368200649),
                             b = c(-2.32360295751295,-2.22627910918598,-2.3787693734589,-2.39192348962588,-2.34249066118252,-1.65550641092781,-2.37851604735124,-2.44571700230746,-2.34121741388962,-2.39340719818715,-1.65251694906984,-1.64569674887371,-1.67228132720796,-1.7548224913487,-1.67515145390934,-1.66729080836813,-1.63808073922792),
                             c = c(0.0126397200417818,0.00822950843337005,0.0163314353163035,0.0154301494872903,0.0125812082880863,-0.00187060171893217,0.0146885897494805,0.0165647799054871,0.0119841220501089,0.014910073633549,0.016724087602713,0.017660492924417,0.0183358457211479,0.0196049052667293,0.0157013643210769,0.0164819759211981,0.0179553258480187),
                             K = c(-16.9608100504465,-21.4564061807386,-13.7341088044704,-13.6483519113557,-14.7282666782522,-38.4740785534197,-12.5266323282912,-10.2113099605425,-13.5405790944116,-12.7672040616553,-35.3915382672584,-33.7280055392781,-31.0614833057482,-28.0406584405676,-33.712226027174,-33.1443929000967,-32.8184662774513),
                             sn = c('20240001','20240002','20240003','20240004','20240005','20240006','20240007','20240008','20240009','20240010','20240011','20240012','20240013','20240014','20240015','20240017','20240018'))

version <- "Methane Insight fluxR"

ui <- fluidPage(
  
  titlePanel(version),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$b("Upload data file"),
      
      fileInput("file", "",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values", 
                           ".csv")),
      
      tags$hr(),
      
      tags$p("Timezone of measurement"), selectizeInput("timezone", "Timezone", width="100%",  
                                           choices = list("GMT-12"="GMT-12",
                                                          "GMT-11"="GMT-11",
                                                          "GMT-10"="GMT-10",
                                                          "GMT-9"="GMT-9",
                                                          "GMT-8"="GMT-8",
                                                          "GMT-7"="GMT-7",
                                                          "GMT-6"="GMT-6",
                                                          "GMT-5"="GMT-5",
                                                          "GMT-4"="GMT-4",
                                                          "GMT-3"="GMT-3",
                                                          "GMT-2"="GMT-2",
                                                          "GMT-1"="GMT-1",
                                                          "GMT"="GMT",
                                                          "GMT+1"="GMT+1",
                                                          "GMT+2"="GMT+2",
                                                          "GMT+3"="GMT+3",
                                                          "GMT+4"="GMT+4",
                                                          "GMT+5"="GMT+5",
                                                          "GMT+6"="GMT+6",
                                                          "GMT+7"="GMT+7",
                                                          "GMT+8"="GMT+8",
                                                          "GMT+9"="GMT+9",
                                                          "GMT+10"="GMT+10",
                                                          "GMT+11"="GMT+11",
                                                          "GMT+12"="GMT+12")),
      tags$hr(),
      
      tags$b("Enter metadata"),
      
      tags$p("Chamber volume (L):"), numericInput("chamber_vol", NULL, 10, min = 0, width = "100px"),
      
      tags$p("Chamber area (m2):"), numericInput("chamber_area", NULL, 0.5, min = 0, width = "100px"),
      
      tags$p("Atmos. pressure (atm):"), numericInput("atm_pres", NULL, 1, min = 0, width = "100px"),
      
      tags$b("Adjust time range"),
      
      sliderInput("range", "",
                  ymd_hm("2024-01-01 12:00"),
                  ymd_hm("2024-12-31 12:00"),
                  c(ymd_hm("2024-01-01 12:00"), 
                    ymd_hm("2024-12-31 12:00")),
                  60*60*24, 
                  timezone="+0000"),
      
      tags$hr(),
      
      tags$b("Select and save flux (repeat)"),
      
      tags$br(),
      
      tags$p("Flux ID (optional):"), textInput("sample_id", NULL, "ID", width = "200px"),
      
      actionButton("save", "Save"),
      
      tags$hr(),
      
      tags$b("Download data"),
      
      tags$br(),
      
      #Button to save the file
      downloadButton('download', 'Download')
      
    ),
    
    mainPanel(
      
      tags$b("Main plot"),
      p("Use slider (step 4) to adjust the x-axis"),
      
      plotOutput("plot",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 )),
      
      tags$hr(),
      
      tags$b("Zoom plot"),
      p("Use mouse to select measurements in the main plot"),
      
      plotOutput("plot_zoom"),
      
      htmlOutput("result_string"), 
      
      tags$hr(),
      
      tags$b("Saved data"),
      p("Table with saved data, export table as '.csv' file using the download button"),
      tableOutput("results"),
      
      tags$hr(),
      
      tags$i("We thank Kenneth T. Martinsen for help developing this app")
      
    )
    
  )
)

server <- function(input, output, session){
  data <- reactive({
    
    req(input$file)
    
    read_csv(input$file$datapath, col_names = F) %>% 
      slice(1:2) %>% 
      separate(X1, c("col", "number"), ":") %>% 
      mutate(number = str_trim(number)) -> sensor_info
    
    calibration_values %>% 
      filter(sn == sensor_info$number[sensor_info$col == "Serial number"]) -> calibration_constants
    
    lookup <- c(rh = "RH")
    
    df <- read_csv(input$file$datapath, skip = 3) %>% 
      rename(rh = starts_with("RH"), 
             ch4_smv=CH4smV) %>% 
      cbind(calibration_constants) %>% 
      filter(!is.na(datetime)) %>%  
      filter(lead(!is.na(SampleNumber)), !is.na(SampleNumber)) %>% 
      mutate(datetime = ymd_hms(datetime),
             airt = as.numeric(tempC),
             abs_H = (6.112*exp((17.67*airt)/(airt+243.5))*rh*18.02)/((273.15+airt)*100*0.08314),
             ppm_H20 = 1358.326542*abs_H,
             co2 = (K33_CO2/(1-(ppm_H20/10^6))),
             V0 = abs_H*5.160442+268.39739,
             RsR0 = ((5000/ch4_smv)-1)/((5000/V0)-1),
             ch4 = a*(RsR0^b)+c*abs_H*(a*RsR0^b) + K,
             datetime = case_when(input$timezone == "GMT-12" ~ datetime-5*3600,
                                  input$timezone=="GMT-11" ~ datetime-4*3600,
                                  input$timezone=="GMT-10" ~ datetime-3*3600,
                                  input$timezone=="GMT-9" ~ datetime-2*3600,
                                  input$timezone=="GMT-8" ~ datetime-1*3600,
                                  input$timezone=="GMT-7" ~ datetime-0*3600,
                                  input$timezone=="GMT-6" ~ datetime+1*3600,
                                  input$timezone=="GMT-5" ~ datetime+2*3600,
                                  input$timezone=="GMT-4" ~ datetime+3*3600,
                                  input$timezone=="GMT-3" ~ datetime+4*3600,
                                  input$timezone=="GMT-2" ~ datetime+5*3600,
                                  input$timezone=="GMT-1" ~ datetime+6*3600,
                                  input$timezone=="GMT" ~ datetime+7*3600,
                                  input$timezone=="GMT+1" ~ datetime+8*3600,
                                  input$timezone=="GMT+2" ~ datetime+9*3600,
                                  input$timezone=="GMT+3" ~ datetime+10*3600,
                                  input$timezone=="GMT+4" ~ datetime+11*3600,
                                  input$timezone=="GMT+5" ~ datetime+12*3600,
                                  input$timezone=="GMT+6" ~ datetime+13*3600,
                                  input$timezone=="GMT+7" ~ datetime+14*3600,
                                  input$timezone=="GMT+8" ~ datetime+15*3600,
                                  input$timezone=="GMT+9" ~ datetime+16*3600,
                                  input$timezone=="GMT+10" ~ datetime+17*3600,
                                  input$timezone=="GMT+11" ~ datetime+18*3600,
                                  input$timezone=="GMT+12" ~ datetime+19*3600)) %>% 
      rename(water = ppm_H20) %>% 
      group_by(datetime) %>% 
      summarise_at(vars(rh, airt, co2, ch4, water), list(mean)) %>% 
      select(datetime, rh, airt, co2, ch4, water) 
    
    time_start <- min(df$datetime)
    time_end <- max(df$datetime)
    
    updateSliderInput(session, "range", value = c(time_start, time_end),
                      min = time_start, max = time_end, step = 60)
    
    return(df)

    
  })
  
  data_out <- data.frame()
  
  output$plot <- renderPlot({
    
    data_subset <- data() %>% 
      filter(between(datetime, input$range[1], input$range[2]))
    
    par(mar = c(5,4,4,4) + 0.1)
    
    plot(x = data_subset$datetime,
         y = data_subset$ch4,
         ylab=expression("CH"[4]*" (ppm)"), 
         xlab="Datetime",
         main = "Overview plot",
         col = "darkorange")
    
     data() %>% 
        reframe(across(co2, c(mean = mean,var = var))) -> co2_status
    
    if (co2_status$co2_mean == 0 & co2_status$co2_var == 0) {
      legend("topright", 
             c(expression("CH"[4])), 
             col = "darkorange", pch=19)
      } else {
    
      mtext(expression("CO"[2]*" (ppm)"), side = 4, line = 3, col="forestgreen")
  
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
      
      points(x = data_subset$datetime, y = co2_scaled, col="forestgreen")
      axis(4, at = co2_at, labels = co2_labels, col="forestgreen", col.ticks="forestgreen")
      
      legend("topright", 
             c(expression("CH"[4]), expression("CO"[2])), 
             col = c("darkorange","forestgreen"), pch=19)
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
      
        legend("topright", 
               c(expression("CH"[4])), 
               col = "darkorange", pch=19)
     
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
    
    par(mar = c(5,4,4,4) + 0.1)
    
    plot(x = zoom_data$df$sec,
         y = zoom_data$df$ch4,
         ylab=expression("CH"[4]*" (ppm)"), 
         xlab="Time steps",
         main= "Zoom plot",
         col = "darkorange")
    
    water_min = min(zoom_data$df$water)
    ch4_min = min(zoom_data$df$ch4)
    ch4_max = max(zoom_data$df$ch4)
    water_max = max(zoom_data$df$water)
    water_scaled = (ch4_max - ch4_min)*((zoom_data$df$water-water_min)/(water_max - water_min))+ch4_min
    points(x = zoom_data$df$sec, y = water_scaled, col="lightblue", type="l")
    
    if (zoom_data$co2_status$co2_mean == 0 & zoom_data$co2_status$co2_var == 0) {
      if (!is.null(ranges2$x)){
        
        output$result_string <- renderText(zoom_data$results_string)
      }
        
        abline(zoom_data$results$CH4_intercept,
               zoom_data$results$CH4_slope/3600,
               col = "darkorange", lwd = 4)
      
        
      
      legend("topright", 
             c(expression("CH"[4]), expression("H"[2]*"O")), 
             col = c("darkorange", "lightblue"), pch=19)
      
      } else {
      
      mtext(expression("CO"[2]*" (ppm)"), side = 4, line = 3, col="forestgreen")
      co2_min = min(zoom_data$df$co2)
      co2_max = max(zoom_data$df$co2)
        
      co2_scaled = (ch4_max - ch4_min)*((zoom_data$df$co2-co2_min)/(co2_max - co2_min))+ch4_min
      co2_labels = pretty(zoom_data$df$co2)
      co2_at = (ch4_max - ch4_min)*((co2_labels-co2_min)/(co2_max - co2_min))+ch4_min
     
      ch4_scaled = (co2_max - co2_min)*((zoom_data$df$ch4-ch4_min)/(ch4_max - ch4_min))+co2_min
      ch4_labels = pretty(zoom_data$df$ch4)
      ch4_at = (co2_max - co2_min)*((ch4_labels-ch4_min)/(ch4_max - ch4_min))+co2_min
      
      points(x = zoom_data$df$sec, y = co2_scaled, col="forestgreen")
      axis(4, at = co2_at, labels = co2_labels, col="forestgreen", col.ticks="forestgreen") 
      
      if (!is.null(ranges2$x)){
        
        output$result_string <- renderText(zoom_data$results_string)
      }
      
      abline(zoom_data$results$CH4_intercept,
             zoom_data$results$CH4_slope/3600,
             col = "darkorange", lwd = 4)
      
      lm_model_co2_scaled <- lm(co2_scaled~zoom_data$df$sec,na.action=na.exclude)
      slope_co2_scaled <- coef(lm_model_co2_scaled)[2]
      intercept_co2_scaled <- coef(lm_model_co2_scaled)[1]
      
      abline(intercept_co2_scaled,
             slope_co2_scaled,
             col = "forestgreen", lwd = 4)
  
    
      legend("topright", 
           c(expression("CH"[4]),expression("CO"[2]), expression("H"[2]*"O")), 
           col = c("darkorange", "forestgreen", "lightblue"), pch=19)
    
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
      paste0(version, "-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(data_out, file, row.names = FALSE)
    })  
  
}

shinyApp(ui, server)
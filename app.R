library(shiny);library(dplyr);library(lubridate);library(ggplot2);library(shinydashboard);library(TTR);library(patchwork);library(HMR);library(DT)
library(readr);library(stringr);library(tidyr);library(purrr)
source("ebul_flux_func.R", local = T)
source("diff_flux_func.R", local = T)

#This app was orignially created by Kenneth Thorø Martinsen, but further developed by Methane Insight to incorporate the Methane Insight sensor
#FluxBandit
#Shiny app for interactive processing and calculation of greenhouse gas emissions using commercial and DIY type sensor systems
#Kenneth Thorø Martinsen
#https://github.com/KennethTM/FluxBandit
options(shiny.maxRequestSize = 100*1024^2)

read_csv("Data/all_sensor_model_coef.csv") %>% 
  rename_with(str_to_lower,everything()) -> model_coef

ui <- dashboardPage(
  dashboardHeader(title = "Flux calculations"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome", tabName = "intro", icon = icon("door-open")),
      menuItem("Start", tabName = "start", icon = icon("play")),
      menuItem("Bubble flux", tabName = "ebul", icon = icon("circle")),
      menuItem("Diffusive flux", tabName = "diff", icon = icon("chart-line"))
      )
    ),
    dashboardBody(
      tabItems(
        # Intro UI tab ----
        tabItem(tabName = "intro",
                HTML("
                     <h2>Introduction to the Methane flux calculator</h2>
                     Welcome to this website which helps to calculate fluxes. 
                     The website allows the user to separate ebullitive (bubble) and diffusive fluxes. 
                     The technique for separation ebullitive and diffusive fluxes is based on the R-package <a href='https://github.com/JonasStage/FluxSeparator'> <em>FluxSeparator</em></a> with minor alterations. 
                     Similar results can be obtained by using the R-package in R, however, this website allows for easy usage and testing how different parameters affect your fluxes.<br>
                     
                     This website was originally made to ease the usage of DIY sensors for <a href='https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2024JG008035'> CH<sub>4</sub> and CO<sub>2</sub></a> supplied by the author. 
                     Nevertheless, increasing interest fueled me to incorporate other types of sensors.<br>
                     <h2>Theory behind the calculations </h2>
                 
                 
                     <h3>Ebullitive flux</h3>
                     Most notable is the runvar_cutoff variable which determines the threshold value of the running variance, with running variances above this threshold being considered bubbles.
                     Furthermore, selections allow the user to include more or less observations before and after the bubble, or set a minimum threshold for the concentration change caused by the ebullitive event.<br>
                     <h3>Diffusive flux</h3>
                     The diffusive flux allows the user to also consider the bubbles from the ebullitive flux function. 
                     If this is selected the function will only calculate diffuisve fluxes before any bubbles occur, to ensure concentrations are not elevated. 
                     A possibility to not look for bubbles is also present, which is useful when looking at CO<sub>2</sub> fluxes.
                     For more information on how to use the different parameters and how the separation of fluxes is done, see <a href='https://doi.org/10.1029/2024JG008035'> this paper</a> <br><br>
                     
                     <h2>Citing this tool</h2>
                     Please cite the <a href='https://doi.org/10.1029/2024JG008035'> paper introducing the R-package</a> and the <a href='https://doi.org/10.5281/zenodo.8297154'> R-package </a> when using this tool: <br><br>
                     <em>Sø, J. S., Sand-Jensen, K., & Kragh, T. (2024). Self-made equipment for automatic methane diffusion and ebullition measurements from aquatic environments. Journal of Geophysical Research: Biogeosciences, 129, e2024JG008035. https://doi.org/10.1029/2024JG008035</em><br><br>
                     <em>Sø, J. S., Sand-Jensen, K., & Kragh, T. (2023). FluxSeparator - Separation of diffusive and ebullitive fluxes (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.8297154</em><br>
                    
                     <h2>Wrapping up</h2>
                     
                     I expect there will be encounters of errors while using the website, which I would gladly try to accomodate, so please let me know.
                     Additionally, don't hesitate to contact me if you have ideas on how to improve this website.<br><br>
                     This website is made by <a href='mailto:Jonassoe@biology.sdu.dk'</a> Jonas Stage Sø </a>, Ph.D., Postdoc at The University of Southern Denmark. <br><br>
                     
                     <center>
                     <a href='https://www.researchgate.net/profile/Jonas-Stage-So?ev=hdr_xprf' <i class='fa-brands fa-researchgate' style='font-size: 6em'></i></a>
                     <a href='https://github.com/JonasStage' <i class='fa-brands fa-github' style='font-size: 6em'></i></a><br><br><br>
                     </center>
                     
                     <h6><em>We thank Kenneth T. Martinsen for help developing this app</em></h6>"),
                ),
        # Start UI tab ----
        tabItem(tabName = "start",
        titlePanel("Methane sensor calculations"),
        
        sidebarLayout(
          
          sidebarPanel(
            radioButtons("file_type", "What type of instrument-file is being used",
                         choices = c("DIY","Other")),
            
            tags$b("Upload data file"),
            
            conditionalPanel(
              condition = "input.file_type == 'DIY'",
              
              fileInput("file", "",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values", 
                                   ".csv")),
              
              radioButtons("calibration_choice","Use own or Jonas' calibration file",
                           choices = c("Jonas","Own")),
              
              conditionalPanel(
                condition = "input.calibration_choice == 'Jonas'",
                textInput("sensor_name","Input sensor name for calibration")),
              
              conditionalPanel(
                condition = "input.calibration_choice == 'Own'",
                tags$b("Upload own calibration file"),
                tags$p("The file should be in csv format with a header and should include the following columns: a, b, c, k, g and s"),
                fileInput("own_sensor_cal_file",
                          label = NULL,
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values", 
                                     ".csv"))),
              
              tags$b("Timezone of measurement (UTC)"), numericInput("timezone",
                                                              label = "",
                                                              value = 0,
                                                              min = -12,
                                                              max = 14),
            ),
            conditionalPanel(
              condition = "input.file_type == 'Other'",
              fileInput("file", "",
                        multiple = FALSE),
              radioButtons("file_format", "Select file format",
                           choices = c(".csv",
                                       ".txt",
                                       ".data")),
              tags$b("How many rows to skip when reading in data"), numericInput("skip_rows",
                                                                    label = "",
                                                                    value = 0,
                                                                    min = 0,
                                                                    max = Inf),
              radioButtons("datetime_format","Select datetime format",
                          choices = c("YMD-HMS",
                                      "YDM-HMS",
                                      "MDY-HMS",
                                      "DMY-HMS")),
              selectInput("datetime_column","Select datetime column",
                          choices = c(colnames(data()))),
              selectInput("concentration_values_ch4_column","Select CH4 concentration column",
                          choices = c(colnames(data()))),
              selectInput("airt_column","Select temperature column (°C)",
                          choices = c(colnames(data()),NA_character_)),
              selectInput("concentration_values_co2_column","Select CO2 concentration column if applicable",
                          choices = c(colnames(data()),NA_character_)),
              selectInput("water_column","Select water vapor concentration column if applicable",
                          choices = c(colnames(data()),NA_character_)),
              selectInput("sep_column","Select measurement separator column if applicable",
                          choices = c(colnames(data()),NA_character_))
              ),
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
            conditionalPanel(
              condition = "input.file_type == 'Other'",
              h5("A preview of you data will show here to help ensure data is correctly formatted. If the preview does not appear as a table try altering the amount of rows that are to be skipped"),
              tableOutput("head_df")
            ),
            
            tags$b("Main plot"),
            p("Use sliders to adjust the x-axis"),
            
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
            DTOutput("results"),
            
            tags$b("Download flux data as csv"),
            
            tags$br(),
            
            #Button to save the file
            downloadButton('download', 'Download'),
            
            tags$hr(style = "border-top: 3px solid #000000;"),
          ))),
        # Ebul UI tab ----
        tabItem(tabName = "ebul",
        titlePanel("Ebullitive flux"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("concentration_values","Select concentration column",
                                choices = c(colnames(data())),
                                selected = "ch4"),
                    sliderInput("runvar_cutoff","Running variance cutoff",
                                min = 0 , max = 2, value = 0.5,step = 0.001),
                    selectInput("top_selection", "Select how to calculate the ebullitive flux",
                                choices = c("Last" = "last","Maximum" = "max")),
                    sliderInput("indexspan", "Decide on number of observations before and after the running variance cutoff",
                                min = 0, max = 100, value = 30, step = 1),
                    sliderInput("concentration_diffusion_cutoff", "Decide on the cutoff value for diffusion",
                                min = 0, max = 100, value = 1, step = 1),
                    sliderInput("number_of_pumpcycles_in_plot", "How many plots to show at the same time",
                                min = 1, max = 24, value = 10, step = 1),
                    tags$b("Choose whether or not to smooth data"),
                    checkboxInput("smooth_data","Smooth data",value = F),
                    
                  ),
                  mainPanel(
                    h3("Start by uploading your data in the Start tab"),
                    plotOutput("plot2"),
                    column(
                      12,
                      sliderInput("plot_number_select", "Switch between plots",
                                  min = 1, max = 100, value = 1, step = 1),
                      align = "center"),
                    DTOutput("ebul_table"),
                    column(
                      12,
                      downloadButton('download_ebul', 'Download shown fluxes'),
                      align = "center")
                    ))),
        # Diff UI tab ----
          tabItem(tabName = "diff",
            titlePanel("Diffusive flux"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("concentration_values_diff","Select concentration column",
                                  choices = c(colnames(data())),
                                  selected = "ch4"),
                      tags$b("Only calculate diffusive fluxes where there has been no previous bubbles"),
                      checkboxInput("look_for_bubbles","Look for bubbles",value = T),
                      sliderInput("remove_observations_prior", "How many observations to remove prior to calculations of diffusive flux",
                                  min = 0, max = 10000, value = 200, step = 1),
                      sliderInput("number_of_observations_used", "How many observations to use for the calculations of diffusive flux",
                                  min = 0, max = 10000, value = 400, step = 1),
                      sliderInput("number_of_observations_required", "How many are required before diffusive flux is calculated",
                                  min = 0, max = 10000, value = 50, step = 1),
                      sliderInput("cutoff_start_value", "The upper value that a starting concentration can be",
                                  min = 0, max = 10000, value = 2, step = 1),
                      sliderInput("number_of_pumpcycles_in_plot_diff", "How many plots to show at the same time",
                                  min = 1, max = 24, value = 10, step = 1),
                      tags$b("Choose whether or not to smooth data"),
                      checkboxInput("smooth_data_diff","Smooth data",value = F),
                      tags$b("Choose whether or not to apply the Hutchinson-Mosier correction"),
                      checkboxInput("hmr_correction","Hutchinson-Mosier correction",value = F),
                      tags$b("Supply volume and area if Hutchinson-Mosier correction is to be applied"),
                        column(
                          6,numericInput("volume_diff", "Volume (L)", 10, min = 0)
                          ),
                        column(
                          6, numericInput("area_diff", HTML(paste0("Chamber area (m",tags$sup(2),"):")), 0.5, min = 0),
                          )),
                    mainPanel(
                      h3("To calculate diffusive fluxes, ensure you have also considered the ebullitive fluxes on the previous page or select to not look for bubbles"),
                      plotOutput("plot_diff"),
                      column(
                        12,
                        sliderInput("plot_number_select_diff", "Switch between plots",
                                    min = 1, max = 100, value = 1, step = 1),
                        align = "center"),
                      DTOutput("diff_table"),
                      column(
                        12,
                        downloadButton('download_diff', 'Download shown fluxes'),
                        align = "center"),
                    )))
      )))

server <- function(input, output, session){
  data <- reactive({

      
  if(input$file_type == "DIY"){  
    req(input$file)
    req(input$timezone)
    if(input$calibration_choice == "Jonas"){        
      req(input$sensor_name)  
        model_coef %>% 
            mutate(sensor = str_to_lower(sensor)) %>% 
            filter(sensor == str_to_lower(input$sensor_name)) -> calibration_constants
        validate(
          need(dim(calibration_constants)[1] != 0,
               message = "Unrecognized sensor name")
        )
      } else {        
        req(input$own_sensor_cal_file)  
        read_csv(input$own_sensor_cal_file$datapath) %>% 
            rename_with(str_to_lower,everything()) -> calibration_constants
        validate(
          need("a" %in% colnames(calibration_constants),
               message = "The 'a' value is missing in the uploaded calibration file"),
          need("b" %in% colnames(calibration_constants),
               message = "The 'b' value is missing in the uploaded calibration file"),
          need("c" %in% colnames(calibration_constants),
               message = "The 'c' value is missing in the uploaded calibration file"),
          need("k" %in% colnames(calibration_constants),
               message = "The 'k' value is missing in the uploaded calibration file"),
          need("g" %in% colnames(calibration_constants),
               message = "The 'g' value is missing in the uploaded calibration file"),
          need("s" %in% colnames(calibration_constants),
               message = "The 's' value is missing in the uploaded calibration file"),
        )
      }    
      req(calibration_constants)
      lookup <- c(rh = "RH")
      
      df <- read_csv(input$file$datapath, col_names = F,
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
               V0 = abs_H*g+s,
               RsR0 = ((5000/ch4_smv)-1)/((5000/V0)-1),
               ch4 = a*(RsR0^b)+c*abs_H*(a*RsR0^b) + k,
               datetime = datetime+(input$timezone-1)*3600) %>% 
        rename(water = ppm_H20) %>% 
        group_by(datetime) %>% 
        summarise_at(vars(rh, airt, co2, ch4, water,PumpCycle), list(mean)) %>% 
        select(datetime, rh, airt, co2, ch4, water,PumpCycle) 
        } else {
      req(input$file)
      req(input$file_format)
      
      if(input$file_format == ".csv"){
        read_csv(input$file$datapath,
                 skip = input$skip_rows,
                 col_types = cols(.default = col_character())) -> file_upload
      } else if(input$file_format == ".txt"){
        read_delim(input$file$datapath,
                   delim = "\t",
                   skip = input$skip_rows,
                   col_types = cols(.default = col_character())) -> file_upload
      } else if(input$file_format == ".data"){
        read_delim(input$file$datapath,
                   delim = "\t",
                   skip = input$skip_rows,
                   col_types = cols(.default = col_character())) -> file_upload
      }
      
      datetime_format <- input$datetime_format
      
      file_upload %>% 
        cbind(datetime_format) %>% 
        rename(datetime = any_of(input$datetime_column),
               ch4 = any_of(input$concentration_values_ch4_column),
               co2 = any_of(input$concentration_values_co2_column),
               water = any_of(input$water_column),
               airt = any_of(input$airt_column),
               PumpCycle = any_of(input$sep_column)) %>%
        mutate(datetime = case_when(datetime_format == "YMD-HMS" ~ ymd_hms(datetime),
                                    datetime_format == "YDM-HMS" ~ ydm_hms(datetime),
                                    datetime_format == "MDY-HMS" ~ mdy_hms(datetime),
                                    datetime_format == "DMY-HMS" ~ dmy_hms(datetime)),
                across(any_of(c("ch4","co2","water","airt","PumpCycle")), ~ parse_number(.x))) %>% 
        select_if(names(.) %in% c('datetime', 'airt', 'co2',"ch4","water","PumpCycle")) -> df
      
      if(!"PumpCycle" %in% colnames(df)) {
        df <- df %>% mutate(PumpCycle = 1)
      } else {}
      if(!"co2" %in% colnames(df)){
        df <- df %>% mutate(co2 = 0)
      }
      if(!"water" %in% colnames(df)){
        df <- df %>% mutate(water = 0)
      }
      if(!"airt" %in% colnames(df)){
        df <- df %>% mutate(airt = 0)
      }
      
      }
    
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
    
    updateSelectInput(session, "concentration_values", choices = colnames(df), select = "ch4")
    updateSelectInput(session, "concentration_values_diff", choices = colnames(df), select = "ch4")
    
    
    return(df)
  })
  
  to_listen<- reactive({
    list(input$file,
         input$skip_rows,
         input$file_format)
  })
  
  observeEvent(to_listen(), {
    req(input$file)
    if(input$file_format == ".csv"){
      read_csv(input$file$datapath,
               skip = input$skip_rows,
               col_types = cols(.default = col_character())) -> file_upload
    } else if(input$file_format == ".txt"){
      read_delim(input$file$datapath,
                 delim = "\t",
                 skip = input$skip_rows,
                 col_types = cols(.default = col_character())) -> file_upload
    } else if(input$file_format == ".data"){
      read_delim(input$file$datapath,
                 delim = "\t",
                 skip = input$skip_rows,
                 col_types = cols(.default = col_character())) -> file_upload
    }
    updateSelectInput(session, "datetime_column", choices = colnames(file_upload), select = "NA_character_")
    updateSelectInput(session, "concentration_values_ch4_column", choices = c(colnames(file_upload),NA_character_), select = "NA_character_")
    updateSelectInput(session, "concentration_values_co2_column", choices = c(colnames(file_upload),NA_character_), select = "NA_character_")
    updateSelectInput(session, "water_column", choices = c(colnames(file_upload),NA_character_), select = "NA_character_")
    updateSelectInput(session, "airt_column", choices = c(colnames(file_upload),NA_character_), select = "NA_character_")
    updateSelectInput(session, "sep_column", choices = c(colnames(file_upload),NA_character_), select = "NA_character_")
    
    output$head_df<- renderTable({
      file_upload %>% 
        head(5)
    })
  })
  
  data_out <- data.frame()
  
  output$plot <- renderPlot({
    if(input$file_type == "Other") {
      validate(
        need(input$datetime_column, message = "Needs to input datetime column"),
        need(input$concentration_values_ch4_column, message = "Needs to input methane concentration column"))
    } else {}
    req(data())
    data_subset <- data() %>% 
      filter(between(datetime, input$range[1], input$range[2]),
             between(ch4, input$ch4_range[1],input$ch4_range[2]),
             between(co2, input$co2_range[1],input$co2_range[2]))
    
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
      
      if(var(data_subset$airt) == 0) {
        ch4_flux = NA_real_
      } else {}
      
      if (co2_status$co2_mean == 0 & co2_status$co2_var == 0) {
       
        results_string <- paste0("<b>CH<sub>4</sub></b>: slope = ", round(slope_ch4*3600, 2), " (ppm h<sup>-1</sup>)",
                                 ", flux = ", round(ch4_flux*3600, 2), " (µmol m<sup>-2</sup> h<sup>-1</sup>)",
                                 ", R<sup>2</sup> = ", round(r2_ch4, 2))
        
        results <- data.frame("processing_date" = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "id" = as.character(input$sample_id),
                              "start" = strftime(ranges2$x[1], "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "end" = strftime(ranges2$x[2], "%Y-%m-%d %H:%M:%S", tz="GMT"),
                              "CH4_slope_h1" = slope_ch4*3600,
                              "CH4_intercept" = intercept_ch4,
                              "CH4_R2" = r2_ch4,
                              "temperature" = mean_temp, 
                              "chamber_volume" = input$chamber_vol,
                              "chamber_area" = input$chamber_area,
                              "CH4_flux_umol_m2_h1" = ch4_flux*3600)
        
      } else {
        lm_model_co2 <- lm(co2~sec, data = data_subset)
        slope_co2 <- coef(lm_model_co2)[2]
        intercept_co2 <- coef(lm_model_co2)[1]
        r2_co2 <- summary(lm_model_co2)$r.squared
        
        co2_flux <- (slope_co2*(input$chamber_vol*input$atm_pres))/(R*(273.15+mean_temp)*input$chamber_area)
        
        if(var(data_subset$airt) == 0) {
          co2_flux = NA_real_
        } else {}
        
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
                              "CH4_slope_h1" = slope_ch4*3600,
                              "CH4_intercept" = intercept_ch4,
                              "CH4_R2" = r2_ch4,
                              "CO2_slope_h1" = slope_co2*3600,
                              "CO2_intercept" = intercept_co2,
                              "CO2_R2" = r2_co2,
                              "temperature" = mean_temp, 
                              "chamber_volume" = input$chamber_vol,
                              "chamber_area" = input$chamber_area,
                              "CH4_flux_umol_m2_h1" = ch4_flux*3600,
                              "CO2_flux_umol_m2_h1" = co2_flux*3600)
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
    if(input$file_type == "Other") {
      validate(
        need(input$datetime_column, message = ""),
        need(input$concentration_values_ch4_column, message = ""))
    } else {}
    zoom_data <- data_subset() 
    
    
    zoom_plot_data <- zoom_data$df %>% 
      filter(between(ch4,input$ch4_range[1], input$ch4_range[2]),
             between(co2,input$co2_range[1],input$co2_range[2]))

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
      
    } else {
      
      co2_min = min(zoom_plot_data$co2)
      co2_max = max(zoom_plot_data$co2)
      
      co2_scaled = (ch4_max - ch4_min)*((zoom_plot_data$co2-co2_min)/(co2_max - co2_min))+ch4_min
      co2_labels = pretty(zoom_plot_data$co2)
      co2_at = (ch4_max - ch4_min)*((co2_labels-co2_min)/(co2_max - co2_min))+ch4_min
      
      ch4_scaled = (co2_max - co2_min)*((zoom_plot_data$ch4-ch4_min)/(ch4_max - ch4_min))+co2_min
      ch4_labels = pretty(zoom_plot_data$ch4)
      ch4_at = (co2_max - co2_min)*((ch4_labels-ch4_min)/(ch4_max - ch4_min))+co2_min
      
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
    data_out <<- rbind(data_out, data_subset()$results) %>% 
      mutate(across(where(is.double), ~round(.x, 3)))
    output$results <- renderDT(
      data_out %>% 
        select_if(names(.) %in% c("id", "start", "end","CH4_R2", "CH4_flux_umol_m2_h1","CO2_R2", "CO2_flux_umol_m2_h1")),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames= FALSE)
  })
  
  # FluxSeparator ----
  ## Ebul ----
  fluxsep_ebul <- reactive({
      data() %>% 
      ungroup %>% 
      mutate(station = 1,
             plot_number = floor((PumpCycle-min(PumpCycle))/input$number_of_pumpcycles_in_plot)+1,
             sensor = 1)  -> ebul_df
    
    ebul_df %>% 
      filter(plot_number == input$plot_number_select) %>% 
      ebul_flux(concentration_values = input$concentration_values, 
                top_selection = input$top_selection, 
                IndexSpan = input$indexspan, 
                runvar_cutoff = input$runvar_cutoff, 
                concentration_diffusion_cutoff = input$concentration_diffusion_cutoff, 
                number_of_pumpcycles_in_plot = input$number_of_pumpcycles_in_plot, 
                smooth_data = input$smooth_data) -> ebul
    
    updateSliderInput(session, "plot_number_select",min = min(ebul_df$plot_number), max = max(ebul_df$plot_number),step = 1)

    return(ebul)
  })

  observeEvent(input$concentration_values, { 
    max_number_of_pump <- fluxsep_ebul()$bubbles %>% select(PumpCycle) %>% ungroup %>% count()
    
    updateSliderInput(session,"number_of_pumpcycles_in_plot", min = 1, max = max_number_of_pump$n, step = 1,
                      value = max_number_of_pump$n/2)
  })
    
  output$plot2  <- renderPlot(fluxsep_ebul()$plot)
  
  output$ebul_table <- DT::renderDT(fluxsep_ebul()$bubbles %>% 
                                     select(c(station:datetime_end, temp, concentration_per_hour = concentration_per_time)) %>% 
                                     mutate(across(temp:concentration_per_hour, ~round(.x, 3)),
                                            datetime_start = strftime(datetime_start, format = '%Y-%m-%d %R'),
                                            datetime_end = strftime(datetime_end, format = '%Y-%m-%d %R')) %>% 
                                     rename("concentration_per_hour (ppm h-1)" = concentration_per_hour,
                                             'airt (°C)' = temp),
                                    options = list(pageLength = 10, autoWidth = TRUE),
                                    rownames= FALSE)
  
  output$download_ebul <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_ebullitive_fluxdata.csv")
    },
    content = function(file) {
      fluxsep_ebul()$bubbles %>% 
        rename("concentration_per_hour (ppm h-1)" = concentration_per_time,
               'airt (°C)' = temp) %>% 
        write.csv(file, row.names = FALSE)
    }
  )
  
  ## Diff ----
  fluxsep_diff <- reactive({
    data() %>% 
      ungroup %>% 
      mutate(station = "1",
             plot_number = floor((PumpCycle-min(PumpCycle))/input$number_of_pumpcycles_in_plot_diff)+1,
             sensor = "1")  -> diff_df

    diff_df %>% 
      filter(plot_number == input$plot_number_select_diff) %>% 
      diff_flux(concentration_values = input$concentration_values_diff, 
                IndexSpan = input$indexspan, 
                runvar_cutoff = input$runvar_cutoff,
                remove_observations_prior = input$remove_observations_prior,
                number_of_observations_used = input$number_of_observations_used,
                cutoff_start_value = input$cutoff_start_value,
                number_of_observations_required = input$number_of_observations_required,
                number_of_pumpcycles_in_plot = input$number_of_pumpcycles_in_plot_diff, 
                smooth_data = input$smooth_data_diff,
                look_for_bubbles = input$look_for_bubbles, 
                Hutchinson_Mosier_correction = input$hmr_correction,
                volume = input$volume_diff,
                area = input$area_diff) -> diff
  
    validate(
      need(diff[1] != "ERROR! NO DATA",
           message = "Couldn't find any diffusive fluxes. Try lowering the runvar_cutoff or turning off looking for bubbles")
    )
    
    diff_df %>% 
      select(input$concentration_values_diff) %>% 
      pull() -> cutoff_start_value_helper
    
    diff_df %>% 
      count(PumpCycle) %>% 
      pull(n) %>% 
      max() -> max_antal_diff
    
     updateSliderInput(session, "plot_number_select_diff",min = min(diff_df$plot_number), max = max(diff_df$plot_number),step = 1)
     updateSliderInput(session, "remove_observations_prior", min = 0, max = max_antal_diff, step = 1)
     updateSliderInput(session, "number_of_observations_used", min = 3, max = max_antal_diff, step = 1)
     updateSliderInput(session, "cutoff_start_value", min = floor(min(cutoff_start_value_helper, na.rm=T)), max = ceiling(max(cutoff_start_value_helper, na.rm=T)), 
                       step = 1, value = ceiling(max(cutoff_start_value_helper, na.rm=T)))
     updateSliderInput(session, "number_of_observations_required", min = 3, max = max_antal_diff, step = 1)
    return(diff)
  })
  
  observeEvent(input$concentration_values_diff, { 
    max_number_of_pump_diff <- fluxsep_diff()$diff %>% ungroup %>% select(PumpCycle) %>% count()
    
    updateSliderInput(session,"number_of_pumpcycles_in_plot_diff", min = 1, max = max_number_of_pump_diff$n, step = 1,
                       value = max_number_of_pump_diff$n/2)
  })
  
  output$plot_diff  <- renderPlot(fluxsep_diff()$plot)
  
  output$diff_table <- renderDT(fluxsep_diff()$diff %>% 
                                  mutate(across(slope_concentration_hr:temp, ~round(.x,3)),
                                         datetime_start = strftime(datetime_start, format = '%Y-%m-%d %R'),
                                         datetime_end = strftime(datetime_end, format = '%Y-%m-%d %R')) %>% 
                                  rename("slope_concentration_hr (ppm h-1)" = slope_concentration_hr,
                                         'airt (°C)' = temp),
                                options = list(pageLength = 10, autoWidth = TRUE),
                                rownames= FALSE)
  
  output$download_diff <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_diffusive_fluxdata.csv")
    },
    content = function(file) {
      fluxsep_diff()$diff %>% 
        rename("slope_concentration_hr (ppm h-1)" = slope_concentration_hr,
               'airt (°C)' = temp) %>% 
      write.csv(file, row.names = FALSE)
    }
  )
  
  # Downloads ----
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "_fluxdata.csv")
    },
    
    content = function(file) {
      write.csv(data_out, file, row.names = FALSE)
    })  
  
  output$ch4_download <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "_ch4data.csv")
    },
    
    content = function(file) {
      data() %>% 
        rename('rh (%)' = rh, 'airt (°C)' = airt, 'CO2 (ppm)' = co2, 'CH4 (ppm)' = ch4, "H2O (ppm)" = water)  %>% 
        write.csv(data_write, file, row.names = FALSE)
    })  

}

shinyApp(ui, server)
library(ggplot2)
#library(googleVis)
library(shiny)
library(shinythemes)
#library(blscrapeR)
#library(data.table)
#library(reshape2)
#library(plotly)
#library(zoo)
#library(scales) 
#library(ggsci)
#library(dplyr) 

options <- c('1. Obs. vs. pred.','2. Case 2', '3. Case 3')

shinyApp(
  ui= tagList(      
    "FITEVAL: Objective assessment of model goodness-of-fit with statistical significance", 
    navbarPage(
      theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
      "FITEVAL: Objective assessment of model goodness-of-fit with statistical significance",
      tabPanel("Main Fiteval",
               sidebarPanel(
               h4('Select Options'),
               selectInput('case_input', 'Choose a case', options),
               numericInput('per_input', 'PER % input (for cases 3 and 4)', value=25, min = 0, max = 100, step = 0.1),
               numericInput('bias_threshold_input', 'Bias threshold %', value=5, min = 0, max = 100, step = 0.5),
               checkboxInput('legates_input', 'Compute Legates & McCabe modified NSE', value = FALSE),
               radioButtons('bootstrap_method_input', 'Bootstrap method:', choices = c('Block','Efrons'), selected = NULL,
                            inline = TRUE, width = NULL),
               h4('Enter Data'),
               p('Browse file or leave blank to load sample data'),
               fileInput("file_input", "Choose File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               checkboxInput("header_input", "File Includes Header", TRUE),
               radioButtons("file_type_input", "File type",
                            choices = c('excel','comma separated values', 'tab separated values'),selected = "comma separated values"),
               radioButtons("display_input", "Display",
                            choices = c("head",
                                        "all"),
                            selected = "head")
               #p('The Tomato Meter is the proportion of positive reviews')
               # selectInput('sel_stats', 'Series of Interest', series_stats),
               # selectInput('sel_utility', 'Utility', utilities,'Tampa'),
               # selectInput('sel_analysis_start','Start of the Analysis',date_range,selected=date_range[75]),
               # selectInput('sel_analysis_end','End of the Analysis',date_range,selected=date_range[126]),
               # checkboxGroupInput('sel_utility', 'Utilities',
               #                    utilities, selected = 'Tampa')
               ),
               mainPanel(
                 DT::dataTableOutput('data')
                 #plotlyOutput("series_plot")
                 # plotOutput('boxchart')
               )
      ),
      # tabPanel("Regression",
      #          sidebarPanel(
      #           selectInput('smooth_y', 'Variable of Interest (Y axis)', fields, selected='TotalWaterUse'),
      #           selectInput('smooth_x', 'Variable of Interest (X axis)', fields, selected='LandArea_Parcel_SF'),
      #           selectInput('sel_analysis_start_reg','Start of the Analysis',date_range,selected=date_range[123]),
      #           selectInput('sel_analysis_end_reg','End of the Analysis',date_range,selected=date_range[126]),
      #           selectInput('sel_analysis_utility', 'Utility', utilities,'Tampa')
      #          ),
      #           mainPanel(
      #             plotlyOutput("regression_plot")
      #           )
      # ),
      tabPanel("About",
               h4("Developed by University of Florida and Univresidad de la Laguna (Spain)"))
    )),
  server = function(input, output) {
    
    output$data <- DT::renderDataTable(
      DT::datatable(
        iris, options = list(
          lengthMenu = list(c(5, 15, 20,100,200,-1), c('5', '15', '20', '100', '200', 'All')),
          pageLength = 15,
          searching = FALSE
          )
        )
    )
    
    output$obs_pred_fig <- 
    
    # output$series_plot <- renderPlotly({
    #   if(input$sel_stats=='Average'){
    #     sel_cols <- c('Date', paste(input$sel_series,'_mean',sep=''))
    #   }else if(input$sel_stats=='median'){
    #     sel_cols <- c('Date', paste(input$sel_series,'_50',sep=''))
    #   }else{
    #     sel_cols <- c('Date', paste(input$sel_series,'_0',sep=''), paste(input$sel_series,'_25',sep=''), paste(input$sel_series,'_50',sep=''), paste(input$sel_series,'_75',sep=''), paste(input$sel_series,'_100',sep=''))
    #   }
    #   utilities <- c('NPR','NWH','Pasco','Pinellas', 'SCH','StPete','Tampa')
    #   dt_all_month <- subset(dt_all_month,subset=((dt_all_month$Date <= input$sel_analysis_end) & (dt_all_month$Date >= input$sel_analysis_start) & (dt_all_month$utility %in% input$sel_utility)),select=c(sel_cols,'utility'));
    #   df_long <- melt(dt_all_month,id.vars=c('utility','Date'))
    #   df_long$variable <- paste(df_long$utility,df_long$variable)
    #   df_long <- df_long[,utility:=NULL]
    #   df_long$Date <- factor(df_long$Date)
    #   
    #   p <- ggplot(data=df_long,aes(x=Date,y=value,colour=variable, group=variable)) + geom_line() + scale_color_npg() + 
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0, size = 7),plot.margin = unit(c(1,1,1,1), "cm"))+ xlab('')+ ylab('')+geom_point();
    #   
    # })
    # 
    # output$regression_plot <- renderPlotly({
    # 
    #   date_start <- as.Date(paste('1',input$sel_analysis_start_reg),"%d %b %Y")
    #   date_start <- 100*year(date_start)+month(date_start)
    #   
    #   date_end <- as.Date(paste('1',input$sel_analysis_end_reg),"%d %b %Y")
    #   date_end <- 100*year(date_end)+month(date_end)
    # 
    #   print(tolower(input$sel_analysis_utility))
    #   q <- paste('SELECT Date,',input$smooth_x, ' , ',input$smooth_y, ' FROM barrera_shiny_tampa_bay.',tolower(input$sel_analysis_utility), ' WHERE Date between ', date_start, ' and ', date_end, ';', sep='');
    #   print(q)
    #   dt_tampa <- data.table(mysqlquery(q));
    #   if (nrow(dt_tampa)>450000) {
    #     dt_tampa <- sample_n(dt_tampa,450000)
    #   }
    #   print('Jorge')
    #   print(head(dt_tampa))
    #   print(nrow(dt_tampa))
    #   #print('Barrera')
    #   #print(as.Date(paste('1',input$sel_analysis_start_reg),"%d %b %Y"))
    #   print('Alviar')
    #         
    #   f <- ggplot(dt_tampa,aes_string(x=input$smooth_x,y=input$smooth_y)) + geom_smooth()+ scale_color_npg();
    #   #f <- ggplot(dt_tampa,aes(TotalWaterUse,LandArea_Parcel_SF)) + geom_smooth()+ scale_color_npg();
    #   ggplotly(f,dynamicTicks=TRUE)
    # })
  }
)


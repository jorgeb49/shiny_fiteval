library(ggplot2)
library(RMySQL)
library(googleVis)
library(shiny)
library(shinythemes)
library(blscrapeR)
library(data.table)
library(reshape2)
library(plotly)
library(zoo)
library(scales) 
library(ggsci)
library(dplyr) 

fields <- c('Yr',
            'Mth',
            'TotalWaterUse',
            'IRR_WaterUse',
            'REC_WaterUse',
            'TotalWaterUse_perDay',
            'IRR_WaterUse_perDay',
            'REC_WaterUse_perDay',
            'NumberOfUnits',
            'Units',
            'HeatArea_Parcel',
            'GrsArea_Parcel',
            'XF_AREA',
            'Parcel_FPE_XF',
            'LandArea_Parcel_SF',
            'GreenSpace_Est',
            'GreenSpace_Frac',
            'GreenSpace_EstXF',
            'GreenSpace_FracXF'
            );

series_fields <- c('TotalWaterUse',
                   'IRR_WaterUse',
                   'REC_WaterUse'
                  );

utilities <- c('NPR','NWH','Pasco','Pinellas', 'SCH','StPete','Tampa')

series_stats <- c('Average','median','quartiles')

# Units is from Parcel Attribute and NumberOfUnits is From Water consumption table

mysqlquery <- function(q){
  rmysql.settingsfile<-"C:/Users/jorgeibarrera/Documents/jorgeibarrera_read.cnf"
  con <- dbConnect(MySQL(), default.file=rmysql.settingsfile,group='group_barrera_read', dbname='barrera_shiny_tampa_bay')
  query <- dbSendQuery(con, q)
  result <- fetch(query, n=-1)
  dbClearResult(query)
  dbDisconnect(con)
  return(result)
}


rmysql.settingsfile<-"C:/Users/jorgeibarrera/Documents/jorgeibarrera_read.cnf";

q <- 'SELECT * FROM all_aggregate_month'
dt_all_month <- data.table(mysqlquery(q));
dt_all_month$Date <- as.yearmon(as.character(dt_all_month$Date),"%Y%m")
date_range <- dt_all_month$Date[dt_all_month$utility=='SCH']

q <- 'SELECT * FROM all_aggregate_year'
dt_all_year <- data.table(mysqlquery(q));




shinyApp(
  ui = tagList(
    navbarPage(
       theme = shinytheme("united"),  # <--- To use a theme, uncomment this
      "Tampa Bay Water use",
      tabPanel("Time Series",
               sidebarPanel(
               selectInput('sel_series', 'Series of Interest', series_fields),
               selectInput('sel_stats', 'Series of Interest', series_stats),
               selectInput('sel_utility', 'Utility', utilities,'Tampa'),
               selectInput('sel_analysis_start','Start of the Analysis',date_range,selected=date_range[75]),
               selectInput('sel_analysis_end','End of the Analysis',date_range,selected=date_range[126]),
               checkboxGroupInput('sel_utility', 'Utilities',
                                  utilities, selected = 'Tampa')
               ),
               mainPanel(
                 plotlyOutput("series_plot")
                 # plotOutput('boxchart')
               )
      ),
      tabPanel("Regression",
               sidebarPanel(
                selectInput('smooth_y', 'Variable of Interest (Y axis)', fields, selected='TotalWaterUse'),
                selectInput('smooth_x', 'Variable of Interest (X axis)', fields, selected='LandArea_Parcel_SF'),
                selectInput('sel_analysis_start_reg','Start of the Analysis',date_range,selected=date_range[123]),
                selectInput('sel_analysis_end_reg','End of the Analysis',date_range,selected=date_range[126]),
                selectInput('sel_analysis_utility', 'Utility', utilities,'Tampa')
               ),
                mainPanel(
                  plotlyOutput("regression_plot")
                )
      ),
      tabPanel("About",
               h2("The National Water Quality Monitoring Council has data from 4 different providers, accounting for about 2313283 monitoring locations across the US. NWIS is the provider covering most locations, 1617562, followed by STORET  with 695491 locations. In Florida there is data covering 130695 Monitoring Locations, ~100K from STORET and ~30K from NWIS.
                  (https://www.waterqualitydata.us/portal/#)"))
    )
  ),
  server = function(input, output) {
    
    output$series_plot <- renderPlotly({
      if(input$sel_stats=='Average'){
        sel_cols <- c('Date', paste(input$sel_series,'_mean',sep=''))
      }else if(input$sel_stats=='median'){
        sel_cols <- c('Date', paste(input$sel_series,'_50',sep=''))
      }else{
        sel_cols <- c('Date', paste(input$sel_series,'_0',sep=''), paste(input$sel_series,'_25',sep=''), paste(input$sel_series,'_50',sep=''), paste(input$sel_series,'_75',sep=''), paste(input$sel_series,'_100',sep=''))
      }
      utilities <- c('NPR','NWH','Pasco','Pinellas', 'SCH','StPete','Tampa')
      dt_all_month <- subset(dt_all_month,subset=((dt_all_month$Date <= input$sel_analysis_end) & (dt_all_month$Date >= input$sel_analysis_start) & (dt_all_month$utility %in% input$sel_utility)),select=c(sel_cols,'utility'));
      df_long <- melt(dt_all_month,id.vars=c('utility','Date'))
      df_long$variable <- paste(df_long$utility,df_long$variable)
      df_long <- df_long[,utility:=NULL]
      df_long$Date <- factor(df_long$Date)
      
      p <- ggplot(data=df_long,aes(x=Date,y=value,colour=variable, group=variable)) + geom_line() + scale_color_npg() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0, size = 7),plot.margin = unit(c(1,1,1,1), "cm"))+ xlab('')+ ylab('')+geom_point();
      
    })
    
    output$regression_plot <- renderPlotly({

      date_start <- as.Date(paste('1',input$sel_analysis_start_reg),"%d %b %Y")
      date_start <- 100*year(date_start)+month(date_start)
      
      date_end <- as.Date(paste('1',input$sel_analysis_end_reg),"%d %b %Y")
      date_end <- 100*year(date_end)+month(date_end)

      print(tolower(input$sel_analysis_utility))
      q <- paste('SELECT Date,',input$smooth_x, ' , ',input$smooth_y, ' FROM barrera_shiny_tampa_bay.',tolower(input$sel_analysis_utility), ' WHERE Date between ', date_start, ' and ', date_end, ';', sep='');
      print(q)
      dt_tampa <- data.table(mysqlquery(q));
      if (nrow(dt_tampa)>450000) {
        dt_tampa <- sample_n(dt_tampa,450000)
      }
      print('Jorge')
      print(head(dt_tampa))
      print(nrow(dt_tampa))
      #print('Barrera')
      #print(as.Date(paste('1',input$sel_analysis_start_reg),"%d %b %Y"))
      print('Alviar')
            
      f <- ggplot(dt_tampa,aes_string(x=input$smooth_x,y=input$smooth_y)) + geom_smooth()+ scale_color_npg();
      #f <- ggplot(dt_tampa,aes(TotalWaterUse,LandArea_Parcel_SF)) + geom_smooth()+ scale_color_npg();
      ggplotly(f,dynamicTicks=TRUE)
    })
  }
)
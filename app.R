
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('\bio\web\secure\barrera\apps\fiteval')
library(ggplot2)
#library(googleVis)
library(shiny)
library(shinythemes)
library(plotly)
library(np)
library(bootstrap)
library(DT)
source('fiteval_core_functions.R')
classlim <- c(0.65,0.8,0.9)
options <- c('1. Obs. vs. pred.','2. Case 2', '3. Case 3')
alpha <- 0.05

shinyApp(
  ui= tagList(      
    navbarPage(
      theme = shinytheme("flatly"),
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
                                      "text/comma-separated-values",
                                      "text/tab-separated-values",
                                      ".csv")),
                 checkboxInput("header_input", "File Includes Header", TRUE),
                 radioButtons("separator_input", "Separator",
                              choices = c(Comma=',', Semicolon=';', Tab='\t', Pipe='|'),selected = "\t"),
                 radioButtons("display_input", "Display",
                              choices = c("head",
                                          "all"),
                              selected = "head")
               ),
               mainPanel(
                 DT::dataTableOutput('data_table')
               )
      ),
      tabPanel("Results",
              mainPanel(
                column(7,
                    br(),
                    br(),
                    p(strong(h3('Observed vs. Predicted'))),
                    plotlyOutput("pred_obs_plot"),
                    br(),
                    br(),
                    br(),
                    br(),
                    p(strong(h3('Cumulative Density Function of NSE'))),
                    plotlyOutput("cdf_plot")
                ),
                column(5,
                       br(),
                       br(),
                    p(strong(h3('===== Goodness of Fit Evaluation ====='))),
                    htmlOutput("testHTML")
                    #verbatimTextOutput("render_print")
                ),
                column(12,
                    br(),
                    br(),
                    p(strong(h3('Observed and Predicted values as time series'))),
                    plotlyOutput("pred_obs_plot2")
                ))
      ),
      tabPanel("About",
               h4("Developed by University of Florida and Universidad de la Laguna (Spain)")
      )
    )
  ),
  server = function(input, output) {
    
    
    #1 Read file with data
    file_data <- reactive({
      inFile <- input$file_input
      if(is.null(inFile)){
        df_file <- read.csv('fiteval.csv',header=FALSE,sep='\t')}
      else{
        df_file<-read.csv(inFile$datapath, header=input$header_input, sep = input$separator_input)
      }
        names(df_file)<-c('Obs','Pred')
        return(df_file)
    })
    
    
    
    
    evaluate_outlier <- reactive({
      f_data <- file_data()
      err0 <- f_data$Obs-f_data$Pred
      err <- sort(err0)
      el <- length(err)
      Qexp_low <- (err[2]-err[1]) / (err[el]-err[1])
      Qexp_upp <- (err[el]-err[el-1])/(err[el]-err[1])
      Qcrit <- 0.187+(2.242-0.187)*(el^-0.862)
      
      outlier_c1 <- 0
      outlier_ind <- c()
      
      
      if(Qexp_low>Qcrit){
        outlier_c1 <- 1
        outlier_ind <- c(outlier_ind,which(err0==err[1]))
      }
      
      if (Qexp_upp>Qcrit){
        outlier_c1 <- outlier_c1 + 1;
        outlier_ind = c(outlier_ind, which(err0==err[el]));
      }
      
      if (outlier_c1>0) {
        outlier_txt <- 'Presence of outliers (Q-test): present and maybe affecting indicators'
        #outlier_pt <- data.frame(pred=Ypred[outlier_ind],obs=Yobs[outlier_ind])
      } else{
        outlier_txt <- 'Presence of outliers (Q-test): No'
        #outlier_pt <- c()
      }
      return(list(txt=outlier_txt,outlier_index=outlier_ind))
    })
    
    
    
    
    
    
    empirical_cdf <- function(){
      # 1) Read data
      data <- file_data()
      #l <- nrow(data)
      nse_data <- nse(data)
      # 2) Generate M Bootstrap resamples (Using: *Politis Romano, 1994)
      Yobs <- data[,1]
      #Ypred <- data[,2]
      indmatrix <- stationary_bootstrap(Yobs)
      # 3) Calculate Nash-Sutcliffe-Efficienty (NSE) and RMSE for each of the M resamples
      bootstraps_nse <- apply(indmatrix,2,function(x) nse(data[x,]))
      bootstraps_rmse <- apply(indmatrix,2,function(x) rmse(data[x,]))
      # 4) Construct empirical probabiliy distribution of the NSE
      ecdf_nse <- ecdf(bootstraps_nse)
      NSE_ <- seq(max(0,min(bootstraps_nse)),1.001,length=1000)
      CDF_ <- ecdf_nse(NSE_)
      #print(head(data.frame(NSE=NSE_,CDF=CDF_)))
      #print(head(bootstraps_nse))
      #print(head(bootstraps_rmse))
      return(list(emp_cdf=data.frame(NSE=NSE_,CDF=CDF_),bootstraps_nse=bootstraps_nse,bootstraps_rmse=bootstraps_rmse))
    }
    

    output$data_table <- DT::renderDataTable(
      DT::datatable(
        file_data(), options = list(
          lengthMenu = list(c(5, 15, 20,100,200,-1), c('5', '15', '20', '100', '200', 'All')),
          pageLength = 15,
          searching = FALSE
        )
      )
    )
    
    output$pred_obs_plot <- renderPlotly({
       plot_ly(file_data(), x = ~Pred, y = ~Obs,type='scatter',mode='markers',showlegend=FALSE,name='table data') %>% add_lines(y=~Pred,name='1:1',line=list(shape="linear",dash='dot',name='')) %>% 
      layout(xaxis=list(title='Predicted'),yaxis=list(title='Oberved'))
    })
    
    
    output$cdf_plot <- renderPlotly({
      emp_cdf<- empirical_cdf()$emp_cdf
      mode_point <- emp_cdf$NSE[min(which(emp_cdf$CDF>0.5))]
      threshold <- classlim;
     # p <- plot_ly(emp_cdf,x=~NSE,y=~CDF,type='scatter',mode='markers', showlegend=FALSE, name='CDF') %>% add_lines(x=c(threshold[1],threshold[1]),y=c(0,1), name='Threshold 1', ,line=list(shape="linear",dash='dot',name='')) %>%
    #    layout(xaxis=list(title='NSE'),yaxis=list(title='CDF'))
      
      
      cdf = emp_cdf$CDF
      NSE = emp_cdf$NSE
      
      p <- ggplot()+
      geom_line(aes(x=NSE,y=cdf,xl=NSE,name=cdf),colour='blue')+
      geom_vline(aes(xintercept=threshold[1],name=NULL),linetype="dotted",size=0.8)+
      geom_vline(aes(xintercept=threshold[2],name=NULL),linetype="dotted",size=0.8)+
      geom_vline(aes(xintercept=threshold[3],name=NULL),linetype="dotted",size=0.8)+
      geom_rect(aes(xmin=0,xmax=classlim[1],ymin=0,ymax=1),alpha=0.4,fill='skyblue')+
      geom_rect(aes(xmin=classlim[1],xmax=classlim[2],ymin=0,ymax=1),alpha=0.3,fill='skyblue')+
      geom_rect(aes(xmin=classlim[2],xmax=classlim[3],ymin=0,ymax=1),alpha=0.2,fill='skyblue')+
      geom_rect(aes(xmin=classlim[3],xmax=1,ymin=0,ymax=1),alpha=0.1,fill='skyblue')+
      geom_point(aes(x=mode_point,y=0.5),color='blue')+
      geom_text(aes(x=classlim[1]/2, label="Unsatisfactory", y=0.8,name=NULL), colour="red")+
      geom_text(aes(x=(classlim[1]+classlim[2])/2, label="Acceptable", y=0.8,name=NULL), colour="red")+
      geom_text(aes(x=(classlim[2]+classlim[3])/2, label="Good", y=0.8,name=NULL), colour="red")+
      geom_text(aes(x=(1.05+classlim[3])/2, label="Very Good", y=0.8,name=NULL), colour="red", hoverinfo = "none",showlegend = F)+
      labs(x='NSE',y='CDF')
      return(ggplotly(p,tooltip=c("xl","name")))
    })
    
    output$pred_obs_plot2 <- renderPlotly({
      plot_ly(file_data(),y = ~Obs,type='scatter',mode='markers',name='Observed') %>% 
        add_trace(y=~Pred,mode="lines+markers",name='Predicted') %>%
        layout(xaxis=list(title='Position within the series'),yaxis=list(title='Oberved and Predicted values'))
    })
    
    output$testHTML <- renderText({
      
      emp_cdf_list<-empirical_cdf()
      emp_cdf_df <- emp_cdf_list$emp_cdf
      bootstraps_rmse <- emp_cdf_list$bootstraps_rmse
      bootstraps_nse <- emp_cdf_list$bootstraps_nse
      
      very_good <- 1 - emp_cdf_df$CDF[emp_cdf_df$NSE>classlim[3]][1]
      good <- 1 - emp_cdf_df$CDF[emp_cdf_df$NSE>classlim[2]][1] - very_good
      acceptable <- 1 - emp_cdf_df$CDF[emp_cdf_df$NSE>classlim[1]][1] - good - very_good
      unsatisfactory <- 1- very_good - good - acceptable
      
      data <- file_data()
      rmse_data <- rmse(data)
      nse_data <- nse(data)
       
      ci_rmse <- bca(data,rmse_data,selective_rmse,bootstraps_rmse,alpha)
      ci_nse <- bca(data,nse_data,selective_nse,bootstraps_nse,alpha)
       
      # 6) Evaluate CI, Bias and Outlier:
      
      ci_eval <- evaluate_ci(ci_nse,classlim)

      bias_eval <- evaluate_bias(Yobs=data$Obs,Ypred=data$Pred,BiasValue=input$bias_threshold_input)
      outlier_eval <- evaluate_outlier()
      
      print(str(bias_eval))

        paste("
              <html>
              <head>
 
              </head>
              <body>
              <p style=\"background-color: cornsilk;\" >
                Evaluation of NSE: <b>",ci_eval," </b> <br>
                Probability of it being:<br>
                - Very good(NSE=0.900-1.000): ", 100*very_good, "% <br>
                - Good (NSE=0.800-0.899): ", 100*good, "% <br>
                - Acceptable (NSE=0.650-0.799): ", 100*acceptable, "% <br>
                - <b> Unsatisfactory (NSE<0.650): ", 100*unsatisfactory, "% (p-value: ", unsatisfactory,")</b> <br>
              </p> 
              <p style=\"background-color: palegreen;\">", outlier_eval$txt," <br>",
              bias_eval[1], " <br>",
              bias_eval[2], 
              "</p> 
              <p style=\"background-color: lightskyblue;\"> 
              RMSE = ",round(rmse_data,digits=4), " [",round(ci_rmse[1],4),"-",round(ci_rmse[2],4),"]* <br>
              NSE =  ",round(nse_data,4), " [",round(ci_nse[1],4),"-",round(ci_nse[2],4),"]*  <br>
              ---------------------------------------------------- <br>
              *: 95% Confidence interval obtained from BCA bootstrapping using Politis and Romano (1994) 
              block bootstrap method for satitionary dependent data.
              </p>
              </body>
              </html>
              ")
    })
    }
)


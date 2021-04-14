
library(shiny)
library(dplyr)
library(DT)

ui<-fluidPage(
  titlePanel("Customer Churn Prediction and Budget Optimization"),
  br(),
  sidebarLayout(
    sidebarPanel(
     
        fileInput("file1", "Upload the dataset",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        tags$hr(),

        checkboxInput("header", "Header", TRUE),
        tags$hr(),
        
        numericInput("budget","Advertising budget",0,min=1,max=NA,step=NA),
        
        tags$hr(),
        numericInput("cost","Cost per advertisement",0,min=0,max=NA,step=NA),
        
        br(),
        submitButton("Submit")
    ),
    mainPanel(
      tabsetPanel(
        type="tab",
        
        tabPanel(
          "Input Data",
         tableOutput("Contents")
        ),
        tabPanel(
          "Updated data",
          tableOutput("up")
        ),
        
        tabPanel(
          "List of Customers",
          tableOutput("res")
          
        ),
        
        tabPanel(
          "Results",
          h3("Maximum number of people"),
          verbatimTextOutput("nadv"),
          br(),
          h3("Number of people to be advertised to"),
          verbatimTextOutput("worthadv"),
          br(),
          h3("Advertising cost"),
          verbatimTextOutput("advcost"),
          br(),
          h3("Total gains from retention"),
          verbatimTextOutput("savings"),
          br(),
          br(),
          br(),
          p("Note: If advertising cost displays 0, it indicates that the cost per advertisement is greater than the maximum profit gained by retaining any customer.")
          
        ),
        tabPanel(
        "Check",
        h3("Budget"),
        verbatimTextOutput("budget"),
        br(),
        h3("Cost"),
        verbatimTextOutput("cost")
        )
        
      )
      
    )))
  
server<- function(input,output){
  tot_data<-reactive({
    req(input$file1)
    dat<-read.csv(input$file1$datapath,header = input$header)
  return(dat)  
  })
  
  budget<-reactive({
    return(as.numeric(input$budget))})
  
  cost<-reactive({
    return(as.numeric(input$cost))})

  
  tot_data_up<- reactive({
    tot_dat<-NA
    tot_dat <- tot_data()
    tot_dat$Contract<-as.numeric(tot_dat$Contract)
    tot_dat$Churn <- ifelse(tot_dat$Churn=="No",0,1)
    tot_dat<- tot_dat%>%filter(confidence.Yes.>=0.5)
    tot_dat$Priority<-NA
    tot_dat$retention<-NA
    tot_dat$discount<-NA
    tot_dat$sa
    for(cust in 1:nrow(tot_dat))
      {
    
        #assign retention probability and discount based on tenure
        if(tot_dat$tenure[cust]<=12){tot_dat$retention[cust]<-0.3 
                                      tot_dat$discount[cust]<-0.09}
        else if(tot_dat$tenure[cust]<=24){tot_dat$retention[cust]<-0.4
                                                                       tot_dat$discount[cust]<-0.15}
        else {tot_dat$retention[cust]<-0.5
              tot_dat$discount[cust]<-0.20}

        #calculate savings for each customer
        if(tot_dat$MonthlyCharges[cust]-(tot_dat$discount[cust]*25)>0){tot_dat$savings[cust]<-tot_dat$retention[cust]*(tot_dat$MonthlyCharges[cust]-(tot_dat$discount[cust]*25))}
        else {tot_dat$savings[cust]<-tot_dat$retention[cust]*(tot_dat$MonthlyCharges[cust])}

        #calculate priority of each customer  
        tot_dat$Priority[cust]<-(0.25*(tot_dat$confidence.No.[cust])+0.15*(tot_dat$tenure[cust])+0.35*(tot_dat$MonthlyCharges[cust])+0.25*(tot_dat$Contract[cust]))
      }
    tot_dat<-tot_dat%>%arrange(desc(Priority))
    return(tot_dat)
  })#end of tot_dat update
  
  max_num<-reactive({
    i <- as.numeric(budget())
    j <- as.numeric(cost())
    return(floor(i/j))
  })#end max_num reactive
  n<- reactive({
    return(nrow(tot_data_up))
  })
  
  number_adv <- reactive({
    return(min(max_num(),nrow(tot_data_up())))
  }) #end num_adv reactive
  
  number_worth<- reactive({
    cost <- cost()
    count<-0
    for(i in 1:number_adv()){
      if(tot_data_up()$savings[i]>cost)
      {
        count<- count+1
      }
    }
    return(as.integer(count))
  })

  advcost<- reactive({
    return(number_worth()*cost())
  })
  tot_savings<- reactive({
    savings<-0
    for(i in 1:number_worth()){
      savings <- savings+tot_data_up()$savings[i]
    }
    return(as.numeric(savings))
  })#end savings reactive
  
  table_of_ppl<- reactive({
    return(head(tot_data_up(),number_worth()))
  })#end of new table
  
  #TAB1 OUTPUT
  output$Contents<-renderTable({tot_data()})
  #TAB
  output$up<-renderTable({tot_data_up()})
  
  #TAB2 RESULTS OUTPUT
  output$res<-renderTable({table_of_ppl()})

  
  #TAB3 RESUTLS OUTPUT
  output$nadv<-renderPrint(number_adv())
  output$worthadv<- renderPrint(number_worth())
  output$advcost<- renderPrint(advcost())
  output$savings<-renderPrint(tot_savings())
 
  #TAB4 OUTPUT
  output$budget<-renderPrint(budget())
  output$cost<-renderPrint(cost())
  }

shinyApp(ui,server)
  
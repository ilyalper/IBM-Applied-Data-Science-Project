# Stat 564 Project 3 Group R-evolution
# Dilay Özkan & Alper Şener

#Important Notice: Please run Covid_Data.R script before running this script

library(shiny)

ui <- fluidPage(
  
  titlePanel("COVID-19 "),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Here user can select the variable that s/he want to analyze
      selectInput("var", 
                  label = h3("Choose a variable to display"),
                  choices = c("Total Cases", 
                              "New Cases",
                              "Total Deaths", 
                              "New Deaths",
                              "Total Cases per Million",
                              "Total Deaths per Million"),
                  selected = "Total Cases"),
      
      #Here limits of y-axis are taken
      numericInput("bottomlim", "Enter the minimum value of the y-axis:", value=0, min = 0, max = 6000000,step=5000),
      numericInput("upperlim", "Enter the maximum value of the y-axis:", value=300000, min = 0, max = 6000000,step=5000),
      
      #Here limits of x-axis are taken
      dateRangeInput("daterange1", label="Date Range for x-axis",
                     start  = "2020-03-01",
                     end    = "2020-05-15",
                     min    = "2020-03-01",
                     max    = "2020-06-01",
                     format = "mm/dd/yyyy",
                     separator = " - "),
      
      #Illustrated countries are selected from a checkboxgroup
      
      checkboxGroupInput("country_list", "Please Select the Countries:",
                         sort(listofcnt), inline = TRUE, selected = c("Spain","Turkey")),
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("selected_var"),
      textOutput("qre"),
      plotOutput(outputId = "CovidPlot")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$CovidPlot <- renderPlot({
    
    #getting the inputs
    
    t=0;
    if (input$var =="Total Cases") { t=3 }
    if (input$var =="New Cases") { t=4 }
    if (input$var =="Total Deaths") { t=5 }
    if (input$var =="New Deaths") { t=6 }
    if (input$var =="Total Cases per Million") { t=7 }
    if (input$var =="Total Deaths per Million") { t=8 }
    
    start_day=input$daterange1[1]; #start day 
    end_day=input$daterange1[2]; # final day
    
    total_days= as.numeric(end_day)-as.numeric(start_day)+1
    firstday_index=as.numeric(start_day)-as.numeric(as.Date("2020-03-01"))+2
    lastday_index=as.numeric(end_day)-as.numeric(as.Date("2020-03-01"))+2
    
    
    nmbofselected= length(input$country_list)
    
    
    if (nmbofselected==0) {   } # to prevent the error message while no country is selected
    else{
      
      bb= cov.data[cov.data$country==input$country_list[1],c(2,t)]
      graphed = bb[firstday_index:lastday_index,]
      graphss= array(0,c(nrow(graphed),nmbofselected))
      graphss[,1]= data.matrix(graphed[,2])
      
      if (nmbofselected>1) {
        
        for (j in 2:nmbofselected) {
          bb2 = cov.data[cov.data$country==input$country_list[j],c(2,t)]
          graphed = bb2[firstday_index:lastday_index,]
          graphss[,j]= data.matrix(graphed[,2])
        }  } 
      
      #Now, we have the data in graphed frame and graphss matrix, so we can construct the graph
      
      col_name=colnames(graphed)
      
      plot(x=graphed$invest_date,y=graphss[,1],xlab="day",ylab=input$var,type="l",ylim=c(input$bottomlim,input$upperlim),xaxt = "n",lwd=2)   
      axis(1, graphed$invest_date, format(graphed$invest_date, "%b %d"), cex.axis = .7)
      for (i in 1:nmbofselected) {
        lines(x=graphed$invest_date,y=graphss[,i],type="l",lwd=2,col=i)   
      }
      
      legend("topleft",inset=.01,legend=c(input$country_list),
             lty=1,text.font=3,box.lty=0,cex=0.9,lwd=2,col=c(1:nmbofselected))
      
      #end of plot
    } }  )
  
  output$qre <- renderText({ 
    saylar1 = input$daterange1[1]
    saylar2 = input$daterange1[2]
    asd=sprintf("for the period of  %s / %s",saylar1,saylar2)
  })
  
  output$selected_var <- renderText({ 
    paste("The data of", input$var , "is illustrated")
  })
  
}

shinyApp(ui = ui, server = server)
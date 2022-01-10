# Stat 564 Project 3 Group R-evolution
# Dilay Özkan & Alper Şener


#Important Notice: Please run Electricity_Data.R script before running this script 


#install.packages("shiny")

library(shiny)



ui <- fluidPage(
  
  titlePanel("Daily Electricity Demand Breakdown for 2020"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "month",
                  label = "Please Select the Month:",
                  min = 1,
                  max = 12,
                  value = 3)   ,
      
      sliderInput(inputId = "day",
                  label = "Please Select the Threshold Day for 2020:",
                  min = 7,
                  max = 24,
                  value = 22),

      textOutput("old_dem"),
      textOutput("fir_dem"),
      textOutput("sec_dem"),
    ),
    
    # Main panel to display the plot
    mainPanel(
      
      plotOutput(outputId = "MarchPlot")
      
    )
  )
)


server <- function(input, output) {
  
  output$MarchPlot <- renderPlot({


    inv_month=input$month; #investigated month
    thres_day=input$day;
    martcons= array(zeros, c(yil, 7)); 
    for (k in 1:7)  {
      for (i in 2:yil-1) {
        mart_gun=ElektrikTalep[ElektrikTalep$WeekDay==k & ElektrikTalep$Year==i+2015 & ElektrikTalep$Month==inv_month,]
        martcons[i-1,k]=mean(mart_gun$`Tuketim(MWh)`)
      }
      mart_ilk=ElektrikTalep[ElektrikTalep$WeekDay==k & ElektrikTalep$Year==sonyil & ElektrikTalep$Month==inv_month & ElektrikTalep$Day<thres_day+1,]
      martcons[4,k]=mean(mart_ilk$`Tuketim(MWh)`)
      mart_son=ElektrikTalep[ElektrikTalep$WeekDay==k & ElektrikTalep$Year==sonyil & ElektrikTalep$Month==inv_month & ElektrikTalep$Day>thres_day,]
      martcons[5,k]=mean(mart_son$`Tuketim(MWh)`)
    }

    #Plotting 
    
    ilkyari=sprintf("2020 [1 - %d]", thres_day);
    ikinciyari=sprintf("2020 [%d - 31]", thres_day+1);
    renkler =  c("gray75","gray43","gray25","firebrick1","mediumblue");
    renkscale = array(renkler, c(yil,3));
    renkscale[1,];
    plot(1:7,martcons[1,],xlab="weekday",ylab="average hourly demand (GWh)",type="l",col=renkscale[1,],ylim=c(20,40),xaxt = "n",lwd=2)   
    axis(1, at=1:7, labels=gunler)
    for (i in 1:(3+2)) {
      lines(1:7,martcons[i,],type="l",col=renkscale[i,],lwd=2)   
      #lines(1:12,aylikort[i,],type="l",col=i)   
    }
    
    #legend("bottom",inset=.01,legend=c(ilkyil:sonyil),col = renkscale,lty=1,text.font=3,box.lty=0,horiz=TRUE,cex=0.6)
    legend("topright",inset=.01,legend=c("2017","2018","2019",ilkyari,ikinciyari),
           col = renkscale,lty=1,text.font=3,box.lty=0,cex=0.9,lwd=2,)
    
    #end of plot
    
  })
  
  
  
  output$old_dem <- renderText({ 
    inv_month=input$month; #investigated month
    sprintf("Average hourly demand of the Month %d in 2017-2019 period is %.1f GWh.",inv_month, mean(aylikort[2:4,inv_month]));
    }
    )
  output$fir_dem <- renderText({ 
    inv_month=input$month; #investigated month
    thres_day=input$day;
    ilk_cons = array(zeros, 7)
    for (k in 1:7){
      mon_ilk=ElektrikTalep[ElektrikTalep$WeekDay==k & ElektrikTalep$Year==sonyil & ElektrikTalep$Month==inv_month & ElektrikTalep$Day<thres_day+1,]
      ilk_cons[k]=mean(mon_ilk$`Tuketim(MWh)`)
    }
    
    sprintf("Average hourly demand until the end of day %d of the Month %d in 2020 is %.1f GWh.",thres_day,inv_month, mean(ilk_cons));
  }
  )
  output$sec_dem <- renderText({ 
    
    inv_month=input$month; #investigated month
    thres_day=input$day;
    iki_cons = array(zeros, 7)
    for (k in 1:7){
      mon_iki=ElektrikTalep[ElektrikTalep$WeekDay==k & ElektrikTalep$Year==sonyil & ElektrikTalep$Month==inv_month & ElektrikTalep$Day>thres_day,]
      iki_cons[k]=mean(mon_iki$`Tuketim(MWh)`)
    }
    
    sprintf("Average hourly demand after day %d of the\n Month %d in 2020 is %.1f GWh.",thres_day,inv_month, mean(iki_cons));
  }
  

  )
}

shinyApp(ui = ui, server = server)
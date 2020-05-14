install.packages("shiny")
install.packages("dplyr")
install.packages("lubridate")

library(shiny)


library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(lubridate)

theme_set(theme_bw())

mdata <- read.csv("momentum_distributions.gz")
mdata$week <- as.Date(mdata$week)

myColors <- brewer.pal(length(unique(mdata$technology_benchmark_name)),"Set1")
names(myColors) <- levels(mdata$technology_benchmark_name)
colScale <- scale_colour_manual(name = "technology_benchmark_name",values = myColors)
fillScale <- scale_fill_manual(name = "technology_benchmark_name",values = myColors)
color_map <- c(Accessibility ="#E41A1C", Interactive = "#377EB8", Social = "#4DAF4A" )

ui <- fluidPage(
  
  navbarPage("Sports Innovation Data",
             
             
             tabPanel("Industry Analysis",
                        sidebarPanel(width=3,
                          
                          "Select a technology benchmark and date range to filter your analysis.",
                          
                          br(),
                          
                          br(),

                          selectInput(inputId="Benchmark",label="Benchmark",choices=unique(mdata$technology_benchmark_name),selected=NULL,multiple=FALSE),
                          
                          dateRangeInput(inputId="Dates",label="Date Range",start=as.Date("2018-01-01"),end=max(mdata$week),min=min(mdata$week),max=max(mdata$week),startview="month",format="yyyy-mm-dd"),
                          
                          "Set the minimum score cutoff and number of vendors to fine tune which leaders you see.",
                          
                          br(),
                          
                          br(),
                          
                          sliderInput(inputId="ScoreCutoff",label="Score Cutoff", min=0, max=100, value=90, step=1),
                          
                          numericInput(inputId="Leaders",label="Vendors to Show?",value=10, min=1,max=length(unique(mdata$vendor_name)))
                        ),
                      
                      mainPanel(width=9,
                        
                        tabsetPanel(
                          
                          tabPanel("Impact", 
                                   
                                   plotlyOutput("ImpactLeaders"),
                                   
                                   br(),
                                   
                                   br(),
                                   
                                   dataTableOutput("AvgImpact"),
                                   
                                   br(),
                                   
                                   br(),
                                   
                                   plotlyOutput("ImpactLines")
                            
                                   ),
                          
                          tabPanel("Momentum",
                                   
                                   plotlyOutput("MomentumLeaders"),
                                   
                                   br(),
                                   
                                   br(),
                                   
                                   dataTableOutput("AvgMomentum"),
                                   
                                   br(),
                                   
                                   br(),
                                   
                                   plotlyOutput("MomentumLines")
                                   
                                   )
                      ))),
             
             tabPanel("Vendor Analysis",
                      
                      sidebarPanel(width=3,
                        
                        "Select a vendor and date range to dig into their performance over a certain time period.",
                        
                        br(),
                        
                        br(),
                                   
                        selectizeInput(inputId="Vendor",label="Vendor",choices=sort(unique(mdata$vendor_name)),selected="Amazon",multiple=FALSE),
                        
                        dateRangeInput(inputId="Dates2",label="Date Range",start=as.Date("2018-01-01"),end=max(mdata$week),min=min(mdata$week),max=max(mdata$week),startview="month",format="yyyy-mm-dd"),
                        
                        "Select a technology benchmark to filter the graph of the vendor's mentions over time vs. the industry.",
                        
                        br(),
                        
                        br(),
                        
                        selectInput(inputId="Benchmark2",label="Benchmark",choices=unique(mdata$technology_benchmark_name),selected=NULL,multiple=FALSE)
                        
                      ),
                      
                      mainPanel(width = 9,
                        
                        plotlyOutput("Boxplot"),
                        
                        br(),
                        
                        plotlyOutput("Combined"),
                        
                        br(),
                        
                        plotlyOutput("ImpactScore"),
                        
                        br(),
                        
                        plotlyOutput("MomentumScore")

                      )
                      ))
)

server <- function(input, output) {
  
###Graphs for Industry analysis tab############
  
  output$ImpactLeaders <- renderPlotly({
    
    df <- filter(mdata,technology_benchmark_name == input$Benchmark & week >= min(input$Dates) & week <= max(input$Dates) & cum_dist_impact >= input$ScoreCutoff)
    
    df2 <- as.data.frame(table(select(df,vendor_name)))
    
    names(df2) <- c("vendor_name","count")
    
    df2 <- df2[order(-df2$count),]
    
    df2 <- head(df2,input$Leaders)
    
    ggplot(df2, aes(x=reorder(vendor_name,count),y=count,fill=vendor_name)) + geom_bar(stat="identity") + coord_flip() +
      ylab(paste("Weeks with Score Above",input$ScoreCutoff)) + xlab("Vendor Name") + ggtitle("Impact Score Leaders") +
      theme(legend.position="none")
    
    ggplotly(tooltip=c("y"))
  })
  
  output$ImpactLines <- renderPlotly({
    df <- filter(mdata,technology_benchmark_name == input$Benchmark & week >= min(input$Dates) & week <= max(input$Dates) & cum_dist_impact >= input$ScoreCutoff)
    
    df2 <- as.data.frame(table(select(df,vendor_name)))
    
    names(df2) <- c("vendor_name","count")
    
    df2 <- df2[order(-df2$count),]
    
    df2 <- head(df2,input$Leaders)
    
    leaders <- unique(df2$vendor_name)
    
    ggplot(filter(mdata,vendor_name %in% leaders & week >= min(input$Dates) & week <= max(input$Dates)),
           aes(x=week,y=cum_dist_impact,group=vendor_name,color=vendor_name, text=paste("Week: ",week,"<br>Vendor: ",vendor_name,"<br>Score: ",cum_dist_impact))) + geom_point()  + theme(legend.position = "left")
    
    ggplotly(tooltip="text")
    
  })
  
  output$AvgImpact <- renderDataTable({
    
   df <- filter(mdata,technology_benchmark_name == input$Benchmark & week >= min(input$Dates) & week <= max(input$Dates) & cum_dist_impact >= input$ScoreCutoff)
    
   df2 <- as.data.frame(table(select(df,vendor_name)))
    
   names(df2) <- c("vendor_name","count")
    
    df2 <- df2[order(-df2$count),]
    
    df2 <- head(df2,input$Leaders)
    
    leaders <- unique(df2$vendor_name)
    
    startDate <- (min(input$Dates))
    
    endDate <- (max(input$Dates))
    
    myDates <- seq(from=startDate, to=endDate, by="days")
    
    weeks <- length(which(wday(myDates)==2))
    
    df2$pctweeks <- paste0(round((df2$count/weeks)*100,0),"%")
    
    avg <- aggregate(cum_dist_impact~vendor_name,data=filter(mdata,vendor_name %in% leaders & week >= min(input$Dates) & week <= max(input$Dates)),mean)
    
    avg$cum_dist_impact <- round(avg$cum_dist_impact,1)
   
   avg <- merge(avg,select(df2,c("vendor_name","pctweeks")),by.x="vendor_name",by.y="vendor_name")
  
   avg <- avg[order(-avg[,2]),]
   
   names(avg) <- c("Vendor","Avg Score","% Weeks Above Cutoff")
   
   avg
    
  })
  
  output$MomentumLeaders <- renderPlotly({
    
    df <- filter(mdata,technology_benchmark_name == input$Benchmark & week >= min(input$Dates) & week <= max(input$Dates) & cum_dist_momentum >= input$ScoreCutoff)
    
    df2 <- as.data.frame(table(select(df,vendor_name)))
    
    names(df2) <- c("vendor_name","count")
    
    df2 <- df2[order(-df2$count),]
    
    df2 <- head(df2,input$Leaders)
    
    p <-  ggplot(df2, aes(x=reorder(vendor_name,count),y=count,fill=vendor_name)) + geom_bar(stat="identity") + coord_flip() +
      ylab(paste("Weeks with Score Above",input$ScoreCutoff)) + xlab("Vendor Name") + ggtitle("Momentum Score Leaders") +
      
      theme(legend.position="none")
    
    ggplotly(p, tooltip=c("y"))
  })
  
  output$MomentumLines <- renderPlotly({
    df <- filter(mdata,technology_benchmark_name == input$Benchmark & week >= min(input$Dates) & week <= max(input$Dates) & cum_dist_momentum >= input$ScoreCutoff)
    
    df2 <- as.data.frame(table(select(df,vendor_name)))
    
    names(df2) <- c("vendor_name","count")
    
    df2 <- df2[order(-df2$count),]
    
    df2 <- head(df2,input$Leaders)
    
    leaders <- unique(df2$vendor_name)
    
    ggplot(filter(mdata,vendor_name %in% leaders & week >= min(input$Dates) & week <= max(input$Dates)),aes(x=week,y=cum_dist_momentum,group=vendor_name,color=vendor_name,text=paste("Week: ",week,"<br>Vendor: ",vendor_name,"<br>Score: ",cum_dist_momentum))) + geom_point()
  
    ggplotly(tooltip="text")
    })
  
  output$AvgMomentum <- renderDataTable({
    
    df <- filter(mdata,technology_benchmark_name == input$Benchmark & week >= min(input$Dates) & week <= max(input$Dates) & cum_dist_momentum >= input$ScoreCutoff)
    
    df2 <- as.data.frame(table(select(df,vendor_name)))
    
    names(df2) <- c("vendor_name","count")
    
    df2 <- df2[order(-df2$count),]
    
    df2 <- head(df2,input$Leaders)
    
    leaders <- unique(df2$vendor_name)
    
    startDate <- (min(input$Dates))
    
    endDate <- (max(input$Dates))
    
    myDates <- seq(from=startDate, to=endDate, by="days")
    
    weeks <- length(which(wday(myDates)==2))
    
    df2$pctweeks <- paste0(round((df2$count/weeks)*100,0),"%")
    
    avg <- aggregate(cum_dist_momentum~vendor_name,data=filter(mdata,vendor_name %in% leaders & week >= min(input$Dates) & week <= max(input$Dates)),mean)
    
    avg$cum_dist_momentum <- round(avg$cum_dist_momentum,1)
    
    avg <- merge(avg,select(df2,c("vendor_name","pctweeks")),by.x="vendor_name",by.y="vendor_name")
    
    avg <- avg[order(-avg[,2]),]
    
    names(avg) <- c("Vendor","Avg Score","% Weeks Above Cutoff")
    
    avg
    
  })
  
  ####################graphs for Vendor Analysis Tab##########
  
  output$Boxplot <- renderPlotly({
    ggplot() + geom_boxplot(data=filter(mdata,technology_benchmark_name==input$Benchmark2 & week>=min(input$Dates2) & week<=max(input$Dates2)),aes(x=week,y=impact_mentions_per_three_months, group=week),alpha=0.5) +
      geom_point(data=filter(mdata,technology_benchmark_name==input$Benchmark2 & vendor_name==input$Vendor & week<=max(input$Dates2) & week >=min(input$Dates2)),aes(x=week,y=impact_mentions_per_three_months,color=technology_benchmark_name),size=4) + colScale +
      theme(legend.position = "none") + 
      xlab("Week") + ylab("Impact Mentions per Three Months") + ggtitle(paste(input$Benchmark2," - ",input$Vendor," vs. Industry Mentions"))
  })
  
  
  output$ImpactScore <- renderPlotly({
    ggplot(filter(mdata,vendor_name==input$Vendor & week>=min(input$Dates2) & week<=max(input$Dates2)), aes(x=week, y=cum_dist_impact, group=technology_benchmark_name, color=technology_benchmark_name,alpha=cum_dist_impact,text=paste("Week: ",week,"<br>Impact Score: ",cum_dist_impact))) +
      geom_point(size=5) + geom_line(alpha=1) + colScale +
      facet_wrap(~technology_benchmark_name) + theme(legend.position="none") +
      xlab("Week") + ylab("Impact Score") + ggtitle("Impact Score over Time")
    
    ggplotly(tooltip="text")
  })
  
  output$MomentumScore <- renderPlotly({
    ggplot(filter(mdata,vendor_name==input$Vendor & week>=min(input$Dates2) & week<=max(input$Dates2)), aes(x=week, y=cum_dist_momentum, group=technology_benchmark_name, color=technology_benchmark_name,alpha=cum_dist_momentum,text=paste("Week: ",week,"<br>Momentum Score: ",cum_dist_momentum))) +
      geom_point(size=5) + geom_line(alpha=1) + colScale +
      facet_wrap(~technology_benchmark_name) + theme(legend.position="none") +
      xlab("Week") + ylab("Momentum Score") + ggtitle("Momentum Score over Time")
    
    ggplotly(tooltip="text")
  })
  
  output$Combined <- renderPlotly({
    ggplot(filter(mdata,vendor_name==input$Vendor & week>=min(input$Dates2) & week<=max(input$Dates2)),aes(text=paste("Week: ",week,"<br>Impact Score: ",cum_dist_impact,"<br>Momentum Score: ",cum_dist_momentum))) +
      geom_line(aes(x=week,y=cum_dist_impact,group=technology_benchmark_name,color=technology_benchmark_name),size=1.1) +
      geom_bar(aes(x=week,y=cum_dist_momentum,fill=technology_benchmark_name),alpha=0.1,stat="identity") + colScale + fillScale +
      facet_wrap(~technology_benchmark_name) +
      theme(legend.position = "none") +
      xlab("Week") + ylab("Scores") + ggtitle ("Impact vs. Momentum Scores over Time")
    
    ggplotly(tooltip="text")
  })
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(ggplot2)  # for the diamonds dataset
library(wordcloud)
library(tidyverse)

budgets <- read_excel("budgets.xlsx")
revenue <- read_excel("revenue.xlsx")
genres <- read_excel('genres1.xlsx')
test <- read_excel("test.xlsx")

ui <- fluidPage(
  titlePanel("Movies"),
  
  sidebarLayout(
    sidebarPanel(
      "Create by Jiajing Cai",
      #release year sliderbar
      sliderInput("years",
                  "Release Year:",
                  min = min(test$`Release Year`),
                  max = max(test$`Release Year`),
                  value = c(1980,max)),
      selectInput("data_label","Revenue or Budgets:",choices = c("Revenue","Budget")),
      checkboxGroupInput("show_genres", "Choice the Genres of the Movie:",
                       budgets$genres, selected = budgets$genres)

    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Line Chart",plotOutput("yearPlot")),
        tabPanel("Barplot", plotOutput("plot1")),
        tabPanel("Worldcloud", h1("WordCloud base on Popularity"),plotOutput("plot2"))
      )
    )
  )
)

server <- function(input, output) {
  output$yearPlot <- renderPlot({
    
    y1 = min(input$years)
    y2 = max(input$years)
    
    df = test %>%
      filter(`Release Year` >= y1 & `Release Year` <= y2)  %>% 
      gather(key = "Revenue&Budget", value = "Value", Budget, Revenue)
    
    
    ggplot(df, aes(x = `Release Year`, y = Value)) + 
      geom_line(aes(color = `Revenue&Budget`), size = 1.2, alpha = 0.8) +
      labs(title = "Revenue & Budget Trend", y = "Revenue") +
      scale_y_continuous(labels=scales::dollar_format(scale = 1e-6, suffix = "M"), 
                         sec.axis = sec_axis(~. , name = "Budget", labels = scales::dollar_format(scale = 1e-6, suffix = "M")))
    
    
  })
  
  plotdata1 <- reactive({
    if (input$data_label=="Revenue") {
      subset(revenue,revenue$genres %in% input$show_genres)
    }else{
      subset(budgets,budgets$genres %in% input$show_genres)
    }
  })

  output$plot1 <- renderPlot({
    thedata <- plotdata1()
    if (input$data_label=="Revenue") {
      ggplot(thedata,aes(x=reorder(genres,Revenue),y=Revenue))+geom_bar(stat="identity",aes(fill=genres))+
        xlab("genres")+coord_flip()
    }else{
      ggplot(thedata,aes(x=reorder(genres,Budget),y=Budget))+geom_bar(stat="identity",aes(fill=genres))+
        xlab("genres")+coord_flip()
    }
  },width = 1000,height = 800)
  
  output$plot2 <- renderPlot({
    pal = brewer.pal(9,"Blues")
    wordcloud(genres$Genres,genres$Popularity,
              random.order = F,
              random.color = F,
              colors = pal)
  },width = 400,height = 400)
  
  
}

shinyApp(ui, server)
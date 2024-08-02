library(shiny)
library(plotly)

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Customizable Gauge Charts"),
  
  # # Sidebar layout with input and output definitions ----
  # sidebarLayout(
  #   
  #   # Sidebar panel for inputs ----
  #   sidebarPanel(
      
      # Inputs ----
      fluidRow(
      column(3, 
             numericInput(inputId = "max", 
                   label = "Maximum Possible", 
                   value = 20),
             numericInput(inputId = "cur", 
                          label = "Current Value", 
                          value = 15),
             radioButtons(inputId = "type",
                          label = "Select Numeric Suffix",
                          choices = list("None" = 1, "%" = 2),
                          selected = 2),
             textInput(inputId = "title",
                       label = "Title (optional)",
                       value = ""),
             radioButtons(inputId = "goal",
                          label = "Add Goal Label?",
                          choices = list("Yes" = 1, "No" = 2),
                          selected = 1)), 
      column(3, 
             selectInput(inputId = "color",
                            label = "Bar Color",
                            choices = list("Red" = "#D2202A", "Yellow" = "#FDBF56", 
                                           "Azure" = "#2CA5DA", "Iris" = "#5B4C93",
                                           "Periwinkle" = "#92A1CD", "Beige" = "#D8D2C4",
                                           "Medium Gray" = "#ABA9A9", "Cool Gray" = "#898989",
                                           "Black" = "#000000", "White" = "#FFFFFF"), selected = 1),
             selectInput(inputId = "color_bkg",
                         label = "Background Color- Minimum to Current",
                         choices = list("Red" = "#D2202A", "Yellow" = "#FDBF56", 
                                        "Azure" = "#2CA5DA", "Iris" = "#5B4C93",
                                        "Periwinkle" = "#92A1CD", "Beige" = "#D8D2C4",
                                        "Medium Gray" = "#ABA9A9", "Cool Gray" = "#898989",
                                        "Black" = "#000000", "White" = "#FFFFFF"), selected = "#FFFFFF"),
             selectInput(inputId = "color_bkg2",
                  label = "Background Color- Current to Maximum",
                  choices = list("Red" = "#D2202A", "Yellow" = "#FDBF56", 
                                 "Azure" = "#2CA5DA", "Iris" = "#5B4C93",
                                 "Periwinkle" = "#92A1CD", "Beige" = "#D8D2C4",
                                 "Medium Gray" = "#ABA9A9", "Cool Gray" = "#898989",
                                 "Black" = "#000000", "White" = "#FFFFFF"), selected = "#FFFFFF")),
      column(3,
             radioButtons(inputId = "target_r",
                          label = "Add Target Line?",
                          choices = list("Yes" = 1, "No" = 2),
                          selected = 2),
             numericInput(inputId = "target", 
                   label = "Target Value", 
                   value = 15), 
             selectInput(inputId = "target_color",
                         label = "Target Color",
                         choices = list("Red" = "#D2202A", "Yellow" = "#FDBF56", 
                                        "Azure" = "#2CA5DA", "Iris" = "#5B4C93",
                                        "Periwinkle" = "#92A1CD", "Beige" = "#D8D2C4",
                                        "Medium Gray" = "#ABA9A9", "Cool Gray" = "#898989",
                                        "Black" = "#000000", "White" = "#FFFFFF"), selected = 1)),
      column(3,
             radioButtons(inputId = "delta_r",
                          label = "Add Delta?",
                          choices = list("Yes" = 1, "No" = 2),
                          selected = 2),
             numericInput(inputId = "prev",
                   label = "Previous Value",
                   value = 13),
             selectInput(inputId = "delta_color",
                  label = "Delta Color",
                  choices = list("Red" = "#D2202A", "Yellow" = "#FDBF56", 
                                 "Azure" = "#2CA5DA", "Iris" = "#5B4C93",
                                 "Periwinkle" = "#92A1CD", "Beige" = "#D8D2C4",
                                 "Medium Gray" = "#ABA9A9", "Cool Gray" = "#898989",
                                 "Black" = "#000000", "White" = "#FFFFFF"), selected = "#92A1CD")),
  fluidRow(
    column(12, 
           helpText("The default is the basic version of the chart. Select 'Yes' for target and/or delta values if you would like to add them to the chart. Hover over the chart and select the camera button on the top right corner of the image to download the chart as an image.")
  ))
      
    ,
    
    # Main panel for displaying outputs ----
    mainPanel(splitLayout(cellwidths = c("45%"), plotlyOutput("gauge"))
     
        # mainPanel(
        #   fluidRow(
        #     column(9,
        #            splitLayout(cellWidths = c("45%", "45%"), plotlyOutput("gaugebasic"),  plotlyOutput("gauge")))
      
      
    
  )
))

# Define server logic required  ----
server <- function(input, output) {
  

type_r <- reactive({
    ifelse(input$type == 2, "%", "")
  }) 
goal_label <- reactive({
  ifelse(input$goal == 1, "Goal: ", "")
})

goal_label_num <- reactive({
  ifelse(input$goal == 1, input$max, "")
})
goal_label_suff <- reactive({
  ifelse(input$goal == 1, type_r(), "")
})
delta_r <- reactive({
  ifelse(input$delta_r == 1, "gauge+number+delta", "gauge+number")
})
delta_color_r <- reactive({
  ifelse(input$delta_r == 1, input$delta_color, input$color_bkg)
})
target_r <- reactive({
  ifelse(input$target_r == 1, input$target, input$cur)
})
target_color_r <- reactive({
  ifelse(input$target_r == 1, input$target_color, "black")
})


  output$gauge <- renderPlotly({

    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)), # creates the background plot
      value = input$cur, #sets the value that you want to use as the current value
      number = list(suffix = type_r()), #put percent sign after
      title = list(text = input$title), #gives a title above the chart
      type = "indicator", #tells us what type of chart we are using
      mode = delta_r(), # here, we can change this to not include the number or delta by removing that portion
      delta = list(reference = input$prev, increasing = list(color = delta_color_r())), #if we are including delta, add reference and other changes here
      gauge = list( #aesthetics within the gauge chart
        axis =list(range = list(NULL, input$max),
                   ticksuffix = type_r()), #range of the gauge chart e.g. here is 0 to 20
        bar = list(color = input$color), #color of the bar for the current value shown on gauge chart
        steps = list( #if you want different color backgrounds to show "steps" within the gauge chart like previous year
          list(range = c(0, input$prev), color = input$color_bkg), #the range in the c(,) argument gives the portion of the axis you are referencing and the color argument gives the color you want the background to be
          list(range = c(input$prev, input$cur), color = delta_color_r()), #same as previous line
          list(range = c(input$cur, input$max), color = input$color_bkg2)), #same as previous line
        threshold = list( #threshold adds a line at a value along the axis
          line = list(color = target_color_r(), width = 2), #what color and thickness you would like the line
          thickness = 1, #1 if you would like the line all the way across the gauge, <1 if you would like it to be shorter
          value = target_r()))) 
    #where you would like the line
    fig <- fig %>% layout(margin = list(l=55,r=55), #change the margins
           font = list(family = "Arial", size = 15),
           width = 500,
           height = 400,
           annotations = list(
             x = 1.1,
             y = .1,
             text = paste(goal_label(), goal_label_num(), goal_label_suff()),
             showarrow = FALSE,
             font = list(family = "Arial",size = 18)))
    
  })
  
  # output$gaugebasic <- renderPlotly({
  #   
  #   fig2 <- plot_ly(
  #     domain = list(x = c(0, 1), y = c(0, 1)), # creates the background plot
  #     value = input$cur, #sets the value that you want to use as the current value
  #     number = list(suffix = "%"), #put percent sign after
  #     title = list(text = input$title), #gives a title above the chart
  #     type = "indicator", #tells us what type of chart we are using
  #     mode = "gauge+number", # here, we can change this to not include the number or delta by removing that portion
  #     gauge = list( #aesthetics within the gauge chart
  #       axis =list(range = list(NULL, input$max), ticksuffix = "%"), #range of the gauge chart e.g. here is 0 to 20
  #       bar = list(color = input$color), #color of the bar for the current value shown on gauge chart
  #       steps = list( #if you want different color backgrounds to show "steps" within the gauge chart like previous year
  #         list(range = c(0, input$prev), color = input$color_bkg), #the range in the c(,) argument gives the portion of the axis you are referencing and the color argument gives the color you want the background to be
  #         list(range = c(input$prev, input$cur), color = input$color_bkg), #same as previous line
  #         list(range = c(input$cur, input$max), color = input$color_bkg2)), #same as previous line
  #       threshold = list( #threshold adds a line at a value along the axis
  #         line = list(color = "black", width = 2), #what color and thickness you would like the line
  #         thickness = 1, #1 if you would like the line all the way across the gauge, <1 if you would like it to be shorter
  #         value = input$cur))) #where you would like the line
  #   fig2 <- fig2 %>% layout(margin = list(l=50,r=50), #change the margins
  #                         font = list(family = "Arial", size = 15),
  #                         annotations = list(
  #                           x = 1.15,
  #                           y = .18,
  #                           text = paste(goal_label(), goal_label_num(), goal_label_suff()),
  #                           showarrow = FALSE,
  #                           font = list(family = "Arial",size = 18)))
  #   fig2
  # })
  
}
shinyApp(ui = ui, server = server)
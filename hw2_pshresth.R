library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(scales)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(lubridate)
library(bslib)

# Load and clean data ----------------------------------------------
churn_df = read.csv("telco_churn.csv")
churn_df = na.omit(churn_df) # omit all the missing values

# categorical variables
cat_cols = c("Gender", "SeniorCitizen", "Partner", "Dependents",
             "PhoneService", "MultipleLines", "InternetService",
             "OnlineSecurity", "OnlineBackup", "DeviceProtection",
             "TechSupport", "StreamingTV", "StreamingMovies",
             "Contract", "PaperlessBilling", "PaymentMethod")

# continuous variables
cont_cols = c("Tenure", "MonthlyCharges", "TotalCharges", "Churn")

# both categorical variables to factors and continuous to numeric in a batch
churn_df[cat_cols] = lapply(churn_df[cat_cols], factor)
churn_df[cont_cols] = sapply(churn_df[cont_cols], as.numeric)

churners_total = sum(churn_df$Churn) # total number of churners

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Churners Dashboard")

# Dashboard Sidebar -------------------

#Exploratory Data Analysis

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Data Exploration", icon = icon("bar-chart"), tabName = "explore"),
    menuItem("Data Table", icon = icon("table"), tabName = "table"),
    menuItem("Model Building", icon = icon("cogs"), tabName = "model"),
    
    hr(),
    
    conditionalPanel("input.tabs === 'explore'", # show the following input layouts when the user
                                                  # is on the explore tab
     # Inputs: select categorical variables to plot ----------------------------------------------
      selectInput(inputId = "catFeatures",
                  label = "Categorical Features:",
                  choices = sort(cat_cols),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "Contract"),
   
     # Continuous Variables Selection ----------------------------------------------
      selectInput(inputId = "contFeatures",
                  label = "Continuous Features:",
                  choices = sort(cont_cols[-length(cont_cols)]),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "Tenure")),
    
    # Features to Show (Build your Own Table) ------------------------------------
    conditionalPanel("input.tabs === 'table'", # conditional panel for table tab
                     
      downloadButton(outputId = "write_csv", 
                     label = "Download data"),              
      
      checkboxGroupInput(inputId = "data_table",
                         label = "Build your table",
                         choices = sort(colnames(churn_df)),
                         selected = c("Churn", "Contract"))),

    
    conditionalPanel("input.tabs === 'model'", # same conditional tab setup with the model
      # Model Building (Build your Predictive Model) ------------------------------------
      checkboxGroupInput(inputId = "select_features",
                         label = "Features for Model",
                         choices = sort(colnames(churn_df))[-1],
                         selected = "Contract"))
)
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("explore",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            valueBoxOutput("cat_variables"),
            valueBoxOutput("cont_variables")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot", # build two tabs in this exploration tab
                   width = 12,
                   tabPanel("Categorical Features", plotlyOutput("plot_barplot")),
                   tabPanel("Continuous Features", plotlyOutput("plot_boxplot")))
          )
  ),
  
  # Data Table Page
  tabItem("table",
          fluidPage(
            box(title = "Selected Features", 
                dataTableOutput("table"), width = 12))
          
  ),
  
  # Building your Predictive Model
  tabItem("model",
          # Value Boxes ----------------------------------------------
          fluidPage(
          h1("Build Your Predictive Model")),
          #   box(title = "Build your Predictive Model")),
          
          fluidRow( # three metrics here to assess the model
            valueBoxOutput("accurracy_rate"), 
            valueBoxOutput("goodness_of_fit"),
            valueBoxOutput("ss_features")),
          fluidRow(
            plotlyOutput("model_building")
          ))
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin = "purple")

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # # Reactive data function -------------------------------------------

  churn_subset = reactive({
    req(input$catFeatures) # ensures availablity of the feature before proceeding
    req(input$contFeatures)
    filter(churn_df, input$catFeatures %in% colnames(churn_df) & 
             input$contFeatures %in% colnames(churn_df))
  })
  
  # data table to be downloaded as CSV
  churn_datatable = reactive({
    subset(churn_df, select = c(input$data_table))
  })
  
  # features used for model building
  churn_subset_model = reactive({
    req(input$select_features)
    subset(churn_df, select = c("Churn", input$select_features))
  })
  
  # model assessment (accuracy rate, goodness of fit, and # of statistically significant variables)
  predictions = reactive({
    dat = churn_subset_model()
    dat$Churn = as.factor(dat$Churn)
    
    glm.fit = glm(Churn ~ ., data = dat, family = "binomial") # fit a logistic regression
    preds = ifelse(glm.fit$fitted.values <= 0.5, 0, 1) # set the cut-off threshold as 0.5
    acc_rate = mean(preds == churn_df$Churn) # accuracy rate
    goodness_of_fit = 1 - (glm.fit$deviance / glm.fit$null.deviance) # goodness of fit
    ss_vars = sum(as.numeric(coef(summary(glm.fit))[,"Pr(>|z|)"] < 0.05)) # statistically significant variables
                                                                        # alpha is 0.05
    summary = c(acc_rate, goodness_of_fit, ss_vars)
    summary
  })
  
  # reactive function to get the residuals for the residual plot
  qq_plot = reactive({
    dat = churn_subset_model()
    dat$Churn = as.factor(dat$Churn)
    
    glm.fit = glm(Churn ~ ., data = dat, family = "binomial")
    qq_plot_df = data.frame(glm.fit$residuals)
    names(qq_plot_df) = "Residuals"
    qq_plot_df
  })

# Outputs  
    
# A plot showing the bar plot of continuous features -----------------------------
  output$plot_barplot <- renderPlotly({
    dat = churn_subset() %>% 
      group_by(get(input$catFeatures)) %>% 
      summarize(pct_churn = Churn / churners_total)
    colnames(dat) = c(input$catFeatures, "Churners")
    
  # bar plot of the categorical features -----------------------------------
    
    ggplot(data = dat, mapping = aes_string(x = input$catFeatures,
                                          y = "Churners",
                                          fill = input$catFeatures)) + 
      geom_bar(stat = "identity") +
      labs(title = "Distribution of Churning",
           y = "Percentage of Churners",
           x = input$catFeatures) +
      scale_y_continuous(labels = percent) +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "none") 
    
    })

# box plot of the continuous features -----------------------------------
  output$plot_boxplot = renderPlotly({
    dat = subset(churn_subset(), select = c("Churn", input$contFeatures))
    dat$Churn = as.factor(dat$Churn)

    ggplot(data = dat, mapping = aes_string(x = "Churn", 
                                            y = input$contFeatures,
                                            fill = "Churn")) + 
      geom_boxplot() +
      labs(x = "Churners \n (1 = Churner)",
           title = "Box Plot of Chuners") +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  })

# Data Table
  
  # Data table of selected features ----------------------------------------------
  output$table = renderDataTable({
    subset(churn_df, select = input$data_table)
  })
  
  # write sampled data as csv file ---------------------------------------
  output$write_csv <- downloadHandler(
    filename <- function(){paste0("churn_", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv", sep = "")}, # asks user where to save the csv file
    content = function(filname){
      write.csv(churn_datatable(), file = filname, row.names = FALSE)
    }
  )
  
# Model Building
  
  #   Accuracy Rate info box ----------------------------------------------
  output$accurracy_rate <- renderValueBox({
    valueBox("Accuracy Rate", value = round(predictions()[1] * 100, digits = 2), # accuracy rate
             # subtitle = paste(nrow(sw), "characters"), 
             icon = icon("bullseye"), color = "red")
  })
  
  output$goodness_of_fit <- renderValueBox({
    valueBox("Goodness of Fit", value = round(predictions()[2] * 100, digits = 2), # goodness of fit
             icon = icon("smile-wink"), color = "green")
  })
  
  output$ss_features <- renderValueBox({
    valueBox("# of Statistically Significant Variables", value = predictions()[3],# statistically significant variables
             icon = icon("sort-numeric-up-alt"), color = "blue")
  })

    
# Building Modeling ----------------------------------------------
  output$model_building = renderPlotly({
    dat = qq_plot()
    ggplot(data = dat, mapping = aes_string(sample = "Residuals")) + # plot the QQ-plot
      stat_qq(distribution = 'qunif', alpha = 0.5) +
      labs(x = "Theoretical Quantiles",
           y = "Residuals",
           title = "Q-Q Plot of Residuals") +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
})

}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)

# Select which columns to include in the analysis. Hit the run button and build a new model.

library(tidyverse)
library(shiny)

calcLift <- function(scored_data, cutoff = 0.5) {
  scored_data %>%
    mutate(bin = ntile(desc(pred), 10),
           prediction = ifelse(pred > cutoff, 1, 0)) %>%
    group_by(bin) %>%
    summarize(count = sum(prediction == label)) %>%
    mutate(prop = count / sum(count)) %>%
    arrange(bin) %>%
    mutate(prop = cumsum(prop)) %>%
    select(-count)
}

train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

features <- tbl_vars(test) %>%
  .[-(. == "label")]

ui <- pageWithSidebar(
  HTML("<center><h1> Predicting Income From Census Data</h1></center> \n <center><h2> Is Your Income >50K? </h2></center>"),
  sidebarPanel(
    selectizeInput('selfeatures', 'Select Features:', features, multiple = TRUE),
    actionButton('fit', "Add Model")
  ),
  mainPanel(
    div(id = "models")
  )
)

server <- function(input, output, session) {

  observeEvent(input$fit, {
    withProgress(message = "Working...", value = 0.1, {
      incProgress(0.25, detail = "Fitting Tensorflow Model")
      f <- paste("label ~ ", paste(input$selfeatures, collapse= "+"))
      model <- glm(as.formula(f), binomial, train)

      incProgress(0.5, detail = "Scoring Model")
      scored <- predict(model, test, type = "response")
      test$pred <- scored

      incProgress(0.75, detail = "Evaluating Model")
      label <- paste0(input$fit, "model")

      insertUI(selector = "#models",
        ui = plotOutput(label)
      )

      output[[label]]  <- renderPlot({
       test %>%
          calcLift %>%
          ggplot(aes(x = bin, y = prop)) +
          geom_point() + geom_line() +
          labs(
            title = paste("Lift Chart: ", f),
            x = "",
            y = ""
          ) +
          geom_abline(slope = 1/10, intercept = 0, color = "red")
      })
    })
  })
}

shinyApp(ui = ui, server = server)

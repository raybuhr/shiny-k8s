library(shiny)
library(ggplot2)
library(data.table)


if (!file.exists("winequality-red.csv")) {
    wine_data_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
    download.file(url = wine_data_url, destfile = "winequality-red.csv")
}

wines <- as.data.frame(fread("winequality-red.csv", sep = ";"))
names(wines) <- gsub(" ", "_", names(wines))
# change quality from 1-6 to 3-8
# wines$quality <- wines$quality - 2L
wines$quality <- factor(wines$quality, levels = seq(3, 8))
axis_vars <- names(wines)[1:11]

ui <- tagList(navbarPage(
    "Red Wine Quality",
    tabPanel(
        "Scatterplot",
        sidebarPanel(
            tags$h5("https://archive.ics.uci.edu/ml/datasets/Wine+Quality"),
            selectInput(
                inputId = "xcol",
                label = "Data to use as the x-axis:",
                choices = axis_vars,
                selected = axis_vars[[1]]
            ),
            selectInput(
                inputId = "ycol",
                label = "Data to use as the y-axis:",
                choices = axis_vars,
                selected = axis_vars[[2]]
            )
        ),
        mainPanel(plotOutput('scatterPlot'))
    ),
    tabPanel(
        "Prediction",
        tags$h5("https://archive.ics.uci.edu/ml/datasets/Wine+Quality"),
        fluidRow(
            column(
                3,
                wellPanel(
                    sliderInput(
                        inputId = "fixed_acidity",
                        label = "Fixed Acidity:",
                        min = min(wines$fixed_acidity),
                        max = max(wines$fixed_acidity),
                        value = mean(wines$fixed_acidity)
                    ),
                    sliderInput(
                        inputId = "volatile_acidity",
                        label = "Volatile Acidity:",
                        min = min(wines$volatile_acidity),
                        max = max(wines$volatile_acidity),
                        value = mean(wines$volatile_acidity)
                    ),
                    sliderInput(
                        inputId = "citric_acid",
                        label = "Citric Acid:",
                        min = 0.0,
                        max = 1.0,
                        value = 0.25
                    ),
                    sliderInput(
                        inputId = "residual_sugar",
                        label = "Residual Sugar:",
                        min = min(wines$residual_sugar),
                        max = max(wines$residual_sugar),
                        value = mean(wines$residual_sugar)
                    ),
                    sliderInput(
                        inputId = "chlorides",
                        label = "Chlorides:",
                        min = min(wines$chlorides),
                        max = max(wines$chlorides),
                        value = mean(wines$chlorides)
                    )
                )
            ),
            column(
                3,
                wellPanel(
                    sliderInput(
                        inputId = "free_sulfur_dioxide",
                        label = "Free Sulfur Dioxide:",
                        min = min(wines$free_sulfur_dioxide),
                        max = max(wines$free_sulfur_dioxide),
                        value = mean(wines$free_sulfur_dioxide)
                    ),
                    sliderInput(
                        inputId = "total_sulfur_dioxide",
                        label = "Total Sulfur Dioxide:",
                        min = min(wines$total_sulfur_dioxide),
                        max = max(wines$total_sulfur_dioxide),
                        value = mean(wines$total_sulfur_dioxide)
                    ),
                    sliderInput(
                        inputId = "density",
                        label = "Density:",
                        min = min(wines$density),
                        max = max(wines$density),
                        value = mean(wines$density)
                    ),
                    sliderInput(
                        inputId = "pH",
                        label = "pH:",
                        min = 0.0,
                        max = 14.0,
                        step = 0.25,
                        value = 3.25
                    ),
                    sliderInput(
                        inputId = "sulphates",
                        label = "Sulphates:",
                        min = 0.0,
                        max = 2.0,
                        step = 0.25,
                        value = 0.75
                    ),
                    sliderInput(
                        inputId = "alcohol",
                        label = "Alcohol:",
                        min = 0.5,
                        max = 25.0,
                        value = 10.0
                    )
                )
            ),
            column(6, wellPanel(
                h2("Predicted Quality"),
                textOutput('predictedQuality')
            ))
        )
    )
))

server <- function(input, output, session) {

    output$scatterPlot <- renderPlot({
        pal <- c("#a84551", "#963e48", "#843640", "#722f37", "#60282e", "#4e2026")

        ggplot(wines, aes(x=!!as.name(input$xcol),
                          y=!!as.name(input$ycol),
                          color=quality,
                          size=quality
                          )) +
            geom_jitter() +
            scale_color_manual(values = pal) +
            theme_minimal(base_size = 16)
    })

    output$predictedQuality <- renderText({
        fit <- lm(as.integer(quality) ~ ., data=wines)
        new_data <- data.frame(
            fixed_acidity = c(input$fixed_acidity),
            volatile_acidity = c(input$volatile_acidity),
            citric_acid = c(input$citric_acid),
            residual_sugar = c(input$residual_sugar),
            chlorides = c(input$chlorides),
            free_sulfur_dioxide = c(input$free_sulfur_dioxide),
            total_sulfur_dioxide = c(input$total_sulfur_dioxide),
            density = c(input$density),
            pH = c(input$pH),
            sulphates = c(input$sulphates),
            alcohol = c(input$alcohol)
        )
        pred <- predict(fit, newdata = new_data)
        paste("Based on the selected values, we would expect a wine quality of",
              round(pred, 2), sep = "\n")
    })
}

shinyApp(ui = ui, server = server)

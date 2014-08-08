library(shiny)
library(ggvis)

shinyUI(fluidPage(theme="bootstrap.min.css",
        titlePanel("RNA Expression Quality Controll"),

        sidebarLayout(
          sidebarPanel(
            selectInput("species", "RNA species:",
                        c("bla")),
            selectInput("group_factor", "Color by:",
                        c("ble"))
          ),
          mainPanel(
            tabsetPanel(type="tabs",
                        tabPanel("Boxplot"),
                        tabPanel("Density"),
                        tabPanel("Heatmap"),
                        tabPanel("MA plot"),
                        tabPanel("PCA")
                        )
          )
        )
))
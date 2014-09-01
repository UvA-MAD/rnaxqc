library(shiny)
library(ggvis)

shinyUI(fluidPage(theme="bootstrap.min.css",
        titlePanel("RNA Expression Quality Controll"),

        sidebarLayout(
          sidebarPanel(
            selectInput("species", "RNA species:",
                        species_list),
            conditionalPanel(
              condition = "input.tabset != 'Density'",
              selectInput("group_factor", "Color by:",
                        group_factors)
            ),
            dataTableOutput("brushed_design")
          ),
          mainPanel(
            tabsetPanel(type="tabs", id="tabset",
                        tabPanel("Boxplot", plotOutput("box")),
                        tabPanel("Density", ggvisOutput("density")),
                        tabPanel("Heatmap"),
                        tabPanel("MA plot"),
                        tabPanel("PCA",
                                 fluidRow(
                                   column(6, ggvisOutput("pca21")),
                                   column(6, ggvisOutput("pca23"))
                                 ),
                                 fluidRow(
                                   column(6, ggvisOutput("pca31")),
                                   column(6, ggvisOutput("pca_proportion"))
                                 )
                        )
          )
        )
)))
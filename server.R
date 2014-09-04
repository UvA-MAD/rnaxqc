library(shiny)
library(reshape)
library(ggvis)
library(hexbin)

source("vis.R")

shinyServer(function(input, output, session) {

  # boxplot ###################################################################
  output$box <- renderPlot({
    species_count_design <- count_design_molten[[input$species]]
    boxplot.rnaxqc(species_count_design, input$group_factor, input$species)
  })

  # density ###################################################################
  observe({
    species_count_design <- count_design_molten[[input$species]]
    densplot.rnaxqc(species_count_design, input$group_factor) %>%
      bind_shiny("density")
  })

  # heatmap ###################################################################
  output$heatmap1 <- renderPlot({
    species_counts <- count_tables[[input$species]]
    heatmap1.rnaxqc(species_counts)
  })
  output$heatmap2 <- renderPlot({
    species_counts <- count_tables[[input$species]]
    heatmap2.rnaxqc(species_counts)
  })

  # ma plots ##################################################################

  species_ma <- reactive({
    # pick for selected species
    ma_tables[[input$species]]
  })

  output$ma <- renderPlot({
    if (input$ma_plot_type == "points") {
      maplot.points.rnaxqc(species_ma(), input$species)
    } else {
      maplot.hexbin.rnaxqc(species_ma(), input$species)
    }
  }, height=150*length(samples))

  # PCA plots #################################################################
  nsamples <- nrow(pca_rot[[1]])

  sub_pca_rot <- reactive({
    # pick up pca rotation only for species of choice
    species_rot <- pca_rot[[input$species]]
    # add ids to be able to do the brushing
    species_rot$id <- seq_len(nsamples)
    species_rot
  })

  # pca proportion of variance
  sub_pca_prop <- reactive({
    species_prop <- pca_prop[[input$species]]
  })

  # tooltip to get sampleid
  pca_sample_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    sampleid <- sub_pca_rot()$sampleid[sub_pca_rot()$id == x$id]
    paste0("<b>", sampleid, "</b>")
  }

  # brushing
  pca_lb <- reactive({
    linked_brush(keys = sub_pca_rot()$id, "grey")
  })

  # get what has been selected with a brush
  pca_selection <- reactive({
    sub_pca_rot()[pca_lb()$selected(), ]
  })

  observe({
    sub_pca_rot %>%
      ggvis(~PC2, ~PC1, key := ~id) %>%
      layer_points(fill = ~factor(sub_pca_rot()[[input$group_factor]]),
                   fillOpacity :=0.3, fillOpacity.hover := 0.7,
                   size := 60, size.hover := 180,
                   fill.brush := "red", fillOpacity.brush := 1) %>%
      add_legend(c("fill"), title="") %>%
      add_tooltip(pca_sample_tooltip, "hover") %>%
      pca_lb()$input() %>%
      add_data(pca_selection) %>%
      layer_points(fill := "red", fillOpacity := 1) %>%
      set_options(width=400, height=300) %>%
      bind_shiny("pca21")
  })

  observe({
    sub_pca_rot %>%
      ggvis(~PC3, ~PC1, key := ~id) %>%
      layer_points(fill = ~factor(sub_pca_rot()[[input$group_factor]]),
                   fillOpacity :=0.3, fillOpacity.hover := 0.7,
                   size := 60, size.hover := 180,
                   fill.brush := "red", fillOpacity.brush := 1) %>%
      add_legend(c("fill"), title="") %>%
      add_tooltip(pca_sample_tooltip, "hover") %>%
      pca_lb()$input() %>%
      add_data(pca_selection) %>%
      layer_points(fill := "red", fillOpacity := 1) %>%
      set_options(width=400, height=300) %>%
      bind_shiny("pca31")
  })

  observe({
    sub_pca_rot %>%
      ggvis(~PC2, ~PC3, key := ~id) %>%
      layer_points(fill = ~factor(sub_pca_rot()[[input$group_factor]]),
                   fillOpacity :=0.3, fillOpacity.hover := 0.7,
                   size := 60, size.hover := 180,
                   fill.brush := "red", fillOpacity.brush := 1) %>%
      add_legend(c("fill"), title="") %>%
      add_tooltip(pca_sample_tooltip, "hover") %>%
      pca_lb()$input() %>%
      add_data(pca_selection) %>%
      layer_points(fill := "red", fillOpacity := 1) %>%
      set_options(width=400, height=300) %>%
      bind_shiny("pca23")
  })

  observe({
    sub_pca_prop %>%
      ggvis(~pc, ~proportion, fill := "blue", fillOpacity := 0.3) %>%
      layer_bars() %>%
      set_options(width=400, height=300) %>%
      bind_shiny("pca_proportion")
  })

  observe({
    selected_samples <- sub_pca_rot()[pca_lb()$selected(), ]$sampleid
    if (length(selected_samples > 0)) {
      selected_design <- design_table[design_table$sampleid %in% selected_samples, ]
      output$brushed_design <- renderDataTable(
        options = list(bPaginate=FALSE, bFilter=FALSE, bSearchable=FALSE, bInfo=FALSE),
                       {selected_design})
    } else {
      output$brushed_design <- renderDataTable(
        options = list(bPaginate=FALSE, bFilter=FALSE, bSearchable=FALSE, bInfo=FALSE), {})
    }
  })
  ## end PCA ##################################################################

})
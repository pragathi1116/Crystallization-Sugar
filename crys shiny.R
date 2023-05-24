library(plotly)
library(ggplot2)
library(patchwork)
library(shiny)
server <- function(input, output) {
  # Create a reactiveValues container
  values <- reactiveValues(design = NULL)
  
  # Define a function to update the design object
  updateDesign <- function() {
    factors <- c("Temperature", "StirringRate", "ImpurityLevel", "Concentration")
    levels <- list(
      Temperature = c("Low", "Medium", "High"),
      StirringRate = c(1, 2),
      ImpurityLevel = c(1, 2, 3, 4),
      Concentration = c("Low", "Medium", "High")
    )
    design <- expand.grid(levels)
    for (factor in factors) {
      design[, factor] <- as.numeric(factor(design[, factor], levels = levels[[factor]]))
    }
    design$Temperature <- rep(c(25, 35, 45), length.out = nrow(design))
    design$StirringRate <- rep(c(1, 2), length.out = nrow(design))
    design$ImpurityLevel <- rep(c(1, 2, 3, 4), length.out = nrow(design))
    design$Concentration <- rep(c(5, 10, 15), length.out = nrow(design))
    
    # Calculate CrystalSize based on the formula
    design$CrystalSize <- measure_crystal_size(design$Temperature, design$StirringRate, design$ImpurityLevel, design$Concentration)
    design$GrowthRate <- NA
    
    values$design <- design
  }
  
  # Update the design object initially
  updateDesign()
  
  # Create reactive expressions for the heatmap
  heatmap_plot <- reactive({
    ggplot(values$design, aes(x = Temperature, y = StirringRate, fill = CrystalSize)) +
      geom_tile() +
      scale_fill_gradient(low = "blue", high = "red") +  # Specify color palette
      labs(x = "Temperature", y = "Stirring Rate", fill = "Crystal Size") +
      ggtitle("Crystal Size Heatmap")
  })
  
  # Render the heatmap plot
  output$heatmap_plot <- renderPlot({
    heatmap_plot()
  })
}

ui <- fluidPage(
  titlePanel("Heatmap Example"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("heatmap_plot")
    )
  )
)

shinyApp(ui = ui, server = server)

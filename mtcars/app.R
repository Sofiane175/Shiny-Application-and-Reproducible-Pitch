library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)


df <- iris
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric))

ui <- page_sidebar(
    sidebar = sidebar(
        varSelectInput("xvar", "X variable", df_num, selected = "Sepal.Length"),
        varSelectInput("yvar", "Y variable", df_num, selected = "Petal.Length"),
        checkboxGroupInput(
            "species", "Filter by species",
            choices = unique(df$Species), 
            selected = unique(df$Species)
        ),
        hr(), # Add a horizontal rule
        checkboxInput("by_species", "Show species", TRUE),
        checkboxInput("show_margins", "Show marginal plots", TRUE),
        checkboxInput("smooth", "Add regression line", TRUE),
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("output", 
                     plotOutput("scatter"),
                     ),
        tabPanel("Documentation",
                 p(h4("Edgar Anderson's Iris Data:")),
                 br(),
                 helpText("This application explore the famous (Fisher's or Anderson's) iris data set.
                          The dataset contains the measurements in centimeters of the variables sepal 
                          length and width and petal length and width, respectively, 
                          for 50 flowers from each of 3 species of iris. 
                          The species are Iris setosa, versicolor, and virginica."),
                
                      
        br(),
        helpText("The applications allows to plot a scatter plot of quantitative variables. 
        You can color the plot by species or filter on some of them. 
        The app allows to plot a marginal distribution per variable 
        as well a regression line per species"
        
        
        )
                 )
    )
    
    )
)


server <- function(input, output, session) {
    subsetted <- reactive({
        req(input$species)
        df |> filter(Species %in% input$species)
    })
    
    output$scatter <- renderPlot({
        p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
            theme(legend.position = "bottom"),
            if (input$by_species) aes(color = Species),
            geom_point(),
            if (input$smooth) geom_smooth(method="lm")
        )
        
        if (input$show_margins) {
            margin_type <- if (input$by_species) "density" else "histogram"
            p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                                     size = 8, groupColour = input$by_species, groupFill = input$by_species)
        }
        
        p
    }, res = 100)
}

shinyApp(ui, server)
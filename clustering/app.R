
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dendextend)
library(purrr)
library(plotly)

data("iris")
ui <- dashboardPage(
    
    dashboardHeader(title = "Clustering"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("K-Means",
                     tabName = "kmeans"),
            menuItem("Hierachical Clustering",
                     tabName = "hclust")
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "kmeans",
                    fluidRow(
                        column(3,
                            sliderInput(
                                inputId = "k",
                                label = "Select the number of clusters",
                                min = 2,
                                max = 10,
                                value = 3
                            )
                        ),
                        column(9,
                               plotOutput("km_plot")
                        )
                    ),
                    hr(),
                    fluidRow(
                        column(12,
                               plotlyOutput("elbow_plot")
                        )
                    )
            ),
            
            tabItem(tabName = "hclust",
                    fluidRow(
                        column(4,
                            pickerInput(
                                inputId = "hc_method",
                                label = "Select the linkage method",
                                choices = c("Complete", "Single", "Average", "Centroid", "Median")
                            ),
                            sliderInput(
                                inputId = "cut_k",
                                label = "Select the number of clusters in which you want to cut the dendrogram",
                                min = 1,
                                max = 10,
                                value = 3
                            )
                        ),
                        column(8,
                               plotOutput("hc_plot"),
                               htmlOutput("hc_clust_vector_title"),
                               textOutput("hc_clust_vector")
                        )
                    )
            )
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ### ------------------------- Kmeans ------------
    
    # function to compute total within-cluster sum of square 
    wss <- function(k) {
        kmeans(iris[,1:4], k)$tot.withinss
    }
    
    output$elbow_plot<-renderPlotly({
       
        # Compute and plot wss for k = 1 to k = 15
        k.values <- 1:10
        
        # extract wss for 2-15 clusters
        wss_values <- map_dbl(k.values, wss)
        df <- data.frame(k = k.values, wss = wss_values)
        fig <- plot_ly(df, x = ~k)
        fig <- fig %>% 
            add_trace(y = ~wss, name = 'Within-cluster Sum of Square (WSS)', mode = 'lines+markers') %>% 
            layout(title = 'Within-cluster Sum of Square (WSS) vs. Number of clusters (k)')
        fig
    })
    
    
    km_res <- reactive({
        kmeans(iris[,1:4], input$k)
    })
    
    output$km_plot <- renderPlot({
        cl <- km_res()$cluster
        cluster = factor(cl)
        iris %>% 
            ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = cluster)) +
            geom_jitter() + 
            labs(
                x = "Length of the sepal",
                y = "Width of the sepal",
                fill = "Cluster"
            )
    })
    
    
    ### ------------------------- Hierarchical clustering ------------
    
    hc_method <- reactive({
        tolower(input$hc_method)
    })
    
    cut_k <- reactive({
        input$cut_k
    })
    
    hc_dist <- reactive({
        # Creating the dist object using Euclidian distance
        dist(iris[,1:4], method = 'euclidean')
    })
    
    output$hc_plot <- renderPlot({
        # Run the hiearchical clustring using the selected method
        hc_iris <- hclust(hc_dist(), method = hc_method())
        
        # Create a dendrogram object from the hclust variable
        dend_iris <- as.dendrogram(hc_iris)
        
        # Color brances by cluster formed from the cut at the k given by the user & plot
        dend_iris_col <- color_branches(dend_iris, k = cut_k())
        
        # Plot the dendrogram with clusters colored according to the number of clusters given by the user.
        plot(dend_iris_col)
    })
    
    output$hc_clust_vector_title <- renderText({
        "<b>Clustering Vector:</b>"
    })
    
    output$hc_clust_vector <- renderText({
        # Calculate the assignment vector with k given by the user.
        cutree(hclust(hc_dist(), method = hc_method()), k = cut_k())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

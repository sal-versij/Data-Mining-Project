set.seed(42)
# Graph App(generate and execute algorithms on it)
library(shiny)
library(igraph)

# Define UI with:
# - a sidebar panel with:
#   - a select input for generation algorithm
#   - an input form for generation algorithm parameters
#   - a button to generate the graph
#   - a select input for execution algorithm
#   - an input form for execution algorithm parameters
#   - a button to execute the algorithm
# - a main panel with a plot output
ui <- fluidPage(
  titlePanel("Graph App"),

  sidebarLayout(

    sidebarPanel(
      selectInput("genAlgo", "Generation Algorithm",
        c("Erdos-Renyi", "Barabasi-Albert")),
      conditionalPanel(condition = "input.genAlgo == 'Erdos-Renyi'",
        textInput("n", "Number of vertices", "10"),
        textInput("p", "Probability", "0.5")),
      conditionalPanel(condition = "input.genAlgo == 'Barabasi-Albert'",
        textInput("n", "Number of vertices", "10"),
        textInput("m", "Number of edges to attach", "3")),
      actionButton("genButton", "Generate"),
      selectInput("execAlgo", "Execution Algorithm",
        c("Breadth-First Search", "Dijkstra")),
      conditionalPanel(condition = "input.execAlgo == 'Breadth-First Search'",
        textInput("source", "Source vertex", "1")),
      conditionalPanel(condition = "input.execAlgo == 'Dijkstra'",
        textInput("source", "Source vertex", "1"),
        textInput("target", "Target vertex", "10")),
      actionButton("execButton", "Execute")
    ),

    mainPanel(
      plotOutput("graphPlot")
    )
  )
)

renderGraph <- function(output, g) {
  output$graphPlot <- renderPlot({
    print("generating graph...")
    if (!is.null(g)) {
      plot(g, vertex.label = NA)
    }
  })
}

# Define server logic
server <- function(input, output, session) {
  graph <- reactiveVal(NULL)
  out <- reactiveVal(NULL)

  # Generate the graph
  observeEvent(input$genButton, {
    if (input$genAlgo == "Erdos-Renyi") {
      g <- erdos.renyi.game(
        n = as.numeric(input$n),
        p = as.numeric(input$p)
      )
    } else if (input$genAlgo == "Barabasi-Albert") {
      g <- barabasi.game(
        n = as.numeric(input$n),
        m = as.numeric(input$m)
      )
    }

    renderGraph(output, g)
    graph(g)
  })

  # Execute the algorithm
  observeEvent(input$execButton, {
    g <- graph()

    if (input$execAlgo == "Breadth-First Search") {
      out <- get.shortest.paths(g, from = as.numeric(input$source))
    } else if (input$execAlgo == "Dijkstra") {
      out <- get.shortest.paths(g, from = as.numeric(input$source), to = as.numeric(input$target))
    }

    print(out)
  })

  # render default with null
  renderGraph(output, NULL)
}

# Run the application
shinyApp(ui = ui, server = server);

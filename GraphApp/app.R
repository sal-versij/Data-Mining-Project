set.seed(42)
library(shiny)
library(shinyalert)
library(igraph)

ui <- fluidPage(
  titlePanel("Graph App"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        selectInput(
          "genAlgo", "Generation Algorithm",
          c(
            "Erdos-Renyi G(n,p)", "Erdos-Renyi G(n,m)",
            "Watts-Strogatz", "Barabasi-Albert"
          )
        ),
        conditionalPanel(
          condition = "input.genAlgo == 'Erdos-Renyi G(n,p)'",
          numericInput("n", "Number of vertices", 10,
            min = 1, max = 1000
          ),
          numericInput("p", "Probability", 0.5,
            min = 0, max = 1, step = 0.01
          )
        ),
        conditionalPanel(
          condition = "input.genAlgo == 'Erdos-Renyi G(n,m)'",
          numericInput("n", "Number of vertices", 10,
            min = 1, max = 1000
          ),
          numericInput("m", "Number of edges", 5,
            min = 0, max = 1000
          )
        ),
        conditionalPanel(
          condition = "input.genAlgo == 'Watts-Strogatz'",
          numericInput("dim", "Dimensionality starting lattice", 2,
            min = 1, max = 3
          ),
          numericInput("size", "Lattice size along each dimension", 5,
            min = 1, max = 30
          ),
          numericInput("nei", "Neighborhood connectivity", 1,
            min = 1, max = 30
          ),
          numericInput("p", "Rewiring probability", 0.1,
            min = 0, max = 1, step = 0.01
          )
        ),
        conditionalPanel(
          condition = "input.genAlgo == 'Barabasi-Albert'",
          numericInput("n", "Number of vertices", 10,
            min = 1, max = 1000
          ),
          numericInput("power", "Power law exponent", 1,
            min = 1, max = 10
          ),
          numericInput("m", "Number of edges to attach", 3,
            min = 0, max = 1000
          )
        ),
        actionButton("genButton", "Generate"),
      ),
      tags$hr(style = "border-color: black;"),
      conditionalPanel(
        condition = "output.graphPlot",
        tags$div(
          selectInput(
            "execAlgo", "Execution Algorithm",
            c("Breadth-First Search", "Dijkstra")
          ),
          conditionalPanel(
            condition = "input.execAlgo == 'Breadth-First Search'",
            numericInput("source", "Source vertex", 1)
          ),
          conditionalPanel(
            condition = "input.execAlgo == 'Dijkstra'",
            numericInput("source", "Source vertex", 1),
            numericInput("target", "Target vertex", 10)
          ),
          actionButton("execButton", "Execute")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Graph",
          tags$div(
            selectInput(
              "layout", "Layout",
              c(
                "Auto", "Circle", "Components", "Grid",
                "Sphere", "Star", "Nicely"
              )
            ),
          ),
          plotOutput("graphPlot")
        ),
        tabPanel(
          "Graph infos",
          tags$dl(
            tags$dt("Number of verticies"),
            tags$dd(textOutput("vcount")),
            tags$dt("Number of edges"),
            tags$dd(textOutput("ecount")),
            tags$dt("Edge density"),
            tags$dd(textOutput("edge_density")),
            tags$dt("Local clustering coeficient"),
            tags$dd(textOutput("local_clustering")),
            tags$dt("Global clustering coeficient"),
            tags$dd(textOutput("global_clustering")),
            tags$dt("Diameter"),
            tags$dd(textOutput("diameter")),
            tags$dt("Mean distance"),
            tags$dd(textOutput("mean_distance")),
            tags$dt("Degree distribution"),
            tags$dd(plotOutput("degree", width = "50%")),
            tags$dt("Degree centrality"),
            tags$dd(textOutput("degee_centrality")),
            tags$dt("Closeness centrality"),
            tags$dd(textOutput("closeness_centrality")),
            tags$dt("Betweenness centrality"),
            tags$dd(textOutput("betweenness_centrality")),
          )
        ),
        tabPanel(
          "Results",
          tableOutput("result")
        )
      )
    )
  )
)

render_graph <- function(output, g, l) {
  output$graphPlot <- renderPlot({
    print("generating graph...")
    if (!is.null(g)) {
      plot(g, layout = l)
    }
  })
  output$vcount <- renderText({
    if (!is.null(g)) {
      vcount(g)
    } else {
      "N/A"
    }
  })
  output$ecount <- renderText({
    if (!is.null(g)) {
      ecount(g)
    } else {
      "N/A"
    }
  })
  output$edge_density <- renderText({
    if (!is.null(g)) {
      edge_density(g)
    } else {
      "N/A"
    }
  })
  output$local_clustering <- renderText({
    if (!is.null(g)) {
      if (is_directed(g)) {
        triad_census(g)
      } else {
        transitivity(g, type = "local")
      }
    } else {
      "N/A"
    }
  })
  output$global_clustering <- renderText({
    if (!is.null(g)) {
      transitivity(g, type = "global")
    } else {
      "N/A"
    }
  })
  output$diameter <- renderText({
    if (!is.null(g)) {
      diameter(g, directed = is_directed(g))
    } else {
      "N/A"
    }
  })
  output$mean_distance <- renderText({
    if (!is.null(g)) {
      mean_distance(g, directed = is_directed(g))
    } else {
      "N/A"
    }
  })
  output$degree <- renderPlot({
    if (!is.null(g)) {
      hist(degree(g), main = "")
    }
  })
  output$degee_centrality <- renderText({
    if (!is.null(g)) {
      result <- centr_degree(g, mode = "in", normalized = TRUE)
      sprintf("%.2f/%.2f", result$centralization, result$theoretical_max)
    } else {
      "N/A"
    }
  })
  output$closeness_centrality <- renderText({
    if (!is.null(g)) {
      result <- centr_clo(g, mode = "all", normalized = TRUE)
      sprintf("%.2f/%.2f", result$centralization, result$theoretical_max)
    } else {
      "N/A"
    }
  })
  output$betweenness_centrality <- renderText({
    if (!is.null(g)) {
      result <- centr_betw(g, directed = is_directed(g), normalized = TRUE)
      sprintf("%.2f/%.2f", result$centralization, result$theoretical_max)
    } else {
      "N/A"
    }
  })
}

choose_layout <- function(g, layout) {
  switch(layout,
    "Auto" = NULL,
    "Circle" = layout_in_circle(g),
    "Components" = layout_components(g),
    "Grid" = layout_on_grid(g),
    "Sphere" = layout_on_sphere(g),
    "Star" = layout_as_star(g),
    "Nicely" = layout_nicely(g),
    "Ranomly" = layout_randomly(g)
  )
}

server <- function(input, output, session) {
  # Generate the graph
  observeEvent(input$genButton, {
    tryCatch(
      {
        if (input$genAlgo == "Erdos-Renyi G(n,p)") {
          session$userData$graph <- sample_gnp(
            n = input$n,
            p = input$p
          )
        } else if (input$genAlgo == "Erdos-Renyi G(n,m)") {
          session$userData$graph <- sample_gnm(
            n = input$n,
            m = input$m
          )
        } else if (input$genAlgo == "Watts-Strogatz") {
          session$userData$graph <- sample_smallworld(
            dim = input$dim,
            size = input$size,
            nei = input$nei,
            p = input$p
          )
        } else if (input$genAlgo == "Barabasi-Albert") {
          session$userData$graph <- sample_pa(
            n = input$n,
            power = input$power,
            m = input$m
          )
        }

        render_graph(
          output, session$userData$graph,
          choose_layout(session$userData$graph, input$layout)
        )
      },
      warning = function(w) {
        shinyalert(title = "Warning", text = w$message, type = "warning")
      },
      error = function(e) {
        shinyalert(title = "Error", text = e$message, type = "error")
      }
    )
  })

  # Execute the algorithm
  observeEvent(input$execButton, {
    if (input$execAlgo == "Breadth-First Search") {
      data <- get.shortest.paths(session$userData$graph,
        from = input$source
      )
      print(data)
      # format data$vpath vector as a string
      output$result <- renderTable({
        data.frame(
          vertex = seq_along(data$vpath),
          path = data$vpath
        )
      })
    } else if (input$execAlgo == "Dijkstra") {
      data <- get.shortest.paths(session$userData$graph,
        from = input$source, to = input$target
      )
      print(data)
      # format data$vpath vector as a string
      output$result <- renderTable({
        data.frame(
          vertex = seq_along(data$vpath),
          path = data$vpath
        )
      })
    }
  })

  # Rerender with layout
  observeEvent(input$layout, {
    render_graph(
      output, session$userData$graph,
      choose_layout(session$userData$graph, input$layout)
    )
  })

  # render default with null
  render_graph(output, NULL)
}

shinyApp(ui = ui, server = server)

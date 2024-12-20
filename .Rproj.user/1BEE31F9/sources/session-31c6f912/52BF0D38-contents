library(shiny)
library(xml2)
library(httr)
library(igraph)
library(dplyr)
library(tibble)
library(shinycssloaders)
library(rsconnect)

# Create a function that handles the plotting logic based on a data frame
generate_plot_from_data <- function(dat_citation_network, PMID_CC) {
  g <- graph_from_data_frame(dat_citation_network, directed = F)
  vertex_colors <- rep("skyblue", vcount(g))
  highlight_indices <- match(PMID_CC, V(g)$name)
  vertex_colors[highlight_indices] <- "red"
  vertex_sizes <- rep(2, vcount(g))
  vertex_sizes[highlight_indices] <- 5
  l <- layout_with_fr(g)
  par(mar = c(0, 0, 0, 0) + 0.2)
  plot(g, layout = l, vertex.label = NA, vertex.size = vertex_sizes, vertex.label.dist = -1, edge.arrow.size = 0.2, edge.curved = F, vertex.color = vertex_colors)
}

# Define UI
ui <- fluidPage(
  titlePanel(
    h1("Citation network", align = "center")
  ),
  textInput("pmid_input", "Enter PMID", value = "28099850,25873307,35201886"),
  actionButton("go", "Run"),
  withSpinner(plotOutput("network_plot"))
)

# Define server logic
server <- function(input, output, session) {
  
  output$network_plot <- renderPlot({
    if (is.null(input$go) || input$go == 0) {
      PMID_CC <- c("28099850","25873307","35201886")
      default_data <- readRDS("dat_citation_network.RDS")
      generate_plot_from_data(default_data, PMID_CC)
      
    } else {
      # When the 'Run' button is clicked, generate a new plot
      isolate({
        PMID_CC <- trimws(strsplit(input$pmid_input, ",")[[1]])
        dat_citation_cc <- vector(mode = "list", length = length(PMID_CC))
        
        for (i in seq_along(PMID_CC)) {
          Sys.sleep(0.5)
          query_url <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=', PMID_CC[i])
          raw_data <- GET(query_url)
          parsed_data <- xml2::read_xml(rawToChar(raw_data$content))
          
          citing_pmids <- xml2::xml_find_all(parsed_data, "//LinkSet/LinkSetDb/Link/Id") %>% xml2::xml_text()
          dat_citation_cc[[i]] <- tibble(From = PMID_CC[i], To = citing_pmids)
        }
        
        dat_citation_cc_flat <- do.call(rbind, dat_citation_cc)
        vector_citation <- dat_citation_cc_flat %>% select(To) %>% unlist()
        dat_citation_cc_by <- vector(mode = "list", length = length(vector_citation))
        
        for (i in seq_along(vector_citation)) {
          Sys.sleep(0.5)
          query_url <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=', vector_citation[i])
          raw_data <- GET(query_url)
          parsed_data <- xml2::read_xml(rawToChar(raw_data$content))
          
          citing_pmids <- xml2::xml_find_all(parsed_data, "//LinkSet/LinkSetDb/Link/Id") %>% xml2::xml_text()
          if (length(citing_pmids) == 0) {
            next
          }
          dat_citation_cc_by[[i]] <- tibble(From = vector_citation[i], To = citing_pmids)
        }
        
        
        dat_citation_network1 <- do.call(rbind, dat_citation_cc_by) %>% bind_rows(dat_citation_cc_flat, .) %>% distinct()
        generate_plot_from_data(dat_citation_network1, PMID_CC)
      })
    }
  })
  
  # This is necessary to make sure the renderPlot gets invalidated when 'go' is clicked
  observe({
    input$go
  })
}

# Run the application
shinyApp(ui = ui, server = server)

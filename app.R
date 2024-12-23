library(shiny)
library(xml2)
library(httr)
library(igraph)
library(dplyr)
library(tibble)
library(visNetwork)
library(shinycssloaders)
library(XML)
library(colourpicker)
library(rentrez)
library(shinyhelper)
library(shinythemes)
library(shinyWidgets)

# 定义获取引用数据的函数
get_citation_data <- function(pmids, depth = 2, relation_type = "pubmed_pubmed_citedin") {
  all_citations <- list()
  current_level_pmids <- pmids
  
  # 逐层获取数据
  for(level in 1:depth) {
    level_citations <- list()
    message(sprintf("Fetching relations for level %d...", level))
    
    for(i in seq_along(current_level_pmids)) {
      Sys.sleep(0.1)
      
      tryCatch({
        # 获取关系数据
        relation_data <- entrez_link(
          dbfrom = "pubmed",
          db = "pubmed",
          id = current_level_pmids[i]
        )
        
        # 获取指定类型的相关文献
        related_pmids <- relation_data$links[[relation_type]]
        
        if(length(related_pmids) > 0) {
          level_citations[[i]] <- tibble(
            From = current_level_pmids[i],
            To = related_pmids
          )
        }
      }, error = function(e) {
        message(paste("Error fetching data for PMID", current_level_pmids[i], ":", e$message))
      })
    }
    
    # 合并当前层级的数据
    if(length(level_citations) > 0) {
      current_level_data <- bind_rows(level_citations)
      all_citations[[level]] <- current_level_data
      
      # 更新下一层要处理的PMID
      current_level_pmids <- unique(current_level_data$To)
    } else {
      break
    }
  }
  
  # 合并所有层级的数据
  dat_citation_network <- bind_rows(all_citations) %>%
    distinct()
  
  return(dat_citation_network)
}

# 添加获取文献信息的新函数
get_pubmed_info <- function(pmid) {
  Sys.sleep(0.5)  # 遵守NCBI API访问限制
  
  # 构建API请求URL
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
                pmid, "&retmode=xml")
  
  # 获取数据
  xml_data <- try({
    xml_content <- GET(url)
    xmlParse(rawToChar(xml_content$content))
  }, silent = TRUE)
  
  if (inherits(xml_data, "try-error")) {
    return(data.frame(
      pmid = pmid,
      title = "Not found",
      authors = "Not found",
      journal = "Not found",
      year = "Not found"
    ))
  }
  
  # 提取信息
  title <- xpathSApply(xml_data, "//ArticleTitle", xmlValue)
  title <- if(length(title) > 0) title[1] else "No title available"
  
  # 取作者列表
  authors <- xpathSApply(xml_data, "//Author/LastName", xmlValue)
  first_names <- xpathSApply(xml_data, "//Author/ForeName", xmlValue)
  if(length(authors) > 0 && length(first_names) > 0) {
    authors <- paste(paste(first_names, authors), collapse = "; ")
    if(nchar(authors) > 100) {
      # 如果作者太多，只显示前三个作者加et al
      author_list <- strsplit(authors, "; ")[[1]]
      authors <- paste(paste(author_list[1:min(3, length(author_list))], collapse = "; "), "et al.")
    }
  } else {
    authors <- "No authors available"
  }
  
  # 获取期刊信息
  journal <- xpathSApply(xml_data, "//Journal/Title", xmlValue)
  journal <- if(length(journal) > 0) journal[1] else "No journal available"
  
  # 获取年份
  year <- xpathSApply(xml_data, "//PubDate/Year", xmlValue)
  year <- if(length(year) > 0) year[1] else "No year available"
  
  # 获取DOI 
  doi <- xpathSApply(xml_data, "//ELocationID[@EIdType='doi']", xmlValue) 
  doi <- paste0("https://doi.org/", doi)
  doi <- if(length(doi) > 0) doi[1] else "No DOI available"
  
  # 获取摘要
  abs <- xpathSApply(xml_data, "//AbstractText", xmlValue)
  abs <- if(length(abs) > 0) abs[1] else "No year available"
  
  return(data.frame(
    pmid = pmid,
    title = title,
    authors = authors,
    journal = journal,
    year = year,
    doi = doi,
    abs = abs,
    stringsAsFactors = FALSE
  ))
}

# 定义绘图函数
generate_plot_from_data <- function(dat_citation_network, PMID_CC, degree_threshold = 5, 
                                  selected_color = "red", unselected_color = "skyblue") {
  tryCatch({
    # 数据验证
    if(is.null(dat_citation_network) || nrow(dat_citation_network) == 0) {
      stop("Invalid citation network data")
    }
    
    if(is.null(PMID_CC) || length(PMID_CC) == 0) {
      stop("Invalid PMID input")
    }
    
    # 创建图形对象
    g <- graph_from_data_frame(dat_citation_network, directed = F)
    
    if(vcount(g) == 0) {
      stop("No nodes in the network")
    }
    
    # 打印调试信息
    print(paste("Number of nodes:", vcount(g)))
    print(paste("Number of edges:", ecount(g)))
    
    # 计算度
    node_degrees <- degree(g)
    
    # 获取所有节点的文献信息
    all_pmids <- unique(c(dat_citation_network$From, dat_citation_network$To))
    
    # 使用 withProgress 显示进度
    pub_info <- shiny::withProgress(
      message = 'Fetching publication details',
      detail = 'This may take a while...',
      value = 0,
      {
        result <- list()
        total <- length(all_pmids)
        
        for(i in seq_along(all_pmids)) {
          incProgress(1/total, 
                     detail = sprintf("Fetching %d of %d", i, total))
          
          result[[i]] <- tryCatch({
            get_pubmed_info(all_pmids[i])
          }, error = function(e) {
            # 如果获取失败，返回默认信息
            data.frame(
              pmid = all_pmids[i],
              title = "Information not available",
              authors = "N/A",
              journal = "N/A",
              year = "N/A",
              stringsAsFactors = FALSE
            )
          })
        }
        result
      }
    )
    
    pub_info_df <- do.call(rbind, pub_info)
    
    # 计算节点大小
    size_scale <- 10
    node_sizes <- size_scale * (1 + log(node_degrees + 1))
    node_sizes[V(g)$name %in% PMID_CC] <- max(node_sizes) * 1.2
    
    # 准备节点数据
    nodes <- data.frame(
      id = V(g)$name,
      label = sapply(V(g)$name, function(pmid) {
        if(node_degrees[which(V(g)$name == pmid)] >= degree_threshold | pmid %in% PMID_CC) {
          info <- pub_info_df[pub_info_df$pmid == pmid, ]
          first_author <- strsplit(info$authors, ";")[[1]][1]
          first_author <- if(first_author == "N/A") "Unknown" else strsplit(first_author, " ")[[1]][2]
          journal_abbrev <- substr(info$journal, 1, 3)
          paste0(first_author, ", ", journal_abbrev, ", ", info$year)
        } else {
          ""
        }
      }),
      title = sapply(V(g)$name, function(pmid) {
        info <- pub_info_df[pub_info_df$pmid == pmid, ]
        paste0(
          "<div style='word-wrap: break-word; white-space: normal;'>",
          "PMID: ", pmid,
          "<br>Title: ", info$title,
          "<br>Authors: ", info$authors,
          "<br>Journal: ", info$journal,
          "<br>Year: ", info$year,
          # "<br>Citations: ", node_degrees[which(V(g)$name == pmid)],
          "<br>Abstract: ", info$abs,
          "<br>DOI: <a href='https://doi.org/", info$doi, "' target='_blank'>", info$doi, "</a>",
          "</div>"
        )
      }),
      
      degree = node_degrees,
      color = ifelse(V(g)$name %in% PMID_CC, selected_color, unselected_color),
      size = node_sizes
    )
    
    # 修改边的数据框，添加箭头属性
    edges <- data.frame(
      from = dat_citation_network$From,
      to = dat_citation_network$To,
      arrows = "to",           # 添加箭头
      color = "#808080",      # 箭头颜色设为灰色
      smooth = list(          # 添加平滑曲线
        enabled = TRUE,
        type = "curvedCW",
        roundness = 0.2
      )
    )
    
    # 打印调试信息
    print(paste("Nodes count:", nrow(nodes)))
    print(paste("Edges count:", nrow(edges)))
    
    # 建网络图
    result <- visNetwork(nodes, edges) %>%
      visPhysics(
        solver = "barnesHut",
        stabilization = list(
          enabled = TRUE,
          iterations = 100,
          fit = TRUE
        ),
        barnesHut = list(
          gravitationalConstant = -2000,
          centralGravity = 0.3,
          springLength = 150,
          springConstant = 0.04,
          damping = 0.09,
          avoidOverlap = 0.1
        )
      ) %>%
      visLayout(
        randomSeed = 123,
        improvedLayout = TRUE
      ) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE, 
          degree = 1,
          hover = TRUE
        ),
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = TRUE
      ) %>%
      visEdges(
        arrows = list(
          to = list(enabled = TRUE, scaleFactor = 0.5)  # 调整箭头大小
        ),
        smooth = list(
          enabled = TRUE,
          type = "curvedCW"   # 使用顺时针曲线
        ),
        color = list(
          color = "#808080",
          highlight = "#333333"  # 高亮时的颜色
        )
      )
    
    # 打印成功信息
    print("Network visualization object created successfully")
    return(result)
    
  }, error = function(e) {
    print(paste("Error in generate_plot_from_data:", e$message))
    stop(e$message)
  })
}

# 添加新的函数用于DOI转PMID
doi_to_pmid <- function(doi) {
  Sys.sleep(0.5)  # 遵守NCBI API访问限制
  
  # 构建API请求URL
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=",
                doi, "[doi]&retmode=xml")
  
  # 获取数据
  xml_data <- try({
    xml_content <- GET(url)
    xmlParse(rawToChar(xml_content$content))
  }, silent = TRUE)
  
  if (inherits(xml_data, "try-error")) {
    stop("Error fetching PMID for DOI: ", doi)
  }
  
  # 提取PMID
  pmid <- xpathSApply(xml_data, "//Id", xmlValue)
  
  if (length(pmid) == 0) {
    stop("No PMID found for DOI: ", doi)
  }
  
  return(pmid[1])
}

# 修改处理输入的函数
process_input <- function(input_str) {
  # 分割输入
  inputs <- unlist(strsplit(input_str, "[,;\n]"))
  
  pmids <- character()
  
  for(i in seq_along(inputs)) {
    input <- gsub("\\s+", "", inputs[i])  # 移除空格
    
    if(input == "") next
    
    if(grepl("^\\d+$", input)) {  # 如果是纯数字，认为是 PMID
      pmids <- c(pmids, input)
    } else if(grepl("^10\\.", input)) {  # 如果以 10. 开头，认为是 DOI
      # 使用 rentrez 搜索 DOI
      tryCatch({
        search_result <- entrez_search(
          db = "pubmed",
          term = paste0(input, "[doi]"),
          retmax = 1
        )
        if(length(search_result$ids) > 0) {
          pmids <- c(pmids, search_result$ids)
        }
      }, error = function(e) {
        message(paste("Error processing DOI:", input, "-", e$message))
      })
    }
  }
  
  return(unique(pmids))
}

# 定义UI
ui <- fluidPage(
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      h3("Citation Network"),
      br(),
      textAreaInput("pmid_input", 
                   "Enter PMIDs or DOIs", 
                   value = "",
                   height = "135px",
                   placeholder = "Enter PMIDs or DOIs (separated by commas, semicolons, or new lines)\nExample:\n37556327\n10.1186/s13619-022-00140-9"),
      # 添加关系类型选择
      selectInput("relation_type",
                 "Select relation type:",
                 choices = list(
                   "Papers citing this (Citations)" = "pubmed_pubmed_citedin",
                   "Related papers" = "pubmed_pubmed",
                   "Most related papers (top 5)" = "pubmed_pubmed_five",
                   "Related reviews" = "pubmed_pubmed_reviews",
                   "Most related reviews (top 5)" = "pubmed_pubmed_reviews_five",
                   "Combined related papers" = "pubmed_pubmed_combined"
                 ),
                 selected = "pubmed_pubmed_citedin") %>%
        helper(type = "inline", 
               title = "Relation Type Help",
               content = c("Papers citing this (Citations): Shows papers that cite the selected paper.",
                           "Related papers: Shows papers that are related in terms of content.",
                           "Most related papers (top 5): Shows the top 5 most relevant papers.",
                           "Related reviews: Shows review articles related to the selected paper.",
                           "Most related reviews (top 5): Shows the top 5 most relevant review articles.",
                           "Combined related papers: A combination of various related papers.")),
      # 添加网络深度选择
      uiOutput("depth_selector"),  # 使用动态UI
      selectInput("degree_threshold", 
                 "Degree threshold for label display:", 
                 choices = setNames(1:20, paste(1:20, "connections")),
                 selected = 5),
      colourInput("selected_color", 
                 "Color for selected nodes:", 
                 width = "45%",
                 value = "red"),
      colourInput("unselected_color", 
                 "Color for other nodes:", 
                 width = "45%",
                 value = "skyblue"),
      hr(),
      actionBttn(
        inputId = "example",
        label = "Example",
        style = "unite",
        color = "danger"
      ),
      hr(),
      actionBttn(
        inputId = "go",
        label = "Generate Network",
        style = "jelly",
        color = "success",
        icon = icon("arrow-rotate-right"),
        block = TRUE,
        size = "sm"),
      width = 3
    ),
    mainPanel(
      withSpinner(visNetworkOutput("network_plot", height = "800px"), type = 6, color = rgb(134/255,157/255,181/255), size = 1),
      width = 9
    )
  )
)

# 定义server
server <- function(input, output, session) {
  # shinyhelper用
  observe_helpers(withMathJax = TRUE, help_dir = "./") # help_dir = "./"作为type=markdown时使用，现在用不到
  
  observeEvent(input$example, {
    updateTextAreaInput(session, "pmid_input", value = "37556327
10.1186/s13619-022-00140-9")
  })
  
  output$depth_selector <- renderUI({
    if (input$relation_type == "pubmed_pubmed_citedin") {
      # 如果选择“Papers citing this (Citations)”，默认为2层
      selectInput("citation_depth", 
                  "Network depth:",
                  choices = list(
                    "1 layer" = 1,
                    "2 layers" = 2,
                    "3 layers" = 3
                  ),
                  selected = 2)
    } else if (input$relation_type == "pubmed_pubmed") {
      # 如果选择“Related papers”，只允许选择“1 layer”
      selectInput("citation_depth", 
                  "Network depth:",
                  choices = list("1 layer" = 1),
                  selected = 1)
    } else {
      # 否则允许选择所有深度，默认为1层
      selectInput("citation_depth", 
                  "Network depth:",
                  choices = list(
                    "1 layer" = 1,
                    "2 layers" = 2,
                    "3 layers" = 3
                  ),
                  selected = 1)
    }
  })
  
  output$network_plot <- renderVisNetwork({
    req(input$go)
    
    isolate({
      tryCatch({
        # 处理输入
        pmids <- process_input(input$pmid_input)
        if(length(pmids) == 0) stop("No valid PMIDs or DOIs found")
        
        # 获取关系数据
        citation_data <- withProgress(
          message = 'Step 1: Fetching data',
          value = 0.2,
          {
            result <- get_citation_data(
              pmids, 
              depth = as.numeric(input$citation_depth),
              relation_type = input$relation_type
            )
            if(nrow(result) == 0) stop("No relations found")
            incProgress(0.4, detail = paste("Found", nrow(result), "connections"))
            showNotification(paste("Found", nrow(result), "connections"), type = "message")
            result
          }
        )
        
        # 生成网络图
        withProgress(
          message = 'Step 2: Creating network visualization',
          value = 0.6,
          {
            result <- generate_plot_from_data(
              citation_data, 
              pmids, 
              as.numeric(input$degree_threshold),
              selected_color = input$selected_color,
              unselected_color = input$unselected_color
            )
            if(is.null(result)) stop("Failed to create network visualization")
            incProgress(1, detail = "Visualization complete")
            result
          }
        )
      }, error = function(e) {
        showNotification(
          paste("Error:", e$message), 
          type = "error",
          duration = NULL
        )
        # 返回错误提示网络图
        visNetwork(
          nodes = data.frame(
            id = 1,
            label = paste("Error:", e$message),
            color = "red"
          ),
          edges = data.frame(from = integer(0), to = integer(0))
        ) %>%
          visLayout(randomSeed = 123)
      })
    })
  })
}

# 运行应用
shinyApp(ui = ui, server = server)

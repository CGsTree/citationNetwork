library(shiny)
library(httr)
library(igraph)
library(dplyr)
library(tibble)
library(visNetwork)
library(shinycssloaders)
library(xml2)
library(colourpicker)
library(shinyhelper)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(jsonlite)
library(DT)

# 鉴于opencitations速度不如crossref，且数据不全（比如sk文章，检索不到sk引用的文献，引用sk的文献也只有一篇）
# 综合来看
# 反向网络图建设使用crossref的api，（注：crossref没有找到提供正向api的查询，但是有`is-referenced-by-count`显示被引用数量）
# ncbi在部分文献中也有引用文献的信息，比如ZN的NP文章，但是SK的文章就没有。还是crossref全一些。
# 正向网络查找使用ncbi的api
# 文献信息还得靠ncbi获得，crossref没有这个内容

# 注册获取ncbi api key
# https://support.nlm.nih.gov/kbArticle/?pn=KA-05318
# https://support.nlm.nih.gov/kbArticle/?pn=KA-05317
# NCBI API访问限制一秒10次，但实际上使用中跑不满这个速度，就不限制了。可能使用本应用的人太多的话会达到这个限制。

ncbi_api_key = ""
# ncbi_api_key = ""

# 迭代获取输入pmid对应文献的引用文献
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
        # url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&api_key=", ncbi_api_key, "&linkname=", relation_type, "&id=", current_level_pmids[i])
        base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed"
        api_key_part <- if(nchar(ncbi_api_key) > 0) paste0("&api_key=", ncbi_api_key) else ""
        url <- paste0(base_url, 
                      api_key_part,
                      "&linkname=", relation_type,
                      "&id=", current_level_pmids[i])
        
        
        xml_content <- GET(url)
        # xml_data <- xmlParse(rawToChar(xml_content$content))
        xml_data <- read_xml(rawToChar(xml_content$content))
        
        # 提取 LinkSetDb 下的 DOI ID
        # related_pmids <- xpathSApply(xml_data, "//LinkSetDb/Link/Id", xmlValue)
        related_pmids <- xml_text(xml_find_all(xml_data, "//LinkSetDb/Link/Id"))
        
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
  # Sys.sleep(0.1)  
  
  # 构建API请求URL
  # url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&api_key=", ncbi_api_key, "&id=",
  #               pmid, "&retmode=xml")
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed"
  api_key_part <- if(nchar(ncbi_api_key) > 0) paste0("&api_key=", ncbi_api_key) else ""
  url <- paste0(base_url,
                api_key_part,
                "&id=", pmid,
                "&retmode=xml")

  
  # 获取数据
  xml_data <- try({
    xml_content <- GET(url)
    # xmlParse(rawToChar(xml_content$content))
    read_xml(rawToChar(xml_content$content))
  }, silent = TRUE)
  
  if (inherits(xml_data, "try-error")) {
    return(data.frame(
      pmid = pmid,
      title = "Not found",
      authors = "Not found",
      journal = "Not found",
      year = "Not found",
      keywords = "Not found",
      abs = "Not found",
      citations = "Not found",
      doi = "Not found"
    ))
  }

  
  # 提取信息
  # title <- xpathSApply(xml_data, "//ArticleTitle", xmlValue)
  title <- xml_text(xml_find_all(xml_data, "//ArticleTitle"))
  
  title <- if(length(title) > 0) title[1] else "No title available"

    # 取作者列表
  # authors <- xpathSApply(xml_data, "//Author/LastName", xmlValue)
  authors <- xml_text(xml_find_all(xml_data, "//Author/LastName"))
  
  # first_names <- xpathSApply(xml_data, "//Author/ForeName", xmlValue)
  first_names <- xml_text(xml_find_all(xml_data, "//Author/ForeName"))
  
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
  # journal <- xpathSApply(xml_data, "//Journal/Title", xmlValue)
  journal <- xml_text(xml_find_all(xml_data, "//Journal/Title"))
  
  journal <- if(length(journal) > 0) journal[1] else "No journal available"
  
  # 获取年份
  # year <- xpathSApply(xml_data, "//PubDate/Year", xmlValue)
  year <- xml_text(xml_find_all(xml_data, "//PubDate/Year"))
  
  year <- if(length(year) > 0) year[1] else "No year available"
  
  # 获取DOI 
  # doi <- xpathSApply(xml_data, "//ELocationID[@EIdType='doi']", xmlValue) 
  doi <- xml_text(xml_find_all(xml_data, "//ELocationID[@EIdType='doi']"))
  
  doi <- paste0("https://doi.org/", doi)
  doi <- if(length(doi) > 0) doi[1] else "No DOI available"
  
  # 获取摘要
  # abs <- xpathSApply(xml_data, "//AbstractText", xmlValue)
  abs <- xml_text(xml_find_all(xml_data, "//AbstractText"))
  abs <- if(length(abs) > 0) abs[1] else "No abstract available"
  
  # 获取关键词
  # 直接定位到所有 Keyword 节点
  keywords <- xml_text(xml_find_all(xml_data, "//Keyword"))
  # 用分号和空格连接关键词
  keywords <- if(length(keywords) > 0) paste(keywords, collapse = "; ") else "No keywords available"
  
  # 获取被引用次数
  # citations <- get_citation_count(pmid)
  citations <- NA
  
  return(data.frame(
    pmid = pmid,
    title = title,
    keywords = keywords,
    authors = authors,
    journal = journal,
    year = year,
    doi = doi,
    abs = abs,
    citations = citations,
    stringsAsFactors = FALSE
  ))
}


# 定义绘图函数
generate_plot_from_data <- function(dat_citation_network, PMID_CC, degree_threshold = 3, 
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
    
    # # 使用 withProgress 显示进度
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
              keywords = "N/A",
              authors = "N/A",
              journal = "N/A",
              year = "N/A",
              doi = "N/A",
              abs = "N/A",
              citations = "N/A",
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
          "<b>PMID</b>: ", pmid,
          "<br><b>Title</b>: ", info$title,
          "<br><b>Authors</b>: ", info$authors,
          "<br><b>Journal</b>: ", info$journal,
          "<br><b>Year</b>: ", info$year,
          "<br><b>Keywords</b>: ", info$keywords,
          "<br><b>Abstract</b>: ", info$abs,
          "<br><b>Citation count</b>: ", "To improve speed, turn off display.",
          "<br><b>DOI</b>: <a href='https://doi.org/", info$doi, "' target='_blank'>", info$doi, "</a>",
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

# 定义函数来从 DOI 获取 PubMed ID
get_pmid_from_doi <- function(doi) {
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  # params <- list(
  #   db = "pubmed",
  #   api_key = ncbi_api_key,
  #   term = paste0(doi, "[doi]"),
  #   retmode = "xml"
  # )
  
  # 基础参数
  params <- list(
    db = "pubmed",
    term = paste0(doi, "[doi]"),
    retmode = "xml"
  )
  
  # 如果 api_key 不为空，则添加到参数列表中
  if (nchar(ncbi_api_key) > 0) {
    params$api_key <- ncbi_api_key
  }
  
  # https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&api_key=07f8d4bf9f9115e0a97cd01fd927778b2f09&term=10.1016/j.celrep.2023.112966[doi]&retmode=xml
  response <- GET(url, query = params)
  
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    doc <- read_xml(content)
    
    # 提取 PubMed ID
    pmid <- xml_text(xml_find_first(doc, "//IdList/Id"))
    
    if (!is.null(pmid)) {
      return(pmid)
    } else {
      return(NA)
    }
  } else {
    stop("Error fetching PMID for DOI: ", doi)
  }
}

# 定义函数来从 PubMed ID 获取 DOI
get_doi_from_pmid <- function(pmid) {
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  
  # 基础参数
  params <- list(
    db = "pubmed",
    id = pmid,
    retmode = "xml"
  )
  
  # 如果 api_key 不为空，则添加到参数列表中
  if (nchar(ncbi_api_key) > 0) {
    params$api_key <- ncbi_api_key
  }

  
  # https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&api_key=07f8d4bf9f9115e0a97cd01fd927778b2f09&id=37556327&retmode=xml
  response <- GET(url, query = params)
  
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    doc <- read_xml(content)
    
    # 提取 DOI
    doi <- xml_text(xml_find_first(doc, "//Item[@Name='DOI']"))
    
    if (!is.null(doi) && doi != "") {
      return(doi)
    } else {
      return(NA)
    }
  } else {
    stop("Error fetching DOI for PMID: ", pmid)
  }
}

# 修改 process_input 函数
process_input <- function(input_str) {
  # 分割输入
  inputs <- unlist(strsplit(input_str, "[,;\n]"))
  
  pmids <- character()
  
  for (i in seq_along(inputs)) {
    input <- gsub("\\s+", "", inputs[i])  # 移除空格
    
    if (input == "") next
    
    if (grepl("^\\d+$", input)) {  # 如果是纯数字，认为是 PMID
      pmids <- c(pmids, input)
    } else if (grepl("^10\\.", input)) {  # 如果以 10. 开头，认为是 DOI
      # 使用 NCBI API 搜索 DOI
      tryCatch({
        pmid <- get_pmid_from_doi(input)
        if (!is.null(pmid)) {
          pmids <- c(pmids, pmid)
        }
      }, error = function(e) {
        message(paste("Error processing DOI:", input, "-", e$message))
      })
    }
  }
  return(unique(pmids))
}


# 输入一个文献doi号，获得该文献文中引用文献的全部doi号
get_references <- function(doi) {
  url <- paste0("https://api.crossref.org/works/", doi, "/transform/application/vnd.citationstyles.csl+json")
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    references <- fromJSON(content)
    
    # 提取并去除NA值
    doi_list <- references$reference$DOI[!is.na(references$reference$DOI)]
    
    return(doi_list)
  } else {
    stop("Error fetching references for DOI: ", doi)
  }
}

# 文献被引次数获得
get_citation_count <- function(pubmedid) {
  # 参数检查
  if (missing(pubmedid) || is.null(pubmedid) || pubmedid == "") {
    warning("Invalid PubMed ID provided")
    return(0)
  }
  
  tryCatch({
    # 获取关系数据
    base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed"
    api_key_part <- if(nchar(ncbi_api_key) > 0) paste0("&api_key=", ncbi_api_key) else ""
    url <- paste0(base_url,
                  api_key_part,
                  "&linkname=pubmed_pubmed_citedin",
                  "&id=", pubmedid)
    
    # 添加超时设置
    xml_content <- GET(url, timeout(10))
    
    # 检查请求状态
    if (xml_content$status_code != 200) {
      warning(paste("API request failed with status code:", xml_content$status_code))
      return(0)
    }
    
    # 解析XML
    xml_data <- read_xml(rawToChar(xml_content$content))
    
    # 计算引用数量
    citation_count <- length(xml_find_all(xml_data, "//LinkSetDb/Link/Id"))
    
    # 返回引用数量
    return(citation_count)
    
  }, error = function(e) {
    # 错误处理
    warning(paste("Error fetching citation count for PMID", pubmedid, ":", e$message))
    return(0)  # 发生错误时返回0
  })
}


# 定义UI----
ui <- fluidPage(
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      h3("Citation Network"),
      br(),
      textAreaInput("citation_network_pmid_input", 
                   "Enter PMIDs or DOIs", 
                   value = "",
                   height = "135px",
                   placeholder = "Enter PMIDs or DOIs (separated by commas, semicolons, or new lines)\nExample:\n10.1016/j.celrep.2023.112966\n38236735"),
      # 添加关系类型选择
      selectInput("citation_network_relation_type",
                 "Select relation type:",
                 choices = list(
                   "Papers citing this (Citations)" = "pubmed_pubmed_citedin",
                   "Most related papers (top 5)" = "pubmed_pubmed_five",
                   "Related reviews" = "pubmed_pubmed_reviews",
                   "Most related reviews (top 5)" = "pubmed_pubmed_reviews_five",
                   "Combined related papers" = "pubmed_pubmed_combined",
                   "Related papers (Return list for first literature)" = "pubmed_pubmed",
                   "References (Return list for first literature)" = "references"
                 ),
                 selected = "pubmed_pubmed_citedin") %>%
        helper(type = "inline", 
               title = "Relation Type Help",
               content = c("Papers citing this (Citations): Other papers that have cited the current paper.",
                           "Related papers: Shows papers that are related in terms of content.",
                           "Most related papers (top 5): Shows the top 5 most relevant papers.",
                           "Related reviews: Shows review articles related to the selected paper.",
                           "Most related reviews (top 5): Shows the top 5 most relevant review articles.",
                           "Combined related papers: A combination of various related papers.",
                           "References: Sources that the current paper has cited." )),
      # 添加网络深度选择
      uiOutput("citation_network_depth_selector"),  # 使用动态UI
      selectInput("degree_threshold", 
                 "Degree threshold for label display:", 
                 choices = setNames(1:20, paste(1:20, "connections")),
                 selected = 3),
      colourInput("citation_network_selected_color", 
                 "Color for selected nodes:", 
                 width = "45%",
                 value = "red"),
      colourInput("citation_network_unselected_color", 
                 "Color for other nodes:", 
                 width = "45%",
                 value = "skyblue"),
      hr(),
      actionBttn(
        inputId = "citation_network_example",
        label = "Example",
        style = "unite",
        color = "danger"
      ),
      hr(),
      actionBttn(
        inputId = "citation_network_go",
        label = "Generate Network",
        style = "jelly",
        color = "success",
        icon = icon("arrow-rotate-right"),
        block = TRUE,
        size = "sm"),
      br(),
      helpText(HTML("Although the code does not restrict the network depth and the number of input documents, it is recommended to input <b>1</b> document with a network depth of <b>2</b> layers or less. For example, if each document is cited an average of <b>20</b> times, a network depth of <b>3</b> layers would require retrieving <b>20^3 + 1 = 8001</b> documents, which could cause this application to <b>crash</b>.")),
      width = 3
    ),
    mainPanel(
      conditionalPanel(
        # 当关系类型不是 references 且不是 pubmed_pubmed 时显示网络图
        condition = "input.citation_network_relation_type != 'references' && input.citation_network_relation_type != 'pubmed_pubmed'",
        visNetworkOutput("network_plot", height = "800px")
      ),
      conditionalPanel(
        # 当关系类型是 references 或者是 pubmed_pubmed 时显示表格
        condition = "input.citation_network_relation_type == 'references' || input.citation_network_relation_type == 'pubmed_pubmed'",
        DT::DTOutput("reference_table")
      ),
      width = 9
    )
  )
)


# 定义server----
server <- function(input, output, session) {
  # shinyhelper用
  observe_helpers(withMathJax = TRUE, help_dir = "./")
  
  # 示例按钮
  observeEvent(input$citation_network_example, {
    updateTextAreaInput(session, "citation_network_pmid_input", value = "10.1016/j.cub.2023.08.061")
  })
  
  # 动态深度选择器
  output$citation_network_depth_selector <- renderUI({
    rel_type <- input$citation_network_relation_type
    
    if (rel_type == "pubmed_pubmed_citedin") {
      # 如果选择"Papers citing this (Citations)"，默认为2层
      selectInput("citation_network_citation_depth", 
                  "Network depth:",
                  choices = list("1 layer" = 1, "2 layers" = 2, "3 layers" = 3),
                  selected = 2)
    } else if (rel_type == "pubmed_pubmed" | rel_type == "references") {
      # 如果选择"Related papers"或"References"，只允许选择"1 layer"
      selectInput("citation_network_citation_depth", 
                  "Network depth:",
                  choices = list("1 layer" = 1),
                  selected = 1)
    } else {
      # 否则允许选择所有深度，默认为1层
      selectInput("citation_network_citation_depth", 
                  "Network depth:",
                  choices = list("1 layer" = 1, "2 layers" = 2, "3 layers" = 3),
                  selected = 1)
    }
  })
  
  
  #########################################
  ######### 核心数据和网络图生成逻辑 #########
  #########################################
  
  # 使用 eventReactive 来隔离所有对输入的依赖，只在点击按钮时执行
  citation_network_result <- eventReactive(input$citation_network_go, {
    
    # 1. 确保输入框有内容
    req(input$citation_network_pmid_input) 
    
    # 2. 捕获所有当前的输入值（这是 eventReactive 隔离依赖的关键）
    pmid_input_str <- input$citation_network_pmid_input
    relation_type_val <- input$citation_network_relation_type
    
    # 确保深度选择器已渲染且有值，并转换为数值
    req(input$citation_network_citation_depth)
    depth_val <- as.numeric(input$citation_network_citation_depth)
    
    threshold_val <- as.numeric(input$degree_threshold)
    selected_color_val <- input$citation_network_selected_color
    unselected_color_val <- input$citation_network_unselected_color
    
    # 如果关系类型是表格类型，则跳过网络图生成，返回一个特殊的信号或 NULL
    if (relation_type_val == "references" || relation_type_val == 'pubmed_pubmed') {
      # 对于表格类型，我们返回一个信号或 NULL，让网络图渲染器知道要跳过
      return(list(type = "table_mode")) 
    }
    
    # --- 网络图生成逻辑 ---
    
    tryCatch({
      # 处理输入
      pmids <- process_input(pmid_input_str)
      if (length(pmids) == 0) stop("No valid PMIDs or DOIs found")
      
      # 获取关系数据
      citation_data <- withProgress(
        message = 'Step 1: Fetching data',
        value = 0.2,
        {
          result <- get_citation_data(
            pmids,
            depth = depth_val,
            relation_type = relation_type_val
          )
          if (nrow(result) == 0) stop("No relations found")
          incProgress(0.4, detail = paste("Found", nrow(result), "connections"))
          showNotification(paste("Found", nrow(result), "connections"), type = "message")
          result
        }
      )
      
      # 生成网络图
      network_plot_obj <- withProgress(
        message = 'Step 2: Creating network visualization',
        value = 0.6,
        {
          result <- generate_plot_from_data(
            citation_data,
            pmids,
            threshold_val,
            selected_color = selected_color_val,
            unselected_color = unselected_color_val
          )
          if (is.null(result)) stop("Failed to create network visualization")
          incProgress(1, detail = "Visualization complete")
          result
        }
      )
      
      # 返回结果和 PMIDs
      return(list(type = "network", plot = network_plot_obj))
      
    }, error = function(e) {
      showNotification(
        paste("Error:", e$message),
        type = "error",
        duration = NULL
      )
      # 返回错误提示网络图
      error_plot <- visNetwork(
        nodes = data.frame(
          id = 1,
          label = paste("Error:", e$message),
          color = "red"
        ),
        edges = data.frame(from = integer(0), to = integer(0))
      ) %>%
        visLayout(randomSeed = 123)
      
      return(list(type = "error", plot = error_plot))
    })
  }, ignoreInit = TRUE) # 忽略应用初始加载时的执行
  
  # 渲染网络图，它只依赖于 eventReactive 的输出
  output$network_plot <- renderVisNetwork({
    res <- citation_network_result()
    if (is.null(res) || res$type == "table_mode") {
      # 返回一个空白或提示信息，以防条件面板显示但无结果
      return(visNetwork(
        nodes = data.frame(id=1, label="Click 'Generate Network' to start", color="lightgray"), 
        edges = data.frame()
      ))
    }
    if (res$type == "error") {
      return(res$plot)
    }
    res$plot
  })
  
  
  #########################################
  ######### 引用文献表格生成逻辑 ############
  #########################################
  
  # 使用 eventReactive 来隔离表格部分的输入依赖
  reference_table_result <- eventReactive(input$citation_network_go, {
    
    # 1. 确保关系类型是表格类型
    rel_type <- input$citation_network_relation_type
    if(rel_type != "references" && rel_type != 'pubmed_pubmed') {
      return(NULL) # 如果不是表格类型，则返回 NULL，避免执行
    }
    
    req(input$citation_network_pmid_input)
    
    # 捕获输入值
    pmid_input_str <- input$citation_network_pmid_input
    
    tryCatch({
      # Process input to get PMIDs
      pmids <- process_input(pmid_input_str)
      # 只处理第一个输入的文献
      pmids <- pmids[1]
      
      if(length(pmids) == 0) stop("No valid PMIDs or DOIs found")
      
      # Create initial data frame with PMIDs and their DOIs
      result_df <- withProgress(
        message = 'Processing reference data',
        value = 0,
        {
          # ... (这里放您原有的 Step 1 到 Step 4 的所有逻辑) ...
          
          # Step 1: Get DOIs for input PMIDs (10%)
          incProgress(0.1, detail = "Getting DOIs for input PMIDs...")
          input_dois <- sapply(pmids, function(pmid) {
            # ... (get_doi_from_pmid 逻辑) ...
            tryCatch({
              doi <- get_doi_from_pmid(pmid)
              if(is.na(doi)) return("DOI not found")
              return(doi)
            }, error = function(e) {
              return("Error retrieving DOI")
            })
          })
          
          # Step 2: Get references for each DOI (20%)
          incProgress(0.2, detail = "Fetching references for each article...")
          all_references <- list()
          total_dois <- length(input_dois)
          for(i in seq_along(input_dois)) {
            incProgress(0.2/total_dois, detail = sprintf("Processing %d of %d", i, total_dois))
            if(input_dois[i] != "DOI not found" && input_dois[i] != "Error retrieving DOI") {
              refs <- tryCatch({
                if (rel_type == "references") {
                  get_references(input_dois[i])
                } else if (rel_type == "pubmed_pubmed") {
                  citation_data <- get_citation_data(pmids, depth=1, relation_type="pubmed_pubmed")
                  if(is.data.frame(citation_data) && ncol(citation_data) >= 2) {
                    unlist(citation_data[, 2])
                  } else {
                    character(0)
                  }
                }
              }, error = function(e) {
                character(0)
              })
              all_references[[pmids[i]]] <- refs
            }
          }
          
          # Step 3: Create expanded dataframe with all references (50%)
          reference_rows <- list()
          total_refs <- sum(sapply(all_references, length))
          current_ref <- 0
          
          for(pmid in names(all_references)) {
            refs <- all_references[[pmid]]
            if(length(refs) > 0) {
              for(ref_id in refs) { # ref_id 可能是 DOI 或 PMID
                current_ref <- current_ref + 1
                incProgress(0.5/total_refs, detail = sprintf("Processing details %d of %d", current_ref, total_refs))
                
                ref_pmid <- if (rel_type == "pubmed_pubmed") {
                  ref_id
                } else {
                  tryCatch(get_pmid_from_doi(ref_id), error = function(e) NA_character_)
                }
                
                related_doi <- if (rel_type == "pubmed_pubmed" && !is.na(ref_pmid)) {
                  tryCatch(get_doi_from_pmid(ref_pmid), error = function(e) NA_character_)
                } else if (rel_type == "references") {
                  ref_id
                } else {
                  NA_character_
                }
                
                citation_count <- if(length(ref_pmid) == 1 && !is.na(ref_pmid)) {
                  tryCatch(get_citation_count(ref_pmid), error = function(e) NA_real_)
                } else {
                  NA_real_
                }
                
                pub_info <- if(length(ref_pmid) == 1 && !is.na(ref_pmid)) {
                  tryCatch(get_pubmed_info(ref_pmid), error = function(e) {
                    data.frame(title = "Not available", authors = "Not available", journal = "Not available", year = "Not available")
                  })
                } else {
                  data.frame(title = "Not available", authors = "Not available", journal = "Not available", year = "Not available")
                }
                
                reference_rows[[length(reference_rows) + 1]] <- data.frame(
                  Input_DOI = input_dois[which(pmids == pmid)],
                  Output_DOI = related_doi,
                  Title = pub_info$title,
                  Authors = pub_info$authors,
                  Journal = pub_info$journal,
                  Year = pub_info$year,
                  Citation_Count = citation_count,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
          
          if(length(reference_rows) > 0) {
            result_df <- do.call(rbind, reference_rows)
            
            # Step 4: Final cleanup and formatting
            result_df$Authors <- sapply(result_df$Authors, function(authors) {
              if (authors == "No authors available" || authors == "Not available") return(authors)
              author_list <- strsplit(authors, "; ")[[1]]
              if (length(author_list) > 3) {
                paste(paste(author_list[1:3], collapse = "; "), "et al.")
              } else {
                authors
              }
            })
            
            # 添加DOI链接
            result_df$Output_DOI <- ifelse(
              is.na(result_df$Output_DOI) | result_df$Output_DOI %in% c("NA", "Not found"),
              result_df$Output_DOI,
              paste0("<a href='https://doi.org/", result_df$Output_DOI, "' target='_blank'>", result_df$Output_DOI, "</a>")
            )
            
            # 仅保留所需列
            result_df <- result_df[, c("Output_DOI", "Title", "Authors", "Journal", "Year", "Citation_Count")]
            
            incProgress(1, detail = "Data processing complete")
            return(result_df)
            
          } else {
            stop("No references or related papers found.")
          }
        }
      )
    }, error = function(e) {
      showNotification(paste("Error generating table:", e$message), type = "error", duration = NULL)
      return(data.frame(
        Output_DOI = "Error",
        Title = paste("Error:", e$message),
        Authors = "", Journal = "", Year = "", Citation_Count = ""
      ))
    })
  }, ignoreInit = TRUE)
  
  # 渲染表格，它只依赖于 eventReactive 的输出
  output$reference_table <- DT::renderDT({
    # 确保只有在表格模式下且有数据时才显示
    req(input$citation_network_relation_type == "references" || input$citation_network_relation_type == 'pubmed_pubmed')
    
    table_data <- reference_table_result()
    
    if (is.null(table_data) || nrow(table_data) == 0) {
      # 如果没有数据，返回一个空表格或提示
      return(DT::datatable(
        data.frame(Output_DOI="No data found", Title="Please check your input or connection.", Authors="", Journal="", Year="", Citation_Count=""),
        options = list(dom = 't')
      ))
    }
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 10,
        columnDefs = list(list(targets = 1, width = '200px')),
        autoWidth = TRUE
      ),
      escape = FALSE, # 允许 HTML 链接渲染
      rownames = FALSE
    )
  }, server = FALSE) # 使用 client-side processing
  
}
# 运行应用
shinyApp(ui = ui, server = server)

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
      # Sys.sleep(0.1)
      
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
    # mainPanel(
    #   conditionalPanel(
    #     condition = "input.citation_network_relation_type != 'references'",
    #     visNetworkOutput("network_plot", height = "800px")
    #   ),
    #   conditionalPanel(
    #     condition = "input.citation_network_relation_type == 'references'",
    #     DT::DTOutput("reference_table")
    #   ),
    #   width = 9
    # )
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
  observe_helpers(withMathJax = TRUE, help_dir = "./") # help_dir = "./"作为type=markdown时使用，现在用不到
  
  
  observeEvent(input$citation_network_example, {
    updateTextAreaInput(session, "citation_network_pmid_input", value = "10.1016/j.celrep.2023.112966")
  })
  
  output$citation_network_depth_selector <- renderUI({
    if (input$citation_network_relation_type == "pubmed_pubmed_citedin") {
      # 如果选择"Papers citing this (Citations)"，默认为2层
      selectInput("citation_network_citation_depth", 
                  "Network depth:",
                  choices = list(
                    "1 layer" = 1,
                    "2 layers" = 2,
                    "3 layers" = 3
                  ),
                  selected = 2)
    } else if (input$citation_network_relation_type == "pubmed_pubmed" | input$citation_network_relation_type == "references") {
      # 如果选择"Related papers"，只允许选择"1 layer"
      selectInput("citation_network_citation_depth", 
                  "Network depth:",
                  choices = list("1 layer" = 1),
                  selected = 1)
    } else {
      # 否则允许选择所有深度，默认为1层
      selectInput("citation_network_citation_depth", 
                  "Network depth:",
                  choices = list(
                    "1 layer" = 1,
                    "2 layers" = 2,
                    "3 layers" = 3
                  ),
                  selected = 1)
    }
  })
  
  ###############################
  ######### 返回网络图 ##########
  ###############################
  observeEvent(input$citation_network_go, {
    # 网络渲染
    if (input$citation_network_relation_type != "references" && input$citation_network_relation_type != 'pubmed_pubmed') {
      output$network_plot <- renderVisNetwork({
        req(input$citation_network_pmid_input)
        
        isolate({
          tryCatch({
            # 处理输入
            pmids <- process_input(input$citation_network_pmid_input)
            # if (input$citation_network_relation_type == "pubmed_pubmed") {
            #   pmids <- pmids[1]
            # }
            if (length(pmids) == 0) stop("No valid PMIDs or DOIs found")
            
            # 获取关系数据
            citation_data <- withProgress(
              message = 'Step 1: Fetching data',
              value = 0.2,
              {
                result <- get_citation_data(
                  pmids,
                  depth = as.numeric(input$citation_network_citation_depth),
                  relation_type = input$citation_network_relation_type
                )
                if (nrow(result) == 0) stop("No relations found")
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
                  selected_color = input$citation_network_selected_color,
                  unselected_color = input$citation_network_unselected_color
                )
                if (is.null(result)) stop("Failed to create network visualization")
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
  }, ignoreNULL = TRUE)
  
  ###############################
  ##### 返回引用文献表格 ########
  ###############################
  observeEvent(input$citation_network_go, {

    # 表格渲染
    if(length(input$citation_network_relation_type) == 1 && 
       (input$citation_network_relation_type == "references" || 
        input$citation_network_relation_type == 'pubmed_pubmed')) {
      output$reference_table <- DT::renderDT({
        req(input$citation_network_pmid_input)

        isolate({
          tryCatch({
            # Process input to get PMIDs
            pmids <- process_input(input$citation_network_pmid_input)
            # 只返回第一篇输入文献的参考文献
            # 后续代码其实都是多输入情况下可用的，但一篇也能用了
            pmids <- pmids[1]
            
            if(length(pmids) == 0) stop("No valid PMIDs or DOIs found")
            
            # Create initial data frame with PMIDs and their DOIs
            withProgress(
              message = 'Processing',
              value = 0,
              {
                # Step 1: Get DOIs for input PMIDs (10%)
                incProgress(0.1, detail = "Getting DOIs for input PMIDs...")
                input_dois <- sapply(pmids, function(pmid) {
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
                  incProgress(0.2/total_dois,
                              detail = sprintf("Processing %d of %d", i, total_dois))
                  if(input_dois[i] != "DOI not found" && input_dois[i] != "Error retrieving DOI") {
                    refs <- tryCatch({
                      # get_references(input_dois[i])
                      ##add related paper
                      if (input$citation_network_relation_type == "references") {
                        get_references(input_dois[i])
                      } else if (input$citation_network_relation_type == "pubmed_pubmed") {
                        # 获取citation数据并提取第二列为向量
                        citation_data <- get_citation_data(pmids, depth=1, relation_type="pubmed_pubmed")
                        if(is.data.frame(citation_data) && ncol(citation_data) >= 2) {
                          unlist(citation_data[, 2])  # 将第二列转换为向量
                        } else {
                          character(0)  # 如果数据不符合预期，返回空字符向量
                        }
                      }
                      ##
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
                    for(ref_doi in refs) {
                      current_ref <- current_ref + 1
                      incProgress(0.5/total_refs,
                                  detail = sprintf("Processing details %d of %d",
                                                   current_ref, total_refs))
                      

                      ref_pmid <- if (input$citation_network_relation_type == "pubmed_pubmed") {
                        ref_doi  # 当关系类型是 pubmed_pubmed 时，直接使用 ref_doi 的值
                      } else {
                        tryCatch({
                          get_pmid_from_doi(ref_doi)  # 其他情况下，通过 DOI 获取 PMID
                        }, error = function(e) {
                          NA
                        })
                      }
                      
                      # 当展示相关文献时，需要获得一次文献doi
                      related_doi <- if (input$citation_network_relation_type == "pubmed_pubmed") {
                        tryCatch({
                          get_doi_from_pmid(ref_doi)
                        }, error = function(e) {
                          NA_real_
                        })
                      }
                      
                      # Get citation count if PMID is available
                      citation_count <- if(length(ref_pmid) == 1 && !is.na(ref_pmid)) {
                        tryCatch({
                          get_citation_count(ref_pmid)
                        }, error = function(e) {
                          NA_real_
                        })
                      } else {
                        NA_real_
                      }
                      
                      # Get publication info if PMID is available
                      pub_info <- if(length(ref_pmid) == 1 && !is.na(ref_pmid)) {
                        tryCatch({
                          get_pubmed_info(ref_pmid)
                        }, error = function(e) {
                          data.frame(
                            title = "Not available",
                            authors = "Not available",
                            journal = "Not available",
                            year = "Not available"
                          )
                        })
                      } else {
                        data.frame(
                          title = "Not available",
                          authors = "Not available",
                          journal = "Not available",
                          year = "Not available"
                        )
                      }
                      
                      reference_rows[[length(reference_rows) + 1]] <- if(input$citation_network_relation_type == "references") {
                        data.frame(
                          # Input_PMID = pmid,
                          Input_DOI = input_dois[which(pmids == pmid)],
                          Output_DOI = ref_doi,
                          # Reference_PMID = if(!is.na(ref_pmid)) ref_pmid else "Not found",
                          Title = pub_info$title,
                          Authors = pub_info$authors,
                          Journal = pub_info$journal,
                          Year = pub_info$year,
                          Citation_Count = citation_count,
                          stringsAsFactors = FALSE
                        )
                      } else if (input$citation_network_relation_type == "pubmed_pubmed") {
                        data.frame(
                          # Input_PMID = pmid,
                          Input_DOI = input_dois[which(pmids == pmid)],
                          Output_DOI = related_doi,
                          # Reference_PMID = if(!is.na(ref_pmid)) ref_pmid else "Not found",
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
                }
                
                if(length(reference_rows) > 0) {
                  result_df <- do.call(rbind, reference_rows)
                  
                  # Simplify authors display - show only first author + et al.
                  result_df$Authors <- sapply(result_df$Authors, function(authors) {
                    if(authors != "Not available" && authors != "N/A") {
                      author_list <- strsplit(authors, "; ")[[1]]
                      if(length(author_list) > 1) {
                        paste0(author_list[1], " et al.")
                      } else {
                        author_list[1]
                      }
                    } else {
                      authors
                    }
                  })
                  
                  # Create clickable DOI links for both Input_DOI and Output_DOI
                  result_df$Input_DOI <- sapply(result_df$Input_DOI, function(doi) {
                    if(length(doi) == 1 && !is.na(doi) && doi != "Not found" && doi != "DOI not found" && doi != "") {
                      paste0('<a href="https://doi.org/', doi, '" target="_blank">', doi, '</a>')
                    } else {
                      doi
                    }
                  })
                  
                  result_df$Output_DOI <- sapply(result_df$Output_DOI, function(doi) {
                    if(doi != "Not found" && doi != "DOI not found" && doi != "") {
                      paste0('<a href="https://doi.org/', doi, '" target="_blank">', doi, '</a>')
                    } else {
                      doi
                    }
                  })
                  
                  # Sort by Citation_Count in descending order
                  result_df$Citation_Count <- as.numeric(result_df$Citation_Count)
                  result_df <- result_df[order(-result_df$Citation_Count, na.last = TRUE), ]
                } else {
                  result_df <- data.frame(
                    # Input_PMID = character(0),
                    Input_DOI = character(0),
                    Output_DOI = character(0),
                    # Reference_PMID = character(0),
                    Title = character(0),
                    Authors = character(0),
                    Journal = character(0),
                    Year = character(0),
                    Citation_Count = numeric(0),
                    stringsAsFactors = FALSE
                  )
                }
                
                return(datatable(result_df, rownames = FALSE, escape = FALSE, options = list(
                  columnDefs = list(
                    list(width = '2000px', targets = which(names(result_df) == "Title") - 1),
                    list(width = '300px', targets = which(names(result_df) == "Input_DOI") - 1),
                    list(width = '100px', targets = which(names(result_df) == "Reference_DOI") - 1),
                    list(width = '100px', targets = which(names(result_df) == "Reference_PMID") - 1),
                    list(width = '100px', targets = which(names(result_df) == "Citation_Count") - 1)
                  ),
                  order = list(list(ncol(result_df) - 1, 'desc'))
                )))
              }
            )
          }, error = function(e) {
            return(data.frame(
              # Input_PMID = "Error",
              Input_DOI = as.character(e$message),
              Output_DOI = "",
              # Reference_PMID = "",
              Title = "",
              Authors = "",
              Journal = "",
              Year = "",
              Citation_Count = NA_real_,
              stringsAsFactors = FALSE
            ))
          })
        })
        
      })
    }
  }, ignoreNULL = TRUE)
  
  

}

# 运行应用
shinyApp(ui = ui, server = server)

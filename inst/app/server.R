shinyServer(function(input, output, session) {

  
  init_data <- function(env=r_data) {
    r_info <- reactiveValues()
    
    isolate({
      query <- parseQueryString(session$clientData$url_search)
    })
    
    if (!missing(query) && "code" %in% names(query)) {
      data.in.pkg = data(package = "radiant.exams")$results[, "Item"]
      
      # Check if the data file is in radiant.exams
      df_name = ifelse(query[["code"]] %in% data.in.pkg, query[["code"]], "titanic")
      df <- data(list = df_name, package = "radiant.exams", envir = environment()) %>% get()
      df_name = paste('data',paste(sample(c(letters,LETTERS,0:9),10),collapse=""),sep="")
    } else { 
      df_name <- getOption("radiant.init.data", default = "titanic")
      df <- data(list = df_name, package = "radiant.biostat", envir = environment()) %>% get()
    }
    
    r_data[[df_name]] <- df
    makeReactiveBinding(df_name, env = r_data)
    
    r_info[[paste0(df_name, "_descr")]] <- attr(df, "description")
    r_info$datasetlist <- df_name
    r_info$url <- NULL
    r_info
  }
  
  
  
    
  ## source shared functions
  source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  
  ## source data & app tools from radiant.data
  for (file in list.files(
    c(
      file.path(getOption("radiant.path.data"), "app/tools/app"),
      file.path(getOption("radiant.path.data"), "app/tools/data")
    ),
    pattern = "\\.(r|R)$", 
    full.names = TRUE)) {
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  }
  
  ## list of radiant menu's to include
  rmenus <- c("radiant.data", "radiant.design", "radiant.basics", "radiant.model")#, "radiant.multivariate")
  
  ## packages to use for example data
  ## options(radiant.example.data = rmenus)
  options(radiant.example.data = "radiant.biostat")
  
  for (i in rmenus[-1]) {
    ## 'sourcing' radiant's package functions in the server.R environment
    eval(parse(text = paste0("radiant.data::copy_all(", i, ")")))
    
    ipath <- paste0(strsplit(i, "\\.")[[1]], collapse = ".path.")
    
    ## help ui
    source(file.path(getOption(ipath), "app/help.R"), encoding = getOption("radiant.encoding"), local = TRUE)
    
    ## source analysis tools for each app
    for (file in list.files(file.path(getOption(ipath), "app/tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE))
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  }
  
  ## ui creation functions
  source(file.path(getOption("radiant.path.model"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  
  ## help ui
  output$help_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_design_panel,
        help_basics_panel,
        help_model_panel,
        #help_multivariate_panel,
        uiOutput("help_text"),
        width = 3
      ),
      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_data"),
        htmlOutput("help_design"),
        htmlOutput("help_basics"),
        htmlOutput("help_model")#,
        #htmlOutput("help_multivariate")
      )
    )
  })
  
  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})




# shinyServer(function(input, output, session) {
#   
#   ## source shared functions
#   source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
#   source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
#   
#   ## source data & app tools from radiant.data
#   for (file in list.files(
#     c(
#       file.path(getOption("radiant.path.data"), "app/tools/app"),
#       file.path(getOption("radiant.path.data"), "app/tools/data")
#     ),
#     pattern = "\\.(r|R)$", 
#     full.names = TRUE)) {
#     source(file, encoding = getOption("radiant.encoding"), local = TRUE)
#   }
#   
#   ## list of radiant menu's to include
#   rmenus <- c("radiant.data", "radiant.design", "radiant.basics", "radiant.model")
#   
#   ## packages to use for example data
#   options(radiant.example.data = rmenus)
#   
#   for (i in rmenus[-1]) {
#     ## 'sourcing' radiant's package functions in the server.R environment
#     eval(parse(text = paste0("radiant.data::copy_all(", i, ")")))
#     
#     ipath <- paste0(strsplit(i, "\\.")[[1]], collapse = ".path.")
#     
#     ## help ui
#     source(file.path(getOption(ipath), "app/help.R"), encoding = getOption("radiant.encoding"), local = TRUE)
#     
#     ## source analysis tools for each app
#     for (file in list.files(file.path(getOption(ipath), "app/tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE))
#       source(file, encoding = getOption("radiant.encoding"), local = TRUE)
#   }
#   
#   ## ui creation functions
#   source(file.path(getOption("radiant.path.model"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
#   
#   ## help ui
#   output$help_ui <- renderUI({
#     sidebarLayout(
#       sidebarPanel(
#         help_data_panel,
#         help_design_panel,
#         help_basics_panel,
#         help_model_panel,
#         #help_multivariate_panel,
#         uiOutput("help_text"),
#         width = 3
#       ),
#       mainPanel(
#         HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
#         htmlOutput("help_data"),
#         htmlOutput("help_design"),
#         htmlOutput("help_basics"),
#         htmlOutput("help_model")
# #        htmlOutput("help_multivariate")
#       )
#     )
#   })
#   
#   ## save state on refresh or browser close
#   saveStateOnRefresh(session)
# })


  ## http://bioinfo1.med.unibs:3800/biostat/?code=anorexia -> opens anorexia.rda
  
  
#   init_data <- function(env = r_data) {
#     
#       ## Joe Cheng: 'Datasets can change over time (i.e., the changedata function).
#       ## Therefore, the data need to be a reactive value so the other reactive functions
#       ## and outputs that depend on these datasets will know when they are changed.'
#       r_info <- reactiveValues()
#       
#       isolate({
#           query <- parseQueryString(session$clientData$url_search)
#       })
#       
#       if (!missing(query) && "code" %in% names(query)) {
#           data.in.pkg = data(package = "radiant.exams")$results[, "Item"]
#           
#           df_names = ifelse(query[["code"]] %in% data.in.pkg, query[["code"]], "titanic")
#           nomi = paste('data',paste(sample(c(letters,LETTERS,0:9),10),collapse=""),sep="")
# 
#       } else
#           {
#               df_names <- getOption("radiant.init.data", default = c("diamonds", "titanic"))
#               nomi = basename(df_names)
#           }
# 
#       for (dn in df_names) {
#           if (file.exists(dn)) {
#               df <- load(dn) %>% get()
#               dn <- nomi %>% #basename(dn) %>%
#                   {gsub(paste0(".", tools::file_ext(.)), "", ., fixed = TRUE)}
#           } else {
#               df <- data(list = dn, package = "radiant.data", envir = environment()) %>% get()
#               r_info[[paste0(dn, "_lcmd")]] <- paste0(dn, " <- data(", dn, ", package = \"radiant.data\") %>% get()\nregister(\"", dn, "\")")
#           }
#           env[[dn]] <- df
#           makeReactiveBinding(dn, env = env)
#           r_info[[paste0(dn, "_descr")]] <- attr(df, "description")
#       }
#       r_info[["datasetlist"]] <- nomi
#       r_info[["url"]] <- NULL
#       r_info
# 
# }
  

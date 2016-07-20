shinyServer(function(input, output, session) {

  ## http://bioinfo1.med.unibs:3800/biostat/?code=anorexia -> opens anorexia.rda

  
  init_data <- function() {

    ## Joe Cheng: 'Datasets can change over time (i.e., the changedata function).
    ## Therefore, the data need to be a reactive value so the other reactive functions
    ## and outputs that depend on these datasets will know when they are changed.'
    r_data <- reactiveValues()

    # df_name <- getOption("radiant.init.data", default = "titanic")

    isolate({
      query <- parseQueryString(session$clientData$url_search)
    })

    if (!missing(query) && "code" %in% names(query)) {
      data.in.pkg = data(package = "radiant.biostat")$results[, "Item"]

      ## Check if the data file is in radiant.biostat
      df_name = ifelse(query[["code"]] %in% data.in.pkg, query[["code"]], "titanic")
    } else df_name <- getOption("radiant.init.data", default = "titanic")

    df <- data(list = df_name, package = "radiant.biostat", envir = environment()) %>%
      get


    r_data[[df_name]] <- df
    r_data[[paste0(df_name, "_descr")]] <- attr(df, "description")
    r_data$datasetlist <- df_name
    r_data$url <- NULL
    r_data
  }
  

  ## source shared functions
  source(file.path(getOption("radiant.path.data"),"app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source(file.path(getOption("radiant.path.data"),"app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
##  source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## help ui
  ## output$help_design_ui <- renderUI({
  ##   sidebarLayout(
  ##     sidebarPanel(
  ##       help_data_panel,
  ##       help_design_panel,
  ##       uiOutput("help_text"),
  ##       width = 3
  ##     ),
  ##     mainPanel(
  ##       HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
  ##       htmlOutput("help_data"),
  ##       htmlOutput("help_design")
  ##     )
  ##   )
  ## })

  ## packages to use for example data
  options(radiant.example.data = c("radiant.biostat"))

  ## source data & app tools from radiant.data
  for (file in list.files(c(file.path(getOption("radiant.path.data"),"app/tools/app"),
                            file.path(getOption("radiant.path.data"),"app/tools/data")),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## 'sourcing' radiant's package functions in the server.R environment
  if (!"package:radiant.biostat" %in% search() && getOption("radiant.path.biostat") == "..") {
    ## for shiny-server and development
    for (file in list.files("../../R", pattern="\\.(r|R)$", full.names = TRUE))
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  } else {
    ## for use with launcher
    radiant.data::copy_all(radiant.biostat)
  }

    ## source analysis tools for design app
  for (file in list.files(c("tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  ## saveStateOnRefresh(session)

    ## SC: stop app when session is ended, browser close or refresh
    session$onSessionEnded(function() {stopApp() })
    
    
})

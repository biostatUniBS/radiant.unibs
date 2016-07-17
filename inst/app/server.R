shinyServer(function(input, output, session) {
    
    ## source shared functions
    source(file.path(getOption("radiant.path.data"),"app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
    source(file.path(getOption("radiant.path.data"),"app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)

    ## source(file.path(getOption("radiant.path.biostat"),"app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
    
    ## source data & app tools from radiant.data
    for (file in list.files(c(file.path(getOption("radiant.path.data"),"app/tools/app"),
                              file.path(getOption("radiant.path.data"),"app/tools/data")),
                            pattern = "\\.(r|R)$", full.names = TRUE))
        source(file, encoding = getOption("radiant.encoding"), local = TRUE)
    
    ## list of radiant menu's to include
    rmenus <- c("radiant.data","radiant.basics","radiant.model","radiant.design","radiant.biostat")
    
    ## packages to use for example data
    ## options(radiant.example.data = rmenus)
    options(radiant.example.data = "radiant.biostat")
    
    for (i in rmenus[-1]) {
        ## 'sourcing' radiant's package functions in the server.R environment
        eval(parse(text = paste0("radiant.data::copy_all(",i,")")))
        
        ipath <- paste0(strsplit(i,"\\.")[[1]], collapse = ".path.")
        
        ## help ui
        if(ipath != "radiant.path.biostat")
            source(file.path(getOption(ipath), "app/help.R"), encoding = getOption("radiant.encoding"), local = TRUE)
        
        ## source analysis tools for each app
        for (file in list.files(file.path(getOption(ipath),"app/tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
            source(file, encoding = getOption("radiant.encoding"), local = TRUE)
    }
    
    ## ui creation functions
    ## source(file.path(getOption("radiant.path.biostat"),"app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)


    ## ## SC: we don't add any help
    
    ## help ui
    ## output$help_ui <- renderUI({
    ##     sidebarLayout(
    ##         sidebarPanel(
    ##             help_data_panel,
    ##             help_design_panel,
    ##             help_basics_panel,
    ##             help_model_panel,
    ##             uiOutput("help_text"),
    ##             width = 3
    ##         ),
    ##         mainPanel(
    ##             HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
    ##             htmlOutput("help_data"),
    ##             htmlOutput("help_design"),
    ##             htmlOutput("help_basics"),
    ##             htmlOutput("help_model"))
    ##     )
    ## })
    
    ## save state on refresh or browser close
    ## saveStateOnRefresh(session)

    ## SC: stop app when session is ended, browser close or refresh
    session$onSessionEnded(function() { stopApp() })
    
})

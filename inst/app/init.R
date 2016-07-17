## urls for menu
r_url_list <- getOption("radiant.url.list")

## r_url_list[["Survival Analysis"]] <- "biostat/survival/"
## r_url_list[["Mixed Models"]] <- "biostat/mixmod/"
## options(radiant.url.list = r_url_list); rm(r_url_list)


## biostat_ui <-
## 	tagList(
## 	  navbarMenu("Advanced Models","AdvModels",
## 	    tabPanel("Survival Analysis", uiOutput("survival")),
## 	    tabPanel("Mixed Models", uiOutput("mixmod"))
## 	  )
## 	)

init_state <- function(r_data) {
    
    ## initial plot height and width
    r_data$plot_height <- 600
    r_data$plot_width <- 600


    
    observe(
    {
        ## http://bioinfo1.med.unibs:3800/biostat/?code=anorexia -> opens anorexia.rda
        data_query <- parseQueryString(session$clientData$url_search)

        iData = "titanic"
        
        if ("code" %in% names(data_query))
        {
            data.in.pkg = data(package="radiant.biostat")$results[,"Item"]
            
            ## Check if the data file is in radiant.biostat
            iData = ifelse(data_query$code %in% data.in.pkg,data_query$code,"titanic")
            
        }
        
        
        df <- data(iData,package="radiant.biostat", envir = environment()) %>% get
        r_data[[ iData ]] <- df
        if(!is.null(attr(df,'description')))
            r_data[[paste(iData,"descr",sep="_")]] <- attr(df,'description')
        r_data$datasetlist <- iData
        
    })
    
    r_data$url <- NULL
    r_data
    
}

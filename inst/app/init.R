## urls for menu
r_url_list <- getOption("radiant.url.list")
## r_url_list[['Random sampling']] <- 'design/sampling/' r_url_list[['Sample size
## (single)']] <- 'design/sample-size/' r_url_list[['Sample size (compare)']] <-
## 'design/sample-size-comp/' r_url_list[['Design of Experiments']] <-
## 'design/doe/'
options(radiant.url.list = r_url_list)
rm(r_url_list)

## design menu design_ui <- tagList( navbarMenu('Design', 'DOE', tabPanel('Design
## of Experiments', uiOutput('doe')), '----', 'Sample', tabPanel('Random
## sampling', uiOutput('sampling')), tabPanel('Sample size (single)',
## uiOutput('sample_size')), tabPanel('Sample size (compare)',
## uiOutput('sample_size_comp')) ) )


init_data <- function(data_query) {
  
  ## Joe Cheng: 'Datasets can change over time (i.e., the changedata function).
  ## Therefore, the data need to be a reactive value so the other reactive functions
  ## and outputs that depend on these datasets will know when they are changed.'
  r_data <- reactiveValues()
  
  df_name <- getOption("radiant.init.data", default = "titanic")
  
  if (!missing(data_query) && "code" %in% names(data_query)) {
    data.in.pkg = data(package = "radiant.biostat")$results[, "Item"]
    
    ## Check if the data file is in radiant.biostat
    df_name = ifelse(data_query$code %in% data.in.pkg, data_query$code, "titanic")
  } else df_name <- getOption("radiant.init.data", default = "titanic")
  
  if (file.exists(df_name)) {
    df <- load(df_name) %>% get
    df_name <- basename(df_name) %>% {
      gsub(paste0(".", tools::file_ext(.)), "", ., fixed = TRUE)
    }
  } else {
    df <- data(list = df_name, package = "radiant.biostat", envir = environment()) %>% 
      get
  }
  
  
  r_data[[df_name]] <- df
  r_data[[paste0(df_name, "_descr")]] <- attr(df, "description")
  r_data$datasetlist <- df_name
  r_data$url <- NULL
  r_data
}




# init_state <- function(r_data) { ## initial plot height and width
# r_data$plot_height <- 600 r_data$plot_width <- 600 observe( { ##
# http://bioinfo1.med.unibs:3800/biostat/?code=anorexia -> opens anorexia.rda
# data_query <- parseQueryString(session$clientData$url_search) iData = 'titanic'
# if ('code' %in% names(data_query)) { data.in.pkg =
# data(package='radiant.biostat')$results[,'Item'] ## Check if the data file is
# in radiant.biostat iData = ifelse(data_query$code %in%
# data.in.pkg,data_query$code,'titanic') } df <-
# data(iData,package='radiant.biostat', envir = environment()) %>% get r_data[[
# iData ]] <- df if(!is.null(attr(df,'description')))
# r_data[[paste(iData,'descr',sep='_')]] <- attr(df,'description')
# r_data$datasetlist <- iData }) r_data$url <- NULL r_data }

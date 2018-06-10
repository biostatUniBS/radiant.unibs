## sourcing from radiant.data
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

options(radiant.path.data = system.file(package = "radiant.data"))
options(radiant.path.design = system.file(package = "radiant.design"))
options(radiant.path.basics = system.file(package = "radiant.basics"))
options(radiant.path.model = system.file(package = "radiant.model"))
options(radiant.path.multivariate = system.file(package = "radiant.multivariate"))

# sourcing from radiant base, note that path is set in base/global.R
# source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

## setting path for figures in help files
addResourcePath("figures_design", file.path(getOption("radiant.path.design"), "app/tools/help/figures/"))
addResourcePath("figures_basics", file.path(getOption("radiant.path.basics"), "app/tools/help/figures/"))
addResourcePath("figures_model", file.path(getOption("radiant.path.model"), "app/tools/help/figures/"))
addResourcePath("figures_multivariate", file.path(getOption("radiant.path.multivariate"), "app/tools/help/figures/"))

## setting path for www resources
addResourcePath("www_design", file.path(getOption("radiant.path.design"), "app/www/"))
addResourcePath("www_basics", file.path(getOption("radiant.path.basics"), "app/www/"))
addResourcePath("www_model", file.path(getOption("radiant.path.model"), "app/www/"))
addResourcePath("www_multivariate", file.path(getOption("radiant.path.multivariate"), "app/www/"))

## loading url information
source(file.path(getOption("radiant.path.design"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
source(file.path(getOption("radiant.path.basics"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
source(file.path(getOption("radiant.path.model"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
source(file.path(getOption("radiant.path.multivariate"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())


## We don't want some menus item

options(radiant.shared_ui =
          tagList(
            ## navbarMenu("R",
            ##            tabPanel("Report", uiOutput("report"), icon = icon("edit")),
            ##            tabPanel("Code", uiOutput("rcode"), icon = icon("code"))
            ## ),
            
            ## navbarMenu("", icon = icon("save"),
            ##            tabPanel(downloadLink("saveStateNav", " Save state", class = "fa fa-download")),
            ##            ## waiting for this feature in Shiny
            ##            # tabPanel(tags$a(id = "loadStateNav", href = "", class = "shiny-input-container",
            ##            #                 type='file', accept='.rmd,.Rmd,.md', list(icon("refresh"), "Refresh"))),
            ##            # tabPanel(uploadLink("loadState", "Load state"), icon = icon("folder-open")),
            ##            tabPanel(actionLink("shareState", "Share state", icon = icon("share"))),
            ##            tabPanel("View state", uiOutput("view_state"), icon = icon("user"))
            ## ),
            
            
            ## stop app *and* close browser window
            navbarMenu("", icon = icon("power-off"),
                       tabPanel(actionLink("stop_radiant", "Stop", icon = icon("stop"),
                                           onclick = "setTimeout(function(){window.close();}, 100); ")),
                       if (rstudioapi::isAvailable()) {
                         tabPanel(actionLink("stop_radiant_rmd", "Stop & Report", icon = icon("stop"),
                                             onclick = "setTimeout(function(){window.close();}, 100); "))
                       } else {
                         tabPanel("")
                       }
                       ##,
                       ## tabPanel(tags$a(id = "refresh_radiant", href = "#", class = "action-button",
                       ##                 list(icon("refresh"), "Refresh"), onclick = "window.location.reload();"))
                       ## ,
                       ##  ## had to remove class = "action-button" to make this work
                       ##  tabPanel(tags$a(id = "new_session", href = "./", target = "_blank",
                       ##                  list(icon("plus"), "New session")))
            )
          )
)


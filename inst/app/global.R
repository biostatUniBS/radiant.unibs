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


## from radian.data -> manage_ui.R

output$ui_Manage <- renderUI({
  data_types_in <- c(
    "rds" = "rds", "rda (rdata)" = "rda", "csv" = "csv",
    "clipboard" = "clipboard", "examples" = "examples",
    "rda (url)" = "url_rda", "csv (url)" = "url_csv",
    "feather" = "feather", "from global workspace" = "from_global",
    "radiant state file" = "state" 
  )
  data_types_out <- c(
    "rds" = "rds", "rda" = "rda", "csv" = "csv",
    "clipboard" = "clipboard", "feather" = "feather",
    "to global workspace" = "to_global", "radiant state file" = "state"
  )
  if (!isTRUE(getOption("radiant.local"))) {
    data_types_in <- data_types_in[-which(data_types_in == "from_global")]
    data_types_out <- data_types_out[-which(data_types_out == "to_global")]
  }
  if (!("feather" %in% rownames(utils::installed.packages()))) {
    data_types_in <- data_types_in[-which(data_types_in == "feather")]
    data_types_out <- data_types_out[-which(data_types_out == "feather")]
  }

  tagList(
    wellPanel(
      selectInput("dataType", label = "Load data of type:", data_types_in, selected = "rds"),
      conditionalPanel(
        condition = "input.dataType != 'clipboard' &&
                     input.dataType != 'examples'",
        conditionalPanel(
          "input.dataType == 'csv' | input.dataType == 'url_csv'",
          with(tags, table(
            td(checkboxInput("man_header", "Header", TRUE)),
            td(HTML("&nbsp;&nbsp;")),
            td(checkboxInput("man_str_as_factor", "Str. as Factor", TRUE))
          )),
          with(tags, table(
            td(selectInput("man_sep", "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), ",", width = "100%")),
            td(selectInput("man_dec", "Decimal:", c(Period = ".", Comma = ","), ".", width = "100%")),
            width = "100%"
          )),
          numericInput(
            "man_n_max", label = "Maximum rows to read:",
            value = Inf, max = Inf, step = 1000
          )
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(
        condition = "input.dataType == 'clipboard'",
        uiOutput("ui_clipboard_load")
      ),
      conditionalPanel(
        condition = "input.dataType == 'from_global'",
        uiOutput("ui_from_global")
      ),
      conditionalPanel(
        condition = "input.dataType == 'examples'",
        actionButton("loadExampleData", "Load", icon = icon("upload"))
      ),
      conditionalPanel(
        condition = "input.dataType == 'state'",
        uiOutput("ui_state_load"),
        uiOutput("refreshOnUpload")
      )
    ),
    #wellPanel(
    #  selectInput("saveAs", label = "Save data to type:", data_types_out, selected = "rds"),
    #  conditionalPanel(
    #    condition = "input.saveAs == 'clipboard'",
    #    uiOutput("ui_clipboard_save")
    #  ),
    #  conditionalPanel(
    #    condition = "input.saveAs == 'state'",
    #    HTML("<label>Save radiant state file (.rda):</label><br/>"),
    #    uiOutput("ui_man_state_save")
    #  ),
    #  conditionalPanel(
    #    condition = "input.saveAs == 'to_global'",
    #    uiOutput("ui_to_global")
    #  ),
    #  conditionalPanel(
    #    condition = "input.saveAs != 'clipboard' &&
    #                 input.saveAs != 'state' &&
    #                 input.saveAs != 'to_global'",
    #    uiOutput("ui_man_save_data")
    #  )
    #),
    # conditionalPanel(
      # "output.is_browser == false",
      #wellPanel(
      #  checkboxInput("man_show_log", "Show R-code", FALSE)
      #),
    # ),
    wellPanel(
      checkboxInput("man_show_remove", "Remove data from memory", FALSE),
      conditionalPanel(
        condition = "input.man_show_remove == true",
        uiOutput("uiRemoveDataset"),
        actionButton("removeDataButton", "Remove data", icon = icon("trash"), class = "btn-danger")
      )
    ),
    help_and_report(
      modal_title = "Manage",
      fun_name = "manage",
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/manage.md")),
      lic = "by-sa"
    )
  )
})

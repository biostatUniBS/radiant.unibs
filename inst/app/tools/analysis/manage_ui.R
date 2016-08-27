## Modify radiant.data/inst/app/tools/data/manage_ui.R

## Lines: 109-180

output$ui_Manage <- renderUI({
  data_types_in <- c("rda" = "rda", "rds" = "rds", "state" = "state", "csv" = "csv",
                  "clipboard" = "clipboard", "from global workspace" = "from_global",
                  "examples" = "examples", "rda (url)" = "url_rda",
                  "csv (url)" = "url_csv")
  data_types_out <- c("rda" = "rda", "rds" = "rds", "state" = "state", "csv" = "csv",
                  "clipboard" = "clipboard", "to global workspace" = "to_global")
  if (!isTRUE(getOption("radiant.local"))) {
    data_types_in <- data_types_in[-which(data_types_in == "from_global")]
    data_types_out <- data_types_out[-which(data_types_out == "to_global")]
  }

  tagList(
    wellPanel(
      selectInput("dataType", label = "Load data of type:", data_types_in, selected = "rda"),
      conditionalPanel(condition = "input.dataType != 'clipboard' &&
                                    input.dataType != 'examples'",
        conditionalPanel("input.dataType == 'csv' | input.dataType == 'url_csv'",
          with(tags, table(td(checkboxInput('man_header', 'Header', TRUE)),
            td(HTML("&nbsp;&nbsp;")),
            td(checkboxInput("man_str_as_factor", "Str. as Factor", TRUE)))),
          checkboxInput("man_read.csv", "use read.csv", FALSE),
          radioButtons("man_sep", "Separator:", c(Comma=",", Semicolon=";", Tab="\t"),
                       ",", inline = TRUE),
          radioButtons("man_dec", "Decimal:", c(Period=".", Comma=","),
                       ".", inline = TRUE)
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        uiOutput("ui_clipboard_load")
      ),
      conditionalPanel(condition = "input.dataType == 'from_global'",
        uiOutput("ui_from_global")
      ),
      conditionalPanel(condition = "input.dataType == 'examples'",
        actionButton("loadExampleData", "Load", icon = icon("upload"))
      ),
      conditionalPanel(condition = "input.dataType == 'state'",
        fileInput("uploadState", "Load previous app state:",  accept = ".rda"),
        uiOutput("refreshOnUpload")
      )
    ),
    ## wellPanel(
    ##   selectInput("saveAs", label = "Save data to type:", data_types_out, selected = "rda"),
    ##   conditionalPanel(condition = "input.saveAs == 'clipboard'",
    ##     uiOutput("ui_clipboard_save")
    ##   ),
    ##   conditionalPanel(condition = "input.saveAs == 'state'",
    ##     HTML("<label>Save current app state:</label><br/>"),
    ##     downloadButton("saveState", "Save")
    ##   ),
    ##   conditionalPanel(condition = "input.saveAs == 'to_global'",
    ##     uiOutput("ui_to_global")
    ##   ),
    ##   conditionalPanel(condition = "input.saveAs != 'clipboard' &&
    ##                                 input.saveAs != 'state' &&
    ##                                 input.saveAs != 'to_global'",
    ##     downloadButton("downloadData", "Save")
    ##   )
    ## ),
    wellPanel(
      checkboxInput("man_show_remove", "Remove data from memory", FALSE),
      conditionalPanel(condition = "input.man_show_remove == true",
        uiOutput("uiRemoveDataset"),
        actionButton("removeDataButton", "Remove data")
      )
    )
   ## ,
   ##  help_modal("Manage","manage_help",inclMD(file.path(getOption("radiant.path.data"),"app/tools/help/manage.md")))
  )
})

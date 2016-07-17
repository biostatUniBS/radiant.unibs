## ui for menus in radiant
do.call(navbarPage,
  c("Biostatistics (UniBS)", getOption("radiant.nav_ui"),  basics_ui, model_ui, design_ui,
    ## biostat_ui, ## no menu yet
    ## multivariate_ui,
    getOption("radiant.shared_ui") #,    help_menu("help_ui")
    )
)

#' Evaluate associations between categorical variables
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var1 A categorical variable
#' @param var2 Another categorical variable
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in cross_tabs as an object of class cross_tabs
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#' result <- newspaper %>% cross_tabs("Income", "Newspaper")
#'
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export
#'
cross_tabs <- function(dataset, var1, var2, tab = NULL, data_filter = "",
                       envir = parent.frame(),
                       samples="independent") {

  if (is.table(tab)) {
    df_name <- deparse(substitute(tab))

    if (missing(var1) || missing(var2)) {
      nm <- names(dimnames(tab))
      var1 <- nm[1]
      var2 <- nm[2]
    }

    if (is_empty(var1) || is_empty(var2)) {
      return("The provided table does not have dimension names. See ?cross_tabs for an example" %>%
        add_class("cross_tabs"))
    }
  } else {
    df_name <- if (!is_string(dataset)) deparse(substitute(dataset)) else dataset
    dataset <- get_data(dataset, c(var1, var2), filt = data_filter, envir = envir)

    ## Use simulated p-values when
    # http://stats.stackexchange.com/questions/100976/n-1-pearsons-chi-square-in-r
    # http://stats.stackexchange.com/questions/14226/given-the-power-of-computers-these-days-is-there-ever-a-reason-to-do-a-chi-squa/14230#14230
    # http://stats.stackexchange.com/questions/62445/rules-to-apply-monte-carlo-simulation-of-p-values-for-chi-squared-test

    not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
    if (length(not_vary) > 0) {
      return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
               add_class("cross_tabs"))
    }
    # if (any(summarise_all(dataset, funs(does_vary)) == FALSE)) {
    #   return("One or more selected variables show no variation. Please select other variables." %>%
    #     add_class("cross_tabs"))
    # }

    tab <- table(dataset[[var1]], dataset[[var2]])
    tab[is.na(tab)] <- 0
    tab <- tab[, colSums(tab) > 0] %>%
      {.[rowSums(.) > 0, ]} %>%
      as.table()
    ## dataset not needed in summary or plot
    rm(dataset)
  }

  ## if(samples=="paired" & nrow(tab) == 2 & ncol(tab) == 2)
  if(samples=="paired")
    {
      cst <- sshhr(mcnemar.test(tab))
      cst$observed = tab
    }
      else
    {
        cst <- sshhr(chisq.test(tab, correct = FALSE))
        ## adding the % deviation table
        cst$chi_sq <- with(cst, (observed - expected) ^ 2 / expected)
    }


  res <- tidy(cst) %>%
    mutate(parameter = as.integer(parameter))
  elow <- sum(cst$expected < 5)

  if (elow > 0) {
    set.seed(1970) ## for repetability in exams
    res$p.value <- chisq.test(cst$observed, simulate.p.value = TRUE, B = 2000) %>% tidy() %>% .$p.value
    res$parameter <- paste0("*", res$parameter, "*")
  }

  res$samples = samples

  rm(envir)

  as.list(environment()) %>% add_class("cross_tabs")
}



summary.cross_tabs <- function(object, check = "", dec = 2, ...) {

  if (is.character(object)) return(object)
  cat("Cross-tabs\n")
  cat("Data     :", object$df_name, "\n")
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter   :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Test     :", object$samples, "\n")
  cat("Variables:", paste0(c(object$var1, object$var2), collapse = ", "), "\n")
  #cat("Null hyp.: there is no association between", object$var1, "and", object$var2, "\n")
  #cat("Alt. hyp.: there is an association between", object$var1, "and", object$var2, "\n")

  if(object$samples=="paired")
    check = check[!check %in% c("expected","chi_sq","dev_std","col_perc","row_perc","perc")]

  rnames <- object$cst$observed %>% rownames() %>% c(., "Total")
  cnames <- object$cst$observed %>% colnames() %>% c(., "Total")

  if ("observed" %in% check) {
    cat("\nObserved:\n")
    observed <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      format(big.mark = ",", scientific = FALSE)

    names(attributes(observed)$dimnames) <- c(object$var1, object$var2)
    print(observed, quote = FALSE)
  }

  if ("expected" %in% check) {
    cat("\nExpected: (row total x column total) / total\n")
    expected <- object$cst$expected %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      round(dec) %>%
      format(big.mark = ",", scientific = FALSE)

    names(attributes(expected)$dimnames) <- c(object$var1, object$var2)
    print(expected, quote = FALSE)
  }

  if ("chi_sq" %in% check) {
    cat("\nContribution to chi-squared: (o - e)^2 / e\n")
    chi_sq <- object$cst$chi_sq %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      round(dec) %>%
      format(big.mark = ",", scientific = FALSE)

    names(attributes(chi_sq)$dimnames) <- c(object$var1, object$var2)
    print(chi_sq, quote = FALSE)
  }

  if ("dev_std" %in% check) {
    cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
    resid <- round(object$cst$residuals, dec) ## standardized residuals
    names(attributes(resid)$dimnames) <- c(object$var1, object$var2)
    print(resid)
  }

  if ("row_perc" %in% check) {
    cat("\nRow percentages:\n")
    row_perc <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      {. / .[, "Total"]} %>%
      round(dec)

    names(attributes(row_perc)$dimnames) <- c(object$var1, object$var2)
    print(row_perc)
  }

  if ("col_perc" %in% check) {
    cat("\nColumn percentages:\n")
    col_perc <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      {t(.) / .["Total", ]} %>%
      t() %>%
      round(dec)

    names(attributes(col_perc)$dimnames) <- c(object$var1, object$var2)
    print(col_perc)
  }

  if ("perc" %in% check) {
    cat("\nProbability table:\n")
    perc <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      {. / .["Total", "Total"]} %>%
      round(dec)

    names(attributes(perc)$dimnames) <- c(object$var1, object$var2)
    print(perc)
  }

  object$res <- format_df(object$res, dec = dec + 1, mark = ",")

  if (object$res$p.value < .001) object$res$p.value <- "< .001"
  cat(paste0("\nChi-squared: ", object$res$statistic, " df(", object$res$parameter, "), p.value ", object$res$p.value), "\n\n")
  cat(paste(sprintf("%.1f", 100 * (object$elow / length(object$cst$expected))), "% of cells have expected values below 5\n"), sep = "")
  if (object$elow > 0) cat("p.value for chi-squared statistics obtained using simulation (2,000 replicates)")
}


## cross_tabs <- function(dataset, var1, var2,
##                        data_filter = "",
##                        samples="indipendent") {

## 	dat <- getdata(dataset, c(var1, var2), filt = data_filter)
##   if (!is_string(dataset)) dataset <- "-----"

##   ## Use simulated p-values when
##   # http://stats.stackexchange.com/questions/100976/n-1-pearsons-chi-square-in-r
##   # http://stats.stackexchange.com/questions/14226/given-the-power-of-computers-these-days-is-there-ever-a-reason-to-do-a-chi-squa/14230#14230
##   # http://stats.stackexchange.com/questions/62445/rules-to-apply-monte-carlo-simulation-of-p-values-for-chi-squared-test

##   ## creating and cleaning up the table
## 	tab <- table(dat[[var1]], dat[[var2]])
## 	tab[is.na(tab)] <- 0
## 	tab <- tab[ ,colSums(tab) > 0] %>% {.[rowSums(.) > 0, ]} %>% as.table


##     ## if(samples=="paired" & nrow(tab) == 2 & ncol(tab) == 2)
##     if(samples=="paired")
##     {
##         cst <- sshhr( mcnemar.test(tab) )
##     }
##     else
##     {
##         cst <- sshhr( chisq.test(tab, correct = FALSE) )

##         ## adding the % deviation table
##         cst$deviation <- with(cst, (observed-expected) / expected)
##         cst$chi_sq	<- with(cst, (observed - expected)^2 / expected)
##     }
##     ## dat not needed in summary or plot
##     rm(dat)

##     environment() %>% as.list %>% add_class(c("cross_tabs",class(.)))
## }

#' Summary method for the cross_tabs function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/cross_tabs.html} for an example in Radiant

#' @param object Return value from \code{\link{cross_tabs}}
#' @param check Show table(s) for variables var1 and var2. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "dev_perc" for the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#' summary(result, check = c("observed","expected","chi_sq"))
#' newspaper %>% cross_tabs("Income", "Newspaper") %>% summary("observed")
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' ## @export
## summary.cross_tabs <- function(object,
##                                check = "",
##                                ...) {

##   # object <- result

##   cat("Cross-tabs\n")
## 	cat("Data     :", object$dataset, "\n")
## 	if (object$data_filter %>% gsub("\\s","",.) != "")
## 		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
## 	cat("Variables:", paste0(c(object$var1, object$var2), collapse=", "), "\n")
##     cat("Test     :", object$samples, "\n")
##     cat("Null hyp.: there is no association between", object$var1, "and", object$var2, "\n")
## 	cat("Alt. hyp.: there is an association between", object$var1, "and", object$var2, "\n")

##     if(object$samples=="paired")
##         check = ""


## 	object$cst$observed %>% rownames %>% c(., "Total") -> rnames
## 	object$cst$observed %>% colnames %>% c(., "Total") -> cnames

## 	if ("observed" %in% check) {
## 		cat("\nObserved:\n")
## 		object$cst$observed %>%
## 			rbind(colSums(.)) %>%
## 			set_rownames(rnames) %>%
## 			cbind(rowSums(.)) %>%
## 			set_colnames(cnames) %>%
## 			print
## 	}

## 	if ("expected" %in% check) {
## 		cat("\nExpected: (row total x column total) / total\n")
## 		object$cst$expected %>%
## 			rbind(colSums(.)) %>%
## 			set_rownames(rnames) %>%
## 			cbind(rowSums(.)) %>%
## 			set_colnames(cnames) %>%
## 			round(2) %>%
## 			print
## 	}

## 	if ("chi_sq" %in% check) {
## 		cat("\nContribution to chi-squared: (o - e)^2 / e\n")
## 		# ((object$cst$observed - object$cst$expected)^2 / object$cst$expected) %>%
## 		object$cst$chi_sq %>%
## 			rbind(colSums(.)) %>%
## 			set_rownames(rnames) %>%
## 			cbind(rowSums(.)) %>%
## 			set_colnames(cnames) %>%
## 			round(2) %>%
## 			print
## 	}

## 	if ("dev_std" %in% check) {
## 		cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
## 		print(round(object$cst$residuals, 2)) 	# these seem to be the correct std.residuals
## 	}

## 	if ("row_perc" %in% check) {
## 		cat("\nRow percentages:\n")
## 		object$cst$observed %>%
## 			rbind(colSums(.)) %>%
## 			set_rownames(rnames) %>%
## 			cbind(rowSums(.)) %>%
## 			set_colnames(cnames) %>%
## 			{. / .[,"Total"]} %>%
## 			round(2) %>%
## 			print
## 	}

## 	if ("col_perc" %in% check) {
## 		cat("\nColumn percentages:\n")
## 		object$cst$observed %>%
## 			rbind(colSums(.)) %>%
## 			set_rownames(rnames) %>%
## 			cbind(rowSums(.)) %>%
## 			set_colnames(cnames) %>%
## 			{t(.) / .["Total",]} %>% t %>%
## 			round(2) %>%
## 			print
## 	}

## 	if ("perc" %in% check) {
## 		cat("\nProbability table:\n")
## 		object$cst$observed %>%
## 			rbind(colSums(.)) %>%
## 			set_rownames(rnames) %>%
## 			cbind(rowSums(.)) %>%
## 			set_colnames(cnames) %>%
## 			{. / .["Total","Total"]} %>%
## 			round(2) %>%
## 			print
## 	}

## 	# if ("dev_perc" %in% check) {
## 	# 	cat("\nDeviation %: (o - e) / e\n")
## 	# 	print(round(object$cst$deviation, 2)) 	# % deviation
## 	# }

## 	# res <- object$cst %>% tidy %>% round(3)
## 	res <- object$cst %>% tidy
## 	elow <- sum(object$cst$expected < 5)

##     if (elow > 0) {
##         set.seed(1234) ## so I get repetible results!!!
##   	res$p.value <- chisq.test(object$cst$observed, simulate.p.value = TRUE, B = 2000) %>% tidy %>% .$p.value
##   	res$parameter <- paste0("*",res$parameter,"*")
##   }

##   round_fun <- function(x) is.na(x) || is.character(x)
## 	res[!sapply(res, round_fun)] %<>% round(3)

## 	if (res$p.value < .001) res$p.value  <- "< .001"
## 	cat(paste0("\nChi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", res$p.value), "\n\n")
## 	cat(paste(sprintf("%.1f",100 * (elow / length(object$cst$expected))),"% of cells have expected values below 5\n"), sep = "")

##     if(object$samples != "paired")
##         cat(paste(sprintf("%.1f",100 * (elow / length(object$cst$expected))),"% of cells have expected values below 5\n"), sep = "")

##     if (elow > 0) cat("p.value for chi-squared statistics obtained using simulation (2000 replicates)")
## }

## #' Plot method for the cross_tabs function
## #'
## #' @details See \url{http://vnijs.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
## #'
## #' @param x Return value from \code{\link{cross_tabs}}
## #' @param check Show plots for variables var1 and var2. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "row_perc", "col_perc", and "perc" for row, column, and table percentages respectively
## #' @param shiny Did the function call originate inside a shiny app
## #' @param ... further arguments passed to or from other methods
## #'
## #' @examples
## #' result <- cross_tabs("newspaper", "Income", "Newspaper")
## #' plot(result, check = c("observed","expected","chi_sq"))
## #' newspaper %>% cross_tabs("Income", "Newspaper") %>% plot(c("observed","expected"))
## #'
## #' @seealso \code{\link{cross_tabs}} to calculate results
## #' @seealso \code{\link{summary.cross_tabs}} to summarize results
## #'
## #' @export
## plot.cross_tabs <- function(x,
##                             check = "",
##                             shiny = FALSE,
##                             ...) {

## 	object <- x; rm(x)

## 	gather_table <- function(tab) {
## 		tab %>%
## 			data.frame(., check.names = FALSE) %>%
## 			mutate(rnames = rownames(.)) %>%
## 			{sshhr( gather_(., "variable", "values", setdiff(colnames(.),"rnames")) )}
## 	}

## 	plot_list <- list()

## 	if ("observed" %in% check) {

## 		fact_names <- object$cst$observed %>% dimnames %>% as.list
##     tab <- as.data.frame(object$cst$observed, check.names = FALSE)
## 		colnames(tab)[1:2] <- c(object$var1, object$var2)
##  	  tab[[1]] %<>% as.factor %>% factor(levels = fact_names[[1]])
## 		tab[[2]] %<>% as.factor %>% factor(levels = fact_names[[2]])

## 		plot_list[['observed']] <-
## 		  ggplot(tab, aes_string(x = object$var2, y = "Freq", fill = object$var1)) +
## 		    geom_bar(stat="identity", position = "fill", alpha = .7) +
## 		    labs(list(title = paste("Observed frequencies for ",object$var2," versus ",object$var1, sep = ""),
## 				  	 x = object$var2, y = "", fill = object$var1)) +
## 		    scale_y_continuous(labels = scales::percent)
## 	}

## 	if ("expected" %in% check) {
## 		fact_names <- object$cst$expected %>% dimnames %>% as.list
##   	tab <- gather_table(object$cst$expected)
## 		tab$rnames %<>% as.factor %>% factor(levels = fact_names[[1]])
## 		tab$variable %<>% as.factor %>% factor(levels = fact_names[[2]])
## 		plot_list[['expected']] <-
## 		  # ggplot(tab, aes_string(x = "rnames", y = "values", fill = "variable")) +
## 		  ggplot(tab, aes_string(x = "variable", y = "values", fill = "rnames")) +
## 		    geom_bar(stat="identity", position = "fill", alpha = .7) +
## 		    labs(list(title = paste("Expected frequencies for ",object$var2," versus ",object$var1, sep = ""),
## 		         x = object$var2, y = "", fill = object$var1)) +
## 		    scale_y_continuous(labels = scales::percent)
## 	}

## 	if ("chi_sq" %in% check) {
##   	tab <- as.data.frame(object$cst$chi_sq, check.names = FALSE)
## 		colnames(tab)[1:2] <- c(object$var1, object$var2)
## 		plot_list[['chi_sq']] <-
## 		  ggplot(tab, aes_string(x = object$var2, y = "Freq", fill = object$var1)) +
## 		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
## 		    labs(list(title = paste("Contribution to chi-squared for ",object$var2," versus ",object$var1, sep = ""),
## 		         x = object$var2, y = ""))
##   }

## 	if ("dev_std" %in% check) {
##   	tab <- as.data.frame(object$cst$residuals, check.names = FALSE)
## 		colnames(tab)[1:2] <- c(object$var1, object$var2)
## 		plot_list[['dev_std']] <-
## 		  ggplot(tab, aes_string(x = object$var2, y = "Freq", fill = object$var1)) +
## 		    geom_bar(stat="identity", position = "dodge", alpha = .7) +
## 		    geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
## 		    geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
## 		    geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
## 		    labs(list(title = paste("Deviation standardized for ",object$var2," versus ",object$var1, sep = ""),
## 		         x = object$var2, y = ""))
## 	}

## 	# if ("dev_perc" %in% check) {
##  #  	tab <- as.data.frame(object$cst$deviation, check.names = FALSE)
## 	# 	colnames(tab)[1:2] <- c(object$var1, object$var2)
## 	# 	ymax <- max(abs(tab$Freq))
## 	# 	ylim <- if (ymax < 1) c(-1,1) else c(-ymax, ymax)
## 	# 	plot_list[['dev_prec']] <-
## 	# 	  ggplot(tab, aes_string(x = object$var1, y = "Freq", fill = object$var2)) +
## 	# 	    geom_bar(stat="identity", position = "dodge", alpha = .7) +
## 	# 	    labs(list(title = paste("Deviation % for ",object$var2," versus ",object$var1, sep = ""),
## 	# 	         x = object$var1, y = "")) +
## 	# 	    scale_y_continuous(labels = scales::percent, limits = ylim)
##  #  }

## 	if ("row_perc" %in% check) {
## 		plot_list[['row_perc']] <-
## 	  	as.data.frame(object$cst$observed, check.names = FALSE) %>%
## 	  	  group_by_("Var1") %>%
## 	  	  mutate(perc = Freq / sum(Freq)) %>%
## 			  ggplot(aes_string(x = "Var2", y = "perc", fill = "Var1")) +
## 			    geom_bar(stat="identity", position = "dodge", alpha = .7) +
## 			 		labs(fill = object$var1) +
## 			 		scale_y_continuous(labels = scales::percent) +
## 			 		ylab("Percentage") + xlab(object$var2) +
## 			    ggtitle("Row percentages")
## 	}

## 	if ("col_perc" %in% check) {
## 		plot_list[['col_perc']] <-
## 	  	as.data.frame(object$cst$observed, check.names = FALSE) %>%
## 	  	  group_by_("Var2") %>%
## 	  	  mutate(perc = Freq / sum(Freq)) %>%
## 			  ggplot(aes_string(x = "Var2", y = "perc", fill = "Var1")) +
## 			    geom_bar(stat="identity", position = "dodge", alpha = .7) +
## 			 		labs(fill = object$var1) +
## 			 		scale_y_continuous(labels = scales::percent) +
## 			 		ylab("Percentage") + xlab(object$var2) +
## 			    ggtitle("Column percentages")
## 	}

## 	if ("perc" %in% check) {
## 		plot_list[['perc']] <-
## 	  	as.data.frame(object$cst$observed, check.names = FALSE) %>%
## 	  	  mutate(perc = Freq / sum(Freq)) %>%
## 			  ggplot(aes_string(x = "Var2", y = "perc", fill = "Var1")) +
## 			    geom_bar(stat="identity", position = "dodge", alpha = .7) +
## 			 		labs(fill = object$var1) +
## 			 		scale_y_continuous(labels = scales::percent) +
## 			 		ylab("Percentage") + xlab(object$var2) +
## 			    ggtitle("Table percentages")
## 	}

## 	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
## 	  { if (shiny) . else print(.) }
## }

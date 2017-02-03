read_plan <- function(file){
  readLines(file, warn = FALSE)
}

extract_task <- function(object){
  tmp <- grepl(pattern = "```\\{todo", x = object)
  tmp2 <- c(1, grep(pattern = "```", x = object))
  tmp3 <- ifelse(tmp[tmp2], TRUE, FALSE)
  tmp4 <- rep(tmp3, times = diff(c(tmp2, length(object) + 1)))
  return(object[tmp4])
}

split_task <- function(object) {
  tmp <- grep(pattern = '```\\{todo', x = object)
  tmp2 <- rep(1:length(tmp), times = diff(c(tmp, length(object) + 1)))
  if (tmp[1] > 1){object <- object[-c(1:(tmp[1] - 1))]}
  return(split(x = object, f = tmp2))
}

extract_info <- function(object,
                         info = c("proj_tags", "proj_start", "proj_end", "todo", "info", "tags",
                                  "personnel", "cost", "deadline", "priority", "start", "end")) {
  if (info == "todo") {
    #browser()
    pattern <- "```\\{todo|\\}"
    tmp <- grepl(pattern = pattern, x = object)
    if (any(tmp)) {
      out <- gsub("^\\s+|\\s+$", "",
                  gsub(pattern = pattern,
                       replacement = "",
                       x = object[tmp]))
    } else {out <- ""}
  } else {
    pattern <- paste("^", info, " =", sep = "")
    tmp <- grepl(pattern = pattern, x = object)
    if (any(tmp)) {
      out <- gsub("^\\s+|\\s+$", "", gsub(pattern = pattern, replacement = "", x = object[tmp]))
    } else {out <- ""}
  }

  return(out)
}

get_task <- function(file) {
  #browser()

  ## read file(s)
  all <- read_plan(file = file)

  ## extract information related to task
  all_task <- extract_task(object = all)

  if (length(all_task) == 0) {
    out <- data.frame(
      task = character(),
      info = character(),
      tags = character(),
      personnel = character(),
      cost = character(),
      deadline = character(),
      priority = character(),
      start = character(),
      end = character()
    )
  } else {
    ## get all tasks
    list_task <- split_task(all_task)

    ## extract todo
    todo <- sapply(list_task, extract_info, info = "todo")

    ## extract task info
    todo_info <- sapply(list_task, extract_info, info = "info")

    ## extract tags
    proj_tags <- extract_info(all_task, info = "proj_tags")
    tmp <- paste(proj_tags, sapply(list_task, extract_info, info = "tags"), sep = "|")
    todo_tags <- sapply(tmp, function(x) paste(unique(unlist(strsplit(tmp, split = "[|]"))), collapse = "|"))

    ## extract personnel
    todo_person <- sapply(list_task, extract_info, info = "personnel")

    ## extract cost
    todo_cost <- sapply(list_task, extract_info, info = "cost")

    ## extract deadline
    todo_deadline <- sapply(list_task, extract_info, info = "deadline")

    ## extract priority
    todo_priority <- sapply(list_task, extract_info, info = "priority")

    ## extract task start
    todo_start <- sapply(list_task, extract_info, info = "start")

    ## extract task end
    todo_end <- sapply(list_task, extract_info, info = "end")

    ## output
    out <- data.frame(task = todo,
                      info = todo_info,
                      tags = todo_tags,
                      personnel = todo_person,
                      cost = todo_cost,
                      deadline = todo_deadline,
                      priority = todo_priority,
                      start = todo_start,
                      end = todo_end)
  }
  return(out)
}

summarize_task <- function(task_info){
  #browser()
  require(lubridate)
  require(dplyr)
  cutoff <- ymd_hms(Sys.time())
  out <- task_info %>%
    filter(info != "" | personnel != "" | cost != "" | deadline != "" | priority != "" | start != "" | end != "") %>%
    mutate(start = ymd_hm(start),
           end   = ymd_hm(end),
           deadline = ymd_hm(deadline),
           cutoff = cutoff,
           started = ifelse(!is.na(start) & start < cutoff, "Yes", "No"),
           completed = ifelse(!is.na(end) & end < cutoff, "Yes", "No"),
           time_left = paste(round(ifelse(completed == "Yes", difftime(deadline, end, units = "day"),
                                          difftime(deadline, cutoff, units = "day")), digits = 1), "days"),
           status = ifelse(is.na(start), "Not scheduled yet",
                           ifelse(started == "No", "Not started yet",
                                  ifelse(completed == "Yes", "Completed", "Ongoing"))))

  ## output
  return(out)
}

#' Addin to insert date
#' @export
insert_date_addin <- function() {
  require(shiny)
  require(miniUI)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Insert Date",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      dateInput("date", "Date:", value = Sys.Date())
    )
  )

  ## server
  server <- function(input, output, session){
    ## stop app when click "Done"
    observeEvent(input$done, {
      rstudioapi::insertText(paste(as.character(input$date), "00:00"))
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
}

ggplot_color <- function(n){
  ## thanks to John Colby (http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette)
  hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
}

plot_task <- function(summary_info) {
  if (is.null(summary_info)) {return("No task available")} else {
    require(dplyr)
    require(ggplot2)
    plotdat <- summary_info %>%
      filter(!is.na(deadline)) %>%
      mutate(id = n():1,
             start = (start - deadline)/ddays(1),
             end = ifelse(completed == "Yes", (end - deadline)/ddays(1),
                          (cutoff - deadline)/ddays(1)),
             current = (cutoff - deadline)/ddays(1))
    xrange <- range(c(plotdat$start, plotdat$end), na.rm = TRUE)

    ggplot(data = plotdat, aes(y = id, colour = priority)) +
      xlim(c(xrange[1] - 10, pmax(xrange[2], 1))) +
      ylim(c(0.5,(nrow(plotdat)))) +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_segment(data = subset(plotdat, !is.na(start) & !is.na(end)), aes(x = start, xend = end, yend = id)) +
      geom_point(data = subset(plotdat, status %in% c("Not scheduled yet", "Not started yet", "Ongoing")), aes(x = current), shape = 17) +
      geom_point(data = subset(plotdat, !is.na(start)), aes(x = start), shape = 4) +
      geom_point(data = subset(plotdat, status == "Completed"), aes(x = end), shape = 16) +
      geom_text(aes(label = task, x = xrange[1] - 10), colour = "black") +
      xlab("Days from deadline") + ylab("") +
      scale_color_manual(breaks = c("high", "low", "medium", ""),
                         values = c(ggplot_color(3), "black")) +
      theme(axis.line.y = element_blank(),
            axis.line.x = element_line(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor.y = element_line(colour = "grey"),
            legend.position = "none")
  }
}

#' @export
manage_task_addin <- function() {
  require(shiny)
  require(miniUI)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Task Summary",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),

    miniTabstripPanel(
      miniTabPanel("Input", icon = icon("folder-open-o"),
                   miniContentPanel(
                     checkboxInput('file', 'File', TRUE),
                     textInput("path", "Path:"),
                     actionButton("data_browse", "Data browse")
                   )
      ),

      miniTabPanel("Table", icon = icon("table"),
                   miniContentPanel(
                     dataTableOutput("table")
                   )
      ),

      miniTabPanel("Chart", icon = icon("area-chart"),
                   miniContentPanel(
                     plotOutput("plot", width = "100%")
                   )
      )
    )
  )

  ## server
  server <- function(input, output, session){
    path <- rstudioapi::getActiveDocumentContext()$path
    observe({
      updateTextInput(session, "path",  value = path)

      if (input$file == TRUE) {
        if (input$data_browse == 0) return()
        updateTextInput(session, "path",  value = file.choose2())
      } else {
        if (input$data_browse == 0) return()
        updateTextInput(session, "path",  value = tcltk::tk_choose.dir())
      }
    })

    output$table <- renderDataTable(
      {
        if (input$file) {
          task <- get_task(file = input$path)
        } else {
          tmp <- list.files(path = input$path, pattern = ".Rmd", recursive = TRUE)
          task <- do.call("rbind",
                          lapply(tmp, function(x) get_task(file.path(input$path, x))))
        }

        summarize_task(task)[, c("task", "tags", "priority", "deadline",
                                 "status", "time_left")]
      }
    )
    output$plot <- renderPlot(
      {
        if (input$file) {
          task <- get_task(file = input$path)
        } else {
          tmp <- list.files(path = input$path, pattern = ".Rmd", recursive = TRUE)
          task <- do.call("rbind",
                          lapply(tmp, function(x) get_task(file.path(input$path, x))))
        }

        out <- summarize_task(task)
        if (nrow(out) == 0) {
          errorMessage("data", "No task available")
        } else {
          plot_task(out)
        }
      }
    )
    ## stop app when click "Done"
    observeEvent(input$done, {
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
}

errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}

file.choose2 <- function(...) {
  pathname <- NULL;
  tryCatch({
    pathname <- file.choose();
  }, error = function(ex) {
  })
  pathname;
}

copy_snippets <- function() {
  path_from <- file.path(system.file(package = "taskeR"), "snippets")
  tmp <- list.files(path_from)
  for (i in (1:length(tmp))) {
    file.copy(from = file.path(path_from, tmp[i]),
              to = file.path(Sys.getenv("HOME"), ".R", "snippets"),
              overwrite = TRUE)
  }
}

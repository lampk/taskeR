
#' To extract all task from an .Rmd file
#'
#' @param file Path to an .Rmd file
#' @return A 'task' object which is a character vector contains all task information extracted from the .Rmd file
#' @example
#' extract_task(file = file.path(system.file(package = "taskeR"), "test", "test.Rmd"))
#' @export
extract_task <- function(file){
  object <- readLines(file, warn = FALSE)
  tmp <- grepl(pattern = "```\\{todo", x = object)
  tmp2 <- c(1, grep(pattern = "```", x = object))
  tmp3 <- ifelse(tmp[tmp2], TRUE, FALSE)
  tmp4 <- rep(tmp3, times = diff(c(tmp2, length(object) + 1)))
  out <- object[tmp4]
  class(out) <- c("character", "task")
  return(out)
}

#' To split task vector into a list of tasks
#'
#' @param object A 'task' object which is a character vector contains all task information extracted from the .Rmd file
#' @return A list of all tasks and their properties
#' @example
#' tmp <- extract_task(file = file.path(system.file(package = "taskeR"), "test", "test.Rmd"))
#' split_task(tmp)
#' @export
split_task <- function(object) {
  tmp <- grep(pattern = '```\\{todo', x = object)
  tmp2 <- rep(1:length(tmp), times = diff(c(tmp, length(object) + 1)))
  if (tmp[1] > 1){object <- object[-c(1:(tmp[1] - 1))]}
  tmp3 <- split(x = object, f = tmp2)
  out <- lapply(tmp3, function(x) {class(x) <- c("character", "task"); x})
  class(out) <- c("list", "task.list")
  return(out)
}

#' To extract a specific property of a task
#'
#' @param object A task object
#' @param info One of pre-defined property of task to extract
#' @return A character vector contains task's property to extract
#' @example
#' tmp <- extract_task(file = file.path(system.file(package = "taskeR"), "test", "test.Rmd"))
#' extract_info(object = tmp, info = "todo")
#' @export
extract_info <- function(object,
                         info = c("proj_tags", "proj_start", "proj_end", "todo", "info", "tags",
                                  "personnel", "cost", "deadline", "priority", "start", "end")) {
  if (length(info) > 1) info <- "todo"
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

#' To get all properties of all task from an .Rmd file and export into a data frame
#'
#' @param file Path to an .Rmd file
#' @return A data frame contains all properties of all task from the file
#' @example
#' get_task(file = file.path(system.file(package = "taskeR"), "test", "test.Rmd"))
#' @export
get_task <- function(file) {

  ## extract information related to task
  all_task <- extract_task(file = file)

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
  class(out) <- c("data.frame", "task.frame")
  return(out)
}

#' To summarize tasks from a data frame of all tasks
#'
#' @param task_info A task.frame object
#' @return A data frame contains all property and additional summaries of all tasks
#' @example
#' tmp <- get_task(file = file.path(system.file(package = "taskeR"), "test", "test.Rmd"))
#' summarize_task(tmp)
#' @export
summarize_task <- function(task_info){
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
  class(out) <- c("data.frame", "task.summary")
  return(out)
}

#' To plot a task.summary object
#'
#' @param summary_info A task.summary object
#' @return A plot created by ggplot
#' @example
#' tmp <- get_task(file = file.path(system.file(package = "taskeR"), "test", "test.Rmd"))
#' tmp2 <- summarize_task(tmp)
#' plot_task(tmp2)
#' @export
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
      ylim(c(0.5,(nrow(plotdat) + 0.5))) +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_segment(data = subset(plotdat, !is.na(start) & !is.na(end)), aes(x = start, xend = end, yend = id)) +
      geom_point(data = subset(plotdat, status %in% c("Not scheduled yet", "Not started yet", "Ongoing")), aes(x = current), shape = 17) +
      geom_point(data = subset(plotdat, !is.na(start)), aes(x = start), shape = 4) +
      geom_point(data = subset(plotdat, status == "Completed"), aes(x = end), shape = 16) +
      geom_text(aes(label = task, x = xrange[1] - 10), colour = "black") +
      geom_hline(yintercept = c(1:(nrow(plotdat) + 1)) - 0.5, colour = "grey") +
      xlab("Days from deadline") + ylab("") +
      scale_color_manual(breaks = c("high", "low", "medium", ""),
                         values = c(ggplot_color(3), "black")) +
      theme(axis.line.y = element_blank(),
            axis.line.x = element_line(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")
  }
}

#' To summarize a task.summary object
#' @export
report_task <- function(summary_info){
  require(lubridate)

  n_total <- nrow(summary_info)
  n_unscheduled <- sum(summary_info$status == "Not scheduled yet")
  n_unstarted <- sum(summary_info$status == "Not started yet")
  n_uncompleted <- sum(summary_info$status == "Ongoing")
  n_overdue <- sum(summary_info$time_left < 0)

  ## find number of task due soon (by 17:00 Friday this week)
  ### the current date
  current <- ymd_hms(Sys.time())
  ### find the Friday this week
  friday <- ymd_hm(paste(ymd(ceiling_date(current, unit = "week") - ddays(2)), "17:00"))
  n_duesoon <- with(summary_info, sum(deadline <= friday & completed == "No"))

  out <- data.frame(Item = c("Total task", "Unscheduled task",
                             "Unstarted task", "Ongoing task", "Overdue task",
                             "Due-soon task"),
                    n = c(n_total, n_unscheduled, n_unstarted, n_uncompleted, n_overdue, n_duesoon))
  out$p <- paste(round(100 * out$n/c(rep(n_total, 4), n_total - (n_unscheduled + n_unstarted), n_uncompleted)), "%")
  return(out)
}

#' Addin to manage all tasks
#' @export
manage_task_addin <- function() {
  require(shiny)
  require(miniUI)
  require(rstudioapi)
  path <- rstudioapi::getActiveDocumentContext()$path

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Task View",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),

    miniTabstripPanel(
      miniTabPanel("Input", icon = icon("folder-open-o"),
                   miniContentPanel(
                     checkboxInput('file', 'File', TRUE),
                     textInput("path", "Path:", path),
                     actionButton("file_browse", "File browse")
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
      ),

      miniTabPanel("Summary", icon = icon("sticky-note-o"),
                   miniContentPanel(
                     tableOutput("summary")
                   )
      )
    )
  )

  ## server
  server <- function(input, output, session){
    #path <- rstudioapi::getActiveDocumentContext()$path
    observe({
      #updateTextInput(session, "path",  value = path)

      if (input$file == TRUE) {
        if (input$file_browse == 0) return()
        updateTextInput(session, "path",  value = file.choose2())
      } else {
        if (input$file_browse == 0) return()
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
          "No task available"
        } else {
          plot_task(out)
        }
      }
    )

    output$summary <- renderTable(
      {
        if (input$file) {
          task <- get_task(file = input$path)
        } else {
          tmp <- list.files(path = input$path, pattern = ".Rmd", recursive = TRUE)
          task <- do.call("rbind",
                          lapply(tmp, function(x) get_task(file.path(input$path, x))))
        }

        tmp <- summarize_task(task)
        report_task(tmp)
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

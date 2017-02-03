extract_task <- function(file){
  tmp <- readLines(file, warn = FALSE)
  tmp2 <- grepl(pattern = "```\\{todo", x = tmp)
  tmp3 <- c(1, grep(pattern = "```", x = tmp))
  tmp4 <- ifelse(tmp2[tmp3], TRUE, FALSE)
  tmp5 <- rep(tmp4, times = diff(c(tmp3, length(tmp) + 1)))
  return(tmp[tmp5])
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
  ## extract information related to task
  all_task <- extract_task(file = file)

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
  return(data.frame(task = todo,
                    info = todo_info,
                    tags = todo_tags,
                    personnel = todo_person,
                    cost = todo_cost,
                    deadline = todo_deadline,
                    priority = todo_priority,
                    start = todo_start,
                    end = todo_end))
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

#' Function to display summary of tasks
#' @export
manage_task_addin <- function() {
  require(shiny)
  require(miniUI)
  require(data.table)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Table Task View",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      dataTableOutput("table")
    )
  )

  ## server
  server <- function(input, output, session){
    context <- rstudioapi::getActiveDocumentContext()
    path <- context$path
    task <- get_task(path)
    output$table <- renderDataTable(summarize_task(task)[, c("task", "tags", "priority", "deadline",
                                             "status", "time_left")])
    ## stop app when click "Done"
    observeEvent(input$done, {
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
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
    xlim(c(xrange[1] - 10, xrange[2])) +
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

#' @export
plot_task_addin <- function() {
  require(shiny)
  require(miniUI)

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Plot Task View",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      plotOutput("plot")
    )
  )

  ## server
  server <- function(input, output, session){
    context <- rstudioapi::getActiveDocumentContext()
    path <- context$path
    task <- get_task(path)
    output$plot <- renderPlot(
      plot_task(summarize_task(task))
    )
    ## stop app when click "Done"
    observeEvent(input$done, {
      stopApp()
    })
  }

  ## run
  runGadget(ui, server)
}




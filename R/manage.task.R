# extract_task <- function(file){
#   tmp <- readLines(file, warn = FALSE)
#   return(tmp[grepl(pattern = "[:][[:alnum:]_]", x = tmp)])
# }
#
# split_task <- function(object) {
#   tmp <- grep(pattern = ":TODO:", x = object)
#   tmp2 <- rep(1:length(tmp), times = diff(c(tmp, length(object) + 1)))
#   if (tmp[1] > 1){object <- object[-c(1:(tmp[1] - 1))]}
#   return(split(x = object, f = tmp2))
# }

extract_info <- function(object,
                         info = c("proj_tags", "proj_close", "todo", "info", "tags",
                                  "personnel", "cost", "deadline", "priority", "start", "end")) {
  if (info == "todo") {
    pattern <- "## ----"
    tmp <- grepl(pattern = pattern, x = object)
    if (any(tmp)) {
      out <- gsub("^\\s+|\\s+$", "",
                  gsub(pattern = pattern,
                       replacement = "",
                       x = sapply(object[tmp], function(y) strsplit(y, split = ",")[[1]][1], USE.NAMES = FALSE)))
    } else {out <- ""}
  } else {
    pattern <- paste(info, "=")
    tmp <- grepl(pattern = pattern, x = object)
    if (any(tmp)) {
      out <- gsub("^\\s+|\\s+$", "", gsub(pattern = pattern, replacement = "", x = object[tmp]))
    } else {out <- ""}
  }

  return(out)
}

extract_task <- function(file, output = "tmp.todo"){
  tmp <- readLines(file, warn = FALSE)

  knitr::purl(file, output = output,
              quiet = TRUE, documentation = 1)

  file.remove(output)
  tmp2 <- grepl(pattern = "## ----", x = tmp)
  tmp3 <- grepl(pattern = 'engine=\"todo\"', x = tmp)
  tmp4 <- rep(tmp3[tmp2], times = diff(c(which(tmp2), length(tmp) + 1)))
  return(tmp[tmp4])
}

split_task <- function(object) {
  tmp <- grep(pattern = 'engine=\"todo\"', x = object)
  tmp2 <- rep(1:length(tmp), times = diff(c(tmp, length(object) + 1)))
  if (tmp[1] > 1){object <- object[-c(1:(tmp[1] - 1))]}
  return(split(x = object, f = tmp2))
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

  ## ui
  ui <- miniPage(
    gadgetTitleBar("Task View",
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(
      tableOutput("table")
    )
  )

  ## server
  server <- function(input, output, session){
    context <- rstudioapi::getActiveDocumentContext()
    path <- context$path
    task <- get_task(path)
    output$table <- renderTable(summarize_task(task)[, c("task", "tags", "priority", "deadline",
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

# a <- summarize_task(Lmisc:::get_task(file.path("~", "Documents", "test.Rmd"))) %>%
#   mutate(id = 1:n())
# library(ggplot2)
# library(tidyr)
# ggplot(data = a, aes(y = id)) +


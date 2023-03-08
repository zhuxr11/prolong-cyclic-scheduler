#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

# Define core function of prolongation cyclic scheduler
schedule_prolong_cyclic <- function(
    idx = seq(1, 99, by = 1),
    new_interv = 1,
    interv_start = 1,
    interv_inc = 1,
    interv_times = 6L,
    interv_inc_method = c("exp", "linear"),
    round_method = c("round", "floor", "ceiling"),
    col_prefix = "task_",
    drop_idx = FALSE
) {
  interv_inc_method <- match.arg(interv_inc_method)
  round_method <- match.arg(round_method)
  if (length(idx) > 1L && idx[2L] < idx[1L]) {
    idx_dec <- TRUE
    arrange_fun <- dplyr::desc
  } else {
    idx_dec <- FALSE
    arrange_fun <- identity
  }
  res <- tibble::tibble(value = idx) %>%
    tibble::rowid_to_column("start_idx") %>%
    dplyr::mutate_at("start_idx", ~ (.x - 1) * new_interv + 1) %>%
    tidyr::expand_grid(
      base_idx = {
        switch(
          interv_inc_method,
          "exp" = interv_start * (1 + interv_inc)^seq(from = 0L, by = 1L, length.out = interv_times - 1L),
          "linear" = seq(from = interv_start, by = interv_inc, length.out = interv_times - 1L),
          stop("[interv_inc_method] not recognized as: ", interv_inc_method)
        ) %>%
          purrr::prepend(0) %>%
          cumsum()
      }
    ) %>%
    dplyr::mutate(idx = base_idx + start_idx) %>%
    dplyr::select(-start_idx, -base_idx) %>%
    dplyr::arrange(arrange_fun(value), arrange_fun(idx)) %>%
    dplyr::mutate_at("idx", ~ {
      switch(
        round_method,
        "round" = as.integer(round(.x)),
        "floor" = floor(.x),
        "ceiling" = ceiling(.x),
        stop("[round_method] not recognized as: ", round_method)
      )
    }) %>%
    dplyr::group_by(idx) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()
  if (drop_idx == TRUE) {
    res <- res %>%
      dplyr::mutate_at("idx", ~ factor(.x, levels = sort(unique(.x), decreasing = idx_dec))) %>%
      dplyr::mutate_at("rank", ~ factor(.x, levels = sort(unique(.x))))
  } else {
    res <- res %>%
      dplyr::mutate_at(c("idx", "rank"), ~ factor(.x, levels = seq(1L, max(.x), by = 1L)))
  }
  res <- res %>%
    tidyr::complete(idx, rank, fill = list(value = NA)) %>%
    tidyr::pivot_wider(id_cols = "idx",
                       names_from = rank,
                       names_prefix = col_prefix,
                       values_from = "value") %>%
    tibble::column_to_rownames("idx")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Enable Font Awesome icons
  includeScript("https://kit.fontawesome.com/6ecbd6c532.js"),
  
  # Application title
  titlePanel("Prolonged Cyclic Memory Scheduler"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("idx_start", "Start index (e.g. start page number):", value = 1),
      numericInput("idx_end", "End index (e.g. end page number):", value = 99),
      numericInput("idx_interv", "Index interval (e.g. page increment):", value = 1),
      numericInput("new_interv", "Interval between new indices:", value = 1),
      numericInput("interv_times", "Memory times:", value = 6),
      numericInput("interv_start", "Start interval between revisions:", value = 1),
      numericInput("interv_inc", "Interval increment rate between revisions:", value = 1),
      selectInput("interv_inc_method", "Interval increment method:",
                  choices = c("Exponential" = "exp", "Linear" = "linear")),
      selectInput("round_method", "Round method for non-integer increments:",
                  choices = c("Round (closest)" = "round",
                              "Floor (advanced)" = "floor",
                              "Ceiling (delayed)" = "ceiling")),
      checkboxInput("drop_idx", "Hide empty rows"),
      actionButton("submit", "Generate schedule"),
      htmlOutput("br_1"),
      htmlOutput("git_repo")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("schedule")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$br_1 <- renderText("</br>")
  
  output$git_repo <- renderText(as.character(
    tags$a(
      tags$i(class = "fab fa-github"),
      "View instructions or report bugs",
      href = "https://github.com/zhuxr11/prolong-cyclic-scheduler#instructions",
      target = "_blank"
    )
  ))
  
  # Check validity
  observe(
    {
      if (is.na(as.numeric(input$idx_start)) == FALSE &&
          is.na(as.numeric(input$idx_end)) == FALSE &&
          is.na(as.numeric(input$idx_interv)) == FALSE) {
        if (input$idx_interv == 0) {
          updateNumericInput(session, "idx_interv", value = 1)
        }
        end_start_sign <- sign(input$idx_end - input$idx_start)
        if (input$idx_end != input$idx_start && sign(input$idx_interv) != end_start_sign) {
          updateNumericInput(session, "idx_interv", value = end_start_sign * abs(input$idx_interv))
        }
      }
    }
  )
  observe(
    if (is.na(input$new_interv) == FALSE && input$new_interv <= 0) {
      updateNumericInput(session, "new_interv", value = 1)
    }
  )
  observe(
    updateNumericInput(session, "interv_times", value = max(round(input$interv_times), 1L))
  )
  observe(
    if (is.na(input$interv_start) == FALSE && input$interv_start <= 0) {
      updateNumericInput(session, "interv_start", value = 1)
    }
  )
  observe(
    if (is.na(input$interv_inc) == FALSE && input$interv_inc <= 0) {
      updateNumericInput(session, "interv_inc", value = 1)
    }
  )
  
  output$schedule <- DT::renderDataTable({
    input$submit
    
    isolate({
      if (is.na(as.numeric(input$idx_start)) == FALSE &&
          is.na(as.numeric(input$idx_end)) == FALSE &&
          is.na(as.numeric(input$idx_interv)) == FALSE &&
          is.na(as.numeric(input$new_interv)) == FALSE &&
          is.na(as.numeric(input$interv_times)) == FALSE &&
          is.na(as.numeric(input$interv_start)) == FALSE &&
          is.na(as.numeric(input$interv_inc)) == FALSE) {
        # Generate schedule
        res <- schedule_prolong_cyclic(
          idx = seq(input$idx_start, input$idx_end, by = input$idx_interv),
          new_interv = input$new_interv,
          interv_start = input$interv_start,
          interv_inc = input$interv_inc,
          interv_times = as.integer(input$interv_times),
          interv_inc_method = input$interv_inc_method,
          round_method = input$round_method,
          drop_idx = input$drop_idx
        )
        length_menu <- outer(c(1L, 2L, 5L), 10^seq(1L, floor(log10(nrow(res))), by = 1L), FUN = "*") %>%
          as.integer() %>%
          pmin(nrow(res)) %>%
          c(nrow(res)) %>%
          unique()
        length_menu_name <- format(length_menu, scientific = FALSE)
        length_menu_name[length(length_menu_name)] <- "All"
        DT::datatable(
          res,
          options = list(
            dom = "lBfrtip",
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            pageLength = nrow(res),
            lengthMenu = list(length_menu, length_menu_name)
          ),
          rownames = TRUE,
          filter = "top",
          selection = "none",
          extension = c("Buttons")
        )
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

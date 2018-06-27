library(shiny)
library(shinydashboard)

library(dplyr)
library(purrr)
library(rlang)
library(stringr)

library(DT)
library(r2d3)

library(odbc)
library(DBI)
library(dbplyr)
library(config)

dw <- config::get("mssql")

con <- DBI::dbConnect(
  odbc::odbc(),
  DSN = dw$DSN
  )

airlines <- tbl(con, "airlines")
airports <- tbl(con, "airports")
flights  <- tbl(con, "flights")

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# Carrier code as the value

airline_list <- airlines %>%
  collect() %>%
  split(.$name) %>%
  map(~ .$carrier)

# Use rlang's set_names() to easily create a valide "choices"
# argument of the dropdown where the displayed text has to be
# different than the value passed as the input selection

month_list <- as.list(1:12) %>%
  set_names(month.name)

month_list$`All Year` <- 99

ui <- dashboardPage(
  dashboardHeader(
    title = "Flights Dashboard",
    titleWidth = 200
  ),
  dashboardSidebar(
    selectInput(
      inputId = "airline",
      label = "Airline:",
      choices = airline_list,
      selected = "DL",
      selectize = FALSE
    ),
    sidebarMenu(
      selectInput(
        inputId = "month",
        label = "Month:",
        choices = month_list,
        selected = 99,
        size = 13,
        selectize = FALSE
      ),
      actionLink("remove", "Remove detail tabs")
    )
  ),
  dashboardBody(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Main Dashboard",
        value = "page1",
        fluidRow(
          valueBoxOutput("total_flights"),
          valueBoxOutput("per_day"),
          valueBoxOutput("percent_delayed")
        ),
        fluidRow(),
        fluidRow(
          column(
            width = 6,
            d3Output("group_totals")
          ),
          column(
            width = 6,
            d3Output("top_airports")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  tab_list <- NULL

  # Use a reactive() function to prepare the base
  # SQL query that all the elements in the dashboard
  # will use. The reactive() allows us to evaluate
  # the input variables
  base_flights <- reactive({
    res <- flights %>%
      filter(carrier == input$airline) %>%
      left_join(airlines, by = "carrier") %>%
      rename(airline = name) %>%
      left_join(airports, by = c("origin" = "faa")) %>%
      rename(origin_name = name) %>%
      select(-lat, -lon, -alt, -tz, -dst) %>%
      left_join(airports, by = c("dest" = "faa")) %>%
      rename(dest_name = name)
    if (input$month != 99) res <- filter(res, month == input$month)
    res
  })

  # Total Flights (server) ------------------------------------------
  output$total_flights <- renderValueBox({
    # The following code runs inside the database.
    # pull() bring the results into R, which then
    # it's piped directly to a valueBox()
    base_flights() %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Flights")
  })

  # Avg per Day (server) --------------------------------------------
  output$per_day <- renderValueBox({
    # The following code runs inside the database
    base_flights() %>%
      group_by(day, month) %>%
      tally() %>%
      ungroup() %>%
      summarise(avg = mean(n, na.rm = TRUE)) %>%
      pull() %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(
        subtitle = "Average Flights per day",
        color = "blue"
      )
  })

  # Percent delayed (server) ----------------------------------------
  output$percent_delayed <- renderValueBox({
    base_flights() %>%
      filter(!is.na(dep_delay)) %>%
      mutate(delayed = ifelse(dep_delay >= 15, 1, 0)) %>%
      summarise(
        delays = sum(delayed, na.rm = TRUE),
        total = n()
      ) %>%
      mutate(percent = (delays / total) * 100) %>%
      pull() %>%
      round() %>%
      paste0("%") %>%
      valueBox(
        subtitle = "Flights delayed",
        color = "teal"
      )
  })

  # Montly/daily trend (server) -------------------------------------
  output$group_totals <- renderD3({
    grouped <- ifelse(input$month != 99, expr(day), expr(month))

    res <- base_flights() %>%
      group_by(!!grouped) %>%
      tally() %>%
      collect() %>%
      mutate(
        y = n,
        x = !!grouped
      ) %>%
      select(x, y)

    if (input$month == 99) {
      res <- res %>%
        inner_join(
          tibble(x = 1:12, label = substr(month.name, 1, 3)),
          by = "x"
        )
    } else {
      res <- res %>%
        mutate(label = x)
    }
    r2d3(res, "col_plot.js")
  })

  # Top airports (server) -------------------------------------------
  output$top_airports <- renderD3({
    # The following code runs inside the database
    base_flights() %>%
      group_by(dest, dest_name) %>%
      tally() %>%
      collect() %>%
      arrange(desc(n)) %>%
      head(10) %>%
      arrange(dest_name) %>%
      mutate(dest_name = str_sub(dest_name, 1, 30)) %>%
      rename(
        x = dest,
        y = n,
        label = dest_name
      ) %>%
      r2d3("bar_plot.js")
  })

  # Get details (server) --------------------------------------------
  get_details <- function(airport = NULL, day = NULL) {
    # Create a generic details function that can be called
    # by different dashboard events
    res <- base_flights()
    if (!is.null(airport)) res <- filter(res, dest == airport)
    if (!is.null(day)) res <- filter(res, day == !!as.integer(day))

    res %>%
      head(100) %>%
      select(
        month, day, flight, tailnum,
        dep_time, arr_time, dest_name,
        distance
      ) %>%
      collect() %>%
      mutate(month = month.name[as.integer(month)])
  }

  # Month/Day column click (server) ---------------------------------
  observeEvent(input$column_clicked != "", {
    if (input$month == "99") {
      updateSelectInput(session, "month", selected = input$column_clicked)
    } else {
      day <- input$column_clicked
      month <- input$month
      tab_title <- paste(
        input$airline, "-", month.name[as.integer(month)], "-", day
      )
      if (!(tab_title %in% tab_list)) {
        appendTab(
          inputId = "tabs",
          tabPanel(
            tab_title,
            DT::renderDataTable(
              get_details(day = day)
            )
          )
        )
        tab_list <<- c(tab_list, tab_title)
      }
      updateTabsetPanel(session, "tabs", selected = tab_title)
    }
  },
  ignoreInit = TRUE
  )


  # Bar clicked (server) --------------------------------------------
  observeEvent(input$bar_clicked, {
    airport <- input$bar_clicked
    month <- input$month
    tab_title <- paste(
      input$airline, "-", airport,
      if (month != 99) {
        paste("-", month.name[as.integer(month)])
      }
    )
    if (!(tab_title %in% tab_list)) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          tab_title,
          DT::renderDataTable(
            get_details(airport = airport)
          )
        )
      )

      tab_list <<- c(tab_list, tab_title)
    }
    updateTabsetPanel(session, "tabs", selected = tab_title)
  })

  # Remote tabs (server) --------------------------------------------
  observeEvent(input$remove, {
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~ removeTab("tabs", .x))
    tab_list <<- NULL
  })
}

shinyApp(ui, server)

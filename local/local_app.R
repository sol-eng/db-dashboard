library(shiny)
library(shinydashboard)

library(dplyr)
library(purrr)
library(rlang)
library(stringr)

library(ggplot2)
library(ggiraph)

library(DT)

library(nycflights13)

update_geom_defaults("text", list(family = "Source Sans Pro"))

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
            girafeOutput("group_totals")
          ),
          column(
            width = 6,
            girafeOutput("top_airports")
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
      summarise(avg = mean(n)) %>%
      pull(avg) %>%
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
        delays = sum(delayed),
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
  output$group_totals <- renderGirafe({
    if(input$month == 99) {
      tbl_month <- tibble(
        month = 1:12, 
        label = substr(month.name, 1, 3)
      )
      
      tbl_months <- base_flights() %>%  
        count(month) %>% 
        inner_join(tbl_month, by = "month") %>% 
        mutate(n_label = prettyNum(n, big.mark = ","))
      
      gg_months <- tbl_months %>% 
        ggplot(aes(as.factor(month), n, label = n_label, data_id = month)) +
        geom_col_interactive(fill = "#009E73", alpha = 0.7) +
        geom_text(vjust = -0.5) +
        scale_x_discrete(labels = substr(month.name, 1, 3)) +
        theme_minimal() +
        theme(
          text = element_text(family = "Source Sans Pro"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(hjust = 0, size = 10),
          axis.title = element_blank(),
          plot.subtitle = element_text(hjust = 1, vjust = 5, size = 8)
        ) +
        labs(
          title = "Total Flights",
          subtitle = "Click on column for more details"
        )
      
      girafe(
        ggobj = gg_months, 
        options = list(opts_selection(type = "single", only_shiny = FALSE))
      )
    } else {
      month_name <- month.name[as.integer(input$month)]
      
      tbl_days <- base_flights() %>%  
        count(day) %>% 
        mutate(n_label = prettyNum(n, big.mark = ",")) %>% 
        ungroup()
      
      max_count <- max(tbl_days$n)
      
      gg_days <- tbl_days %>% 
        ggplot(aes(day, n, label = n_label, data_id = day)) +
        geom_path(alpha = 0.5) +
        geom_point_interactive(color = "#009E73", alpha = 0.7, size = 5) +
        geom_text(vjust = -1, size = 2.5) +
        ylim(0, max_count) +
        theme_minimal() +
        theme(
          text = element_text(family = "Source Sans Pro"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(hjust = 0, size = 8),
          axis.title = element_blank(),
          panel.grid = element_blank(), 
          plot.subtitle = element_text(hjust = 1, vjust = 5, size = 8)
        ) +
        scale_x_continuous(breaks = 1:31) +
        labs(
          title = paste0("Total Flights - ", month_name),
          subtitle = "Click on dot for more details"
        )
      
      girafe(
        ggobj = gg_days, 
        options = list(opts_selection(type = "single", only_shiny = FALSE))
      )
    }
    
  })

  # Top airports (server) -------------------------------------------
  output$top_airports <- renderGirafe({
    # The following code runs inside the database
    tbl_airports <- base_flights() %>%
      group_by(dest, dest_name) %>%
      tally() %>%
      collect() %>%
      arrange(desc(n)) %>%
      head(10) %>%
      arrange(dest_name) %>%
      mutate(
        dest_name = str_sub(dest_name, 1, 30),
        n_label = prettyNum(n, big.mark = ",")
      ) 
    
    gg_airports <- tbl_airports %>%  
      ggplot(aes(x = dest_name, y = n, data_id = dest, label = n_label)) +
      geom_col_interactive(fill = "#0072B2", alpha = 0.7) +
      geom_text_interactive(hjust = 1.1, color = "#ffffff", family = "Source Sans Pro") +
      coord_flip() +
      theme_minimal() +
      theme(
        text = element_text(family = "Source Sans Pro"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0, size = 10),
        axis.title = element_blank(),
        plot.title = element_text(hjust = -2),
        plot.subtitle = element_text(hjust = 1, vjust = 5, size = 8)
      ) +
      labs(
        title = "Top 10 Destination Airports",
        subtitle = "Click on bar for more details"
      )
    
    girafe(
      ggobj = gg_airports, 
      options = list(opts_selection(type = "single", only_shiny = FALSE))
    )
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
  observeEvent(input$group_totals_selected != "", {
    if (input$month == "99") {
      updateSelectInput(session, "month", selected = input$group_totals_selected)
    } else {
      day <- input$group_totals_selected
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
  observeEvent(input$top_airports_selected, {
    airport <- input$top_airports_selected
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

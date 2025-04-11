library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(httr)
library(jsonlite)
library(shinyjs)
library(bslib)

data <- mpg
api_key <- Sys.getenv("ANTHROPIC_API_KEY")

ui <- page_fluid(

  shinyjs::useShinyjs(),

  h1("Shiny Style Shuffle"),

  layout_columns(
    col_widths = c(4, 8),

    card(
      card_header("Data Controls"),

      selectInput("manufacturer", "Manufacturer:",
                  choices = c("All", unique(data$manufacturer))),

      selectInput("cyl", "Number of Cylinders:",
                  choices = c("All", unique(data$cyl))),

      selectInput("trans", "Transmission:",
                  choices = c("All", unique(data$trans))),

      textAreaInput("prompt", "AI Styling Prompt:",
                    placeholder = "Example: Make this app look like a New York Times front page",
                    height = "100px"),

      actionButton("styleBtn", "Apply AI Styling", class = "btn-primary"),

      verbatimTextOutput("apiStatus"),

      h4("Applied CSS:"),
      verbatimTextOutput("cssOutput")
    ),

    card(
      card_header("Data Visualization"),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Scatter Plot"),
          plotOutput("scatterPlot")
        ),
        card(
          card_header("Bar Chart"),
          plotOutput("barPlot")
        )
      ),

      card(
        card_header("Data Table"),
        DTOutput("dataTable")
      )
    )
  ),

  tags$head(
    tags$style(id = "ai-styles", "")
  )
)

server <- function(input, output, session) {
  css_reactive <- reactiveVal("")

  filtered_data <- reactive({
    result <- data

    if (input$manufacturer != "All") {
      result <- result %>% filter(manufacturer == input$manufacturer)
    }

    if (input$cyl != "All") {
      result <- result %>% filter(cyl == input$cyl)
    }

    if (input$trans != "All") {
      result <- result %>% filter(trans == input$trans)
    }

    return(result)
  })

  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = displ, y = hwy, color = class)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(
        title = "Highway MPG vs. Engine Displacement",
        x = "Engine Displacement (L)",
        y = "Highway MPG",
        color = "Vehicle Class"
      ) +
      theme_minimal()
  })

  output$barPlot <- renderPlot({
    avg_mpg <- filtered_data() %>%
      group_by(class) %>%
      summarize(avg_hwy = mean(hwy), count = n()) %>%
      filter(count > 0)

    ggplot(avg_mpg, aes(x = reorder(class, avg_hwy), y = avg_hwy, fill = class)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Average Highway MPG by Vehicle Class",
        x = "Vehicle Class",
        y = "Average Highway MPG"
      ) +
      theme_minimal() +
      guides(fill = FALSE)
  })

  output$dataTable <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })

  output$cssOutput <- renderText({
    css_reactive()
  })

  observeEvent(input$styleBtn, {
    req(input$prompt)

    if (api_key == "") {
      output$apiStatus <- renderText("ERROR: ANTHROPIC_API_KEY environment variable not set.")
      return()
    }

    output$apiStatus <- renderText("Requesting styling from Anthropic Claude...")

    styling_prompt <- paste0(
      "I need CSS to style a Shiny app based on this theme: '",
      input$prompt,
      "'. Respond ONLY with CSS code, no explanations, no backticks, no markdown formatting. ",
      "The CSS is for an app with these elements: ",
      "- h1 title ",
      "- Multiple card elements with card-header elements ",
      "- Select inputs, a textarea, and buttons ",
      "- Plots (pngs) and tables within card elements ",
      "- you can also apply new styles to the following classes: btn, pagination, table, paginate_button,  selectize-input, bslib-grid, form-group ",
      "Make the styles very dramatic and visually obvious - use bright colors, borders, and other elements that will make it clear the styling has been applied."
    )

    tryCatch({
      response <- POST(
        url = "https://api.anthropic.com/v1/messages",
        add_headers(
          "x-api-key" = api_key,
          "anthropic-version" = "2023-06-01",
          "Content-Type" = "application/json"
        ),
        body = toJSON(list(
          model = "claude-3-7-sonnet-20250219",
          max_tokens = 1000,
          messages = list(
            list(
              role = "user",
              content = styling_prompt
            )
          )
        ), auto_unbox = TRUE),
        encode = "json"
      )

      if (status_code(response) == 200) {
        response_content <- content(response, "text", encoding = "UTF-8")
        result <- fromJSON(response_content)

        css_code <- ""

        if (is.list(result) && !is.null(result$content) &&
            is.data.frame(result$content) && ncol(result$content) >= 2) {
          if ("text" %in% names(result$content)) {
            css_code <- result$content$text[1]
          }
        }

        if (css_code == "") {
          output$apiStatus <- renderText("Error: Could not extract CSS from the API response")
          return()
        }

        css_code <- gsub("```css|```", "", css_code)

        css_reactive(css_code)

        js_code <- paste0('
          const styleElement = document.getElementById("ai-styles");
          styleElement.innerHTML = ', toJSON(css_code), ';
        ')

        shinyjs::runjs(js_code)

        output$apiStatus <- renderText("Styling applied successfully!")
      } else {
        output$apiStatus <- renderText(paste("API Error:", content(response, "text", encoding = "UTF-8")))
      }
    }, error = function(e) {
      output$apiStatus <- renderText(paste("Error:", e$message))
    })
  })
}

shinyApp(ui, server)

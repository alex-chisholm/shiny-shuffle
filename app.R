library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(httr)
library(jsonlite)
library(shinyjs)

# Sample dataset - mpg data from ggplot2
data <- mpg

# UI
ui <- page_fluid(

  # Include shinyjs
  shinyjs::useShinyjs(),

  # Title
  h1("Shiny Style Shuffle"),

  # Main layout
  layout_columns(
    col_widths = c(4, 8),

    # Left column - Controls
    card(
      card_header("Data Controls"),

      # Dataset filters
      selectInput("manufacturer", "Manufacturer:",
                  choices = c("All", unique(data$manufacturer))),

      selectInput("cyl", "Number of Cylinders:",
                  choices = c("All", unique(data$cyl))),

      selectInput("trans", "Transmission:",
                  choices = c("All", unique(data$trans))),

      # AI styling controls
      textAreaInput("prompt", "AI Styling Prompt:",
                    placeholder = "Example: Make this app look like a New York Times front page",
                    height = "100px"),

      actionButton("styleBtn", "Apply AI Styling", class = "btn-primary"),

      verbatimTextOutput("apiStatus"),

      # Add an output to show the CSS being applied (for debugging)
      h4("Applied CSS:"),
      verbatimTextOutput("cssOutput")
    ),

    # Right column - Visualizations
    card(
      card_header("Data Visualization"),

      # Plots side by side in a row
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

      # Data table below the plots
      card(
        card_header("Data Table"),
        DTOutput("dataTable")
      )
    )
  ),

  # CSS container to inject AI-generated styles - placed in head
  tags$head(
    tags$style(id = "ai-styles", "")
  )
)

# Server
server <- function(input, output, session) {
  # Store CSS code in a reactive value for display and application
  css_reactive <- reactiveVal("")

  # Reactive data based on filters
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

  # Scatter plot
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

  # Bar plot
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

  # Data table
  output$dataTable <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })

  # Display the CSS being applied
  output$cssOutput <- renderText({
    css_reactive()
  })

  # AI styling functionality
  observeEvent(input$styleBtn, {
    req(input$prompt)

    # API key from environment variable
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")

    if (api_key == "") {
      output$apiStatus <- renderText("ERROR: ANTHROPIC_API_KEY environment variable not set.")
      return()
    }

    output$apiStatus <- renderText("Requesting styling from Anthropic Claude...")

    # Use a simpler prompt that clearly requests only CSS
    styling_prompt <- paste0(
      "I need CSS to style a Shiny app based on this theme: '",
      input$prompt,
      "'. Respond ONLY with CSS code, no explanations, no backticks, no markdown formatting. ",
      "The CSS is for an app with these elements: ",
      "- h1 title ",
      "- Multiple card elements with card-header elements ",
      "- Select inputs, a textarea, and buttons ",
      "- Plots and tables within card elements ",
      "- you can also apply new styles to the following classes: table, paginate_button,  selectize-input, bslib-grid, form-group ",
      "Make the styles very dramatic and visually obvious - use bright colors, borders, and other elements that will make it clear the styling has been applied."
    )

    # Call Anthropic API
    tryCatch({
      response <- POST(
        url = "https://api.anthropic.com/v1/messages",
        add_headers(
          "x-api-key" = api_key,
          "anthropic-version" = "2023-06-01",
          "Content-Type" = "application/json"
        ),
        body = toJSON(list(
          model = "claude-3-haiku-20240307",
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
        # Parse response
        response_content <- content(response, "text", encoding = "UTF-8")
        result <- fromJSON(response_content)

        # Extract CSS code
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

        # Clean up CSS code - remove any markdown code blocks or comments
        css_code <- gsub("```css|```", "", css_code)

        # Update reactive value with CSS
        css_reactive(css_code)

        # Apply CSS to the app using direct DOM manipulation
        # This approach is more reliable than using textContent
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

# Run the app
shinyApp(ui, server)

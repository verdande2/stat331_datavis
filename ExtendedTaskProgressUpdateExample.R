library(shiny)
library(bslib)
library(future)
library(ipc) # For AsyncProgress
plan(multisession) # Run tasks in separate R sessions

ui <- fluidPage(
  input_task_button("run", "Run Long Task"), # The bslib button shows a busy indicator
  plotOutput("plot"),
  # Add a standard Shiny progress bar container (optional, the button provides basic feedback)
  div(id = "progress_container")
)

server <- function(input, output, session) {
  # 1. Define the long-running function outside the server
  # The progress updates are handled within the function using the 'progress' object
  long_computation <- function(progress) {
    for (i in 1:15) {
      progress$set(value = i, message = paste("Step", i, "of 15"))
      Sys.sleep(0.5) # Simulate work
    }
    progress$close()
    return("Task Complete!")
  }

  # 2. Create the ExtendedTask object
  # The function passed to ExtendedTask$new needs to accept the progress object
  task <- ExtendedTask$new(function() {
    # Initialize AsyncProgress inside the task function to send updates back to the UI session
    progress <- AsyncProgress$new(session, min = 1, max = 15)
    on.exit(progress$close()) # Ensure progress bar closes even on error

    long_computation(progress)
  }) |>
    bind_task_button("run") # Bind the task to the input_task_button

  # 3. Observe the button click to invoke the task
  observeEvent(input$run, {
    task$invoke()
  })

  # 4. Render output when the task is complete
  output$plot <- renderPlot({
    req(task$result()) # reactive read that waits for the result
    # Code to generate plot using the task's result
    print(paste("Result:", task$result()))
    plot(rnorm(100)) # Placeholder plot
  })
}

shinyApp(ui, server)

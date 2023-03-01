#add upload capacity to shiny to 30MB

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  ## Declaring Variables
  Data <- reactiveVal()
  Consolidated <- reactiveVal()
  Error_Download <- reactiveVal()
  
  ##### 1. Load Data ######
  
  imported <- import_copypaste_server("myid")
  
  output$status <- renderPrint({
    imported$status() })
  
  output$data <- renderPrint({
    source("R/1_1_read_data_local.R")
    
    Data(read_data_2022_local(imported$data()))
    showNotification("Data Processing Complete",duration = 10, type = "error")
    
    updateSelectInput(session,"country_name",choices = unique(Data()$Country))
    updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
    
    
  })
  
  ## Data Preview
  output$Preview_Data <- DT::renderDataTable({Data()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip', 
    # add B for button
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE
  ))
  
  observeEvent(input$Run_Script,{
    source("R/1_read_data.R")
    Data(read_data_2022())
    showNotification("Data Processing Complete",duration = 10, type = "error")
    updateSelectInput(session,"country_name",choices = c("All",unique(Data()$Country)))
    updateSelectInput(session,"country_name_agg",choices = c("All",unique(Data()$Country)))
  })
  
  ##### 2. Data Quality Check ###### 
  
  observeEvent(input$run_err_report,{
    source("R/2_data_quality_check.R")
    Error_report <- r4v_error_report(data = Data(),
                                     countryname = input$country_name,
                                     write = "yes")
    
    
    Error_Download(Error_report$ErrorReportclean)
    
    # To output number of activities and error 
    
    output$Number_of_Activities <- renderText({nrow(Error_report$ErrorReportclean)})
    output$Number_of_Errors_Pre <- renderText({sum(!is.na(Error_report$ErrorReportclean$Review))})
    output$Percentage_of_Errors <- renderText(round({sum(!is.na(Error_report$ErrorReportclean$Review))}/{nrow(Error_report$ErrorReportclean)}*100, digits = 1))
    
    # PLOTLY section
    
    output$plot <- renderPlotly({
      Error_report$ErrorReportclean %>%
        filter(!is.na(Review)) %>%
        ggplot() +
        aes(x = Appealing_org, size = Review) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        theme_minimal()})
    
    output$plot2 <- renderPlotly({
      Error_report$ErrorReportclean %>%
        filter(!is.na(Review)) %>%
        ggplot() +
        aes(x = Country, size = Review) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        theme_minimal()})
    
    showNotification("Successful",duration = 10, type = "error")
  })
  
  ## Download Error report
  output$downloadprecleaned <- downloadHandler(
    filename = function() {
      paste("Error Report", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(Error_Download(), file)
    }
  )
  
  output$Preview_Error_Report <- DT::renderDataTable({Error_Download()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip',
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE))
  
  ##### 3. Consolidated report ######
  
  observeEvent(input$run_aggregation,{
    source("R/3_consolidated_report.R")
    Consolidated<-(r4v_consolidated(data = Data(),countryname = input$country_name_agg,totalmodel = input$totalmodel_agg))
    showNotification("Successful",duration = 10, type = "error")
    
    Consolidated(Consolidated$ConsolidatedReport)
    
    ## Preview consolidated
    
    output$Preview_conslidated <- DT::renderDataTable({Consolidated()},extensions = c("Buttons"), options = list(
      dom = 'lfrtip',
      paging = TRUE,
      ordering = TRUE,
      lengthChange = TRUE,
      pageLength = 10,
      scrollX = TRUE,
      autowidth = TRUE,
      rownames = TRUE))
    
    ## Download Consolidated
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Consolidated_Report", ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(Consolidated(), file)
      }
    )
    
  })
})
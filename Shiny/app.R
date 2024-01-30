path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/")
source(paste0(path, "server.R"))
source(paste0(path, "ui.R"))

shinyApp(ui = ui, server = server)

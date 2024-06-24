# ~~~~~~~~~ server ~~~~~~~~~ #

server <- function(input, output){

# Data inventory  --------------------------  
  output$inventory_plot <- data_inventory_plot(input)
  output$data_text_guam <- data_text_guam(input)
}

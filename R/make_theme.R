# fresh css

library(fresh)



theme <- 
  create_theme(theme = "default",
               bs_vars_global(link_color = "black"),
               bs_vars_pills(active_link_hover_bg = "black"),
  bs_vars_navbar(
    default_bg = "#2c3e50",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF",
    default_link_hover_color = "#FFFFFF"
      
  ),
  bs_vars_tabs(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
      
  ),
  
  bs_vars_button(
    font_weight = 500,
    default_border = "#607D8B",
    default_color = "#EEEEEE",
    default_bg = "#4a647c"
  ),
  bs_vars_wells(
    bg = "#f5f5f5"#,
    #border = "#4a647c"
  ),
  bs_vars_font(
    size_base = "15px"
  ),
  
  
  
  
  output_file = "www/theme.css"
  )


# theme2 <- 
#   create_theme(theme = "default",
#                bs_vars_navbar(
#                  default_bg = "#FFFFFF",
#                ),
#                bs_vars_button(
#                  font_weight = 500,
#                  default_border = "#607D8B",
#                  default_color = "#EEEEEE",
#                  default_bg = "#4a647c"
#                ),
#                bs_vars_font(
#                  size_base = "14px",
#                  family_sans_serif = "Roboto"
#                   
#                ),
#                
#                
#                
#                
#                output_file = "www/theme.css"
#   )

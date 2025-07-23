create_attribute_scale <- function(attribute_name) {
  
  {
    attributes %>% 
      pivot_longer(c(-First, -Last, -Team)) %>% 
      mutate(Section = str_extract(name, "^.+\\s-\\s"),
             Section = str_remove(Section, "\\s-\\s"),
             Subsection = str_remove(name, "^.+\\s-\\s")) %>% 
      select(First, Last, Team, Section, Subsection, value) %>% 
      group_by(Section, Subsection) %>% 
      mutate(subsection_avg = mean(value)) %>% 
      ungroup() %>% 
      mutate(Subsection = fct_reorder(Subsection, subsection_avg),
             type = "individual") %>% 
      union(attribute_averages) %>% 
      filter(Subsection %in% c(attribute_name)) %>% 
      mutate(tooltip_text = if_else(type == "individual",
                                    true = glue::glue("This {type} scored {scales::percent(value)} for \"{Subsection}\"."),                                             ,
                                    false = glue::glue("The {type} score was {scales::percent(value)} for \"{Subsection}\"."))) %>% 
      ggplot(aes(x = value,
                 y = Subsection,
                 color = type,
                 alpha = type,
                 size = type,
                 shape = type,
                 text = tooltip_text)) +
      geom_point() +
      scale_alpha_manual(values = c(1.0, 0.5)) +
      scale_color_manual(values = c(TI_blue, TI_grey)) +
      scale_shape_manual(values = c(15, 16)) + # square, circle
      scale_size_manual(values = c(6, 3)) +
      scale_x_continuous(labels = scales::percent,
                         breaks = seq(0, 1, 0.2)) +
      theme(panel.grid.minor.x = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            strip.text = element_text(color = TI_blue, 
                                      face = "bold"),
            panel.spacing = unit(1, "cm"))
  } %>% 
    ggplotly(tooltip = "text",
             autosize = T, 
             width = 500, 
             height = 150) %>% 
    layout(xaxis = list(
      range = c(0, 1),
      autorange = F
    )) %>%
    config(displayModeBar = FALSE)
  
}


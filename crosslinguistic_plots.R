# To read csv data files for the plots below you need to eval relevant exporting chunks in language-CDI-WG-comp.Rmd files.

# Average SE by number of items plot:

read_csv("Data/meanSE_slovak.csv") -> slovak
read_csv("Data/meanSE_polish.csv") -> polish
read_csv("Data/meanSE_norwegian.csv") -> norwegian
read_csv("Data/meanSE_hebrew.csv") -> hebrew

bind_rows(list(Hebrew = hebrew, Norwegian = norwegian, Polish = polish, Slovak = slovak), .id = "Language") %>%
     ggplot(aes(x = item, y = mean_SE, colour = Language)) +
     geom_line(linewidth = 1) +
#     geom_point() +
     xlab ("Test length") +
     ylab ("Mean SE") +
     theme(
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "lightgrey"),
          text = element_text(size=16)
     ) +
     scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 1), limits=c(0, .25)) +
     scale_x_continuous(breaks = seq(0, nrow(params), by=20), limits = c(1, nrow(params)))
# ggsave("meanSE.png", width = 4000, height = 2000, units = "px")


# SE by ability plot:

read_csv("Data/scores_slovak.csv") %>% select(-1) -> slovak
read_csv("Data/scores_polish.csv") %>% select(-1) -> polish
read_csv("Data/scores_norwegian.csv") %>% select(-1) -> norwegian
read_csv("Data/scores_hebrew.csv") %>% select(-1) -> hebrew

bind_rows(list(Hebrew = hebrew, Norwegian = norwegian, Polish = polish, Slovak = slovak), .id = "Language") %>%
     ggplot(aes(x = F1, y = SE_F1, colour = Language)) +
     geom_point() +
     xlab("Ability level (theta)") +
     ylab("Standard error (SE)") +
     theme(
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "lightgrey"),
          text = element_text(size=16)
     )

# ggsave("smiles.png", width = 3000, height = 2000, units = "px")

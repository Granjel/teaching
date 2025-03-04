# download and plot career path

# load packages
library(googledrive)
library(tidyverse)
library(readxl)

# download data
drive_find(pattern = "career-path", type = "spreadsheet") %>%
    drive_download(
        type = "xlsx",
        path = "presentations/recodyn-intro-techs/career-path.xlsx",
        overwrite = TRUE
    )

# load data
career <- readxl::read_xlsx(
    "presentations/recodyn-intro-techs/career-path.xlsx",
    sheet = 1
)

# plot
career_graph <- career %>%
    pivot_longer(
        cols = starts_with("20"),
        names_to = "year",
        values_to = "status"
    ) %>%
    filter(status == 1) %>%
    ggplot(aes(
        x = year,
        y = paste(
            position,
            paste(institution, country, sep = ", "),
            sep = "\n"
        ),
        fill = type
    )) +
    geom_raster() +
    scale_fill_manual(
        values = c(
            "Education" = "#bf5459",
            "Internship" = "#545bbf",
            "Education/Job" = "#bfa454",
            "Job" = "#54bfa6"
        ),
        guide = "none"
    ) +
    scale_y_discrete(
        limits = rev(
            career %>%
                pivot_longer(
                    cols = starts_with("20"),
                    names_to = "year",
                    values_to = "status"
                ) %>%
                mutate(
                    y_order = factor(
                        paste(
                            position,
                            paste(institution, country, sep = ", "),
                            sep = "\n"
                        ),
                        levels = unique(paste(
                            position,
                            paste(institution, country, sep = ", "),
                            sep = "\n"
                        )[order(n)]),
                        ordered = TRUE
                    )
                ) %>%
                pull(y_order)
        )
    ) +
    theme_minimal() +
    theme(
        axis.title = element_blank(),
        axis.text = element_text(face = "bold", color = "white"),
        legend.position = "top",
        legend.title = element_blank()
    )

# save graph
ggsave(
    "presentations/recodyn-intro-techs/career-graph.png",
    plot = career_graph,
    dpi = 640,
    width = 10,
    height = 5.5
)

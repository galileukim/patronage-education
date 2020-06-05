# set-up
packages <- c(
    "rgdal",
    "rgeos",
    "RColorBrewer",
    "viridis",
    "scales",
    "rmapshaper"
)

load_p(packages)


# plot municipal map
plot_map <- function(
                     data, fill, breaks = 6, title = "", label = "", palette = "RdYlBu", legend_position = "bottom",
                     limits = NULL) {
    plot <- ggplot() +
        geom_polygon(
            data = data,
            aes(
                x = long,
                y = lat,
                group = group,
                fill = get(fill)
            ),
            color = NA
        ) +
        {
            if (is.factor(data[[fill]])) {
                  scale_fill_manual(
                      values = rev(
                          brewer.pal(n = length(levels(data[[fill]])), name = palette)
                      ),
                      breaks = levels(data[[fill]]),
                      na.value = "gray50"
                  )
              } else {
                  scale_fill_distiller(
                      palette = palette,
                      breaks = pretty_breaks(breaks),
                      direction = -1,
                      na.value = "gray50",
                      limits = limits
                  )
              }
        } +
        guides(fill = guide_legend(reverse = T)) +
        labs(fill = label) +
        theme_void(base_size = 17) +
        xlim(range(data$long)) +
        ylim(range(data$lat)) +
        coord_quickmap() +
        theme(
            legend.position = legend_position,
            legend.text = element_text(size = 18),
            text = element_text(size = 18),
            legend.title = element_blank(),
            plot.caption = element_text(size = 8)
        ) +
        ggtitle(title)

    return(plot)
}

# build map of average test scores by municipality
saeb_exam_mun <- read_data(
    "saeb",
    "saeb_exam_mun.rds"
)

map_br <- readOGR(
    here("data/raw/maps/"),
    "municipio"
)

map_br <- SpatialPolygonsDataFrame(
    gSimplify(map_br, tol = 0.01, topologyPreserve = T),
    data = map_br@data
)

map_br@data <- map_br@data %>%
    mutate(id = row.names(map_br))

map_br <- map_br %>%
    fortify() %>%
    left_join(., map_br@data, by = "id") %>%
    transmute(
        long,
        lat,
        group,
        cod_ibge_6 = as.numeric(levels(cod_ibge_6))[cod_ibge_6],
        mun_name = sem_acento
    )

# state boundaries
# map_state <- readOGR(
#   here("data/maps/"),
#   "estados_2010"
# ) %>%
#   fortify

map_score <- map_br %>%
    mutate(
        cod_ibge_6 = as.integer(cod_ibge_6)
    ) %>%
    left_join(
        saeb_exam_mun %>%
            filter(
                year == 2015,
                subject == "port",
                grade == 5
            ),
        by = c("cod_ibge_6")
    )

map_grade <- plot_map(
    map_score,
    fill = "mean_grade_exam_wgt",
    legend_position = "none",
    limits = c(100, 250)
)

ggsave(
    p_file_here("figs", "saeb_map.pdf"),
    width = 5,
    height = 5
)

detach_p(packages)
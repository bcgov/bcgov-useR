library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)
library(bcmaps)
library(rmapshaper)
library(lwgeom)

bc <- bc_bound() %>%
  ms_simplify() %>%
  st_make_valid()

make_sf_for_bc_animation <- function(sf, name, simplify = TRUE) {
  if (simplify) sf <- ms_simplify(sf)

  sf %>%
    mutate(state = name) %>%
    mutate(id = seq_len(n())) %>%
    st_intersection(st_geometry(bc)) %>%
    st_make_valid() %>%
    select(state, id)
}

layer_list = list(
  rd = combine_nr_rd() %>%
    make_sf_for_bc_animation("Reginal Districts"),

  nr = nr_regions() %>%
    make_sf_for_bc_animation("NR Regions"),

  nrd = nr_districts() %>%
    make_sf_for_bc_animation("NR Districts"),

  az = airzones() %>%
    make_sf_for_bc_animation("Airzones", simplify = FALSE),

  # eco = ecosections() %>%
  #   make_sf_for_bc_animation("Ecosections"),

  hz = hydrozones() %>%
    make_sf_for_bc_animation("Hydrozones", simplify = FALSE)
)

all <- do.call(rbind, layer_list)

p <- ggplot() +
  geom_sf(data = st_geometry(bc_neighbours()), colour = "white", alpha = 0.5) +
  geom_sf(data = all, aes(fill = id), colour = "white", alpha = 0.5) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Oranges", guide = "none") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_blank()) +
  # labs(title = "{closest_state}") +
  transition_states(state, state_length = 1, transition_length = 1)

# animate(p)

anim_save("bc_layers.gif", animate(p, width = 1000, height = 800))
cat(knitr::imgur_upload("bc_layers.gif"), file = "imgur_url", append = FALSE)

dat %>%
  ggplot() +
  geom_col(aes(y = reorder(var, prop), x = prop, fill = type)) +
  geom_errorbar(aes(y = reorder(var, prop), x = prop, xmin = l.ci, xmax = u.ci), width = .2, size = .4) +
  geom_text(aes(color = type, y = reorder(var, prop), label = prop.ci, x = 120), hjust = 1, size = 3, show.legend = FALSE) +
  facet_grid(type ~ ., scales = "free_y", switch = "y", space = "free_y") +
  theme(strip.placement = "outside", panel.border = element_rect(color = "black", fill = NA, size = .5),
        strip.background = element_rect(fill = 'grey45', color = "black"),
        legend.position = "none", axis.title = element_blank(), plot.margin = unit(c(1, 6.2, 1, 1), "lines")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off", xlim = c(0, 100))
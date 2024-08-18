dat %>%
  ggplot() +
  theme_light() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(y = reorder(cat, sort), x = estimate, xmin = `2.5 %`, xmax = `97.5 %`, color = type),
                  position = position_dodge(width = 0.5)) +
  facet_grid(var ~ ., space = "free_y", scales = "free") +
  labs(x = "Estimate (95% CI)") +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = 'grey60'),
    strip.text.y.right = element_markdown(angle = 360, hjust = 0, size = 10.5),
    strip.text.x = element_text(face = "bold", size = 10.5),
    strip.placement = "outside",
    axis.title.x = element_text(face = "bold", size = 10.5),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 10.5),
    legend.position = "bottom",
    axis.title.y = element_blank()
  ) 

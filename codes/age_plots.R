new_plot <- my_data %>%
  group_by(Age, Genre, Type.of.acquisition) %>%
  summarise(Value = sum(Value))
new_plot


new_plot <- new_plot %>%
  mutate(Value = ifelse(Genre == "Female",
                        Value, -Value))

new_plot <- new_plot %>% 
  mutate(category_age=cut(Age, breaks=c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, Inf), 
                          labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59", "60 - 69", "70 - 79", "80 +")))



new_plot <- new_plot %>%
  select(Genre, category_age, Value, Type.of.acquisition) %>%
  mutate(Genre = as.factor(Genre), Cat_age = as.factor((category_age)),
         Type.of.acquisition = as.factor(Type.of.acquisition)) %>%
  group_by(Genre, category_age, Type.of.acquisition) %>%
  summarise(Value = sum(Value))


q <- ggplot(new_plot,
            aes(x = category_age, y = Value,
                fill = interaction(Genre,Type.of.acquisition))) +
  geom_col() +
  theme_bw() +
  theme(#legend.position = c(0.7,0.7),
    plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age", y = "Number of naturalisations", fill = "Type of naturalisations by gender",
       title = "Naturalisation by gender and age between 2011 and 2020") + 
  scale_fill_brewer(palette = c("RdGy")) +
  scale_y_continuous(breaks = seq(-100000, 100000, by = 10000), labels = abs) +
  coord_flip()  
q

grob <- grobTree(textGrob(c("Male","Female"), x=c(0.35,0.55),  y=c(0.95,0.95), 
                          hjust=0,
                          gp=gpar(col="slategray4", fontsize=10, fontface="bold")))
# Plot
q1 <- q + annotation_custom(grob) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9))
q1



################## ANIMATUION

new_age <- my_data %>%
  group_by(Year, Age, Genre, Type.of.acquisition) %>%
  summarise(Value = sum(Value))

new_age <- new_age %>%
  mutate(Value = ifelse(Genre == "Female",
                        Value, -Value))

new_age <- new_age %>% 
  mutate(category_age=cut(Age, breaks=c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, Inf), 
                          labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59", "60 - 69", "70 - 79", "80 +")))



new_age <- new_age %>%
  select(Year, Genre, category_age, Value, Type.of.acquisition) %>%
  mutate(Genre = as.factor(Genre),Year = as.integer(Year), Cat_age = as.factor((category_age)),
         Type.of.acquisition = as.factor(Type.of.acquisition)) %>%
  group_by(Year, Genre, category_age, Type.of.acquisition) %>%
  summarise(Value = sum(Value))



##### plot
ggplot(new_age,
       aes(x = category_age, y = Value,
           fill = interaction(Genre,Type.of.acquisition))) +
  geom_col() +
  theme_bw() + scale_fill_brewer(palette = c("RdGy")) +
  theme(# legend.position = c(0.7,0.7),
    plot.title = element_text(hjust = 0.5))  +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000), labels = abs) +
  coord_flip() + 
  transition_time(as.integer(Year)) + 
  labs(x = "Age", y = "Number of naturalisations", fill = "Type of naturalisations by gender", 
       title = "Naturalisation by Age and Gender in: {frame_time}") + 
  annotation_custom(grob) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9))


#THE FUNCTIONS I USED, IN ORDER OF APPEARENCE 

#Initial cleaning function (practice)
clean_function <- function(penguins_data) {
  penguins_data %>%
    select(-starts_with("Delta")) %>%
    select(-Comments) %>%
    clean_names()
}

# A function to clean column names (make computer readable)
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove empty columns and rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}

# A function to subset the data based on the list of column names
subset_columns <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

# A function to subset the penguins data set based on species
filter_by_species <- function(penguins_data, selected_species) {
  penguins_data %>%
    filter(species == selected_species)
}

# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}

#A function to create an explanatory plot
explanatory_plot <- function(cleaned_data) {
  ggplot(cleaned_data, 
         aes(x = culmen_length_mm, 
             y = body_mass_g, 
             colour = species)) +
    geom_point(size = 2.5, alpha = 1) +
    labs(x = "Culmen Length (mm)", y = "Body Mass (g)", title = "Explanatory Plot") +
    theme_light() +
    theme(panel.grid = element_line(color = "black", linetype = "dashed"))
}


# A function to save the explanatory plot as a PNG
save_explanatory_png <- function(data, file_path = "figures/explanatory_plot.png", width = 6, height = 6.8, dpi = 300) {
  plot <- explanatory_plot(data)
  ggsave(file_path, plot, width = width, height = height, dpi = dpi)
}

#A function to create the final graph with transformed data
#note: instead of inputting transformed data, I just log the aestetics x and y

final_linear_model_plot <- function(cleaned_data) {
  ggplot(cleaned_data,
         aes(x = log(culmen_length_mm), 
             y = log(body_mass_g))) + 
    geom_point(size = 2.5, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 2, color = "hot pink") + 
    ylim(7.5,9.0) +
    xlim(3.57,4.01) +
    labs(x = "log(Culmen Length (mm))", y = "log(Body Mass (g))", title = "The relationship between Culmen Length and Body Mass for Palmer Penguins") +
    theme_light()
}













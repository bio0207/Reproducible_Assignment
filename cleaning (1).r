
#Removing columns that are irrelevant 
clean_function <- function(penguins_raw) {
  penguins_raw %>%
    select(-starts_with("Delta")) %>%
    select(-Comments) %>%
    clean_names()
  
}
# Change column names to lower case
clean_column_names <- function(penguins_raw) 
    penguins_raw %>%
        clean_names()


# Shorten Species Nmaes
shorten_species <- function(penguins_raw) {
    penguins_raw %>%
        mutate(species = case_when(
            species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
            species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
            species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
        ))
}

# Remove empty columns or rows
remove_empty_columns_rows <- function(penguins_raw) {
    penguins_raw %>%
        remove_empty(c("rows", "cols"))
}


# A function to subset the data based on the list of column names
subset_columns <- function(penguins_raw, column_names) {
    penguins_raw %>%
        select(all_of(column_names))
}

# Subset the penguins data set based on species
filter_by_species <- function(penguins_raw, selected_species) {
    penguins_raw %>%
        filter(species == selected_species)

}

# Subset the penguins data set based on sex
filter_by_sex <- function(penguins_raw, selected_sex) {
  penguins_raw %>%
    filter(sex == selected_sex)
  
}

# A function to filter the penguins data set based on island
filter_by_island <- function(penguins_raw, selected_island) {
  penguins_raw %>%
    filter(sex == selected_island)
  
}

# --- --- ---


# Remove rows containing NA values
remove_NA <- function(penguins_data) {
    penguins_data %>%
        na.omit()
}



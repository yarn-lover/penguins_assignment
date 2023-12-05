## ---------------------------
##
## Script name: plotting.r
##
## Purpose of script: 
##      Codes functions to produce figures used in the analysis for Question 2 of the assignment
## 
##      Author: **unnamed candidate**
##
## Date Created: 2023-12-04
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

#a function to create an exploratory plot: 'exploratory_figure'
##it will use the dataset penguins_explore
##it will create a scatterplot, with x = flipper length; y = body mass; species shown by colour; sex shown by shape
plot_exploratory_figure <- function(penguins_explore){
  penguins_explore %>%
    ggplot(aes(x=flipper_length_mm, y=body_mass_g, colour=species, shape=sex)) +
  geom_point()+
  labs(x="Flipper length (mm)", y="Body mass (g)", title="How penguin flipper length varies with body mass", species="Species", sex = "Sex", scale_shape_discrete(name = "Your Legend Title"))+
  theme_bw()
}

#a function to save 'exploratory_figure' as a .svg vector file in the 'figures' folder
save_exploratory_figure_svg <- function(penguins_explore,
                                          filename, size, scaling){
  size_inches = size / 2.54
  svglite(filename, width = size_inches, 
                    height = size_inches,
                    scaling = scaling)
  exploratory_figure<- plot_exploratory_figure(penguins_explore)
  print(exploratory_figure)
  dev.off()
}

#a function to save 'exploratory_figure' as a .png report file in the 'figures' folder
save_exploratory_figure_png <- function(penguins_explore,
                                          filename, size, res, scaling){
  agg_png(filename, width = size,
                    height = size,
                    units = "cm",
                    res = res, 
                    scaling = scaling)
  exploratory_figure <- plot_exploratory_figure(penguins_explore)
  print(exploratory_figure)
  dev.off()
}

#a function to create a results plot: 'results_figure'
##it will use the dataset penguins_female_gentoo
##it will create a scatterplot, with x = flipper length; y = body mass
##it will also display the value of r (correlation coefficient) and associated p value
plot_results_figure <- function(penguins_female_gentoo){
  penguins_female_gentoo %>%
    ggplot(aes(x=flipper_length_mm, y=body_mass_g)) +
    geom_point(colour="#0073C2FF", size = 3)+
    geom_text(x =220, y=4050, label = paste("Statistical test results:", "\n",
      "r:", round(r, 3), "\n", "p value:", format(round(p_value, 5), scientific=FALSE)))+
    labs(x="Flipper length (mm)", y="Body mass (g)", title="How Female Gentoo penguin flipper length correlates with body mass") +
    theme_bw()
}

#a function to save 'results_figure' as a .svg vector file in the 'figures' folder
save_results_figure_svg <- function(penguins_female_gentoo,
                                          filename, size, scaling){
  size_inches = size / 2.54
  svglite(filename, width = size_inches, 
          height = size_inches,
          scaling = scaling)
  results_figure <- plot_results_figure(penguins_female_gentoo)
  print(results_figure)
  dev.off()
}

#a function to save 'results_figure' as a .png report file in the 'figures' folder
save_results_figure_png <- function(penguins_female_gentoo,
                                          filename, size, res, scaling){
  agg_png(filename, width = size,
          height = size,
          units = "cm",
          res = res, 
          scaling = scaling)
  results_figure <- plot_results_figure(penguins_female_gentoo)
  print(results_figure)
  dev.off()
}





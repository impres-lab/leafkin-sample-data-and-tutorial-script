# leafkin tutorial script
# Author: Jonas Bertels
# Last revision: 2020-08-18
#' ------------------------
#' 
#' This script demoes the functionality of the leafkin package
#' 
#' leafkin is a package which allows the user to easily perform the data
#' analysis involved in a kinematic anlaysis. leafkin is published through the 
#' IMPRES-lab GitHub leafkin repository.
#' 
#' For an in depth description of the functions, please refer to the publication
#' of the leafkin package and/or the user manual (Supplemental File 1).
#' 
# Script structure ----
#' ---------------------
#' 
#' This script has following structure:
#' 
#' 0. Quick start
#' 1. Install leafkin and the tidyverse packages
#' 2. Load the libraries which this script uses (leafkin and tidyverse)
#' 3. Performing the kinematic analysis
#' 3.A. Calculating leaf elongation rates (LERs)
#' 3.B. Fit cell lengths and plot the fits to evaluate cell length fit
#' 3.C. Extract fitted cell lengths
#' 3.D. Perform final data analysis 
#' 4. Some extra functionalities
#' 4.A Extra functionalities of calculate_LER
#' 4.B Recreate plot for average cell lengths

#'##############################################################################
#'##############################################################################
#'##############################################################################

# 0. Quick start ----
#'-------------------
# In the quick start section, a limited amount of information is given. This
# section however clearly demonstrates how a small set of functions can perform
# an entire kinematic analysis.
# For a more in depth explanation, please start at:
# 1. Install leafkin and the tidyverse packages

# 0.1 Install the leafkin package from the IMPRES-lab GitHub leafkin repository.
#' We will use the install_github function from the devtools package. Therefore,
#' we must install the devtools package first. Since we will also use functions
#' from the tidyverse package collection, we will also install the tidyverse
#' package (https://www.tidyverse.org/).
install.packages(c("tidyverse", "devtools"))

#' Windows users will have a cleaner installation with Rtools40 installed.
#' This software can be downloaded for free from:
#' https://cran.r-project.org/bin/windows/Rtools/

#' Up next, we can install leafkin. During its installation, you might be asked
#' whether or not to install/update all packages which leafkin uses. During your
#' first installation try, select: 1. All by typing 1 in the console and
#' pressing ENTER. When asked whether you want to compile uncompiled versions,
#' most stable here is to select: no by typing no in the console and pressing
#' ENTER. When a package fails to update, it will stop the leafkin installation.
#' Just run the installation line again. When you are then asked again whether
#' or not to install/update the remaining packages, just hit ENTER, skipping the
#' update.
devtools::install_github("impres-lab/leafkin")  # could give a warning concerning Rtools when Rtools is not installed, but should install

# 0.2 Load libraries
library("leafkin")
library("tidyverse")

# 0.3 Load required data into R
#' Leaf length data
leaf_length_measurements_path <- file.path("data files",
                                           "growth_measurements_millimetre.txt")
leaf_length_measurements <- read_tsv(leaf_length_measurements_path)

#' Cell length data
cell_length_measurements_path <- file.path("data files",
                                           "cell_length_measurements_micrometre.txt")
cell_length_measurements <- read_tsv(cell_length_measurements_path)

#' Meristem size data
mersitem_length_measurements_path <- file.path("data files",
                                               "meristem_size_micrometre.txt")
meristem_size <- read_tsv(mersitem_length_measurements_path)

# 0.4 Perform kinematic analysis
# 0.4.1 Average leaf elongation rates of each plant
result_LER_means <- calculate_LER(leaf_length_data = leaf_length_measurements,
                                  n_LER_for_mean = 2,
                                  output = "means")
view(result_LER_means)

# 0.4.2 Evaluate cell length fits and store bandwidths
#' A pdf file with the cell length fits will be created in the working directory.
bw_tibble <- get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements,
                                                interval_in_cm = 0.1,
                                                bw_multiplier = 1,
                                                output_bw_tibble = TRUE) 

# 0.4.3 Obtain fitted cell lengths.
fitted_cell_lengths <- get_all_fitted_cell_lengths(cell_length_data = cell_length_measurements, 
                                                   interval_in_cm = 0.1, 
                                                   bw_multiplier = 1, 
                                                   alternative_bw = mean(bw_tibble$collected_bandwidths, 
                                                                         na.nm = TRUE), 
                                                   tidy_cell_lengths = TRUE) 
view(fitted_cell_lengths)

# 0.4.4 Perform kinematic analysis calculations
#' final_kinematic_analysis performs all kinematics calculations at once.
final_kinematic_analysis <- kinematic_analysis(LER_means = result_LER_means, 
                                               tidy_cell_lengths = fitted_cell_lengths,
                                               meristem_size_micrometre = meristem_size)
view(final_kinematic_analysis)

# DONE
#' All calculation involved are done. You can opt to write the results to a 
#' txt-file:
write_tsv(final_kinematic_analysis, "kinematic_analysis_results.txt")

#'##############################################################################
#'##############################################################################
#'##############################################################################

# Detailed explanation of the functions and their posibilities.----

# 1. Install leafkin and the tidyverse packages----
#'-------------------------------------------------
#' We will use the install_github function from the devtools package. Therefore,
#' we must install the devtools package first. Since we will also use functions
#' from the tidyverse package collection, we will also install the tidyverse
#' package (https://www.tidyverse.org/).
install.packages(c("tidyverse", "devtools"))

#' Windows users will have a cleaner installation with Rtools40 installed.
#' This software can be downloaded for free from:
#' https://cran.r-project.org/bin/windows/Rtools/

#' Up next, we can install leafkin. During its installation, you might be asked
#' whether or not to install/update all packages which leafkin uses. During your
#' first installation try, select: 1. All by typing 1 in the console and
#' pressing ENTER. When asked whether you want to compile uncompiled versions,
#' most stable here is to select: no by typing no in the console and pressing
#' ENTER. When a package fails to update, it will stop the leafkin installation.
#' Just run the installation line again. When you are then asked again whether
#' or not to install/update the remaining packages, just hit ENTER, skipping the
#' update.
devtools::install_github("impres-lab/leafkin")  # could give a warning concerning Rtools when Rtools is not installed, but should install



# 2. Load the libraries which this script uses (leafkin and tidyverse) ----
#'-------------------------------------------------------------------------
# Now that you have installed leafkin and tidyverse, you need to load these
# libraries in the current R-session. We will load the leafkin package itself,
# which will allow us to perform the kinematic analysis. Next, we will load the
# tidyverse package, which will load functions that we will use to make some
# plots of our own and read in the datasets.

# Load the leafkin package:
library("leafkin")
# Load the tidyverse package:
library("tidyverse")

# leafkin help files can be consulted when interested:
# Running the line below, will open the help file of leafkin in the help tab of RStudio.
?leafkin

# 3. Performing the kinematic analysis ----
#'-----------------------------------------
# Now, we are all set to perform the kinematic analysis in R.

# 3.A. Calculating leaf elongation rates (LERs) ----
#'--------------------------------------------------
# The first step is to calculate the leaf elongation rates (LERs).
# For this, we will need our leaf length measurements data.
# First, we will create a file path which tells R where to find these data.
# Next, we run the calculate_LER function to calculate the LERs.

# Remark: 
# Note that file paths are operating system specific. Normally, the file.path
# function will create the right path to the example file and the read_tsv
# function should be able to use this path on both Windows and Mac OS.
#
# If you kept the structure of the folders within the leafkin-sample-data-and
# -tutorial-script folder intact, the following line of code will create the
# right file path for you.

# Create the path to the leaf length measurements file:
leaf_length_measurements_path <- file.path("data files",
                                           "growth_measurements_millimetre.txt")
# Read the leaf length data file
leaf_length_measurements <- read_tsv(leaf_length_measurements_path)
# Show first 5 rows of leaf length date to inspect the data
head(leaf_length_measurements, 5)

# Calculate LERs:
# As described in the article, the calculate_LER function has three parameters:
# leaf_length_data = here we put the variable name which contains our leaf length measurements
# n_LER_for_mean = here we indicate how many LERs we want to use to calculate the mean
# output = here we indicate that we want the function to output the mean LER of each plant

# Calculate the mean LERs and store them in result_LER_means:
result_LER_means <- calculate_LER(leaf_length_data = leaf_length_measurements,
                                  n_LER_for_mean = 2,
                                  output = "means")
# To view the contents of result_LER_means, run the following line:
view(result_LER_means)


# 3.B. Fit cell lengths and plot the fits to evaluate cell length fit ----
#'------------------------------------------------------------------------
# The next step is to use our cell length measurements to obtain all fitted cell
# lengths. Before we extract these fitted cell lengths, we must first evaluate
# the quality of the fit using the get_pdf_with_cell_length_fit_plots function.

# To fit the measured cell lengths, we again need the path to file which
# contains these measurements. 

# If you kept the structure of the folders within the leafkin-sample-data-and
# -tutorial-script folder intact, the following line of code will create the
# right file path for you.
cell_length_measurements_path <- file.path("data files",
                                           "cell_length_measurements_micrometre.txt")

# Read the cell length data file
cell_length_measurements <- read_tsv(cell_length_measurements_path)
# Show first 5 rows of cell length date to inspect the data
head(leaf_length_measurements, 5)

# Now, we can use the get_pdf_with_cell_length_fit_plots. This this function
# will create a pdf file in the work directory (i.e. the one with the R-project
# file which you have just opened here in R Studio) containing all cell length
# fit plots. This file can be used to evaluate the fits.
# We want cell lengths to be estimated every millimeter. For this, we set
# interval_in_cm to 0.1 cm.
get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements,
                                   interval_in_cm = 0.1) # we want cell lengths to be estimated every 0.1 cm
# When you have ran the line above, you will find a pdf in your work directory.

# When you open this file, you will find cell length fit plots and plots of
# their derivative. What you want to pay attention to is that the cell lengths
# are fitted appropriately: In the meristem and meristem to elongation zone
# transition, you want the fit to be quite tight. If the fit is too loose, it
# might result in wrongly estimated cell lengths at these positions. However, a
# tight fit comes also with a trade-off, especially towards the end of the
# elongation zone. Here, you might encounter more variation in the measured cell
# lengths and a very tight fit will incorporate all these details. Ideally, you
# would want to approach a smooth sigmoid curve.

# As an example for good looking fits, you can have a look at the created pdf.

# If you want to alter the fit, the function has an extra parameter available
# which you can use to manipulate the bandwidth of the fit (essentially altering
# how tight the fit follows any variations in the data). This parameter is
# called bw_multiplier. If you set this bw_multiplier to a number between 0 and
# 1, the fit will follow the pattern stricter, while values larger than 1 will
# results in an more smooth fit.

# We can choose to have a MORE strict fit by multiplying the bandwidth by e.g. 0.3
get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements, 
                                   interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                   bw_multiplier = 0.3) # MULTIPLY BANDWIDTH BY 0.3
# We can choose to have a LESS strict fit by multiplying the bandwidth by e.g. 3
get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements, 
                                   interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                   bw_multiplier = 3) # MULTIPLY BANDWIDTH BY 3


# Lastly, there is one more thing we need to check:
# Were all cell length profiles fitted ?

# Sometimes, insuffucient data is available for bandwidth calculation and the
# function will generate a warning message, indicating that for X number of
# plants, the bandwidth was not calcualted. In the pdf file, this will result in
# plots of cell length data without a fit, but also in missing bars in the
# bandwidth barplot at the end of the pdf. In that case, you would want to use
# an alternative bandwidth. A good alternative is to take the mean of the
# successfully calculated bandwidths. To extract the calculated bandwidths, you
# can set the output_bw_tibble parameter to TRUE. A pdf will still be created.

# In the following line of code, we will store the returned bandwidths in
# bw_tibble.
bw_tibble <- get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements,
                                                interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                                output_bw_tibble = TRUE) # return all bandwidths
# Now, bw_tibble contains all calcuted bandwidths. 
view(bw_tibble)
# The following code also checks whether or not there are missing bandwidths in
# bw_tibble (returns TRUE if there are missing bandwidths).
anyNA(bw_tibble$collected_bandwidths)


# 3.C. Extract fitted cell lengths ----
#'-------------------------------------
# When you have had a look a the plots and if needed, selected a pleasing
# bw_multiplier, you can extract all cell lengths for every interval position.
# If some plants failed to have their bandwidth caculated, you can also use the
# mean of the succesfully calculated bandwidths here in the alternative_bw
# argument.

# To use the cell lengths in the kinematic_anlaysis funtions, they have to be in
# the tidy format. Therefore, the tidy_cell_lengths parameter is set to TRUE.

# The other parameters, i.e. cell_length_data and interval_in_cm remain the
# same as in get_pdf_with_cell_length_fit_plots.
fitted_cell_lengths <- get_all_fitted_cell_lengths(cell_length_data = cell_length_measurements, 
                                                   interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                                   bw_multiplier = 1, # 1 does not change the bandwidth, but we put it just to illustrate its use here.
                                                   alternative_bw = mean(bw_tibble$collected_bandwidths, na.nm = TRUE), # in our case, including this is not neccesary, since all bandwidth were calculated. We include this for the sake of completeness.
                                                   tidy_cell_lengths = TRUE) # This is the format we need our cell lengths to be in, in order to enter them in the kinematic analysis function.
view(fitted_cell_lengths)


# 3.D. Perform final data analysis ----
#'-------------------------------------
# The last part of the kinematic analysis will perform all final calculations at
# once and return the results in a tibble.

# For this, it needs three datasets:
# - the mean leaf elongation rates in a tidy format
# - the fitted cell lengths in a tidy format
# - meristem sizes in a tidy format

# We have already calculated the mean leaf elongation rates and stored them in
# result_LER_means
view(result_LER_means)
# Also the fitted cell lengths are already calculated in the previous steps and 
# available in fitted_cell_lengths
view(fitted_cell_lengths)

# We will now create the path to the meristem size file.
# Again, if you kept the structure of the folders within the zipped folder in 
# tact, the following line of code will create the right file path for you.
meristem_length_measurements_path <- file.path("data files",
                                               "meristem_size_micrometre.txt")

# Read the meristem length data file
meristem_size <- read_tsv(meristem_length_measurements_path)
# Show first 5 rows of meristem length data to inspect the data
head(meristem_size, 5)

# The kinematic_analysis function will now combine all this data and perform all 
# remaining kinematics calculations at once.
final_kinematic_analysis <- kinematic_analysis(LER_means = result_LER_means, 
                                               tidy_cell_lengths = fitted_cell_lengths,
                                               meristem_size_micrometre = meristem_size)

# The analysis is now finished and all results are stored in 
# final_kinematic_analysis.
view(final_kinematic_analysis)

# If you want to use these data outside R, you can export them to a txt-file 
# with the following line:
write_tsv(final_kinematic_analysis, "kinematic_analysis_results.txt")
# After running this line, the kinematic_analysis_results.txt-file is created
# in the work directory (i.e. the one with the R-project file which you have 
# just opened here in R Studio).



# 4. Some extra functionalities ----
#'----------------------------------
# 4.A Extra functionalities of calculate_LER ----
#'-----------------------------------------------
# It is also possible to extract all calculated LERs in the tidy or wide format.
# Tidy and wide formats are ways to store data. Tidy data are userfriendly when
# it comes to data processing in R, while the wide format tends to be more human
# readable.

# All calculated LERs in the tidy format:
all_LER_tidy <- calculate_LER(leaf_length_data = leaf_length_measurements,
                              output = "tidy_LER")
# All calculated LERs in the wide format:
all_LER_wide <- calculate_LER(leaf_length_data = leaf_length_measurements,
                              output = "wide_LER")

# If you are interested in what this data looks like, you can run the following
# two lines:
view(all_LER_tidy)
view(all_LER_wide)

# These can for instance be of use to make a LER plot.
# In the following lines, we use the all_LER_tidy data. Out of these data, we
# filter plant C.11. Then, we use na.omit() to remove the first LER value, which
# is NA. It is NA, because when you take the first leaf length measurement,
# there is no previous measurement available to calculate a LER. Hereafter, we
# use the ggplot function to have date_and_hour on the x-axis and LER on the
# y-axis. Lastly, we indicate that it should be a line plot using geom_line().
all_LER_tidy %>% 
  filter(plant_id == "C.11") %>% 
  na.omit() %>% 
  ggplot(aes(x = date_and_hour, y = LER)) +
  geom_line()
# Of course, much more complicated plots are possible, but out of the scope of
# this tutorial.

# 4.B Recreate plot for average cell lengths ----
#'-----------------------------------------------

# SE function: a function to calculate the SE when data has more then two points
error_function <- function(data) {
  number_of_data <- length(data)
  if (number_of_data > 2) {
    SE_mean = sd(data, na.rm = TRUE)/sqrt(sum(!is.na(data)))
  } else {
    SE_mean = NA
  }
  return(SE_mean)
}

# Function to extract letters, which we use to extract the treatment letters
letter_function <- function(x) {
  return(sub("^([[:alpha:]]*).*", "\\1", x))
}

# Add treatments to fitted_cell_lengths
fitted_cell_lengths$treatment <- letter_function(fitted_cell_lengths$plant_id)
fitted_cell_lengths$treatment[fitted_cell_lengths$treatment == "C"] <- "Control"
fitted_cell_lengths$treatment[fitted_cell_lengths$treatment == "M"] <- "Mild"
fitted_cell_lengths$treatment[fitted_cell_lengths$treatment == "S"] <- "Severe"

# Calculate the mean cell lenghts and their standard error
cell_lenghts_mean_SE <- fitted_cell_lengths %>%
  group_by(treatment, position) %>% 
  summarise(meanAmount = mean(cell_length, na.rm = TRUE), 
            SE_mean = error_function(cell_length))

# **** Set graph parameters
# Choose size of lines
line_size <- 0.3
geom_line_line_size <- 0.3
# Point size
point_size <- 0.75
stroke_size <- 0.5
# Choose size of text
text_size <- 10

# Y-min / max
x_min <- 0
x_max <- 10
y_min <- 0
y_max <- 150

# labels
x_label <- "Distance from the base of the leaf (cm)"
y_label <- expression(atop("Cell length", paste("[µm]"))) 

# Error bars width
error_bar_width <- 0.05
error_bar_line_size <- 0.3

# Colors
# Treatment
# Gray scale
color_control <- "black"   
color_mild <- "blue" #
color_severe <- "red"
# Shapes
shape_control <- 8   # star
shape_mild <- 1  # empty circle
shape_severe <- 19   # full circle
# Linetype
linetype_control <- "solid"
linetype_mild <- "solid"
linetype_severe <- "solid"


# **** Make the graph
graph <- cell_lenghts_mean_SE %>% 
  # make the plot
  ggplot(aes(group = treatment, 
             x = position, 
             y = meanAmount, 
             ymin = meanAmount - SE_mean, 
             ymax = meanAmount + SE_mean)) +
  # add lines
  geom_line(aes(linetype = treatment),
            size = geom_line_line_size) +
  # add errorbars
  geom_errorbar(aes(colour = treatment),
                size = error_bar_line_size,
                width = error_bar_width,
                na.rm = TRUE) +
  # add points:
  geom_point(aes(colour = treatment,
                 shape = treatment),
             alpha = 1,
             size = point_size,
             stroke = stroke_size) +
  # Fix line types:
  scale_linetype_manual(name = "Treatment",
                        values = c("Control" = linetype_control,
                                   "Mild" = linetype_mild,
                                   "Severe" = linetype_severe)) +
  # Fix treatment colors
  scale_color_manual(name = "Treatment",
                     values = c("Control" = color_control,
                                "Mild" = color_mild,
                                "Severe" = color_severe)) +
  # Fix treatment shapes:
  scale_shape_manual(name = "Treatment", 
                     values = c("Control" = shape_control,
                                "Mild" = shape_mild,
                                "Severe" = shape_severe)) +
  # Make sure the x-axis has a nice an even distribution of cm values
  scale_x_continuous(limits = c(x_min, x_max),
                     breaks = 0:10,
                     expand = expansion(mult = c(0, 0.05))) + # get zero on y-axis level
  scale_y_continuous(limits=c(y_min, y_max),
                     breaks = c(0,25,50,75,100,125,150),
                     expand = expansion(mult = c(0, 0.05))) + # get zero on x-axis level
  # Set labels
  labs(y = y_label, x = x_label) + 
  # Make sure title is in the middle, if there is a title
  theme(plot.title = element_text(hjust = 0.5)) +
  # # Make a plot for each mineral separate
  # facet_grid(mineral~., scales = "free") +
  # Set theme to classic
  theme_classic() +
  # change text size of figure
  theme(text = element_text(size = text_size)) +
  # change axis line thickness
  theme(axis.line = element_line(colour = "black", size = line_size)) +
  # change thickness of ticks on axis
  theme(axis.ticks = element_line(colour = "black", size = line_size)) +
  # make all text black
  theme(axis.text = element_text(colour = "black")) +
  # make full square around graph
  theme(panel.border = element_rect(colour = "black", fill = NA, size = line_size))
graph


# Save plot
plot_name = "cell_length_plot"
plot_width = 15.9  #A4 width = 21 cm / 15.9 
plot_height = 6 #A4 height = 29.7 cm / 24.6 
plot_units = "cm"
plot_dpi = 300 #320 is retina dpi. 300 would be print dpi
# change line and text size at graph section

# Folder and filename
folder = "images"
dir.create(folder)
plot_device = "tiff"
file_name = paste(paste(paste(folder, "/", sep = ""), paste(plot_name, plot_width, plot_height, plot_dpi, paste("textsize", text_size, sep = ""), sep = "_"), sep = ""), plot_device, sep = ".")

# Save the plot
ggsave(plot = graph,
       file = file_name,
       device = plot_device,
       width = plot_width,
       height = plot_height,
       units = plot_units,
       dpi = plot_dpi)

















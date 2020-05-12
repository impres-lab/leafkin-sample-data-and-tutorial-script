# leafkin demo script ----
#' -----------------------
#' 
#' This script demoes the functionality of the leafkin package, published in:
#' PUBLICATION INFO HERE
#' 
#' leafkin is a library which allows the user to easily perform the data
#' analysis involved in a kinematic anlaysis. leakin is published through the 
#' impres-lab GitHub leakin repository.
#' 
#' For an in depth description of the functions, please refer to the publication
#' of the leafkin package.
#' 
#' The example datasets come from the following publication:
#' PUBLICATION INFO HERE
#' 
# Script strucuture ----
#' ---------------------
#' 
#' This script has following structure:
#' 
#' 0. Quick start
#' 1. Install leafkin and the tidyverse packages
#' 2. Loading the libraries which this script uses (leafkin and tidyverse)
#' 3. Performing the kinematic analysis
#' 3.A. Calculating leaf elongation rates (LERs)
#' 3.B. Fit cell lengths and plot the fits to evaluate cell length fit
#' 3.C. Extract fitted cell lengths
#' 3.D. Perform final data analysis 
#' 4. Some extra functionalities
#' 4.A Extra functionalities of calculate_LER

#' Note: 
#' -----
#' You can skip step 1 and 2 (0.1 from quick start) if you have the most recent
#' script and required packages already installed on your computer.


#'##############################################################################
#'##############################################################################
#'##############################################################################

# 0. Quick start ----
#'-------------------
# In the quick start section, a limited amount of information is given. This
# section however cleary demonstrates how a small set of functions can perform
# an entire kinematic analysis.
# For a more in depth explanation, please start at:
# 1. Install leafkin and the tidyverse packages

# 0.1 Install the leafkin library grom the impres-lab GitHub leafkin repository.
#' We will use the install_github function from the devtools library.
#' Therefor, we must install the devtools library first. Since we will also use
#' functions from the tidyverse library collection, we will also install the
#' tidyverse libraries.
install.packages(c("tidyverse", "devtools"))
devtools::install_github("impres-lab/leafkin")  

# 0.2 Load libraries
library("leafkin")
library("tidyverse")

# 0.3 Load required data into R
#' Leaf length data
leaf_length_measurements_path <- file.path(getwd(), 
                                           "data files",
                                           "growth_measurements_millimetre.txt")
leaf_length_measurements <- read_tsv(leaf_length_measurements_path)

#' Cell length data
cell_length_measurements_path <- file.path(getwd(), 
                                           "data files",
                                           "cell_length_measurements_micrometre.txt")
cell_length_measurements <- read_tsv(cell_length_measurements_path)

#' Meristem size data
mersitem_length_measurements_path <- file.path(getwd(), 
                                               "data files",
                                               "meristem_size_micrometre.txt")
meristem_size <- read_tsv(mersitem_length_measurements_path)

# 0.4 Perform kinematic analysis
# 0.4.1 Leaf elongation rate
result_LER_means <- calculate_LER(leaf_length_data = leaf_length_measurements,
                                  n_LER_for_mean = 2,
                                  output = "means")
view(result_LER_means)

# 0.4.2 Evaluate cell length fits and store bandwidths
#' A pdf file with the cell length fits will be created in the work directory.
bw_tibble <- get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements,
                                                interval_in_cm = 0.1, 
                                                output_bw_tibble = TRUE) 

# 0.4.3 Obtain fitted cell lengths.
fitted_cell_lengths <- get_all_fitted_cell_lengths(cell_length_data = cell_length_measurements, 
                                                   interval_in_cm = 0.1, 
                                                   bw_multiplier = 1, 
                                                   alternative_bw = mean(bw_tibble$collected_bandwidths, na.nm = TRUE), 
                                                   tidy_cell_lengths = TRUE) 
view(fitted_cell_lengths)

# 0.4.4 Perform kinematic analysis
#' Remaining kinematics calculations, all at once.
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

# Detailed explanation of the fuctions and their posibilities.----

# 1. Install leafkin and the tidyverse packages----
#'-------------------------------------------------
# We will use the install_github function from the devtools library.
# Therefor, we must install the devtools library first. Since we will also use
# functions from the tidyverse library collection, we will also install the
# tidyverse libraries.
install.packages(c("tidyverse", "devtools"))
devtools::install_github("impres-lab/leafkin")


# 2. Load the libraries which this script uses (leafkin and tidyverse) ----
#'-------------------------------------------------------------------------
# In step 1, you have installed the libraries on your computer. Here, in step 3,
# you will load the required libraries in the current R-session. We will load
# the leafkin library itself, which will allow us to perform the kinematic
# analysis. Next, we will load the tidyverse package, which will load some
# functions which we will use to make some plots of our own and read in the
# datasets.

# Load the leafkin library:
library("leafkin")
# Load the tidyverse library:
library("tidyverse")

# leafkin help files can be consulted when interested:
# Running the line below, will open the help file of leafkin in the help tab of Rstudio.
?leafkin

# 3. Performing the kinematic analysis ----
#'-----------------------------------------
# Now, we are all set to perfom the kinematic analysis in R.

# 3.A. Calculating LER ----
#'-------------------------
# The first step is to calculate the leaf elongation rates (LERs).
# For this, we will need our leaf length measurements data.
# First, will will create a file path which tells R where to find these data.
# Next, we run the calculate_LER function to calculate the LERs.

# Remark: 
# Note that file paths are operating system specific. Normally, the file.path function
# will create the right path to the example file and the read_tsv function should be able
# to use this path on both Windows and Mac OS.
#
# If you kept the structure of the folders within the zipped folder in tact,
# the following line of code will create the right file path for you.

# Create the path to the leaf length measurements file:
leaf_length_measurements_path <- file.path(getwd(), 
                                           "data files",
                                           "growth_measurements_millimetre.txt")
# Read the leaf length data file
leaf_length_measurements <- read_tsv(leaf_length_measurements_path)
# Show first 5 rows of leaf length date to inspect the data
head(leaf_length_measurements, 5)

# Calculate LERs:
# As described in the article, the calculate_LER function has three parameters:
# LER_data = here we put the variable name which contains our leaf length measurements
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
# lengths.  Before we extract these fitted cell lengths, we must first evaluate
# the quality of the fit using the get_pdf_with_cell_length_fit_plots function.

# To fit the measured cell lengths, we again need the path to file which
# contains these measurements. 

# If you again kept the structure of the folders within the zipped folder in 
# tact, the following line of code will create the right file path for you.

cell_length_measurements_path <- file.path(getwd(), 
                                           "data files",
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

# We can choose to have a MORE strict fit by multiplying the bandwidth by e.g. 0.33
get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements, 
                                   interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                   bw_multiplier = 0.33) # MULTIPLY BANDWIDTH BY 0.33
# We can choose to have a LESS strict fit by multiplying the bandwidth by e.g. 3
get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements, 
                                   interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                   bw_multiplier = 3) # MULTIPLY BANDWIDTH BY 3


# Lastly, there is one more thing we need to check:
# Were all cell length profiles fitted ?
#
# Sometimes, insuffucient data is available for bandwidth calculation. This will
# result in empty plots, but also in missing bars in the bandwidth barplot at
# the end of the pdf. In that case, you would want to use an alternative
# bandwidth. A good alternative is to take the mean of the successfully
# calculated bandwidths. To extract the calculated bandwidths, you can set the
# output_bw_tibble parameter to TRUE. A pdf will still be created.

# In the following line of code, we will store the returned bandwidths in
# bw_tibble.
bw_tibble <- get_pdf_with_cell_length_fit_plots(cell_length_data = cell_length_measurements,
                                                interval_in_cm = 0.1, # we want cell lengths to be estimated every 0.1 cm
                                                output_bw_tibble = TRUE) # return all bandwidths
# Now, bw_tibble contains all calcuted bandwidths. 
view(bw_tibble)
# The following code also checks whether or not there are missing there are
# missing bandwidths (returns TRUE if there are missing bandwidths).
anyNA(bw_tibble$collected_bandwidths)


# 3.C. Extract fitted cell lengths ----
#'-------------------------------------
# When you have had a look a the plots and if needed, selected a pleasing
# bw_multiplier (if needed), you can extract all cell lengths for every interval
# position. If some plants failed to have their bandwidth caculated, you can
# also use the mean of the succesfully calculated bandwidths in the
# alternative_bw parameter.

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
# - the mean leaf elongation rates
# - the fitted cell lengths in a tidy format
# - meristem sizes

# We have already calculated the mean leaf elongation rates and stored them in
# result_LER_means
view(result_LER_means)
# Also the fitted cell lengths are already calculated in the previous steps and 
# available in fitted_cell_lengths
view(fitted_cell_lengths)

# We will now create the path to the meristem size file.
# Again, if you kept the structure of the folders within the zipped folder in 
# tact, the following line of code will create the right file path for you.
meristem_length_measurements_path <- file.path(getwd(), 
                                               "data files",
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


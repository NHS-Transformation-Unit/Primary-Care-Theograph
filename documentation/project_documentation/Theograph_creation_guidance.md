# Theograph Proof of Concept Dashboard

## Introduction
This documentation covers the intended methodology for running the Theograph Proof of Concept Dashboard. This guidance provides an overview of the different stages and in which order these need to be undertaken. 

<br/>

## Downloading R and RStudio
Before the dashboard can be created, users will need to ensure that they have downloaded the [R programming language](https://www.r-project.org/) and the [RStudio](https://posit.co/download/rstudio-desktop/) environment to their machine.

<br/>

## Running the dashboard itself
The file `Theograph_Dashboard.rmd` will run the tool and create the necessary visualisations for each part of the tool. The output of this is an `.html` file. To create this file ensure that all files are saved to their respective folders as per the online repository. Changing the location of these files will result in the tool not initialising. The outputs can be created by selecting the **run document** button at the top of the RStudio script pane. The dashboard output will then render and load the document into a viewing window. The output will also be available within the `src/visualisation` folder as an `.html` file, and can also be loaded within the internet browsing software of your choice.

<br/>

## Extracting the DynAirX data
This step is obsolete as of this latest version of this tool. A preprocessed data file, 'Theograph_Synethetic_Patient_Dataset.xlsx' is now included for ease of loading.

The file `Raw_Patient_Extract.sql` used to be required to be ran on the appropriate DynAirX SQL dataset before the dashboard can be created. For external users, they will need to request access to this database with the Data Into Action (DIA) team. It is worth noting that the current script is configured to be used by an external user, and as such may need to be altered to be ran by a Data Into Action employee who has different permissions. The original `DIA_Original_Script.sql` provided by the DIA team is located within the `src/data_extraction/sql_scripts` subfolder.

<br/>

## Amending the Theograph Proof of Concept Dashboard
All text within the dashboard can be changed directly within the `Theograph_Dashboard.rmd` file itself. Please take care when editing character strings within any code blocks as erroneously deleting any punctuation within these blocks can lead to the dashboard itself not functioning.

<br/>

## Packages
As the model is built and ran within the R programming language it will require dependent packages to be installed by the user. These packages will only require installing once. Please ensure that these packages are installed prior to running the model. This can be undertaken by using the `install.packages()` command within the console. For example `install.packages("tidyr")` will install the `tidyr` package. For a full list of packages used in this model, please see the `packages.R` script found in the `config/r_scripts` folder.

<br/>

## The constituent files
In total, there are three scripts that are necessary to create the dashboard. These are:

1. `Raw_Patient_Extract.sql` - this script, located in the `src/data_extraction` subfolder, extracts 20 patients from the DynAirX dataset.
2. `Data_Extraction_Processing.R` - this script, located in the `src/data_processing` subfolder, processes the raw DynAirX data to ready to be visualised.
3. `Theograph_Dashboard.rmd` - this script, located in the `src/visualisation` subfolder, contains the necessary code to run the R Shiny application which renders the primary care timeseries visualisations.

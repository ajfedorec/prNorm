# Deprecated - please use ucl-cssb/flopr instead

# Plate Reader Data Normalisation

R package for the normalisation of plate reader data.

A function is included to parse the output from the Tecan Spark plate reader into a format that is usable by this code.

## For example:

An example data file and a plate layout file are include in the example folder.

The workflow for working with this data is:
  1. parse the data file:
    `parsed_data <- sparkParse("example/tecan_data.csv", "example/plate_layout.csv")`
  2. normalise the data:
    `normed_data <- prNorm(pr_data = parsed_data, blank_well = "H1", neg_well = "G9", OD_name = "OD700", flu_names = c("GFP", "BFP", "mCherry"))`

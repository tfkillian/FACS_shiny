# FACS_shiny

## R Shiny dashboard app for visualizing FACS and other count matrix data

This Shiny app accepts .csv count matrix files (such as those resulting from
FACS analysis, metabolomics, proteomics or RNAseq experiments) as input and
creates a series of dynamically generated customizable diagnostic plots using
`Plotly` including: PCA, tSNE, heatmap, correlation plot, and a violin plot. The
resulting figures can be downloaded as .png or .pdf files. The data is also
displayed in a dynamic HTML table that allows specific entries in the dataset to
be queried.

The .csv input file must be in the following format:
* First column must contain genes, markers or metabolites
* Columns consist of samples or cells
* Values must be non-normalized, non-negative discrete numeric counts or
continuous decimal integers with no NA or missing values
* The dataframe must contain no row names

An example .csv file can be found in the `/data` directory.
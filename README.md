# FACS_shiny

## Shiny dashboard app for visualizing FACS and other count matrix data

This Shiny app accepts .csv count matrix files (such as those resulting from
FACS analysis, metabolomics, proteomics or RNAseq) as input and creates a series
of dynamically generated customizable diagnostic plots using Plotly including
PCA, tSNE, heatmap, correlation plot, and a violin plot. The resulting figures
can be downloaded as .png files.

The .csv input file must be in the following format:
* First column must contain genes, markers or metabolites
* Columns consist of samples or cells
* Values must be non-normalized, non-negative discrete numeric counts or
continuous decimal integers
* The dataframe must contain no row names

An example .csv file can be found in the `/data` directory.
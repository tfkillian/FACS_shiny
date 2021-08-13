# FACS_shiny

## R Shiny dashboard app for visualizing FACS and other count matrix data

Count data is frequently encountered in bioinformatics in the context of an
omics analysis, such as FACS analysis, metabolomics, proteomics, ATACseq,
CRISPR screens or RNAseq experiments. This `Shiny` app accepts .csv count matrix
files as input and creates a series of dynamically generated customizable
diagnostic plots using `Plotly` including: PCA, tSNE, heatmap, correlation plot,
and a violin plot. The resulting figures can be downloaded as .png or .pdf
files. The data is also displayed in a dynamic HTML table that allows specific
entries in the dataset to be queried.

The .csv input file must be in the following format:
* First column must contain a *unique* list of genes, markers or metabolites
* Columns consist of samples or cells
* Values must be non-normalized, non-negative discrete numeric counts or
continuous decimal integers with no NA or missing values
* The dataframe must contain no row names

An example .csv input file can be found in the `/data` directory.

This app is released under GPL-3.0 License.
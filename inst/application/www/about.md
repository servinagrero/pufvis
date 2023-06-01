<a href="https://servinagrero.github.io/pufvis"><img src="https://raw.githubusercontent.com/servinagrero/pufvis/develop/man/figures/logo.svg" align="right" height="94" /></a>

# What is PUFVis?

PUFVis is a shiny app to calculate, explore and visualize in detail Physical Unclonable Function metrics.

# How to use PUFVis?

First, CRPs should be loaded. This can be done directly through R while calling `pufvis` or through the _Import_ tab.
Once the CRPs have been imported, the metrics will be calculated automatically.

## Importing CRPs

CRPs can be imported from CSV, TSV or RDS files. If CSV or TSV is provided is read as a data frame. The data frame requires the columns `device`, `challenge`, `response` and `sample`. It can contain other columns. If an RDS file is provided, it's contents are read as a 3D matrix, where each row is a device, each column is a challenge and the 3rd dimension is used to represent the different samples.

## Creating CRPs from responses

__WORK IN PROGRESS__


## Explore

The explore tab provides an initial analysis of the CRPs. Some information about the CRPs is provided along with a response heatmap and the response bit ratio.


## Metrics

This tab displays all computed metrics for the given set of CRPs. The metrics provided are `Uniformity`, `Bitaliasing`, `Uniqueness` and `Reliability`.


## Export

This tab allows to export the metrics summary into different formats.
Moreover, the metrics can be exported to an excel file. In this way, each sheet corresponds to a different metric.

# References

Internally, PUFVis uses [PUFR](https://servinagrero.github.io/pufr) to calculate the metrics of the responses.

To know more about PUFVis, please refer to the official [online documentation](https://servinagrero.github.io/pufvis)

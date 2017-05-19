# The Syrian Archive

[![Build Status](https://travis-ci.org/seankross/syrianarchive.svg?branch=master)](https://travis-ci.org/seankross/syrianarchive)

[The Syrian Archive](https://syrianarchive.org/) is an evidence gathering effort
with the goal of collecting and creating metadata for videos depicting 
atrocities committed during the Syrian Civil War. These videos are often
recorded by civilians with their phones and uploaded to video sharing sites like
YouTube. This data package is inteded to be used for creating awareness about
the humanitarian cost of the conflict through analysis, visualization, and
storytelling.

This package contains two datasets:

- `violations`: Metadata for each video including latitude, longitude, date,
time, and description.
- `syrian_places`: Latitude and longitude data for locations in Syria.

## Installation

```R
# install.packages("devtools")
devtools::install_github("seankross/syrianarchive")
```

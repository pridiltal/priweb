+++
abstract = "This article proposes a framework that provides early detection of anomalous series within a large collection of non-stationary streaming time series data. We define an anomaly as an observation that is very unlikely given the recent distribution of a given system. The proposed framework first calculates a boundary for the system's typical behavior using ex- treme value theory. Then a sliding window is used to test for anomalous series within a newly arrived collection of series. The model uses time series features as inputs, and a density-based comparison to detect any significant changes in the distribution of the features. Using various synthetic and real world datasets, we demonstrate the wide applicability and usefulness of our proposed framework. We show that the proposed algorithm can work well in the pres- ence of noisy non-stationarity data within multiple classes of time series. This framework is implemented in the open source R package oddstream. R code and data are available in the supplementary materials."
abstract_short = "This article proposes a framework that provides early detection of anomalous series within a large collection of non-stationary streaming time series data."
authors = ["Priyanga Dilini Talagala", "Rob J. Hyndman", "Kate Smith-Miles", "Sevvandi Kandanaarachchi", "Mario A. Munoz"]
date = "2019"

image_preview = ""
math = true
publication_types = ["3"]
publication = "In *Journal of Computational and Graphical Statistics* (to appear)" 
publication_short = "In *JCGS*"
selected = true
title = "Anomaly Detection in Streaming Nonstationary Temporal Data"

url_pdf = "https://robjhyndman.com/papers/oddstream.pdf"

url_project = "https://github.com/pridiltal/oddstream"
#url_slides = "#"
#url_video = "#"
[[url_custom]]
name = "Custom Link"
url = "https://github.com/pridiltal/oddstream"

# Optional featured image (relative to `static/img/` folder).
[header]
image = "headers/mvtsplot.png"
caption = "Multivariate time series plot of a dataset obtained using a fiber optic cable. Axis Cable represents individual points of the sensor cable. There are 640 time series each with 1459 time points. Yellow corresponds to low values and black to high values. The black region near the upper end point of the cable (around 350 to 500) indicates the presence of an anomalous event (e.g., intrusion attack, gas pipeline leak, etc.) that has taken place during the 500-1300 time period."

+++
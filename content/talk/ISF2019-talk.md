+++
date = "2019-06-18"
title = "A feature-based framework for detecting technical outliers in water-quality data from in situ sensors"
abstract = "Outliers due to technical errors in water-quality data from *in situ* sensors can reduce data quality and have a direct impact on inference drawn from subsequent data analysis. However, outlier detection through manual monitoring is infeasible given the volume and velocity of data the sensors produce. Here, we propose an automated framework that provides early detection of outliers in water-quality data from *in situ* sensors caused by technical issues. We compare two approaches to this problem: (1) using forecasting models; and (2) using feature vectors with extreme value theory.\par In the forecasting models, observations are identified as outliers when they fall outside the bounds of an established prediction interval. For this comparison study we considered two strategies: anomaly detection (AD) and anomaly detection and mitigation (ADAM) for the detection process. With ADAM, the detected outliers are replaced with the forecast prior to the next prediction, whereas AD simply uses the previous measurements without making any alteration to the detected outliers.\par The feature-based framework first identifies the data features that differentiate outlying instances from typical behaviours. Then statistical transformations are applied to make the outlying instances stand out in transformed data space. Unsupervised outlier scoring techniques are then applied to the transformed data space. An approach based on extreme value theory is used to calculate a threshold for each potential outlier. This threshold calculation process starts by computing a boundary using a subset of data containing half of the observations with the smallest outlier scores and then tests for potential outliers in the remaining subset. This approach successfully identified outliers involving abrupt changes in turbidity, conductivity and river level, including sudden spikes, sudden isolated drops and level shifts, while maintaining very low false detection rates.\par The proposed framework was evaluated using  two data sets obtained from *in situ* sensors in rivers flowing into the Great Barrier Reef lagoon. The proposed framework is implemented in the open source R package `oddwater`."
abstract_short = ""
event = "39th International Symposium on Forecasting,Thessaloniki, Greece"
event_url = "https://isf.forecasters.org/"
location = "Makedonia Palace, Thessaloniki, Greece."


selected = false
math = true

url_pdf = "https://www.monash.edu/business/ebs/research/publications/ebs/wp01-2019.pdf"

url_slides = "talks/ISF2019/ISF2019.html"
url_video = ""



# Optional featured image (relative to `static/img/` folder).
[header]
#image = "headers/talks_ISF2017.jpg"
#caption = "37th International Symposium on Forecasting Cairns, Australia."


#Embed your slides or video here using [shortcodes](https://gcushen.github.io/hugo-academic-demo/post/writing-markdown-latex/). Further details can easily be added using *Markdown* and $\rm \LaTeX$ math code. 

+++




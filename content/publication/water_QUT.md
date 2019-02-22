+++
abstract = "Monitoring the water quality of rivers is increasingly conducted using automated in situ sensors, enabling timelier identification of unexpected values or trends. However, the data are confounded by anomalies caused by technical issues, for which the volume and velocity of data preclude manual detection. We present a framework for automated anomaly detection in high-frequency water-quality data from in situ sensors, using turbidity, conductivity and river level data collected from rivers flowing into the Great Barrier Reef. After identifying end-user needs and defining anomalies, we ranked anomaly importance and selected suitable detection methods. High priority anomalies included sudden isolated spikes and level shifts, most of which were classified correctly by regression-based methods such as autoregressive integrated moving average models. However, incorporation of multiple water-quality variables as covariates reduced performance due to complex relationships among variables. Classifications of drift and periods of anomalously low or high variability were more often correct when we applied mitigation, which replaces anomalous measurements with forecasts for further forecasting, but this inflated false positive rates. Feature-based methods also performed well on high priority anomalies and were similarly less proficient at detecting lower priority anomalies, resulting in high false negative rates. Unlike regression-based methods, however, all feature-based methods produced low false positive rates and have the benefit of not requiring training or optimization. Rule-based methods successfully detected a subset of lower priority anomalies, specifically impossible values and missing observations. We therefore suggest that a combination of methods will provide optimal performance in terms of correct anomaly detection, whilst minimizing false detection rates. Furthermore, our framework emphasizes the importance of communication between end-users and anomaly detection developers for optimal outcomes with respect to both detection performance and end-user application. To this end, our framework has high transferability to other types of high frequency time-series data and anomaly detection applications."
abstract_short = "A mobile visual clothing search system is presented whereby a smart phone user can either choose a social networking image or capture a new photo of a person wearing clothing of interest and search for similar clothing in a large cloud-based ecommerce database. The phone's GPS location is used to re-rank results by retail store location, to inform the user of local stores where similar clothing items can be tried on."
authors = ["Catherine Leigh", "Omar Alsibai", Rob J. Hyndman", "Sevvandi Kandanaarachchi", "Olivia C. King", "James M. McGree", "Catherine Neelamraju", "Jennifer Strauss", "Priyanga Dilini Talagala", "Ryan D. R. Turner", "Kerrie Mengersen", "Erin E. Peterson"]
date = "2019-05-10"
image_preview = ""
math = true
publication_types = ["3"]
publication = "In *Science of The Total Environment*" 
#publication_short = "In *ICMEW*"
selected = true
title = "A framework for automated anomaly detection in high frequency water-quality data from in situ sensors"

url_pdf = "https://www.sciencedirect.com/science/article/pii/S0048969719305662"


[[url_custom]]
name = "Custom Link"
url = "https://www.sciencedirect.com/science/article/pii/S0048969719305662"

# Optional featured image (relative to `static/img/` folder).
[header]
image = "headers/water_QUT.jpg"
caption = "The ten-step Anomaly Detection (AD) framework for high frequency water-quality data, which includes ranking the importance of different anomaly types (e.g. sudden spikes A, sudden shifts D, anomalously high variation type E), based on end-user needs and data characteristics, to inform algorithm choice, implementation and performance evaluation. Framework numbers indicate the order of steps taken. Arrows indicate directions of influence between steps."

+++
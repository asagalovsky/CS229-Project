# CS229 Final Project
# Making Our Cities Safer: A Study of Neighborhood Crime Patterns

# Data Sources
Census data <br />
http://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml <br />
Related Stack Exchange post: http://gis.stackexchange.com/questions/9380/where-to-get-2010-census-block-data <br />

Crime data  <br />
New York - https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i  <br />
Los Angeles - http://shq.lasdnews.net/CrimeStats/CAASS/desc.html <br />
Chicago - https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2 <br />
Philadelphia - https://www.opendataphilly.org/dataset/crime-incidents <br />
DC - http://opendata.dc.gov/datasets/fdacfbdda7654e06a161352247d3a2f0_34 <br />
Detroit - https://data.detroitmi.gov/Public-Safety/DPD-All-Crime-Incidents-2009-Present-Provisional-/b4hw-v6w2 <br />
San Francisco - https://data.sfgov.org/Public-Safety/SFPD-Incidents-from-1-January-2003/tmnf-yvry <br />

Note that only D.C. has labels for Census tract for each crime incident. To convert Latitude/Longitude to Census tract, follow instructions on this page: http://stackoverflow.com/questions/29872109/binning-longitude-latitude-labeled-data-by-census-block-id  <br />

Shape files for each county can be downloaded here: http://www.census.gov/cgi-bin/geo/shapefiles2010/main <br />

# To Do
Download relevant demographics data from Census  <br />
Merge along crime totals  <br />
Start training models  <br />

# Model Ideas
Cluster Census tracts from different cities with similar demographics, see if there is crime prominance is similar <br />
Create models that uses Census data to predict crime rates on tract level  <br />
Then, identify principal components (i.e. most relevant features) <br />
Given the total incidences of crime, can we predict the distribution by location? Is it concentrated is specific neighborhoods? What are the demographics of these neighborhoods? <br />


# General Purpose
To understand what demographic factors influence crime.  More specifically, do these factors differ across major cities or do they differ more at the neighborhood level?  Do factors influence the type of crime or time of a day a crime is committed?  

Why do we find this important? <br />
Many reasons...How can we better serve our cities to make them a safer place?  Should policy be implemented at a neighborhood level (police station) or a city level (police department).  

What we don't want to do <br />
Predict what type of crime will occur given a particular time of day and neighborhood.  We know this has been done before with very high accuracy.  Further, there are certain commonly accepted biases (like more crime in low income neighborhoods) that we don't wish comment on.   

What we want to do (this is what needs to be more clearly defined): <br />

Unsupervised - Deeply explore crime patterns at the neighborhood level.  Is an all black neighborhood more susceptible to crime than a racially integrated neighborhood?  At the city level, do segregated cities see more or less crime by neighborhood?  Across cities, are the major factors influencing crime the same or different?  For example, do the high poverty areas of LA see the same types of crimes as the high poverty areas of NYC?  

Supervised - This is a CRAZY idea.  What if we take demographics for a NEW city, or a NEW neighborhood, and try to predict the type/frequency of crime in that place?  I'm not feeling like I have many really interesting supervised ideas.  Maybe we can say this is important for helping a city react to changing demographics?

# Forecasting-Chicago-Homeless-Shelter-Bed-Usage
Functional time series recipe that walks through time series-based, exploratory data analysis and assess the performance of Microsoft's automated time series solution vs Hyndman's methods. Chicago homeless shelter bed data was used because it is more meaningful than most alternatives, and was intended to raise awareness of an often forgotten population.

### Packages

This script requires 5 packages:

* [Tidyverse](https://www.tidyverse.org/packages/) - Awesome data bending/mashing tools
* [RSocrata](https://www.rdocumentation.org/packages/RSocrata/versions/1.4) - Used for pulling data from Socrata open data portals. Government agencies commonly use this system.
* [forecast](https://github.com/robjhyndman/forecast) - Developed by Rob Hyndman, this is a time series analysis package that is a must-have in anyone's toolbox.
* [urca](https://www.rdocumentation.org/packages/urca/versions/1.3-0) - Used for unit root testing.
* [ggthemes](https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/) - One of my favorite themes package for ggplot.

### Data

Data was pulled from the [Chicago Data Portal](https://data.cityofchicago.org/). For convenience, the script pulls the data directly from this portal via API.

### Analysis

this read-me is under construction

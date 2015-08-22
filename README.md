# dissertation-data-analysis
*Updated 7/15/2015*
Files for analyzing data from dissertation experiment.

* These are current as of the defense draft of my dissertation. They are provided as-is in case anyone wants to replicate the analysis and so are somewhat messier than ideal.
* Main files are stanmodels.R (processing.R gets called from here) and the two PlotBounds files.
* Requires rStan, ggplot2, coda, runJags, and a slightly modified version of programs available from [John K. Kruschke's website](https://sites.google.com/site/doingbayesiandataanalysis/software-installation)
* This was done using Stan version 2.6.0
* Data is *slightly* cleaned up from the raw eprime output for ease of loading into R.
* If following along with the dissertation, training corresponds to the labeling task, testing corresponds to the discrimination task.

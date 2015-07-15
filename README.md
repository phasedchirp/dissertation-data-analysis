# dissertation-data-analysis
*Updated 7/15/2015*
Files for analyzing data from dissertation experiment.

* These are current as of the defense draft of my dissertation, and are somewhat messy.
* Main files are stanmodels.R (processing.R gets called from here) and the two gridApprox files (slightly misnamed)
* Requires rStan, ggplot2, coda, runJags, and a slightly modified version of programs available from [John K. Kruschke's website](https://sites.google.com/site/doingbayesiandataanalysis/software-installation)
* Data is *slightly* cleaned up from the raw eprime output for ease of loading into R.
* In file descriptions here, training corresponds to the labeling task, testing corresponds to the discrimination task.
# Prompts used to generate simulation shown in calibration_companion_script.R
## run date: 03/12/26
## platform:: VS Code v1.111.0
## LLM: Claude Sonnet 4.6

Full prompt (passed sequentially to address length limits):

# CONTEXT
I'm a fisheries stock assessment scientist working on a blog post to lightly introduce and illustrate the concept of calibration curves and why we should be thinking about them for fisheries. The blog post is written in .qmd and rendered on my quarto website. 

# GOAL
I'd like you to help me make a companion R script to my blog post that will produce the data & figures for my blog. The simulations we use should be minimal and implemented in base R to the greatest extent possible. I want the figures to be striking, well-colored, and minimalistic with clear labels and no nuisance gridlines etc (ggplot is fine). Ideally we will just run a single set of simulations and pull out different quantities for each example. 

You are welcome to set global parameters, calcultations, and data generation that is shared at the top, but make clear sections for each example so the reader can quickly see what was done to produce each example and associated plots quickly. Save each figure and insert each into my existing blog post with a descriptive caption.

# SIMULATION CASE STUDIES
We are NOT going to simulate an entire population and estimation model as one would in a real stock assessment; we just need a plausible set of "estimates" for a fixed population parameter  (call it R0~N(1000,200)) and an "estimated' associated time series of biomass; use a logistic to decay the population. Let's have 30 years of 'historical' estimates and 20 years of 'future' estimates, and minimum 100 simulation replicates. All estimates (time series and parameters) have normally-distributed error. We will need a well-calibrated and poorly-calibrated example; force the poor calibration via a multiplier on the SE.  

## EXAMPLE 1:
This shows how one could calculate nominal coverage bins from a simulation experiment. Looking at the 100 'estimates' of the R0 parameter, calculate the  empirical coverage as a decimal for each nominal CI interval from 0 to 100 in increments of 0.05.  Show two panels, one with a truncated table  (can just show 50%, 80%, and 95%) with a column for the nominal CI and a column for empirical coverage for each of two approaches (well-calibrated and poorly-calibrated), and another panel showing the 'calibration curve' using these binned calcs, colored by well- or poorly-calibrated, with a clear dashed 1:1 line.

## EXAMPLE 2:
Ideally using the same data as above, use the probability integral transform to calculate the predictive distribution from the historical part of the dataset. This should involve calculating the CDF of each simulation replicate and evaluating the probability that the estimated parameter is <= truth. Show a panel plot of the histograms (PIT histogram) of these predictive distributions, colored by approach (well calibrated should be flat or uniform; poorly calibrated should be humped or u shaped).

## EXAMPLE 3: Forecast predictive intervals
Pretty similar approach to #1, but showing how well calibration acts for the forecast itself. For example, there might be a 70% CI associated with biomass in future year y; check how often the truth lies inside it across simulations within years.

## EXAMPLE 4: putting it together
Illustrate that we can have a situation where parameters are well calibrated, but state estimates are overconfident and forecasts are underconfident. You might  need to throw in one last simulation to get this, but see if you can recycle material and/or figure approaches from above to clearly demonstrate this in a ~3 panel plot with a similar coloring and layout scheme as above. OK if they are all PIT histograms. 



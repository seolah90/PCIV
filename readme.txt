The following syntax is how you run a Per-Cluster Instrumental Variable regression:

	pciv depvar [exog_varlist] (endo_varlist = iv_varlist) [, options]


For exogenous variables, they are common parameters, which are run by using the whole sample, not by using cluster-level sample. 

For endogenous and instrumental variables, they are cluster-level parameters. The regression results will show average treatment effects, which are the average of those cluster-level parameters.

For "options", 

	cluster(varname): define a cluster level

	weight(varname): define a weight for estimating average treatment effect (beta_hat). For this, _gwtmean package needs to be installed.

	rf: show reduced-form regression results

	first: show first-stage regression results


Sample data and .do files are attached for your information.

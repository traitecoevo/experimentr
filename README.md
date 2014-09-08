# experimentr

**This is all a hack: use at your own risk.  This is pretty heavily tailored around a specific need that we have**

# What is this for?

This is to address a pattern in computational research.  Suppose you have a set of parameters that you need to run a simulation on each of -- some sort of embarrassingly parallel job.  You might be running this on your own computer or on a cluster, which might have a scheduler, which in turn requires you write a little bash script to launch it.  This gets really annoying as you recreate parameter lists in a shell file and propagate them through R's less-than-awesome command line passing tools.

# Terminology

* A **project** may have multiple **experiments** in it.
* Each **experiment** may have multiple **tasks**.
* Each experiment has a set of parameters that are common to all tasks.  Each task will be run on each set of parameters.

# The set up

Run `experimentr::create()`.  This creates directories:

* `experments/parameters/`
* `experiments/output/`

The first directory will hold sets of parameters associated with each experiment, while the second holds generated output.

Create a set of parameters.  `expand.grid` is useful here.  Something like:

```r
pars <- expand.grid(a=1:10, b=letters[1:5])
```

Generates 100 parameter combination with pairwise combinations of the integers 1 to 10 and the letters "a" to "e".

### SketchGibbs

SketchGibbs is an MCMC sampler that is designed to facilitate making and playing with light weight model "sketches".  

The goal is that models can be quickly constructed, sampled from and then improved without the baggage that comes from heavy general purpose samplers like pymc or stan.

This approach of model "sketching" is useful in three circumstances

1. For quickly prototyping models and playing around, where the general shape of a distribution is more important than the fine detail
2. When we only care about point estimates of the first few moments (mean, variance)
3. Models that are simple enough that the sketches are very close to the truth 

In keeping with the "sketch" philosophy, the sampler is as open as possible.  Everything should be on show and simple to extend.  This means that:

* Everything is accessible through standard scala objects
* Where feasible, intelligibility is favoured over performance
* There's no magic of auto optimizations and tuning, SketchGibbs does what you tell it to do
* There's almost no tuning required, everything's set up to be simple enough to not require it

## How it works

Models in SketchGibbs are created by gluing together simple distributions to form complex posteriors.  Generally each variable conditional on its parents will be some preset distrbution (Gaussian, Beta etc.)

The sampler exploits the relative simplicity of the markov blanket for each variable to improve performance.  Each variable is currently sampled using slice sampling

## How to use

Each variable in the join distribution is represented by a `node` which contains:
 * The variable the node is about
 * The distribution of the node conditional on the node's parents
 * The current value of the variable in the sampling chain
 
Nodes are linked together through `edges` which can be either directed or undirected.  SketchGibbs works best in a belief network world where each variable has small n parents

It is possible to create arbirary nodes, but, for convienience, there are wrappers for the standard distributions
For example, if we want to create a `node` for a `variable` `theta` wtih mean 1.0 and st_dev 10.0:

``` scala
    val theta_var = VariableFactory("theta")
    val theta_node = Gaussian(theta_var, 1.0, 10.0)
```

Or if we want a beta variable

``` scala
    val beta_variable = VariableFactory("beta")
    val beta_node = Beta(beta_variable, 3, 4)
```

Both of these examples hard-code the parameters of the distributions, but we can combine nodes together by passing `variables` as parameters.
For example, we can represent a model of a gaussian with a known st-dev and unknown mean by

``` scala
    val known_st_dev = 1.0
    val prior = VariableFactory("prior")
    val prior_mean = Gaussian(prior, 0.0, 100.0)
    val unknown_value = Gaussian(VariableFactory("unknown_var", prior, known_st_dev))
```

Behind the scenes, the process of passing variables as parameters creates the edges between the relevant nodes.
Examples of more complex combinations can be found in the Examples folder.

Once we have the structure of our `nodes`, we can pass them into a `graph` which handles the complexity of gluing everything together

``` scala
    val nodes : List[Node] = // MODEL_NODES
    val graph = GraphFactory.gibbs(nodes)
    graph.infer_edges() // Automatically create all the edges
```

It is perfectly possible to use the `graph` object to coordinate the sampling process:

``` scala
    val graph = .. 
    for (_<-1 to n_burn){
        graph.run_iteration()
    }
    for(_<-1 to n_sample){
        graph.run_iteration()
        println(node.current_value)
    }
```

Alternatively, there are pre-made `graph-runners` which can handle the running of a graph and recording of the samples.
Presently the pre-made options are to record the full trace, or just to record the mean of the trace, but it is trivial to extend this functionality

``` scala
    val graph = ...
    val runner = TraceGibbsGraphRunner(graph, List(prop_nodes.head))
    runner.run(500, 500)
    runner.output(prop_nodes.head).foreach(println)
```

## Performance

Performance on small data sets (< 500 data points) is comparable to pymc3 and can even be 3-4 x faster on very small datasets.  Performance is _acceptable_ on bigger datasets, but there are a million and one ways in which it can be improved through time.

### GibbsSampling

GibbsSampler is an MCMC sampler that focuses on large models that can be factorized into relatively simple conditionals.

Many models have complex joint posteriors, but the conditional densities of any one variable is a simple product of a small number of simple distributions (Gaussian, Beta etc.)
For such models, we can gain significant performance gains by exploiting their structure

A side goal is for the sampler to be as easy to play around with as possible.  This means that:

* Everything is accessible through standard scala objects
* Where feasible, intelligibility is favoured over performance
* It's hopefully simple to throw together simple models as prototypes

## How to use

The core object in the sampler is a `node` which represents a single variable in the join distribution.
Nodes are linked together through `edges` which can be either directed or undirected.

Beneath the hood, each node is comprised of a `variable` and a `distribution` over that `variable`.  Additionally the node stores the current value of the variable in the sample chain 

There are simple wrapper factories for creating nodes for standard distributions, for example, if we want to create a `node` for a `variable` gaus wtih mean 1.0 and st_dev 10.0:

``` scala
    val gaussian_variable = VariableFactory("gauss")
    val gaussian_node = Gaussian(gaussian_variable, 1.0, 10.0)
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
    val unknown_value = Gaussian(VariableFactor("unknown_var", prior, known_st_dev) 
```

Examples of more complex combinations can be found in the Examples folder.

Once we have the structure of our `nodes`, we can pass them into a `graph` which handles the complexity of gluing everything together

``` scala
    val nodes : List[Node] = // MODEL_NODES
    val graph = GraphFactory.gibbs(nodes)
    graph.infer_edges()
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

For problems which meet the right structural criteria, GibbsSampler smokes Stan and pymc3

-- Insert comparison to pymc and stan here
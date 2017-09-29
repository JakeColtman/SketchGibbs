import pymc3 as pm
import pandas as pd
from datetime import datetime
import numpy as np

data = pd.DataFrame.from_csv("data.csv", sep = ",", parse_dates = False, header = None).reset_index()
data.columns = ["mid_level_name","low_level_name","n","y"]

n_mid = len(data["mid_level_name"].unique())
n_lower = data.shape[0]
mid_levels = list(data["mid_level_name"])
n = list(data["n"])
y = list(data["y"])

l, mid_level_lookup = np.unique(data["mid_level_name"], return_inverse = True)

with pm.Model() as model:
    prior = pm.Beta("prior", 1, 1)
    cp = pm.Normal("cp", 1.0, 100.0)
    cm = pm.Normal("cm", 1.0, 100.0)

    mids = pm.Beta("m", cm * prior, cm * (1 - prior), shape = n_mid)
    p = pm.Beta("p", cp * mids[mid_level_lookup], cp * (1 - mids[mid_level_lookup]), shape = n_lower)
    t_v = pm.Binomial("t", n, p, shape = n_lower, observed = y)

with model:
    print(datetime.now())
    trace=pm.sample(1000,progressbar=True, n_init = 10000)
    print(datetime.now())
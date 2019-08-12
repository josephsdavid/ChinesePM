import pandas as pd
import mxnet as mx
from mxnet import gluon
import numpy as np
import matplotlib.pyplot as plt
import json
import os
from itertools import islice
from pathlib import Path
mx.random.seed(0)
np.random.seed(0)

ds = pd.read_csv("gluonSet.csv")
ds.columns
ds.head

train_ds = list


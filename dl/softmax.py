import numpy as np
import matplotlib.pyplot as plt
def softmax(x):
    r = np.exp(x)
    sum = np.sum(r)
    return r / sum

print(softmax([1.0, 2.0, 3.0]))
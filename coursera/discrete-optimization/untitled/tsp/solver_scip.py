import numpy as np
import pandas as pd
from pyscipopt import Model
from model import *
import random

def solve_it(input_data):
    # Modify this code to run your optimization algorithm

    # parse the input
    lines = input_data.split('\n')

    nodeCount = int(lines[0])

    points = []
    for i in range(1, nodeCount+1):
        line = lines[i]
        parts = line.split()
        points.append(Point(float(parts[0]), float(parts[1])))

    cost_matrix = np.zeros((nodeCount,nodeCount),dtype="float32")
    for i in range(nodeCount):
        for j in range(i,nodeCount):
            cost_matrix[i,j] = length(points[i],points[j])
    cost_matrix = pd.DataFrame(cost_matrix)
    cost_matrix = cost_matrix + cost_matrix.transpose()

    model = Model("tsp")
    model.hideOutput()
    xs = cost_matrix.applymap(lambda x: model.addVar(vtype="B"))
    us = pd.Series(range(nodeCount)).map(lambda x: model.addVar(vtype="I") )

    for index,row in  xs.iterrows():
        model.addCons(row.sum() == 1)
    for index,col in  xs.iteritems():
        model.addCons(col.sum() ==1)
        
    # (xs + xs.transpose()).applymap(lambda const: model.addCons(const <=1))
    for i in range(nodeCount):
        for j in range(nodeCount):
            if i > j:
                model.addCons(xs[j][i] + xs[i][j] <=1)
            if i == j:
                model.addCons(xs[i][j] == 0)
            if 0 < i and i != j :
                model.addCons(us[i] - us[j] + nodeCount * xs[i][j] <= nodeCount - 1)
                
    objective = (xs * cost_matrix).sum().sum()
    model.setObjective(objective)
    model.setMinimize()

    model.setRealParam("limits/time", 30)
    model.optimize()
    print({
        "status":model.getStatus(),
        "obj": model.getObjVal(),
        "time": model.getTotalTime()
    })
    result = xs.applymap( model.getVal )
    route = np.argmax(result.values,axis=1)

    start = 0
    solution = []
    for i in range(nodeCount):
        start = route[start]
        solution.append(start)
    # build a trivial solution
    # visit the nodes in the order they appear in the file

    # # calculate the length of the tour
    obj = total_length_by_order(points,solution)

    # prepare the solution in the specified output format
    output_data = '%.2f' % obj + ' ' + str(0) + '\n'
    output_data += ' '.join(map(str, solution))

    return output_data
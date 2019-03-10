#!/usr/bin/python
# -*- coding: utf-8 -*-
import numpy as np 
import pandas as pd 
import networkx as nx
from pyscipopt import Model
from ortools.sat.python import cp_model


# def model_optimize(edges,node_count,color_size):
#     model = Model("color")
#     nodes = pd.Series(range(node_count)).map(lambda x: model.addVar(str(x),vtype="INTEGER",lb=0,ub=color_size) )
#     model.addCons(nodes[edges[0][0]] == 0 )
#     model.addCons(nodes[edges[0][1]] == 1 )

#     for edge in edges:
#         f = nodes[edge[0]]
#         t = nodes[edge[1]]
#         cons = abs(f - t) >=1
#         cons = f*f + t*t - 2 * f * t >=1
#         model.addCons(cons)
#     model.setRealParam("limits/time", 30)
#     model.optimize()
#     return model,nodes.map( model.getVal ).map(int).map(str)

# def model_optimize(edges,node_count,color_size):
#     model = Model("color")
#     values = [
#         model.addVar("%s-%s" %(i,j),vtype="B")
#         for i in range(node_count)
#         for j in range(color_size)
#     ]
#     values = np.array(values).reshape((node_count,color_size))
#     model.addCons(values[0,0] == 1)
#     for row in values:
#         model.addCons(row.sum() == 1)
        
#     for edge in edges:
#         f = values[edge[0]]
#         t = values[edge[1]]
#         model.addCons((f * t).sum() ==  0)

#     model.setRealParam("limits/time", 60)
#     model.optimize()
#     nodes = pd.DataFrame(values).applymap(model.getVal).values.argmax(axis=1)
#     return model,pd.Series(nodes)

def get_all_edge(edges):
    edges_1 = pd.DataFrame(edges,columns=["f","t"])
    edges_2 = edges_1.copy()
    edges_2.columns = ["t","f"]

    all_edges = pd.concat([edges_1,edges_2],axis=0,sort=False).drop_duplicates()
    return all_edges

def find_max_full_graph(G):
    max_fc_graph = []
    i = 0
    for graph in nx.find_cliques(G):
        i += 1
        if len(graph) > len(max_fc_graph):
            max_fc_graph = graph
            i = 0
        if i >=100000:
            break
    return max_fc_graph

def model_optimize(edges,node_count,max_color_size,max_fc_graph=None):
    node_freq = get_all_edge(edges).groupby("f").size().sort_values(ascending=False)
    model = cp_model.CpModel()
    solver = cp_model.CpSolver()
    if max_fc_graph is  None:
        max_fc_graph = []

    xs_node_color = pd.Series([
        model.NewIntVar(lb=0,ub=max_color_size,name="n:" + str(i)) 
        for i in range(node_count)])

    for value,node in enumerate(max_fc_graph):
        model.Add(xs_node_color[node] == value)
        
    for edge in edges:
        if len(set(edge).difference(max_fc_graph)) == 0:
            continue
        model.Add(xs_node_color[edge[0]] != xs_node_color[edge[1]])

    solver.parameters.max_time_in_seconds = 20.0
    status = solver.Solve(model)
    if status == 4 or status == 2:
        result = xs_node_color.map(lambda x: solver.Value(x))
    else:
        result = pd.Series(range(node_count))
    return {
        "status":solver.StatusName(status).lower() ,
        "color_size":len(result.unique()),
        "nodes":result,
        "time":solver.WallTime(),
    }


def solve_it(input_data):
    # Modify this code to run your optimization algorithm

    # parse the input
    lines = input_data.split('\n')

    first_line = lines[0].split()
    node_count = int(first_line[0])
    edge_count = int(first_line[1])

    edges = []
    for i in range(1, edge_count + 1):
        line = lines[i]
        parts = line.split()
        edges.append((int(parts[0]), int(parts[1])))

    G = nx.Graph()
    G.add_edges_from(edges)
    greedy_result = nx.greedy_color(G)
    greedy_result = pd.DataFrame(list(greedy_result.items())).set_index(0)[1].sort_index()
    greedy_color_size = len(greedy_result.unique())
    max_fc_graph = find_max_full_graph(G)

    try:
        print(" --------node size : %s;  greedy size :%s; max_fc_graph_size:%s----------" %(node_count,greedy_color_size,len(max_fc_graph)))
        max_value = greedy_color_size - 2
        results = []
        final_result = None
        while max_value > 0:
            result = model_optimize(edges,node_count,max_value,max_fc_graph)
            results.append(result)
            info = result.copy()
            info["nodes"] = None
            print("%s" %(info))   
            max_value = min(max_value,result["color_size"]) - 1     
            if result["status"]== "optimal" or result["status"] == "feasible":
                final_result = result 
            else:
                break
                
        solution = final_result["nodes"].tolist()
        color_size = final_result["color_size"]
    except Exception as e:
        print(e)
        color_size = greedy_color_size
        solution = greedy_result

    # prepare the solution in the specified output format
    output_data = str(color_size) + ' ' + str(0) + '\n'
    output_data += ' '.join(map(str, solution))

    return output_data


import sys

if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        file_location = sys.argv[1].strip()
        with open(file_location, 'r') as input_data_file:
            input_data = input_data_file.read()
        print(solve_it(input_data))
    else:
        print('This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/gc_4_1)')


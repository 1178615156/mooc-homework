import math
import numpy as np 
import pandas as pd 
from collections import namedtuple

from pyscipopt.scip import Variable
from pyscipopt import Model,quicksum,quickprod,log,sqrt
from numba import jit,njit


Point = namedtuple("Point", ['x', 'y'])
Facility = namedtuple("Facility", ['index', 'setup_cost', 'capacity', 'location'])
Customer = namedtuple("Customer", ['index', 'demand', 'location'])

def read_data(input_data):
    lines = input_data.split('\n')

    parts = lines[0].split()
    facility_count = int(parts[0])
    customer_count = int(parts[1])
    
    facilities = []
    for i in range(1, facility_count+1):
        parts = lines[i].split()
        facilities.append(Facility(i-1, float(parts[0]), int(parts[1]), Point(float(parts[2]), float(parts[3])) ))

    customers = []
    for i in range(facility_count+1, facility_count+1+customer_count):
        parts = lines[i].split()
        customers.append(Customer(i-1-facility_count, int(parts[0]), Point(float(parts[1]), float(parts[2]))))
    return facilities,customers

@njit
def length_xy(x1,y1,x2,y2):
    return math.sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

def length(point1, point2):
    return length_xy(point1.x,point1.y,point2.x,point2.y)

def length_point(point1, point2):
    return math.sqrt((point1.x - point2.x)**2 + (point1.y - point2.y)**2)

def gen_min_facility_size(facilities,customers):
    customer_total_demand = sum(list(map(lambda x: x.demand,customers)))
    facility_min_size = 0
    for capacity in sorted(list(map(lambda f:f.capacity,facilities)),reverse=True):
        if customer_total_demand > 0:
            facility_min_size +=1
            customer_total_demand -= capacity
    return facility_min_size

def gen_customer_cost(facilities,customers):
    customer_cost = np.zeros((len(customers),len(facilities)))
    for c in range(len(customers)):
        for f in range(len(facilities)):
            customer_cost[c,f] = length(customers[c].location,facilities[f].location)
    customer_cost = pd.DataFrame(customer_cost)
    return customer_cost

def cost_result(used,solution,facilities,customers):
    obj = sum([f.setup_cost*used[f.index] for f in facilities])
    for customer in customers:
        obj += length(customer.location, facilities[solution[customer.index]].location)
    return obj
def backup_solution(facilities,customers):
    solution = [-1]*len(customers)
    capacity_remaining = [f.capacity for f in facilities]

    facility_index = 0
    for customer in customers:
        if capacity_remaining[facility_index] >= customer.demand:
            solution[customer.index] = facility_index
            capacity_remaining[facility_index] -= customer.demand
        else:
            facility_index += 1
            assert capacity_remaining[facility_index] >= customer.demand
            solution[customer.index] = facility_index
            capacity_remaining[facility_index] -= customer.demand
    return solution
def write_result(model):
    rt = open("result.out","w")
    rt.write(str(model.cost()) + "\n")
    for x in model.solution:
        rt.write(str(x) + " ")
    rt.close()

class ModelHelp:
    def set_min_facility(self,n):
        self.model.addCons(self.fs.sum() >= n)
    def set_max_facility(self,n):
        self.model.addCons(self.fs.sum() <= n)
    def set_facility_size(self,n):
        self.model.addCons(self.fs.sum() == n)
    def set_limit_time(self,time):
        self.model.setRealParam("limits/time", time)
    def cs_value(self):
        return self.cs.applymap(self.model.getVal)
    def fs_value(self):
        return self.fs.map(self.model.getVal)
    def fs_zero(self):
        return self.fs_value()[self.fs_value() == 0].index
    def fs_one(self):
        return self.fs_value()[self.fs_value() == 1].index

    def optimize(self):
        model = self.model 
        model.optimize()
        solution = np.argmax(self.cs_value().values,axis=1)
        used = list(self.fs_value().astype("int"))
        used = np.array(used)
        used[solution] = 1
        self.cost = cost_result(used,solution,facilities=self.facilities,customers=self.customers)
        self.used = used
        self.solution = solution
        return {
            "status": model.getStatus(),
            "time": model.getSolvingTime(),
            "total_time": model.getTotalTime(),
            "obj ": model.getObjVal(),
            "cost":self.cost
        }
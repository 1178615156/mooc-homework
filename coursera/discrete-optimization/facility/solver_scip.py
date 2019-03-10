#!/usr/bin/python
# -*- coding: utf-8 -*-
from python_help import * 
    
def solve_it(input_data):
    # Modify this code to run your optimization algorithm

    # parse the input
    lines = input_data.split('\n')
    facilities,customers = read_data(input_data)

    customer_cost = gen_customer_cost(facilities,customers)
    customer_damend = pd.Series(list(map(lambda x:x.demand,customers )))
    facility_cost = pd.Series(list(map(lambda x:x.setup_cost,facilities)))
    facility_capacity = pd.Series(list(map(lambda x:x.capacity,facilities)))
    facility_size = len(facilities)
    customer_size = len(customers)
    facility_min_size = gen_min_facility_size(facilities,customers)

   ####################################################################

    def takeVar(l):return list(filter(lambda x:type is Variable,l))

    class MyModel(ModelHelp):
        def __init__(self,model,fs_vtype,cs_vtype):
            super().__init__()
            
            fs = facility_cost.map(lambda x: model.addVar(vtype=fs_vtype,lb=0,ub=1))
            cs = customer_cost.applymap(lambda x: model.addVar(vtype=cs_vtype,lb=0,ub=1))
            
            objective = (fs * facility_cost).sum() + (cs * customer_cost).sum().sum()
            
            for index,row in cs.iterrows():
                model.addCons(row.sum() == 1)
                
            for f,fc,c in zip(fs,facility_capacity,cs):
                damend = (cs[c] * customer_damend).sum()
                model.addCons(damend <=f * fc )
            
            model.setObjective(objective)
            model.setRealParam("limits/time", 30)
            self.model = model 
            self.objective = objective
            self.fs = fs 
            self.cs = cs
            self.facilities = facilities
            self.customers = customers
    lr_model = MyModel(Model("lr-facility"),fs_vtype="C",cs_vtype="C")
    lr_model.set_min_facility(facility_min_size)
    lr_model.optimize()

    mip_model = MyModel(Model("mip-facility"),fs_vtype="B",cs_vtype="B")
    mip_model.set_limit_time(60)
    mip_model.set_min_facility(facility_min_size)
    for value in mip_model.fs[lr_model.fs_value() <= 0.0 ]:
        mip_model.model.fixVar(value,0)
    mip_model.optimize()    

   ####################################################################
  
    solution = mip_model.solution
    used = mip_model.used
    # calculate the cost of the solution
    obj = sum([f.setup_cost*used[f.index] for f in facilities])
    for customer in customers:
        obj += length(customer.location, facilities[solution[customer.index]].location)

    # prepare the solution in the specified output format
    output_data = '%.2f' % obj + ' ' + str(0) + '\n'
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
        print('This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/fl_16_2)')


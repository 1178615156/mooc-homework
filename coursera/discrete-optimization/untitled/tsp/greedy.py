from model import *
from typing import List
def solution_function(points:List[Point]):
    sorted_points = sorted(points,key= lambda point:point.distance_zero())
    return sorted_points

if __name__ == '__main__':
    l = [4,3,2,1]

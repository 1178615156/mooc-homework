import math
from dataclasses import dataclass
from typing import List


@dataclass(eq=True,frozen=True)
class Point:
    x: float
    y: float

    def distance_zero(self):
        return self.x * self.x + self.y * self.y

def length(point1: Point, point2: Point):
    return math.sqrt((point1.x - point2.x) ** 2 + (point1.y - point2.y) ** 2)


def total_length_by_order(points: List[Point], solution):
    obj = length(points[solution[-1]], points[solution[0]])
    for index in range(0, len(points) - 1):
        obj += length(points[solution[index]], points[solution[index + 1]])
    return obj


def total_length_by_point(points: List[Point]):
    obj = length(points[-1], points[0])
    for index in range(0, len(points) - 1):
        obj += length(points[index], points[index + 1])
    return obj


def point_to_order(points: List[Point], original_points: List[Point]):
    point_dict = dict([(point, index) for index, point in enumerate(original_points)])
    return [point_dict[point] for point in points]


if __name__ == '__main__':
    points = [Point(1, 2),
              Point(3, 4),
              Point(5, 6), ]
    print(total_length_by_point(points))
    print(point_to_order(points,points))

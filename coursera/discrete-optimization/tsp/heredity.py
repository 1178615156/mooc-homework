from model import *
from random import random, randint
from collections import deque
import numpy as np
import time


@dataclass
class Env:
    points: list = None
    gene_size: int = None
    gene_range: range = None
    gene_random_max: int = None
    p_mutation = 0.1
    p_crossover = 0.1

    def __post_init__(self):
        self.gene_range = range(self.gene_size)
        self.gene_random_max = self.gene_size - 1


env: Env = None


def gene_swap(gene, a, b):
    gene[a], gene[b] = gene[b], gene[a]


def gen_random_gene(size):
    l = np.array(range(size))
    np.random.shuffle(l)
    return l


class Individual:

    def __init__(self, gene):
        self.gene = gene
        self.fitness = total_length_by_order(env.points, self.gene)

    def copy(self):
        return Individual(self.gene.copy())


def crossover(a_gene, b_gene):
    start = randint(0, env.gene_size - 1)
    end = randint(start, env.gene_size - 1)

    def do_cross(gen1, gen2):
        b_gen_tmp = gen2[start:end]
        gen_diff = np.setdiff1d(gen1, b_gen_tmp, assume_unique=True)
        gen_new = np.zeros(env.gene_size, dtype="int")
        gen_new[0:start] = gen_diff[0:start]
        gen_new[start:end] = b_gen_tmp
        gen_new[end:] = gen_diff[start:]
        return gen_new

    a_gen_new = do_cross(a_gene, b_gene)
    b_gen_new = do_cross(b_gene, a_gene)

    a_individual = Individual(a_gen_new)
    b_individual = Individual(b_gen_new)
    return a_individual, b_individual

def mutation(individual, p=None):
    if p is None:
        p = env.p_mutation
    gene = individual.gene
    if random() < p:
        a = randint(0, env.gene_random_max)
        b = randint(0, env.gene_random_max)
        gene[a], gene[b] = gene[b], gene[a]
        return Individual(gene)
    else:
        return individual

def fitness(individual):
    return individual.fitness


class Population:
    def __init__(self, size, points):
        global env
        self.size = size
        self.points = points
        self.gene_size = len(points)
        env = Env(points=points, gene_size=self.gene_size)

        self.individuals = [Individual(gen_random_gene(len(points)))
                            for i in range(size)]

    def evolution(self):
        children = deque()
        children.extend(self.individuals[0:3])
        children.extend(map(lambda x: mutation(x.copy(), 0.5), self.individuals[0:3]))
        # shuffle(self.individuals)
        individuals =self.individuals[0:-10]

        for i in range(self.size - 10):
            father = self.individuals[i]
            mother1 = self.individuals[randint(0, self.size - 1 - 10)]
            mother2 = self.individuals[randint(0, self.size - 1 - 10)]
            for j in range(2):
                mother = self.individuals[randint(0, self.size - 1 - 10)]
                a, b = crossover(father.gene, mother.gene)
                p = 1 - a.fitness / (a.fitness + b.fitness)
                if random() < p:
                    children.append(mutation(a))
                else:
                    children.append(mutation(b))
        self.individuals = self.competition(list(children))

    def max_fitness(self):
        return self.bast_individual().fitness

    def competition(self, individuals):
        x = sorted(individuals, key=fitness)
        return x[0:self.size]
        # the_fitness = 1/ np.array(list(map(fitness,individuals)))
        # the_fitness = np.log(the_fitness)
        # the_fitness = the_fitness / the_fitness.sum()
        # print(the_fitness)
        # return sorted(np.random.choice(individuals,self.size,p=the_fitness,replace=False),key=fitness)

    def bast_individual(self):
        return self.individuals[0]


def solution_function(points: List[Point]):
    population = Population(size=200, points=points)
    for i in range(10):
        start_time = time.time()
        for j in range(10):
            before_max_fitness = population.max_fitness()
            population.evolution()
            after_max_fitness = population.max_fitness()
            if after_max_fitness == before_max_fitness:
                env.p_mutation = 0.5
            else:
                env.p_mutation = 0.1
        end_time = time.time()
        print("iter %s , max:%s , time:%s" % (i, population.max_fitness(), end_time - start_time))
    result = population.bast_individual().gene
    return result


if __name__ == '__main__':
    # l = [1, 2, 3, 4]
    # l[2], l[1] = l[1], l[2]
    # print(np.arange(0, 10))
    # print(randint(0, 1))
    # l = deque()
    # l.append(1)
    # l.append([1,2,3])
    # print(l)
    a = np.arange(0, 10)
    a[5]= 111
    print(a)

    # print(np.swapaxes(a,0,1))
    # print(a)

import subprocess
from sklearn.datasets import load_svmlight_file
from random import shuffle
import re
import numpy as np
import scipy.sparse.linalg

# TODO better name for class
class Instance():
    def __init__(self, train_dat_num, test_dat_num, c=None,j=None):
        self.data_num = train_dat_num
        self.c = c
        self.j = j
        self.train_file = '../data/svm_data/dat_' + str(train_dat_num) +'.txt'
        prefix = '../data/svm_out/dat_' + str(train_dat_num)
        if c != None:
            prefix += '_c_' + str(c).replace('.','-')
        if j != None:
            prefix += '_j_' + str(j)
        prefix += '.'
        self.model = prefix + 'model'

def learn(instance):
    args = ['./svm_learn']
    if instance.c is not None:
        args.append('-c')
        args.append(str(instance.c))
    if instance.j is not None:
        args.append('j')
        args.append(str(instance.j))
    args.append(instance.train_file)
    args.append(instance.model)
    popen = subprocess.Popen(args, stdout=subprocess.PIPE)
    popen.wait()


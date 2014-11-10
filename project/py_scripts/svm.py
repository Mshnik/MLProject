import numpy as np
from random import shuffle
import re
import subprocess
import scipy.sparse.linalg
from sklearn.datasets import load_svmlight_file
import tempfile

# TODO better name for class
class Instance():
    def __init__(self, train_dat_num, test_dat_num, c=None,j=None):
        self.data_num = train_dat_num
        self.c = c
        self.j = j
        self.train_file = '../data/svm_data/dat_' + str(train_dat_num) +'.txt'
        self.test_file = '../data/svm_data/dat_' + str(test_dat_num) +'.txt'
        prefix = '../data/svm_out/dat_' + str(train_dat_num)
        if c != None:
            prefix += '_c_' + str(c).replace('.','-')
        if j != None:
            prefix += '_j_' + str(j)
        prefix += '.'
        self.model = prefix + 'model'
        self.prediction = prefix + 'prediction'
        self.classify_out = prefix +'_classify.out'
        self.kernel = None

class Kernel():
    def __init__(self, type, pd, pg, ps, pr, pu, hfi, num_t_docs, num_sup_vec_plus_1, threshold):
        self.type = type
        self.pd = pd
        self.pg = pg
        self.ps = ps
        self.pr = pr
        self.pu = pu
        self.hfi = hfi
        self.num_t_docs = num_t_docs
        self.num_sup_vec_plus_1 = int(num_sup_vec_plus_1)
        self.b = float(threshold)


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

def classify(instance):
    output = open(instance.classify_out, 'w')
    args = ('./svm_classify', instance.test_file, instance.model, instance.prediction)
    popen = subprocess.Popen(args, stdout=output)
    popen.wait()
    output.close()

def parse_kernel(lines):
    args = [x.split()[0] for x in lines]
    return Kernel(*args)

def read_model(instance):
    f = open(instance.model, 'r')
    lines = f.readlines()
    f.close()
    kernel = parse_kernel(lines[1:11])
    f_tmp = tempfile.TemporaryFile()
    f_tmp.writelines(lines[11:])
    x_data, y_data = load_svmlight_file(f_tmp)
    f_tmp.close()
    return kernel, x_data, y_data


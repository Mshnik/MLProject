import numpy as np
from random import shuffle
import re
import subprocess
import scipy.sparse.linalg
from sklearn.datasets import load_svmlight_file
import tempfile
import texttable

# TODO better name for class
class Instance():
    def __init__(self, train_dat_num, test_dat_num, c=None,j=None):
        self.train_data_num = train_dat_num
        self.test_data_num = test_dat_num
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
        self.accuracy = None
        self.fp = None
        self.np = None
        self.w = None

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

def read_model(instance):
    f = open(instance.model, 'r')
    lines = f.readlines()
    f.close()
    args = [x.split()[0] for x in lines[1:11]]
    instance.kernel = Kernel(*args)
    f_tmp = tempfile.TemporaryFile()
    f_tmp.writelines(lines[11:])
    x_data, y_data = load_svmlight_file(f_tmp)
    f_tmp.close()
    return x_data, y_data


def find_w(instance):
    x_data, y_data = read_model(instance)
    x_data = x_data.transpose()
    x_mat = scipy.sparse.linalg.aslinearoperator(x_data)
    w = x_mat.matmat(y_data)
    instance.w = w

def find_results(instance):
    f = open(instance.prediction, 'r')
    _, y_data = load_svmlight_file(instance.test_file)
    predictions = [float(x.split()[0]) for x in f.readlines()]
    instance.fn = sum([1 for (t, p) in zip(y_data, predictions) if t > 0 and p <= 0])
    instance.fp = sum([1 for (t, p) in zip(y_data, predictions) if t <= 0 and p > 0])
    instance.accuracy = 1 - float(instance.fn + instance.fp)/len(y_data)
    f.close()

def process(instance):
    learn(instance)
    classify(instance)
    find_results(instance)
    find_w(instance)


DAT_NUMS = range(1,11) #TODO zero index the data files

table = texttable.Texttable()
table.header(['Train File', 'Accuracy',  '# FN ', '# FP'])
for i in DAT_NUMS[:-1]:
    instance = Instance(i,10)
    process(instance)
    table.add_row([instance.train_file, instance.accuracy, instance.fn, instance.fp])

print table.draw()
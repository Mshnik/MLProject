from sklearn.datasets import load_svmlight_file, dump_svmlight_file
from sklearn.preprocessing import normalize

def normalize_data(input, output):
    x_data, y_data = load_svmlight_file(input)
    x_normal = normalize(x_data)
    dump_svmlight_file(x_normal, y_data, output)

DAT_NUMS = range(1,11)

inputs = ['../data/svm_raw/dat_' + str(i) +'.txt' for i in DAT_NUMS]
outputs = ['../data/svm_norm/dat_' + str(i) +'.txt' for i in DAT_NUMS]

for (input, output) in zip(inputs, outputs):
    normalize_data(input,output)
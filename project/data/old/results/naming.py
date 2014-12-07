import csv

header = ['Train File', 'Test File', 'j', 'c', 't', 'b', 'FN ', 'FP', 'Accuracy','Precision','Recall','F1','F.5', 'w', 'sorted_w']
names = ['output.csv', 'output_fiftyfifty_bin.csv', 'output_fiftyfifty.csv', 'output_bin.csv']

for name in names:
	output = open(name, 'wb')
	writer = csv.writer(output) 
	writer.writerow(header)
	output.close()
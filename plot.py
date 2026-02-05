import matplotlib.pyplot as plt
import pandas as pd
import re
import os

top_dir = "./vtk_data/"
output_regex = re.compile("output-*")
output_list = list(filter(output_regex.match,os.listdir(top_dir)))
output_list.sort()
if len(output_list) > 1:
    for i,out in enumerate(output_list):
        print("{}: {}".format(i,out))
    output_dir = "{}./{}/".format(top_dir,output_list[int(input())])
else:
    output_dir = "{}./{}/".format(top_dir,output_list[0])

data = pd.read_csv("load-disp.csv")

mpm = pd.read_csv(output_dir+"disp.csv")

plt.plot(data["disp"].values ,data["load"].values,label="Data")
plt.plot(-1e3*mpm["disp"].values,.5* 0.013*mpm["load"].values,label="MPM")

plt.legend()
plt.show()

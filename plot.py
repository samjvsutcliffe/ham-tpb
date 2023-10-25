import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_csv("load-disp.csv")

mpm = pd.read_csv("output/disp.csv")

plt.plot(data["disp"],data["load"],label="Data")
plt.plot(-1*mpm["disp"],0.012*mpm["load"],label="MPM")
plt.legend()
plt.show()

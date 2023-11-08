import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_csv("load-disp.csv")

mpm = pd.read_csv("output/disp.csv")

plt.plot(data["disp"],data["load"],label="Data")
plt.plot(-1e3*mpm["disp"],2*13e-3*mpm["load"],label="MPM")
plt.plot(-1e3*mpm["disp"],2*0.5*13e-3*mpm["nload"],label="MPM-node")
plt.legend()
plt.show()

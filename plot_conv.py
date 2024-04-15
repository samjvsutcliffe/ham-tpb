import matplotlib.pyplot as plt
import pandas as pd
import os
import re
from scipy import integrate
def calculate_gf(disp,load):
    return integrate.trapz(load,disp)/(0.102*0.6*13e-3)


#folders = os.listdir("./output")
#folders = list(filter(os.path.isdir,os.listdir("./")))
regex = re.compile(r'output-.*')
folders = list(filter(regex.search,os.listdir("./")))
#folders = [
#        "output-1.0-1.0-1.0",
#        "output-2.0-1.0-1.0",
#        "output-4.0-1.0-1.0"]
##print("Folders:",folders)



data_ref = pd.read_csv("load-disp.csv")
plt.plot(data_ref["disp"],data_ref["load"],label="Data")
print("GF experimental:",calculate_gf(1e-3*data_ref["disp"],data_ref["load"]))
#files = os.listdir("conv")

for i in folders:
    mpm = pd.read_csv("./{}/disp.csv".format(i))
    plt.plot(-1e3*mpm["disp"],2*13e-3*mpm["load"],label="MPM-{}".format(i))
    #plt.plot(-1e3*mpm["disp"],13e-3*mpm["nload"],label="MPM-{}-reaction".format(i))
    #plt.plot(-1e3*mpm["disp"],13e-3*0.5*(mpm["load"]+mpm["nload"]),label="MPM-{}-average".format(i))
    print("GF ",i," :",calculate_gf(-1*mpm["disp"],13e-3*mpm["load"]))
plt.xlabel("Displacement (mm)")
plt.ylabel("Load (N)")
plt.title("Vary h")
plt.legend()
#plt.savefig("test.png")
plt.show()

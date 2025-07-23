import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)

plt.plot(x, y, label='sin(x)')
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.title('Example Plot')
plt.legend()
plt.grid(True)
plt.savefig("plot.pdf")  # or .svg for LaTeX inclusion
plt.show()
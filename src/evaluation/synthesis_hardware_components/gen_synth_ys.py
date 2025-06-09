from pathlib import Path
import sys, os

class bcolors:
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    UNDERLINE = '\033[4m'


def gen_yosys_synth(vhdl_project_path):
    content = ""
    vhdl_files = list(Path(vhdl_project_path).rglob("*.vhdl"))
    for file in vhdl_files:
        relative_path = Path(*file.parts[1:])
        content += f"ghdl --std=08 {relative_path}\n"
    content += "\nghdl --std=08 -m monitor\n"
    content += "\nsynth -top monitor\n"
    content += "\nstat\n"
    with open(f"{vhdl_project_path}/synth.ys", "w") as f:
        f.write(content)

    

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"{bcolors.FAIL}Please provide a path to the vhdl project as an argument{bcolors.ENDC}")
        exit()
    vhdl_project_path = sys.argv[1]
    gen_yosys_synth(vhdl_project_path)
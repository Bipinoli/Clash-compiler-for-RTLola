from pathlib import Path
import sys, os

class bcolors:
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    UNDERLINE = '\033[4m'


def analyze(vhdl):
    return f"ghdl -a --std=08 --work=work {vhdl}\n"

def analyze_bulk(folder_path):
    content = ""
    for file in list(Path(folder_path).rglob("*.vhdl")):
        relative_path = Path(*file.parts[-2:])
        content += analyze(relative_path)
    return content


def gen_script(vhdl_project_path):
    content = f"#!/bin/bash\n\n"
    content += f"ghdl --clean\n\n"
    content += analyze("my_array_package.vhdl")
    content += analyze("my_math_package.vhdl")
    content += analyze_bulk(f"{vhdl_project_path}/hlc")
    content += analyze_bulk(f"{vhdl_project_path}/llc")
    content += analyze_bulk(f"{vhdl_project_path}/queue")
    content += analyze("monitor.vhdl")
    content += analyze_bulk(f"{vhdl_project_path}/pre_processing")
    content += analyze("implementation.vhdl")
    content += "ghdl -e --std=08 --work=work implementation\n"
    content += f'yosys -m ghdl -p "ghdl --latches --std=08 --work=work implementation; synth -top implementation; stat" > stats.txt\n'
    with open(f"{vhdl_project_path}/synth.sh", "w") as f:
        f.write(content)

    

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"{bcolors.FAIL}Please provide a path to the vhdl project as an argument{bcolors.ENDC}")
        exit()
    vhdl_project_path = sys.argv[1]
    gen_script(vhdl_project_path)
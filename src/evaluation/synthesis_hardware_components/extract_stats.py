import sys, os, subprocess

class bcolors:
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    UNDERLINE = '\033[4m'

def extract_synthesis_specs(content):
    return content[content.find("3. Printing statistics."): content.find("End of script.")]


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"{bcolors.FAIL}Please provide a path to the specs directory as an argument{bcolors.ENDC}")
        exit()
    specs_path = sys.argv[1]

    for folder in os.listdir(specs_path):
        work_dir = f"{specs_path}/{folder}/generated/vhdl/Spec.topEntity"
        content = subprocess.run(["yosys", "-m", "ghdl", "synth.ys"], cwd=work_dir, stdout=subprocess.PIPE, text=True).stdout
        with open(f"{work_dir}/../../stats.txt", "w") as f:
            f.write(content)

    subprocess.run(["bash"])



    
import os, subprocess

specs_path = "specs"

if __name__ == "__main__":
    for folder in os.listdir(specs_path):
        subprocess.run(["rm", "-rf", "generated"], cwd=f"{specs_path}/{folder}")
        subprocess.run(["chmod", "+x", "run.sh"], cwd=f"{specs_path}/{folder}")
        subprocess.run(["./run.sh"], cwd=f"{specs_path}/{folder}")


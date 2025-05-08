#!/usr/local/bin/python3
import os, sys, re
import subprocess

def read_expected_output(test_dir):
    def parse(line):
            pattern = r'\[([^\]]+)\]\[([^\]]+)\]\[([^\]]+)\]\[([^\]]+)\] = ([^\s]+)'
            match = re.match(pattern, line)
            if match:
                groups = match.groups()
                return groups
            else:
                raise Exception(f"couldn't parse the line {line}")
    with open(f"{test_dir}/expected_output.txt", 'r') as f:
        content = f.read().strip().split("\n")
        content = list(filter(lambda data: len(data) > 0, content))
        expected = list(map(parse, content))
        expected = list(filter(lambda data: data[1] != "Input", expected))
        grouped = {}
        for (time, data_type, output_name, _, value) in expected:
            if time not in grouped:
                 grouped[time] = []
            grouped[time].append(value)
        grouped = list(grouped.values())
        return grouped


def read_actual_output(test_dir):
    def parse(line):
            pattern = r'Time (\d+): Active outputs: \(([^)]+)\), Outputs: \(([^)]+)\)'
            match = re.match(pattern, line)
            if match:
                groups = match.groups()
                return groups
            else:
                return ()
    with open(f"{test_dir}/generated/output.txt", 'r') as f:
        content = f.read().split("\n")
        actual = list(map(parse, content))
        actual = list(filter(lambda data: len(data) > 0, actual))
        grouped = []
        for data in actual:
            has_output = list(data[1].split(', '))
            output = list(data[2].split(', '))
            group = []
            for i in range(len(has_output)):
                if has_output[i] == '1':
                    group.append(output[i])
            grouped.append(group)
        return grouped

class bcolors:
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    UNDERLINE = '\033[4m'

def test(test_dir):
    results = []
    subprocess.run(['bash', 'run.sh'], cwd=f'tests/{test_dir}')
    expected = read_expected_output(f"tests/{test_dir}")
    actual = read_actual_output(f"tests/{test_dir}")
    if actual == expected:
        results.append(f"{bcolors.OKGREEN}Passed: {test_dir}{bcolors.ENDC}")
        passed = True
    else:
        results.append(f"{bcolors.FAIL}Failed: {test_dir}{bcolors.ENDC}")
        for i in range(len(actual)):
            if actual[i] != expected[i]:
                results.append(f"{bcolors.WARNING}expected: {expected[i]}\nactual: {actual[i]}\n{bcolors.ENDC}")
                break
        passed = False
    return (passed, results)

def test_matching(pattern):
    results = []
    for test_dir in os.listdir('tests'):
        if pattern in test_dir and "ignore" not in test_dir:
            results.append(test(test_dir))
    all_tests_passed = all (rslt[0] for rslt in results)
    actual_results = [rslt[1] for rslt in results]
    return (all_tests_passed, actual_results)

def test_all():
    return test_matching("")
    
def show_help():
    print(f"{bcolors.WARNING}--all          \t\tto run all tests")
    print("--match pattern\t\tto run all tests with a pattern in the name")
    print(f"\n{bcolors.OKBLUE}example: .\\run.py --match graph{bcolors.ENDC}\n")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"{bcolors.FAIL}Please provide a valid argument{bcolors.ENDC}")
        show_help()
        exit()
    if len(sys.argv) == 2 and sys.argv[1] != "--all":
        print(f"{bcolors.FAIL}Unknown argument {sys.argv[1]}{bcolors.ENDC}")
        show_help()
        exit()
    if len(sys.argv) == 3 and sys.argv[1] != "--match":
        print(f"{bcolors.FAIL}Unknown argument {sys.argv[1]}{bcolors.ENDC}")
        show_help()
        exit()
    if len(sys.argv) == 2:
        (all_tests_passed, results) = test_all()
    else:
        (all_tests_passed, results) = test_matching(pattern=sys.argv[2])

    print(f"\n\n\n{bcolors.UNDERLINE}{bcolors.OKBLUE}Test results:{bcolors.ENDC}")
    for result in results:
        for rslt in result:
            print(rslt)   

    if all_tests_passed:
        print("\nAll tests passed!\n")



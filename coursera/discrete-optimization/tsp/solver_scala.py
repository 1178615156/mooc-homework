
import os
from subprocess import Popen, PIPE

def solve_it(input_data):

    # Writes the inputData to a temporay file

    tmp_file_name = 'tmp.data'
    tmp_file = open(tmp_file_name, 'w')
    tmp_file.write(input_data)
    tmp_file.close()

    # Runs the command: java Solver -file=tmp.data
    #-server -XX:+UnlockDiagnosticVMOptions -XX:MinInliningThreshold=1000
    #-XX:MaxInlineSize=128 -XX:MaxInlineLevel=10 -XX:+LogCompilation -XX:LogFile=mylogfile.log -XX:+TraceClassLoading
    #-XX:+PrintAssembly -XX:+TraceClassLoading

    env = os.environ.copy()
    env["JAVA_OPTS"] = "-Xmx5G -Xms1G -Xss4m -server -XX:MaxInlineSize=128 -XX:MaxInlineLevel=10"
    process = Popen(['scala', 'src/SimulateAnnealArithmetic.scala', tmp_file_name],env=env, shell=True)

    (stdout, stderr) = process.communicate()

    # removes the temporay file
    os.remove(tmp_file_name)

    return open("result.out").read().strip()
    #return stdout.strip().decode()
	
import sys

if __name__ == '__main__':
    if len(sys.argv) > 1:
        file_location = sys.argv[1].strip()
        with open(file_location, 'r') as input_data_file:
            input_data = input_data_file.read()
        print (solve_it(input_data))
    else:
        print('This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/ks_4_0)')

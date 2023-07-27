import csv
import os 


which_system = 1 if os.getcwd() == "/home/yahui/future_condition/infer_TempFix" else 0 

loris1_path = "/home/yahui/future_condition/infer_TempFix/" 
mac_path = "/Users/yahuis/Desktop/git/infer_TempFix/"
path = loris1_path if which_system == 1 else mac_path 
output_report =  path + "TempFix-out/report.csv" 
output_detail =  path + "TempFix-out/detail.txt" 

print(output_report + "\n")
print(output_detail + "\n")


file = open(output_report, 'r')
csvreader = csv.reader(file)
content = list(csvreader) 
record_length= len(content)
title = content[0]

def sum_up(col):
    sum = 0 
    for i in range(1, record_length):
        sum = sum + int(content[i][col])
    return (sum)

def sum_up_float(col):
    sum = 0 
    for i in range(1, record_length):
        sum = sum + float(content[i][col])
    return (sum)



#print()

loc = sum_up(1)
loS = content[1][2]
protocols = content[1][3]
exec_time =  sum_up_float(4)
totol_Assert = sum_up(5)
fail_Assert = sum_up(6)
succeed_Assert = sum_up(7)

print("===================================")
print("[Lines of  Code]" + str(loc))
print("[Lines of  Spec]" + loS)
print("[Num  Protocols]" + protocols)
print("[Totoal  Assert]" + str(totol_Assert))
print("[Failed  Assert]" + str(fail_Assert))
print("[Succeed Assert]" + str(succeed_Assert))
print("[Time  (Second)]" + str(exec_time))

# "Filename, Loc, LoS,  #protocols, Execution Time(s), Totoal Assertion, Failed, Succeed\n"





import csv
import os 


which_system = 1 if os.getcwd()[0:5] == "/home" else 0 

loris1_path = "/home/yahui/future_condition/infer_TempFix/" 
mac_path = "/Users/yahuis/Desktop/git/infer_TempFix/"
path = os.getcwd () + "/"  
#(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.getcwd ()))))) + "/"  

output_report =  path + "TempFix-out/report.csv" 
output_detail =  path + "TempFix-out/detail.txt" 

print(output_report + "\n")
print(output_detail + "\n")


file = open(output_report, 'r')
csvreader = csv.reader(file)
content = list(csvreader) 

record_length= len(content)
title = content[0]

#print(record_length)

def existRecord (record, str):
    for i in range(0, len(record)):
        if record[i][0] == str : 
            return True 
        else: continue
    return False 

def removeDuplicate(col):
    uniqueRecord = []
    for i in range(1, record_length):
        if existRecord (uniqueRecord, content[i][0]) == True : continue
        else:
            uniqueRecord = uniqueRecord + [content[i]]
    return (uniqueRecord)


content1 = removeDuplicate(content)
record_length= len(content1)

#print(record_length)

def sum_up_prime(col):
    sum = 0 
    for i in range(1, record_length):
        sum = sum + int(content[i][col])
    return (sum)

def sum_up(col):
    sum = 0 
    for i in range(0, record_length):
        sum = sum + int(content1[i][col])
    return (sum)

def sum_up_float(col):
    sum = 0 
    for i in range(0, record_length):
        sum = sum + float(content1[i][col])
    return (sum)



#print(content)

#loc = sum_up_prime(1)
loS = content[1][2]
protocols = content[1][3]
generated_protocols = content[len(content)-1][3]

analysis_time =  sum_up_float(4)
repair_time =  sum_up_float(5)
fail_Assert = sum_up(6)
succeed_Assert = sum_up(7)

print("===================================")
#print("[Lines of  Code] " + str(loc))
print("[Lines  of  Spec] " + loS)
print("[Predefined Spec] " + protocols)
print("[Generated  Spec] " + str(int(generated_protocols)-int(protocols)))
print("[Failed   Assert] " + str(fail_Assert))
print("[Repaired   Bugs] " + str(succeed_Assert))
print("[Analysis    (s)] " + str(analysis_time))
print("[Repair      (s)] " + str(repair_time))


# "Filename, Loc, LoS,  #protocols, analysis Time(s), repair time,  Failed, Repaired\n"





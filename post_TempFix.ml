
let which_system = if String.compare (Sys.getcwd()) "/home/yahui" == 0 then 1 else 0 
let loris1_path = "/home/yahui/future_condition/infer_TempFix/" 
let mac_path = "/Users/yahuis/Desktop/git/infer_TempFix/"
let path = if which_system == 1  then loris1_path else mac_path 
let output_report =  path ^ "TempFix-out/report.csv" 
let output_detail =  path ^ "TempFix-out/detail.txt" 




let tempFixDataAnalysis () = 
  print_endline ("tempFixDataAnalysis")




let () = 
  tempFixDataAnalysis ()

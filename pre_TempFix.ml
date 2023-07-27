
let which_system = if String.compare (Sys.getcwd()) "/home/yahui" == 0 then 1 else 0 
let loris1_path = "/home/yahui/future_condition/infer_TempFix/" 
let mac_path = "/Users/yahuis/Desktop/git/infer_TempFix/"
let path = if which_system == 1  then loris1_path else mac_path 
let output_report =  path ^ "TempFix-out/report.csv" 
let output_detail =  path ^ "TempFix-out/detail.txt" 

let tempFixInitialize () = 
  let oc_report = open_out output_report in 
  let oc_detail = open_out output_detail in 

  try 
    Printf.fprintf oc_report "Filename, Loc, LoS,  #protocols, Execution Time(s), Totoal Assertion, Failed, Succeed\n";
    Printf.fprintf oc_detail "";
    close_out oc_report;
    close_out oc_detail;
    ()

  with e ->                      (* 一些不可预见的异常发生 *)
    close_out_noerr oc_report;           (* 紧急关闭 *)
    close_out_noerr oc_detail;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  ;; 





let tempFixDataAnalysis () = 
  print_endline ("tempFixDataAnalysis")




let () = 
  tempFixInitialize ()

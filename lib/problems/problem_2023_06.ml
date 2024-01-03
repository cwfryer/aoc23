open Core

let year = 2023
let day = 6

module Part_1 = struct
  let run (_input : string) : (string, string) Result.t =
    Ok
      "I worked this one out in my head and did the calculations in Excel\n\
       It's roundown(sqrt(((t / 2) ^ 2) - d)) * 2 (if t is even, minus 1)"
  ;;
end

module Part_2 = struct
  let run (_input : string) : (string, string) Result.t =
    Ok "I worked this one out in my head and did the calculations in Excel"
  ;;
end

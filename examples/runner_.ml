let () =
  for i = 1 to Array.length Sys.argv - 1 do
    ignore @@ Sys.command Sys.argv.(i)
  done

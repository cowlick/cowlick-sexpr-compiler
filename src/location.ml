type t = {
  begin_column: int;
  begin_line: int;
  begin_bol: int;
  end_column: int;
  end_line: int;
  end_bol: int;
}

let zero = {
  begin_column = 0;
  begin_line = 0;
  begin_bol = 0;
  end_column = 0;
  end_line = 0;
  end_bol = 0
}

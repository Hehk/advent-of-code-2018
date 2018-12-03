let getDay = () => {
  let index = 1;

  if (Array.length(Sys.argv) <= index) {
    None
  } else {
    Some(Sys.argv[index]);
  }
}

let () = {
  switch (getDay()) {
    | Some(day) =>
      switch day {
      | "1" => Day1.Answer.make()
      | "2" => Day2.Answer.make()
      | "3" => Day3.Answer.make()
      | _ => Printf.printf("Invalid Day");
      }
    | None => Printf.printf("Invalid Day");
  }
};

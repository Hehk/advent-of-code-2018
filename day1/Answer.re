/* Map of Frequencies */
module IntMap =
  Map.Make({
    type t = int;
    let compare = compare;
  });

let getAnswerOne = input => input |> List.fold_left((+), 0);

let rec getAnswerTwo =
        (~startingFrequency=0, ~startingMap=IntMap.empty, input) => {
  let (map, frequency, hit) =
    input
    |> List.fold_left(
         ((map, frequency, stop), change) =>
           if (stop) {
             (map, frequency, stop);
           } else {
             let newFrequency = frequency + change;
             let value = IntMap.find_opt(newFrequency, map);
             let newMap = IntMap.add(newFrequency, 1, map);
             switch (value) {
             | None => (newMap, newFrequency, stop)
             | Some(x) => (newMap, newFrequency, true)
             };
           },
         (startingMap, startingFrequency, false),
       );

  if (hit) {
    frequency;
  } else {
    getAnswerTwo(~startingFrequency=frequency, ~startingMap=map, input);
  };
};

let make = () => {
  let input =
    Input.input |> String.split_on_char('\n') |> List.map(int_of_string);
  let answer1 = getAnswerOne(input);
  let answer2 = getAnswerTwo(input);

  Printf.printf(
    "Part 1 \nAnswer: %d\n\nPart 2\nAnswer: %d",
    answer1,
    answer2,
  );
};

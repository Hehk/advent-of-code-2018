module CharMap =
  Map.Make({
    type t = char;
    let compare = compare;
  });

let input = Input.input |> String.split_on_char('\n');

type ids =
  | Pair(string, string)
  | None;

let testIds = (a, b) =>
  if (String.length(a) !== String.length(b)) {
    false;
  } else {
    let misses = ref(0);
    for (i in 0 to String.length(a) - 1) {
      if (a.[i] !== b.[i]) {
        misses := misses^ + 1;
      };
    };

    switch (misses^) {
    | 1 => true
    | _ => false
    };
  };

let findMatch = (ids, id) =>
  ids
  |> List.fold_left(
       (pair, s) =>
         switch (pair) {
         | None => testIds(s, id) ? Pair(s, id) : None
         | Pair(_, _) => pair
         },
       None,
     );

let getAnswer2 = input =>
  input
  |> List.fold_left(
       (pair, s) =>
         switch (pair) {
         | None => findMatch(input, s)
         | Pair(_, _) => pair
         },
       None,
     );

let make = () => {
  let answer =
    input
    |> List.map(s => {
         let map = ref(CharMap.empty);

         /* Sadly there is no string fold */
         String.iter(
           c => {
             let count = CharMap.find_opt(c, map^);

             let newCount =
               switch (count) {
               | None => 1
               | Some(x) => x + 1
               };

             map := CharMap.add(c, newCount, map^);
           },
           s,
         );

         map^;
       })
    |> List.fold_left(
         ((twos, threes), map) => {
           let (hasPair, hasTriple) =
             CharMap.fold(
               (_key, count, (hasPair, hasTriple)) =>
                 switch (count) {
                 | 2 => (true, hasTriple)
                 | 3 => (hasPair, true)
                 | _ => (hasPair, hasTriple)
                 },
               map,
               (false, false),
             );

           (hasPair ? twos + 1 : twos, hasTriple ? threes + 1 : threes);
         },
         (0, 0),
       );

  Printf.printf("Answer 1: %d\n\n", fst(answer) * snd(answer));

  switch (getAnswer2(input)) {
  | None => ()
  | Pair(a, b) => Printf.printf("Answer 2: \nId: %s\nId: %s", a, b)
  };
};

module TupleMap =
  Map.Make({
    type t = (int, int);
    let compare = ((a, b), (c, d)) =>
      if (compare(a, c) === 0) {
        compare(b, d);
      } else {
        compare(a, c);
      };
  });

type claim = {
  id: int,
  x: int,
  y: int,
  height: int,
  width: int,
};

let input =
  Input.input
  |> String.split_on_char('\n')
  |> List.map(claim => {
       let def = {id: 0, x: 0, y: 0, height: 0, width: 0};
       switch (String.split_on_char(' ', claim)) {
       | [id, _, loc, dim, ..._] =>
         switch (
           String.sub(loc, 0, String.length(loc) - 1)
           |> String.split_on_char(',')
         ) {
         | [x, y, ..._] =>
           switch (dim |> String.split_on_char('x')) {
           | [width, height, ..._] => {
               id: String.sub(id, 1, String.length(id) - 1) |> int_of_string,
               x: int_of_string(x),
               y: int_of_string(y),
               width: int_of_string(width),
               height: int_of_string(height),
             }
           | _ => def
           }
         | _ => def
         }
       | _ => def
       };
     });

let make = () => {
  let claimMap =
    input
    |> List.fold_left(
         (claimMap, claim) => {
           let map = ref(claimMap);
           for (i in claim.x + 1 to claim.x + claim.width) {
             for (j in claim.y + 1 to claim.y + claim.height) {
               let value = TupleMap.find_opt((i, j), map^);

               let newValue =
                 switch (value) {
                 | None => 1
                 | Some(x) => x + 1
                 };

               map := TupleMap.add((i, j), newValue, map^);
             };
           };

           map^;
         },
         TupleMap.empty,
       );

  let overlapCount =
    TupleMap.fold(
      (key, value, count) => {
        value >= 2 ? count + 1 : count;
      },
      claimMap,
      0,
    );

  let validClaim =
    List.find(
      claim => {
        let failed = ref(false);

        for (i in claim.x + 1 to claim.x + claim.width) {
          for (j in claim.y + 1 to claim.y + claim.height) {
            let value = TupleMap.find((i, j), claimMap);
            if (value > 1) {
              failed := true;
            };
          };
        };

        ! failed^;
      },
      input,
    );

  Printf.printf("Answer 1: %d\nAnswer 2: %d\n", overlapCount, validClaim.id);
};

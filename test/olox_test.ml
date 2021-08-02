open Olox

let kind (token : Olox.Scanner.Token.t) = token.kind

let token =
  let open Scanner in
  Alcotest.testable Token.pp_kind Token.equal_kind

let token_line =
  let open Scanner in
  Alcotest.testable Token.pp Token.equal

let get src = Scanner.tokenize src |> List.map (fun t -> Scanner.Token.(t.kind))

let test name expect src =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list token)) name expect (get src))

let () =
  let open Alcotest in
  run "Scanner"
    [
      ( "single-char",
        [
          test "Left_paren" [ Left_paren; Eof ] "(";
          test "Right_paren" [ Right_paren; Eof ] ")";
          test "Left_brace" [ Left_brace; Eof ] "{";
          test "Right_brace" [ Right_brace; Eof ] "}";
          test "Comma" [ Comma; Eof ] ",";
          test "Dot" [ Dot; Eof ] ".";
          test "Minus" [ Minus; Eof ] "-";
          test "Plus" [ Plus; Eof ] "+";
          test "Semicolon" [ Semicolon; Eof ] ";";
          test "Slash" [ Slash; Eof ] "/";
          test "Star" [ Star; Eof ] "*";
        ] );
      ( "multi-char",
        [
          test "Bang" [ Bang; Eof ] "!";
          test "Bang_equal" [ Bang_equal; Eof ] "!=";
          test "Equal" [ Equal; Eof ] "=";
          test "Equal_equal" [ Equal_equal; Eof ] "==";
          test "Greater" [ Greater; Eof ] ">";
          test "Greater_equal" [ Greater_equal; Eof ] ">=";
          test "Less" [ Less; Eof ] "<";
          test "Less_equal" [ Less_equal; Eof ] "<=";
        ] );
      ( "literals",
        [
          test "Identifier" [ Identifier "var_name"; Eof ] "var_name";
          test "String" [ String "string"; Eof ] "\"string\"";
          test "Number" [ Number 12.23; Eof ] "12.23";
          test "NumberInt" [ Number 1223.0; Eof ] "1223";
          test "NumberTrailingDot" [ Number 1223.; Dot; Eof ] "1223.";
        ] );
      ( "random-go-code",
        [
          test_case "Go code" `Quick (fun () ->
              (check (list token_line))
                "go code" Go.ref (Scanner.tokenize Go.src));
        ] );
    ]

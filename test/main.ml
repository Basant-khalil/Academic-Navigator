open OUnit2
(** TEST PLAN: We have written over 80 tests to ensure correctness in
    our project.

    The code in bin/main.ml is used to take in user input and run the
    appropriate functions at a given point. Its function is manually
    tested by running our project.

    The code in src/formatter.ml is used to format some of the outputs
    to the user. This was also tested manually, because the functions'
    effects can only be seen visually in the GUI.

    We did not feel the need to test the code in src/get.ml because its
    output is a json file which we could easily visually confirm as
    matching the one given in the API.

    The code in src/database.ml is used to control the database of
    courses. Since this involves functions that control the backend to
    what the user sees, we were able to cover over 90% of the code in
    it. We tested every function except those that had to be added to
    support testing.

    The code in src/major.ml is partially tested because major.ml
    contains two types of functions: helper functions that work with the
    data about courses, and a large amount of functions which are
    involved in displaying data/output to the user. We manually tested
    most of the functions in major.ml because our GUI allowed us to do
    so. By running our application and getting the output we expected,
    we confirmed that the output-oriented function in major.ml work.

    For all of our test functions, we had to use glass box testing.
    There were various match cases and if statements that we had to go
    through, and looking at the code allowed us to cover them.
    Additionally, a lot of our functions involve receiving information
    from data in the form of JSON. Therefore, while testing, we had to
    test our JSON-related functions by looking at the json files which
    would be used in our test cases.

    I believe our approach proves the correctness of our system for two
    reasons: 1. We used bisect coverage to determine that the various
    pathways for the functions which we could manually test were
    covered. 2. For functions that involve the GUI and output data, we
    did a significant amount of manual testing as a team.

    In conclusion, by using a mixture of manual and automatic testing,
    we were able to construct a system of tests which helps us reassure
    that our code works the way we want it to. *)

open Source
open Major
open Database
open Yojson.Basic.Util

(*****************************************************************)
(************            Helper Functions             ************)
(*****************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_pair pp1 pp2 (a, b) =
  "(" ^ pp1 (string_of_int a) ^ "," ^ pp2 (string_of_int b) ^ ")"

let rec pp_string_list = function
  | s :: t -> s ^ ";" ^ pp_string_list t
  | [] -> ""

let rec pp_string_string_list (lst : (string * string list) list) :
    string =
  match lst with
  | [] -> ""
  | (s1, a :: b) :: h ->
      "(" ^ s1 ^ ",(" ^ a ^ ";" ^ pp_string_list b ^ ")"
      ^ pp_string_string_list h ^ ")"
  | (s1, []) :: t -> "(" ^ s1 ^ pp_string_string_list t ^ ")"

(*****************************************************************)
(************                  Tests                  ************)
(*****************************************************************)

let csjson = Yojson.Basic.from_file "data/majors.json"
let rosterjson = Yojson.Basic.from_file "data/FA19 MATH.json"

(** [nth_course pos json] gives back the data about a course as type
    Database.t, where the course is in the [pos] position of the [json]
    json file. *)
let nth_course pos json =
  let course_lst =
    json |> member "data" |> member "classes" |> to_list
  in
  let t_json = List.nth course_lst pos in
  from_json t_json

let first_math19 = nth_course 0 rosterjson
let fifth_math19 = nth_course 4 rosterjson
let seventh_math19 = nth_course 6 rosterjson
let sixtysecond_math19 = nth_course 61 rosterjson

let major_parse_test
    (name : string)
    (input : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse input)

(************************ Database TESTS *****************************)
(*********************************************************************)

let from_json_roster_test name input expected_output =
  name >:: fun _ ->
  assert_equal
    (from_json_roster input)
    expected_output ~printer:string_of_bool

let from_json_roster_tests =
  [
    from_json_roster_test
      {| "xyz" is not a course in the roster json file. |} "xyz" false;
    from_json_roster_test
      {| "math1920" is not a correct course code in the roster json file. |}
      "math1920" false;
    from_json_roster_test
      {| "MATH1920" is a correct course code in the roster json file. |}
      "MATH1920" true;
    from_json_roster_test
      {| "MATH2220" is a correct course code in the roster json file. |}
      "MATH2220" true;
    from_json_roster_test
      {| "MATH1920" is a correct course code in the roster json file. |}
      "MATH1920" true;
    from_json_roster_test
      {| "" is not a course code in the roster json file. |} "" false;
  ]

let find_prereq_test name input expected_output : test =
  name >:: fun _ ->
  assert_equal (find_prereq input) expected_output
    ~printer:String.escaped

let find_prereq_tests =
  [
    find_prereq_test
      "The first course in the FA19 MATH json file does not have any \
       prereqs."
      first_math19 "";
    find_prereq_test
      "The fifth course in the FA19 MATH json file has a high school \
       math prereq."
      fifth_math19
      "Prerequisite: three years of high school mathematics, including \
       trigonometry and logarithms.";
    find_prereq_test
      "The seventh course in the FA19 MATH json file has a MATH 1110 \
       prereq."
      seventh_math19
      "Prerequisite: MATH 1110 with a grade of C or better, excellent \
       performance in MATH 1106, or equivalent AP credit.";
    find_prereq_test
      "The 62nd (also the last) course in the FA19 MATH json file does \
       not have any prereqs."
      sixtysecond_math19 "";
  ]

let find_credits_test name input expected_output : test =
  name >:: fun _ ->
  assert_equal (find_credits input) expected_output
    ~printer:string_of_int

let enroll_group1 = enrolled_group 0 first_math19
let enroll_group2 = enrolled_group 0 seventh_math19
let enroll_group3 = enrolled_group 5 seventh_math19
let enroll_group4 = enrolled_group 0 sixtysecond_math19

let find_credits_tests =
  [
    find_credits_test
      "The first enrollment group of MATH 1110 in FA19 MATH.json is 1 \
       credit."
      enroll_group1 1;
    find_credits_test
      "The first enrollment group of MATH 1120 is 4 credits."
      enroll_group2 4;
    find_credits_test
      "The sixth enrollment group of MATH 1120 is 4 credits."
      enroll_group3 4;
    find_credits_test
      "The first (and only) enrollment group in the last class in FA19 \
       MATH.json is 1 credit."
      enroll_group4 1;
  ]

let find_forbidden_overlap_test name input expected_output : test =
  name >:: fun _ ->
  assert_equal
    (find_forbidden_overlap input)
    expected_output ~printer:String.escaped

let find_forbidden_overlap_tests =
  [
    find_forbidden_overlap_test
      "The first class in the FA19 MATH json file does not have any \
       forbidden overlaps."
      first_math19 "";
    find_forbidden_overlap_test
      "The seventh class in the FA19 MATH json file has a couple of \
       forbidden overlaps with MATH 1120, MATH 1220, and MATH 1910."
      seventh_math19
      "Forbidden Overlap: due to an overlap in content, students will \
       receive credit for only one course in the following group: MATH \
       1120, MATH 1220, MATH 1910. For guidance in selecting an \
       appropriate course, please consult First Steps in Math.";
    find_forbidden_overlap_test
      "The 62nd / last course in the FA19 MATH json file does not have \
       any overlaps."
      sixtysecond_math19 "";
  ]

let find_location_test name input expected_ouput : test =
  name >:: fun _ ->
  assert_equal (find_location input) expected_ouput
    ~printer:String.escaped

let find_location_tests =
  [
    find_location_test "This course is located in Ithaca."
      (group_section enroll_group1)
      "ITH";
    find_location_test "This course is located in Ithaca."
      (group_section enroll_group2)
      "ITH";
    find_location_test "This course is located in Ithaca."
      (group_section enroll_group3)
      "ITH";
  ]

let find_description_test name input expected_output : test =
  name >:: fun _ ->
  assert_equal
    (find_description input)
    expected_output ~printer:String.escaped

let find_description_tests =
  [
    find_description_test
      "The first course in the FA19 MATH json file has a description \
       about the content of MATH 1011."
      first_math19
      "Reviews material presented in MATH 1110 lectures, provides \
       problem-solving techniques and tips as well as prelim review. \
       Provides further instruction for students who need \
       reinforcement. Not a substitute for attending MATH 1110 \
       lectures.";
    find_description_test
      "The last course in the FA19 MATH json file describes how it is \
       for research."
      sixtysecond_math19
      "Supervised research for the doctoral dissertation.";
    find_description_test
      "The fifth course in the FA19 MATH json file describes a linear \
       algebra course."
      fifth_math19
      "Introduction to linear algebra, probability, and Markov chains \
       that develops the parts of the theory most relevant for \
       applications. Specific topics include equations of lines, the \
       method of least squares, solutions of linear systems, matrices; \
       basic concepts of probability, permutations, combinations, \
       binomial distribution, mean and variance, and the normal \
       approximation to the binomial distribution. Examples from \
       biology and the social sciences are used.";
    find_description_test
      "The seventh course in the FA19 MATH json describes Calc 2."
      seventh_math19
      "Focuses on integration: applications, including volumes and arc \
       length; techniques of integration, approximate integration with \
       error estimates, improper integrals, differential equations \
       (separation of variables, initial conditions, systems, some \
       applications). Also covers infinite sequences and series: \
       definition and tests for convergence, power series, Taylor \
       series with remainder, and parametric equations.";
  ]

let sum_of_scores_test
    (name : string)
    (input : (string * float) list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Database.sum_of_scores input)
    ~printer:string_of_float

let sum_of_scores_tests =
  [
    sum_of_scores_test "sum of scores of MATH1910 with 80, 90 score"
      [ ("MATH1910", 80.) ]
      320.;
    sum_of_scores_test
      "sum of scores of MATH2210 and MATH2220, with 80, 90 scores \
       respectively"
      [ ("MATH2210", 80.); ("MATH2220", 90.) ]
      680.;
    sum_of_scores_test
      "sum of scores of MATH1910, MATH1920, MATH2210, with 80, 90, 90 \
       scores respectively"
      [ ("MATH1910", 80.); ("MATH1920", 90.); ("MATH2210", 90.) ]
      1040.;
    sum_of_scores_test
      "sum of scores of MATH1910, MATH1920, MATH2210, MATH, with 80, \
       90, 90,90 scores respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
      ]
      1400.;
    sum_of_scores_test
      "sum of scores of MATH1910, MATH1920, MATH2210, MATH2940, with \
       80, 90, 90, 90, 85 scores respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
        ("MATH2940", 85.);
      ]
      1740.;
  ]

let sum_of_credits_test
    (name : string)
    (input : (string * float) list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Database.sum_of_credits input)
    ~printer:string_of_float

let sum_of_credits_tests =
  [
    sum_of_credits_test "sum of scores of MATH1910, with 4 credits"
      [ ("MATH1910", 80.); ("MATH1920", 90.) ]
      8.;
    sum_of_credits_test
      "sum of scores of MATH2210 and MATH2220, with 4, 4 credits \
       respectively"
      [ ("MATH2210", 80.); ("MATH2220", 90.) ]
      8.;
    sum_of_credits_test
      "sum of scores of MATH1910, MATH1920, MATH2210, with 4, 4, 4 \
       scores respectively"
      [ ("MATH1910", 80.); ("MATH1920", 90.); ("MATH2210", 90.) ]
      12.;
    sum_of_credits_test
      "sum of scores of MATH1910, MATH1920, MATH2210, MATH, with 4, 4, \
       4, 4 credits respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
      ]
      16.;
    sum_of_credits_test
      "sum of scores of MATH1910, MATH1920, MATH2210, MATH2940, with \
       4, 4, 4, 4, 4 credits respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
        ("MATH2940", 85.);
      ]
      20.;
    sum_of_credits_test
      "sum of scores of MATH1910, MATH1920, MATH2210, , MATH2930, \
       MATH2940, with 4, 4, 4, 4, 4, 4 credits respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
        ("MATH2930", 85.);
        ("MATH2940", 85.);
      ]
      24.;
  ]

let calculate_gpa_test
    (name : string)
    (input : (string * float) list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Database.calculate_gpa input)
    ~printer:string_of_float

let calculate_gpa_tests =
  [
    calculate_gpa_test
      "sum of scores of MATH1910 and MATH1920, with 80, 90 scores \
       respectively"
      [ ("MATH1910", 80.); ("MATH1920", 90.) ]
      85.;
    calculate_gpa_test
      "sum of scores of MATH2210 and MATH2220, with 80, 90 scores \
       respectively"
      [ ("MATH2210", 80.); ("MATH2220", 90.) ]
      85.;
    calculate_gpa_test
      "sum of scores of MATH1910, MATH1920, MATH2210, with 80, 90, 100 \
       scores respectively"
      [ ("MATH1910", 80.); ("MATH1920", 90.); ("MATH2210", 100.) ]
      90.;
    calculate_gpa_test
      "sum of scores of MATH1910, MATH1920, MATH2210, MATH, with 80, \
       90, 90, 90 scores respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
      ]
      87.5;
    calculate_gpa_test
      "sum of scores of MATH1910, MATH1920, MATH2210, MATH2940, with \
       80, 90, 90, 90, 85 scores respectively"
      [
        ("MATH1910", 80.);
        ("MATH1920", 90.);
        ("MATH2210", 90.);
        ("MATH2220", 90.);
        ("MATH2940", 85.);
      ]
      87.;
  ]

let conv_time_test
    (name : string)
    (input : string)
    (expected_output : Database.timings) : test =
  name >:: fun _ ->
  assert_equal expected_output (Database.conv_time input)

let conv_time_tests =
  [
    conv_time_test "conv tim test early morning" "earlymorning"
      Early_Morning;
    conv_time_test "conv tim test morning" "morning" Morning;
    conv_time_test "conv tim test lunch time" "lunchtime" Lunch;
    conv_time_test "conv tim test afternoon" "afternoon" Afternoon;
    conv_time_test "conv tim test evening" "evening" Evening;
  ]

let time_sort_test
    (name : string)
    (input1 : string)
    (input2 : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Database.time_sort input1 input2)
    ~printer:(pp_list pp_string)

let time_sort_tests =
  [
    time_sort_test "time sort test 1" "MATH" "earlymorning"
      [
        "Introduction To Analysis";
        "Linear Algebra";
        "Multivariable Calculus Engrs";
        "Calculus For Engineers";
        "Calculus II";
        "Calculus II";
        "Calculus II";
        "CalculusI";
        "CalculusI";
        "CalculusI";
        "CalculusI";
        "CalculusI";
        "CalculusI";
        "Finite Math Life/Soc Sciences";
      ];
  ]

let credits_from_json_test name input expected_output : test =
  name >:: fun _ ->
  assert_equal
    (credits_from_roster input)
    expected_output ~printer:string_of_int

let credits_from_json_tests =
  [
    credits_from_json_test "The course MATH1920 is 4 credits."
      "MATH1920" 4;
    credits_from_json_test "The course MATH1011 is 1 credit." "MATH1011"
      1;
  ]
(************************** Major TESTS ******************************)
(*********************************************************************)

let check_dobmajor_coll_test
    (name : string)
    (input1 : string)
    (input2 : string)
    (input3 : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Major.check_dobmajor_coll input1 input2 input3)
    ~printer:string_of_bool

let check_dobmajor_coll_tests =
  [
    check_dobmajor_coll_test
      "check double major and college test CS and ECE not in CAS" "CS"
      "ECE" "cas" false;
  ]

let newcourses_gen_test
    (name : string)
    (input1 : string list)
    (input2 : string list)
    (expected_output : 'a list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Major.newcourses_gen input1 input2)
    ~printer:(pp_list pp_string)

let newcourses_gen_tests =
  [
    newcourses_gen_test "new courses generation ocaml and 3110"
      [ "ocaml" ] [ "3110" ] [ "ocaml"; "3110" ];
    newcourses_gen_test "new courses generation JAVA and 2110"
      [ "JAVA" ] [ "2110" ] [ "JAVA"; "2110" ];
    newcourses_gen_test "new courses generation JAVA and 2110, not 3110"
      [ "JAVA"; "2110"; "not3110" ]
      [ "2110" ]
      [ "JAVA"; "2110"; "not3110" ];
    newcourses_gen_test "new courses generation JAVA and 2110, not 3110"
      [ "ALGO"; "2110"; "not3110" ]
      [ "2110" ]
      [ "ALGO"; "2110"; "not3110" ];
    newcourses_gen_test "new courses generation JAVA and 2110, not 3110"
      [ "ML"; "4780"; "not3110" ]
      [ "2110" ]
      [ "ML"; "4780"; "not3110"; "2110" ];
  ]

let major_parse_exception_test
    (name : string)
    (input : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> parse input)

let major_parse_tests =
  [
    major_parse_test "Major.parse test: 'major cs' is [Major cs]"
      " major cs" (Major "cs");
    major_parse_exception_test
      "Major.parse test: 'not interested' raises Malformed"
      " not interested" Malformed;
    major_parse_exception_test "Major.parse test: ' ' raises Empty" ""
      Empty;
    major_parse_test "Goback.parse test: 'goback' is [Goback]" "goback"
      Goback;
    major_parse_test "Quit.parse test: 'quit' is [Quit]" "quit" Quit;
    major_parse_test
      "College.parse test: 'college engineering' is [College \
       engineering]"
      "college engineering" (College "engineering");
    major_parse_test
      "Notinterested.parse test: 'Notinterested' is [Notinterested]"
      "notinterested" Not_interested;
    major_parse_exception_test
      "Major.parse test: 'not Interested' raises Malformed"
      " not Interested" Malformed;
    major_parse_test
      "Course.parse test: 'course math1910' is [Course math1910]"
      "course math1910" (Course "math1910");
    major_parse_test
      "DoubleMajor.parse test: 'doublemajor cs ece' is [Doublemajor cs \
       ece]"
      "doublemajor cs ece"
      (Doublemajor [ "cs"; "ece" ]);
    major_parse_exception_test {|"no" raises Malformed. |} "no"
      Malformed;
    major_parse_exception_test {| "double cs ece" raises Malformed. |}
      "double cs ece" Malformed;
  ]

let remove_spaces_test
    (name : string)
    (input : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Major.remove_spaces input)
    ~printer:(pp_list pp_string)

let remove_spaces_tests =
  [
    remove_spaces_test "remove spaces test yes is yes" [ " yes" ]
      [ " yes" ];
    remove_spaces_test "remove spaces test \"\" no is no" [ ""; "no" ]
      [ "no" ];
    remove_spaces_test "remove spaces test \"\"\"\"3110 is 3110"
      [ ""; ""; "3110" ] [ "3110" ];
    remove_spaces_test "remove spaces test \"\"\"\" 3110 \"\" is 3110"
      [ ""; ""; "3110"; "" ] [ "3110" ];
    remove_spaces_test "remove spaces test \"\"\"\" 3110 \"\"\"\""
      [ ""; ""; "3110"; ""; "" ]
      [ "3110" ];
  ]

let return_first_char_test
    (name : string)
    (input1 : string)
    (input2 : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Major.return_first_char input1 input2)
    ~printer:string_of_int

let return_first_char_tests =
  [
    return_first_char_test "return_first_char_test first 2 is 6"
      "first 2" 2 6;
    return_first_char_test "return_first_char_test second?2 is 7"
      "second?2" 2 7;
    return_first_char_test "return_first_char_test ocaml3110 is 5"
      "ocaml3110" 1 5;
    return_first_char_test "return_first_char_test algo4820 0 is 4"
      "algo4820" 0 4;
    return_first_char_test "return_first_char_test java 2110 1 is 4"
      "java2110" 1 4;
  ]

let capitalize_and_add_test
    (name : string)
    (input : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Major.capitalize_and_add input)
    ~printer:(pp_list pp_string)

let capitalize_and_add_tests =
  [
    capitalize_and_add_test
      "capitalize_and_add_test yes 1; no 2 is YES 1; NO 2"
      [ "yes1"; "no2" ] [ "YES 1"; "NO 2" ];
    capitalize_and_add_test
      "capitalize_and_add_test ocaml 3110; algo 4820 is OCAML 3110; \
       ALGO 4820"
      [ "ocaml3110"; "algo4820" ]
      [ "OCAML 3110"; "ALGO 4820" ];
    capitalize_and_add_test
      "capitalize_and_add_test ocaml 3110; algo 4820; java2110 is \
       OCAML 3110; ALGO 4820; JAVA 2110"
      [ "ocaml3110"; "algo4820"; "Java2110" ]
      [ "OCAML 3110"; "ALGO 4820"; "JAVA 2110" ];
    capitalize_and_add_test
      "capitalize_and_add_test ocaml 3110; algo 4820; java2110 is \
       OCAML 3110; ALGO 4820; JAVA 2110; ml4780"
      [ "ocaml3110"; "algo4820"; "Java2110"; "mL4780" ]
      [ "OCAML 3110"; "ALGO 4820"; "JAVA 2110"; "ML 4780" ];
    capitalize_and_add_test
      "capitalize_and_add_test python1110; ocaml 3110; algo 4820; \
       java2110 is PYTHON 1110; OCAML 3110; ALGO 4820; JAVA 2110; ML \
       4780"
      [ "pythOn1110"; "ocaml3110"; "algo4820"; "Java2110"; "mL4780" ]
      [
        "PYTHON 1110"; "OCAML 3110"; "ALGO 4820"; "JAVA 2110"; "ML 4780";
      ];
  ]

(*********************************************************************)
(*********************************************************************)

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           check_dobmajor_coll_tests;
           newcourses_gen_tests;
           major_parse_tests;
           from_json_roster_tests;
           find_prereq_tests;
           find_credits_tests;
           find_forbidden_overlap_tests;
           find_location_tests;
           find_description_tests;
           sum_of_scores_tests;
           sum_of_credits_tests;
           calculate_gpa_tests;
           conv_time_tests;
           time_sort_tests;
           credits_from_json_tests;
           remove_spaces_tests;
           return_first_char_tests;
           capitalize_and_add_tests;
         ]

let _ = run_test_tt_main suite

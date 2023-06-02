open Source.Major
open Source.Database
open Source.Formatter

exception NotValid

let strip_space str =
  let acc = ref "" in
  ignore
    (for i = 0 to String.length str - 1 do
       let letter = str.[i] in
       if letter = ' ' then () else acc := !acc ^ String.make 1 letter
     done);
  !acc

let rec sec lst =
  match lst with
  | [ x; y ] -> y
  | _ -> raise NotValid

let file = "data/took.txt"

let rec print_file lst =
  let () =
    let oc = open_out file in
    match lst with
    | [] -> print_endline ""
    | h :: t ->
        Printf.fprintf oc "%s\n" h;
        print_file t;
        close_out oc
  in
  print_endline ""

let rec sum_of_scores = function
  | (course, score) :: t ->
      let credits = credits_from_roster course |> float_of_int in
      (credits *. score) +. sum_of_scores t
  | [] -> 0.

let rec sum_of_credits = function
  | (course, score) :: t ->
      let credits = credits_from_roster course |> float_of_int in
      credits +. sum_of_credits t
  | [] -> 0.

let rec calculate_gpa_aux (lst : (string * float) list) : float =
  let change_upper =
    List.map (fun (str, fl) -> (String.uppercase_ascii str, fl)) lst
  in
  let total_scores = sum_of_scores change_upper in
  let total_credits = sum_of_credits change_upper in
  total_scores /. total_credits

let calculate_gpa = calculate_gpa_aux

[@@@ocamlformat "disable=true"]

let prnt_valid_courss () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Invalid course: enter a valid one. List of valid ones are 
    ['math 1910; math1920']"

let prnt_command () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Please enter a command: list of available commands : ['quit', 'major 
    some_major', 'doublemajor' 'some major' 'some major'] list of available 
    majors: ['cs', 'ece']. Please note that if you don't enter a valid 
    major/college, the program is going to ask you again. Thus, for the sake of 
    effiencency, check that the information you enter is valid."

let back_courss_option () =
  print_endline "Would you like to go back and rechoose. say 'no' or 'goback"

let prnt_charactrs () =
  ANSITerminal.print_string [ ANSITerminal.black ]
    {| Please Note that characters are case sensitive so should be 
written as shown! |}

let prnt_timing () =
  print_endline
    {| Please enter the preferred timing. Please say: 
                'early morning': before 10 A.M. 
                'morning': 10 A.M. to noon 
                'lunch time': noon to 2 P.M. 
                'after noon': 2 P.M. to 5:00 P.M. 
                'evening': after 5:00 P.M. |}

let empty_red () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    {| Error: Empty command. Try again with a valid command this time."|}

let learn_msg () =
  print_endline
    {|Would you like to learn more about the available courses in a specific 
    subject that happen in a certain time?|}

let empty_green () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    {| Error: Empty command. Try again with a valid command this time. |}

let malformed_red () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    {| Error: Malformed command. Try again with a valid command \
   this time. |}

let malformed_green () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    {| Error: Malformed command. Try again with a valid command \
   this time. |}

let unvalid_mjr () =
  ANSITerminal.(print_string [ yellow ] "\nInvalid major name. \n")

let unvalid_course () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "\n\nError: One of your courses is not valid. Try again.\n"

let quit_msg () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nNice seeing you. Bye! \n\n"

let go_back_msg () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "We recognize you would like to go back. The game didn't start yet so 
    no worries! \n\n"


let wrong_command () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Error: Malformed command as you did not enter a valid command. 
    Try again with a valid command this time."

let no_worries () = print_endline "Ok. No worries!"

let gpa_not_strng () =
  print_endline "Error: You did not enter it as a string. Try again"

let rentr_doub_option () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "you have 2 options: start the game again, or reenter doublemajor command. 
    say 'restart' or 'goback'\n"

let know_courss () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Is there any course you would like to know more about? If so please say 
    course 'coursename' where for ex we could say course MATH1920 . If you 
    don't have any then please say notinterested!\n"

let courses_option () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Would you like to enter any courses you took so that we can output the 
    updated roadmap? Say 'yes' or 'no'. Or you can type ['quit'] to quit.\n"

let enter_courses () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Cool! Enter them now and please make sure to write one course as a string 
    and seaprate each course name by at least one space. For ex: I can say 
    [math1910 math1920].\n"

let prnt_gpa () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    " Would you like to calculate your GPA?\n"

let valid_mjrs major_name =
  (not (List.mem (List.hd major_name) [ "cs"; "ece" ]))
  || not (List.mem (sec major_name) [ "cs"; "ece" ])

let go_back_double () =
  print_endline
    "you have 2 options: start the game again, or reenter doublemajor command. 
    Say 'restart' or 'goback'\n"

let reenter_double () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "Would you like to go back to reenter the doublemajor, yes or no?\n"

let enter_double () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "Please go ahead, and enter the 2 majors you would like saying 'doublemajor'
     'first_major' 'second_major''.\n"

let enter_coll () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Please enter a college: college 'name' . List of available names: 
    ['engineering']. Or you can type ['quit'] to quit \n"

let unvalid_mjr_msg () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nOne of the majors or colleges are not valid.\n"

let go_back_magenta () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "\nWould you like to go back, yes or no? \n"

let quit_green () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\n Okay, bye! \n\n"

let quit_blue () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\n Okay, bye! \n\n"

let as_you_wish () = print_endline "\nas you wish!\n"
let lovely () = print_endline "\nlovely!\n"

let wrong_maj_coll () =
  print_endline "\nError:wrong major or college. try again"

[@@@ocamlformat "disable=false"]

let rec play_game () =
  prnt_command ();
  prnt_charactrs ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match parse user_input with
      | exception Empty ->
          empty_red ();
          play_game ()
      | exception Malformed ->
          malformed_red ();
          play_game ()
      | Major t -> (
          match List.mem t [ "cs"; "ece" ] with
          | false ->
              unvalid_mjr ();
              go_back_unvalidmajor ()
          | true -> go_back_validmajor t)
      | Doublemajor t -> play_doublemajor t
      | Quit ->
          quit_msg ();
          exit 0
      | Goback ->
          go_back_msg ();
          play_game ()
      | _ ->
          wrong_command ();
          play_game ())

(*[play_doublemajor majorname] will start the game if the user inputted
  a double major.*)
and play_doublemajor major_name =
  match List.length major_name < 2 with
  | true ->
      rentr_doub_option ();
      unvalid_entry_double ()
  | false -> (
      match valid_mjrs major_name with
      | true ->
          unvalid_mjr ();
          go_back_double ();
          unvalid_entry_double ()
      | false -> valid_double major_name)

(*[unvalid_entry_double ()] will be executed when the user enters a
  wrong major for doublemajor.*)
and unvalid_entry_double () =
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          empty_green ();
          unvalid_entry_double ()
      | [ "restart" ] ->
          lovely ();
          play_game ()
      | [ "goback" ] ->
          as_you_wish ();
          allow_only_doublemajor ()
      | [ "quit" ] ->
          quit_green ();
          exit 0
      | exception Empty ->
          empty_red ();
          unvalid_entry_double ()
      | exception Malformed ->
          malformed_green ();
          unvalid_entry_double ()
      | _ ->
          malformed_red ();
          unvalid_entry_double ())

(*[allow_only_doublemajor ()] will be executed to allow the user to only
  enter as a command doublemajor and quit.*)
and allow_only_doublemajor () =
  enter_double ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match parse user_input with
      | Doublemajor t -> play_doublemajor t
      | Quit ->
          ANSITerminal.print_string [ ANSITerminal.green ]
            "\n\nNice seeing you. Bye! \n\n";
          exit 0
      | exception Empty ->
          empty_red ();
          allow_only_doublemajor ()
      | exception Malformed ->
          malformed_red ();
          allow_only_doublemajor ()
      | _ ->
          malformed_red ();
          allow_only_doublemajor ())

(*[valid_double major_name] is executed when the user enters a
  doublemajor that includes valid majors.*)
and valid_double major_name =
  reenter_double ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          empty_red ();
          valid_double major_name
      | [ "yes" ] ->
          lovely ();
          allow_only_doublemajor ()
      | [ "no" ] ->
          as_you_wish ();
          play_double (List.hd major_name) (sec major_name)
      | [ "quit" ] ->
          quit_green ();
          exit 0
      | _ ->
          malformed_red ();
          go_back_unvalidmajor ())

(*[play_double firstmaj secondmaj] will start the game of executing
  doublemajor assuming the inputs are valid.*)
and play_double firstmaj secondmaj =
  enter_coll ();
  prnt_charactrs ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match parse user_input with
      | exception Empty ->
          empty_red ();
          play_double firstmaj secondmaj
      | exception Malformed ->
          malformed_red ();
          play_double firstmaj secondmaj
      | Quit ->
          quit_green ();
          exit 0
      | College y -> (
          match enterinfodoub firstmaj secondmaj y with
          | exception WrongMajor ->
              unvalid_mjr_msg ();
              play_game ()
          | _ ->
              ask_timing ();
              play_gpa ();
              play_course ();
              play_game ())
      | _ ->
          malformed_red ();
          play_double firstmaj secondmaj)

(*[go_back_validmajor major_name] will implement the functionality of
  being able to go back if the user inputted a valid major.*)
and go_back_validmajor major_name =
  go_back_magenta ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          empty_red ();
          go_back_validmajor major_name
      | [ "yes" ] ->
          lovely ();
          play_game ()
      | [ "no" ] ->
          as_you_wish ();
          play_college major_name
      | [ "quit" ] ->
          quit_green ();
          exit 0
      | _ ->
          malformed_red ();
          go_back_validmajor major_name)

(*[go_back_unvalidmajor ()] will implement the functionality of being

  able to go back if the user did not input a valid major.*)

and go_back_unvalidmajor () =
  go_back_magenta ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "" ] ->
          empty_red ();
          go_back_unvalidmajor ()
      | [ "yes" ] ->
          lovely ();
          play_game ()
      | [ "no" ] ->
          as_you_wish ();
          play_game ()
      | [ "quit" ] ->
          quit_green ();
          exit 0
      | _ ->
          malformed_red ();
          go_back_unvalidmajor ())

(*[play_college t] will start the process playing knowing the major the
  person chose.*)
and play_college t =
  enter_coll ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match parse user_input with
      | exception Empty ->
          empty_red ();
          play_college t
      | exception Malformed ->
          malformed_red ();
          play_college t
      | Quit ->
          quit_blue ();
          exit 0
      | College y -> (
          match enterinfo t y with
          | true ->
              play_course ();
              play_game ()
          | false -> (
              match program_exists t y with
              | true ->
                  play_course ();
                  play_game ()
              | false -> play_game ()))
      | _ ->
          malformed_red ();
          play_college t)

(*[play_course ()] will ask the user to input courses if he would like
  to know more about them.*)
and play_course () =
  know_courss ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match parse user_input with
      | exception Empty ->
          empty_red ();
          play_course ()
      | exception Malformed ->
          malformed_red ();
          play_course ()
      | Course y ->
          let a = from_json_roster (String.uppercase_ascii y) in
          if a then play_course ()
          else
            let _ = prnt_valid_courss () in
            let _ = play_course () in
            play_game ()
      | Not_interested ->
          print_endline
            "No problem! Knowing more about courses is benefecial \
             though!"
      | Quit ->
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "\n\nNice seeing you. Bye! \n\n";
          exit 0
      | _ ->
          malformed_red ();
          play_course ())

(*[enterinfo maj coll] will ask the user to input the courses he
  likes.*)
and enterinfo maj coll : bool =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\n\
     Would you like to enter any courses you took so that we can \
     output the updated roadmap? Say 'yes' or 'no'. Or you can type \
     ['quit'] to quit \n";
  match read_line () with
  | exception End_of_file -> false
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "yes" ] -> go_back_entercourses maj coll true
      | [ "no" ] ->
          no_worries ();
          go_back_entercourses maj coll false
      | [ "" ] ->
          empty_red ();
          enterinfo maj coll
      | [ "quit" ] ->
          print_endline "Ok, bye!";
          exit 0
      | _ ->
          malformed_red ();
          enterinfo maj coll)

and ask_timing () =
  learn_msg ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> begin
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "no" ] -> ()
      | [ "yes" ] -> begin
          print_endline
            "Please enter the subject name. For example: 'CS' or 'MATH'";
          match read_line () with
          | exception End_of_file -> ()
          | input_subj -> begin
              prnt_timing ();

              match read_line () with
              | exception End_of_file -> ()
              | input_timing ->
                  let subject_name =
                    List.hd
                      (remove_spaces
                         (String.split_on_char ' ' input_subj))
                  in
                  let timing = strip_space input_timing in
                  let courses_names = time_sort subject_name timing in
                  print_col "Courses" courses_names
            end
        end
      | _ -> ask_timing ()
    end

(*[go_back_entercourses maj coll indicator] will allow the user to go
  back and reenter courses.*)
and go_back_entercourses maj coll indicator =
  back_courss_option ();

  match read_line () with
  | exception End_of_file -> false
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "goback" ] ->
          print_endline "Sure.";
          enterinfo maj coll
      | [ "no" ] -> (
          match indicator with
          | false ->
              ask_timing ();
              false
          | true ->
              ask_timing ();
              take_courses maj coll)
      | _ -> go_back_entercourses maj coll indicator)

(*[take_courses maj coll will allow the user to enter his courses
  assuming he is set on them and does not want to change them.]*)
and take_courses maj coll =
  enter_courses ();
  match read_line () with
  | exception End_of_file -> false
  | inp -> (
      let b = remove_spaces (String.split_on_char ' ' inp) in
      match List.map (fun x -> return_first_char x 0) b with
      | exception NoNumber ->
          unvalid_course ();
          take_courses maj coll
      | _ -> (
          match from_json_producing_state maj coll with
          | exception WrongMajor ->
              wrong_maj_coll ();
              let _ = play_game () in
              false
          | _ -> (
              updated_roadmap (from_json_producing_state maj coll) b;
              prnt_gpa ();
              match read_line () with
              | exception End_of_file -> false
              | input ->
                  if input = "yes" then play_gpa () else ();
                  true)))

(* iterate the strings in took.txt,
 * for each 'course' string, we ask: what's your GPA of 'course';
 * then we calculate the GPA *)
and play_gpa () =
  (* read from took.txt *)
  let file = "data/took.txt" in
  let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true do
        lines := input_line chan :: !lines
      done;
      !lines
    with
    | End_of_file ->
        close_in chan;
        List.rev !lines
  in
  let str_lst = read_file file in
  let printstr = List.fold_left (fun acc x -> x ^ acc) "" str_lst in
  let _ = print_endline printstr in
  let new_str =
    match str_lst with
    | h :: _ -> h
    | [] -> failwith "Empty string list from took.txt"
  in
  let new_lst str =
    let not_empty_str str = str <> "" in
    String.split_on_char ' ' str |> List.filter not_empty_str
  in
  let course_score_assoc = ref [] in
  let lst = new_lst new_str in
  let rec read_txt txt_lst =
    match txt_lst with
    | h :: t -> begin
        print_endline ("Please enter your score of " ^ h);
        match read_line () with
        | exception End_of_file -> ()
        | input -> (
            try
              let new_elt = (h, float_of_string input) in
              course_score_assoc := new_elt :: !course_score_assoc;
              read_txt t
            with
            | _ ->
                gpa_not_strng ();
                play_gpa ())
      end
    | [] -> print_endline ""
  in
  read_txt lst;
  let cumulative_gpa = calculate_gpa !course_score_assoc in
  print_endline (string_of_float cumulative_gpa)

(*[enterinfodoub firstmaj secondmaj coll] will allow the user to enter
  any courses asuming he is double majoring.*)
and enterinfodoub firstmaj secondmaj coll =
  courses_option ();
  match read_line () with
  | exception End_of_file -> ()
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "yes" ] -> (
          enter_courses ();
          match read_line () with
          | exception End_of_file -> ()
          | inp -> (
              let b = remove_spaces (String.split_on_char ' ' inp) in
              match List.map (fun x -> return_first_char x 0) b with
              | exception NoNumber ->
                  unvalid_course ();
                  enterinfodoub firstmaj secondmaj coll
              | _ -> (
                  match doubrecord firstmaj secondmaj coll with
                  | exception WrongMajor ->
                      wrong_maj_coll ();
                      play_game ()
                  | _ ->
                      reenter_courses_doubmajor firstmaj secondmaj coll
                        b)))
      | [ "no" ] ->
          no_worries ();
          doub firstmaj secondmaj coll
      | [ "" ] ->
          empty_red ();
          enterinfodoub firstmaj secondmaj coll
      | [ "quit" ] ->
          quit_blue ();
          exit 0
      | _ ->
          malformed_red ();
          enterinfodoub firstmaj secondmaj coll)

(*[reenter_courses_doubmajor first_maj second_maj coll b] will allow the
  user to reenter courses if the courses he chose is valid.*)
and reenter_courses_doubmajor first_maj second_maj coll b =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "\n\
     Your courses are valid. However, before outputting the updated \
     roadmap, would you like to go back and reenter any courses? Yes, \
     or no?\n";
  match read_line () with
  | user_input -> (
      match remove_spaces (String.split_on_char ' ' user_input) with
      | [ "yes" ] -> enterinfodoub first_maj second_maj coll
      | [ "no" ] ->
          print_endline "As you wish!";
          updated_roadmap (doubrecord first_maj second_maj coll) b
      | [ "" ] ->
          empty_red ();
          reenter_courses_doubmajor first_maj second_maj coll b
      | _ ->
          malformed_red ();
          reenter_courses_doubmajor first_maj second_maj coll b)

let main () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "\n\nWelcome to our database!\n";
  play_game ()

(* Execute the game engine. *)
let () = main ()

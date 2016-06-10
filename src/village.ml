
let repeat n f =
  let rec loop count acc =
    if count = n
    then acc
    else loop (count + 1) (f () :: acc) in
  loop 0 []

let professions = [
  "Accountant";
  "Actor";
  "Architect";
  "Astronomer";
  "Author";
  "Baker";
  "Bricklayer";
  "Bus driver";
  "Butcher";
  "Carpenter";
  "Chef";
  "Cleaner";
  "Dentist";
  "Designer";
  "Doctor";
  "Dustman";
  "Electrician";
  "Engineer";
  "Factory worker";
  "Farmer";
  "Fireman";
  "Fisherman";
  "Florist";
  "Gardener";
  "Hairdresser";
  "Journalist";
  "Judge";
  "Lawyer";
  "Lecturer";
  "Librarian";
  "Lifeguard";
  "Mechanic";
  "Model";
  "Newsreader";
  "Nurse";
  "Optician";
  "Painter";
  "Pharmacist";
  "Photographer";
  "Pilot";
  "Plumber";
  "Politician";
  "Police officer";
  "Postman";
  "Real estate agent";
  "Receptionist";
  "Scientist";
  "Secretary";
  "Shop assistant";
  "Soldier";
  "Tailor";
  "Taxi driver";
  "Teacher";
  "Translator";
  "Traffic warden";
  "Travel agent";
  "Veterinary doctor";
  "Waiter";
  "Window cleaner";
]

let professions_length = List.length professions
let random_profession () =
  List.nth professions (Random.int professions_length)

type person = {
  age        : int;
  family     : string;
  home       : [`North | `South | `West | `East];
  name       : string;
  profession : string;
  sex        : [`M | `F];
}

let age {age} = age
let sex {sex} = sex

let random_person () =
  let age = Random.int 100 in
  { name       = String.capitalize_ascii @@ Names.random_left ();
    family     = String.capitalize_ascii @@ Names.random_right ();
    home       = List.nth [`North; `South; `West; `East] (Random.int 4);
    profession = (if age < 18 then "student" else random_profession ());
    sex        = List.nth [`M; `F] (Random.int 2);
    age }

let random_village population =
  repeat population random_person




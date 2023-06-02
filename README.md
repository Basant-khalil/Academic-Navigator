# Academic Navigator 
The project's vision is to create a user-friendly application that helps students navigate their college course selection process. My goal is to provide various tools and features that will make the experience more convenient and personalized for the student. Some features of the project include:
- I successfully parsed data from the Cornell class roster API, which allowed us to provide students with the most up-to-date information on available courses. 
- I implemented a feature that allows students to sort classes based on their preferred time frames, making it easier for them to create a schedule that works for them. For instance, the student can ask the engine to output classes occuring at certain time frame such as the morning.
- I added a GPA calculator to our application, which will help students monitor their academic progress throughout their college years. 
- I gave students the ability to go back and fix their input or quit the application at any time, ensuring that their experience with the application is as stress-free and flexible as possible. 
![Custom Kitchen Drawing](images/frst_ocaml.png)
The engine first starts by welcoming the user to the database. Then, it informs the user of the available comamnds that he can input such as major **_some major_** or **_quit_** command.

The engine has verifiability, meaning if you input an invalid command or even invalid major whether in the **_major_** or **_doublemajor_** command, it will promp you to enter the right one. 
For example, if the user inputted **_major lk_** where **_lk_** is not a valid major, the engine will let the user know of such mistake and will ask if the user would like correct the error. 
![Custom Kitchen Drawing](images/scnd_ocaml.png)
The engine will then asks the user to input the college as one major could exist in different colleges and would thus have different requirements based on that. If the college inputted by the user is invalid, the engine would let the user know of such error and promt him to reenter the choice as shown below.
![Custom Kitchen Drawing](images/thrd_ocaml.png)
Once the user inputs the right major and college, the engine would go on the next stage and ask if there are any courses the user took so it can remove it from the roadmap for the major.
![Custom Kitchen Drawing](images/frth_ocaml.png)
Then the updated roadmap of the major courses gets printed.
![Custom Kitchen Drawing](images/fifth_ocaml.png)
<br> The engine asks if there the user is interested to explore courses of certain subject happening at some timeframe and ifsuch it prints those courses.
<br>
![Custom Kitchen Drawing](images/sxth_ocaml.png)
<br>
Finally, the engine asks if the user is interested to know more information about any course and if so it prints that information from the class roster.
![Custom Kitchen Drawing](images/ninth_ocaml.png)
[Video Demo of Academic Navigator Project](images/OCaml Video.mp4)



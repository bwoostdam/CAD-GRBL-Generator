# CAD-GRBL-Generator
AutoLISP for generating GRBL Code for laser
----------------------------------------------
This AutoLISP-code is 'as is' and delevopment is currently not active as there are no wishes, or plans on my part.
Written in AutoLISP. No Visual Lisp commands were used to guarantee compatibility.
Code is developed, tested and used under ZWCAD and AutoCAD.
----------------

Why this code was developed: 
----------------------------
Many years ago I learned to use an industrial CNC machine during my education. (G-code)
I also had access to very early versions of AutoCAD and learned AutoLISP.
I created code that translated CAD into G-code for CNC machines.
This code was far form complete or perfect and due to career choices got lost.
(I could never afford a CNC machine anyway.)
During my career I came back to CAD and 'solid state laser machines' running GRBL were gaining in 'working area' and 'cutting-power by' the month.
(I was never inclined to buy a 3D printer for a meriad of reasons.)
I waited untill they met my budget, power and work-area criteria and finally bought one.
1 of my 2 hobbies during my lifetime is passionately building and flying Model aircraft.
Currently I fly mostly high performance F3A type aircraft and F3A-sequences.
Ofcourse I ran into trouble because of the tool-offset needed (kerf) to produce accurate parts.
GRBL is limited and does not support the original G-codes for this.
Thinking it would be easily solvable by using some free application like FreeCAD (which btw I am a big fan of),
it turned out that no software was available that met my criteria coming from a CAD background.
All were/are horribly cumbersome to me.
So, as I found nothing was available on CAD-platforms I use, and having gained more knowledge over the years, I decided to create my own code.
Today I am using it with great joy as it fulfills all my current usage-wishes.
Ik hope others will try it, and maybe use it as a base to create their own version and possibly elaborate in it.

Installation:
-------------
  A manual with how to add this to your CAD-application will be provided soon.
  For those who are familiar with the menu-workings, Support file Search paths in profiles and the APPLOAD command will have no problem.


Requirements:
-------------
Machines:
 The code is written for a NEJE MAX 4 laser machine with a controllable Z-axis.
 The code uses material-configuration files wich also allows for machines that have a fixed Z-axis

CAD: Any CAD platform that can run AutoLISP

Usage:
------
A manual will be added shortly
I will not answer any questions nor will I change the code actively upon requests.


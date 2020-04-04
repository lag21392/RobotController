; *********************************************************************************************
; * SimpleController.nlogo
; *
; *  SETTINGS:
; *    Location of origin: Corner, Top Left
; *    max-pxcor 38
; *    min-pycor -23
; *    Patch size 14
; *
; *  INPUT:
; *    Maze file
; *
; *  OUTPUT:
; *    Controller
; *
; *
; *  @univ1   University of Limerick, Ireland
; *  @lab1    Biocomputational and Developmental System (BDS)
; *  @head    Conor Ryan
; *  @author1 Enrique Naredo
; *  @author2 Lucas Agustin Gonzalez
; *  @date    March 2020
; *  @version 1.0.0
; *  @notes   1) Implementation of a Mapper
; *********************************************************************************************


;;; dependencies ;;;;


;; agents for the environment
breed       [ pellets pellet ]
pellets-own [ powerup? ]
patches-own
[
  wall?                  ;; true if the patch is a wall of the maze
  target?                ;; true if the patch is the target of the maze
  pellet-grid?
]

breed [robots robot]

globals
[
  steps
  step-time
  controller
  genotype
  tool
  Number_Genotype_Loader
  Score-list
  number-robots
]

;;;; SETUP ;;;;;;
to setup
  clear-all              ;; Combines the effects of all clear commands into one
  initiate
  load-Multi_Genotype_Random

  setup-patches
  setup-maze

  ifelse(verification = true)[
  set number-robots length split Genotypes-to-run "\n"
  setup-robots
  show "se verifico bien"
  ][
  set number-robots 0
  setup-robots
  show "Se verifico mal"
  ]

  set tool "Eraser"

  update

end
to update
  load-multi-phenotype
end
to setup-patches
  ask patches
  [
    set wall?    false
    set target?  false
    set pcolor   white    ;; all environment patches
  ]
end
to initiate
  set parent-1-genotype ""
  set parent-2-genotype ""
  set Crossover_1_Genotype ""
  set Crossover_2_Genotype ""
  set parent-1 1
  set parent-2 2
  set Number 1
  set number-robots 1
  set bit-position 1
  set Score-list [ 1 1 1 1 1 1 1 1 1 1 ]
end
to Load_Genotype
  set Number_Genotype_Loader Number
  let Multi_Genotype_List load-Multi_Genotype_List
  let number_genotype ""
  set number_genotype (item (10 - Number_Genotype_Loader) Multi_Genotype_List)
  set genotype number_genotype
  output-print genotype

  set Score item  (Number_Genotype_Loader - 1)  Score-List
end




to setup-maze
  let maze [
 "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E00000000000000.......0000000000000000E"
 "E00000000000000.00*00.0000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "E0000000000000000000000000000000000000E"
 "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"]
  let maze-line ""
  let char ""
  let x 0
  let y 0
  while [y > (-1 * length maze  ) ]
  [
    set maze-line item (-1 * y) maze

    while [x < length maze-line]
    [
     set char item x maze-line
     if char = "E" [ ask patch x y [ext-wall]  ] ;; exterior wall
     if char = "." [ ask patch x y [int-wall]  ] ;; interior wall
     if char = "*" [ ask patch x y [add-target]] ;; target
     set x x + 1   ;; increase x to read next char
    ]
    set y y - 1    ;; decrease a row, and
    set x 0        ;; reset x to read first char
  ]

end

;;;; Rules to build the environment ;;;;;
;; Exterior wall
to ext-wall
  set wall? true
  set pcolor blue
end

;; Interior wall
to int-wall
  set wall? true
  set pcolor brown
end

;; Target
to add-target
  set target? true
  set pcolor  red
end

;;;;; ROBOTS ;;;;
to setup-robots
  clear-turtles               ;; kill all previous turtles
  clear-ticks                 ;; resets the tick counter to zero
  let colors [ 5 15 25 35 45 55 65 75 85 95 ]
  create-robots number-robots
  [

    set size 1.7              ;; robot size
    set color item 0 colors           ;; robot color
    SET colors remove-item 0 colors
  ]
  reset-ticks                 ;; resets the tick counter to zero
  place-robots
end

to place-robots
  let r 0
  while [r < number-robots]
  [
    let x 0
    let y 0
    while [ [pcolor] of patch x y != white ]
    [
      set x random 36 + 1
      set y random -21 - 1
    ]
    let h item random 4 [0 90 180 270]
    ask robot r [setxy x y set heading h]
    set r r + 1
  ]
end

to Load_Number_Robots


  ifelse(verification = true)[
  set number-robots length split Genotypes-to-run "\n"
  setup-robots
  show "se verifico bien"
  ][
  set number-robots 0
  setup-robots
  show "Se verifico mal"
  ]


end
to-report verification
    let verificationList split (replace-string Genotypes-to-run "10" "0") "\n"
  let i 0
  let verefication true
  while[i < length split Genotypes-to-run "\n"][
    show length item i verificationList
    ifelse(length item i verificationList  <= 6)[


    ][
    set verefication false
    ]
    set i i + 1
  ]
  report verefication
end

to-report split [ text c ]
  let position_ini 0
  let position_end 0
  let vector_strings []

  ifelse (length text > 0 and not member? c text )

  [ set vector_strings insert-item 0 vector_strings text
    report vector_strings]
  [
  while [position_end < length text and (substring text position_end (position_end + (length c) )) != c and member? c text ][

    while [(substring text position_end (position_end + (length c) )) != c and position_end < length text ]
     [set position_end position_end + 1

    ]
     set vector_strings insert-item (length vector_strings) vector_strings (substring text position_ini position_end)
    set text substring text (position_end + length c ) (length text)
    ifelse (not member? c text and length text > 0)[set vector_strings insert-item (length vector_strings) vector_strings (substring text position_ini length text)][]

    set position_end 0
    set position_ini 0

      ]
  report vector_strings
  ]
end
to-report load-controller_GA [number_genotype]
  let phenotype_code ""
  set controller (split grammars "\n")

;  file-open ("templates.txt")
;  while [not file-at-end?]
;  [ set controller lput file-read-line controller ]
;  file-close

  ;;genotype load

  ;let Multi_Genotype_List load-Multi_Genotype_List
  ;set genotype (item (10 - Number) Multi_Genotype_List)


  ;;template load
  let template string-to-binary-to-integer substring number_genotype 0 3
  set phenotype_code item template controller

  let list-sensors [-1 -1 -1 -1 -1 -1 -1 -1]
  let list-actuators [-1 -1 -1 -1 -1 -1 -1 -1 -1]
  let i 0
  while [17 > i]
   [
      ifelse (i < 8)
        ;;8 sensor load
        [set list-sensors replace-item i list-sensors (string-to-binary-to-integer substring number_genotype (3 + (i * 2))  (5 + (i * 2)))]
        ;;load of the 9 actuators
        [set list-actuators replace-item (i - 8) list-actuators (string-to-binary-to-integer substring number_genotype (3 + (i * 2))  (5 + (i * 2)))]
      set i (i + 1)
   ]
  ;;user-message list-sensors
  ;;user-message list-actuators

  ;;**SENSORS**
  let list-names-sensors ["wall-back?" "wall-ahead?" "wall-left?" "wall-right?" ]
  ;;**ACTUATORS**
  let list-names-actuators ["move-back" "move-ahead" "turn-left" "turn-right" ]

  ;;replacement of template variables
  set i 0
  while [8 > i]
  [
 ifelse (item i list-sensors > -1)[

  set phenotype_code replace-string phenotype_code word "S" (word (i + 1)) (item (item i list-sensors) list-names-sensors)
    ][]
  set i (i + 1)

  ]
  set i 0
  while [9 > i]
  [

  ifelse (item i list-actuators > -1)[


  set phenotype_code replace-string phenotype_code word "A" (word (i + 1)) (item (item i list-actuators) list-names-actuators)
    ][]

  set i (i + 1)
  ]

report phenotype_code
end

to-report replace-string [ str old new ]
  ifelse (member? old str and (length new) > 0)
   [
  let position_old position old str
  let position_eraser_fin (position_old + length new)
  let i position_old + ((length new) )
  set str replace-item position_old str new
  while [position_eraser_fin > i]
  [
    set str replace-item i str ""
    set i (i + 1)
  ]
  ]
  [
  ]

  report str
end
to load-multi-phenotype

  let Number_Genotype_Loader_ 0
  let Multi_Genotype_List load-Multi_Genotype_List
  let number_genotype ""
  set multi_phenotype ""
  set number_genotype (item ( Number_Genotype_Loader_) Multi_Genotype_List)

  set multi_phenotype  (load-controller_GE number_genotype 2)
  set Number_Genotype_Loader_ Number_Genotype_Loader_ + 1
  while [Number_Genotype_Loader_ < 10]
    [set number_genotype (item (Number_Genotype_Loader_) Multi_Genotype_List)


    ;carga de phenotype vercion GA
    ;set multi_phenotype (word multi_phenotype "\n" (load-controller_GA number_genotype) )

    ;carga de phenotype vercion GE
    set multi_phenotype (word multi_phenotype "\n" (load-controller_GE number_genotype 2) )

    set Number_Genotype_Loader_ Number_Genotype_Loader_ + 1]
end

to Run-Controllers
  ;;cargar
  let list_phenotype split multi_phenotype "\n"
  let list_nums_phenotypes split Genotypes-to-run "\n"
  let num_robot 0
  let i 0
  let list_num_phenotype split (item i list_nums_phenotypes) ","


    while [length list_nums_phenotypes > num_robot][

      set list_num_phenotype split (item num_robot list_nums_phenotypes) ","
      ;show item ((read-from-string (item 0 list_num_phenotype)) - 1) list_phenotype
      foreach list_num_phenotype  [ x -> ask robot num_robot  [show (item ((read-from-string x) - 1) list_phenotype) run (item ((read-from-string x) - 1) list_phenotype) ]  wait ms / 1000 ]
      set num_robot num_robot + 1
    ]



end
to-report string-to-binary-to-integer [text]
  let i 0
  let numDecimal 0
  while [length text > i]
  [
    ifelse (item i text = "1")
     [set numdecimal (numDecimal + (1 * (2 ^ (length text - 1 - i))))]
     [set numdecimal (numDecimal + (0 * (2 ^ (length text - 1 - i))))]
    set i (i + 1)
  ]
  report numDecimal
end

;;;;; RUN ;;;;;
to Run-Controllers_anterior
  let Multi_Genotype_List load-Multi_Genotype_List
  let number_genotype ""

  let number_robot 0
  while [number-robots > number_robot][
   set number_genotype (item  (length Multi_Genotype_List  - number_robot - 1) Multi_Genotype_List)

   let penotype_code load-controller_GE number_genotype 2

   ask robot number_robot
   [run penotype_code ]
   wait ms / 1000
    set number_robot number_robot + 1
  ]

end

;;;;; RUN ;;;;;
to Run-Controller
let num 0
  let num-robot 0
  while [num < length load-Multi_Genotype_List][
   Run-Controller-num num-robot num
    set num num + 1
    wait ms / 1000
  ]
end
to Run-Controller-num [num-robot num]

  let Multi_Genotype_List load-Multi_Genotype_List
  let number_genotype ""

  let number_robot Number_Genotype_Loader - 1

   set number_genotype (item  ((length Multi_Genotype_List  - num) - 1) Multi_Genotype_List)

   let penotype_code load-controller_GE number_genotype 2
   ask robot num-robot
   [run penotype_code ]

end

to Save-Score
  set Score-List replace-item  Number_Genotype_Loader  Score-List score
end
to Crossover

  set Crossover_1_Genotype word (substring parent-1-genotype 0 (Bit_Position )) (substring parent-2-genotype (Bit_Position ) 37)
  set Crossover_2_Genotype word (substring parent-2-genotype 0 (Bit_Position )) (substring parent-1-genotype (Bit_Position ) 37)
end
to replace [parent Crossover_Genotype]
    ifelse not (Crossover_Genotype = "")[
    let Multi_Genotype_List load-Multi_Genotype_List
    set Multi_Genotype_List replace-item (10 - parent) Multi_Genotype_List Crossover_Genotype
    set Multi_Genotype append-List Multi_Genotype_List

  ][
  ]

end
to Load
    ;;load Crossover
  let Multi_Genotype_List load-Multi_Genotype_List
  set parent-1-genotype item (10 - parent-1) Multi_Genotype_List
  set parent-2-genotype item (10 - parent-2) Multi_Genotype_List
end
;;MUTATE
to mutate
ifelse genotype != ""
  [

  ifelse substring genotype (bit-position - 1) bit-position = "1"
    [set genotype (replace-item (bit-position - 1) genotype "0")]
    [set genotype (replace-item (bit-position - 1) genotype "1")]

  let Multi_Genotype_List load-Multi_Genotype_List
  set Multi_Genotype_List replace-item (10 - Number_Genotype_Loader) Multi_Genotype_List genotype
  set Multi_Genotype append-List Multi_Genotype_List

  let number_genotype ""
  ;;set number_genotype (item (10 - Number_Genotype_Loader) Multi_Genotype_List)
  ;;set genotype number_genotype
  output-print genotype


  update

  ][]

end
to-report load-Multi_Genotype_List
 let Multi_Genotype_List ["" "" "" "" "" "" "" "" "" ""]

  let i 0
  let j 0
  while [10 > i]
  [

    set Multi_Genotype_List replace-item i Multi_Genotype_List (substring Multi_Genotype j (j + 37))
    set i (i + 1)
    set j (j + 1 + 37)
  ]
  report Multi_Genotype_List
end

to load-Multi_Genotype_Random

  set Multi_Genotype ""
  let i 0
  let j 0

  while [i < 10][
    ifelse (i = 0) [][

      set Multi_Genotype word Multi_Genotype "\n"
    ]

    set j 0
    while [j < 37][
      ifelse (random 2 = 0)
        [set Multi_Genotype word Multi_Genotype "0"]
        [set Multi_Genotype word Multi_Genotype "1"]
      set j (j + 1)
    ]
    set i (i + 1)

  ]

end
to-report append-List [xs]
    let i 1
    let result ""
    set result item 0 xs
      while [10 > i]
    [
    set result word result "\n"
    set result word result (item i xs)
    set i (i + 1)
    ]
  report result
end

;;;;  SENSORS  ;;;;;
to-report wall-ahead?
  report [wall?] of patch-ahead 1
end

to-report wall-back?
  report [wall?] of patch-ahead -1
end

to-report wall-left?
  report [wall?] of patch-left-and-ahead 90 1
end

to-report wall-right?
  report [wall?] of patch-right-and-ahead 90 1
end

;;;; ACTUATORS ;;;;
to move-ahead
  ifelse not wall-ahead? and not target? [
  forward 1
  ]
  [
  ]
end

to move-back
  ifelse not wall-back? and not target?[
  back 1
  ]
  [
  ]
end

to turn-right
  right 90


end

to turn-left
  left 90

end

to rand-turn
  set heading item random 4 [0 90 180 270]
end


;; If the mouse is down, use the Current Tool on the patch the mouse is over
to draw
  if mouse-down?
  [

    if  tool = "Eraser"
    [ erase ]

    if tool = "Draw Wall"
    [ Draw-Wall  ]

    if tool = "Target"
    [ Draw-Target    ]
  ]
end

;; Clears Walls/Gates, Removes Pellets from the patch the mouse is over
to erase
  ask patch (round mouse-xcor) (round mouse-ycor)
  [
    set pcolor white
    set pellet-grid? false
    set wall? false
    set target? false
  ]
end
to Draw-Wall

  ask patch (round mouse-xcor) (round mouse-ycor)
  [
    ifelse not wall? and not target? and not any? robots-here           ;; true if the patch is the target of the maze

    [ int-wall ]
    [

    ]
  ]
end
to Draw-Target
  remove-targets
  ask patch (round mouse-xcor) (round mouse-ycor)
  [
    ifelse not wall? and not target? and not any? robots-here           ;; true if the patch is the target of the maze

    [
      add-target ]
    [

    ]
  ]
end
to remove-targets
  ask patches
  [
    ifelse target?[
        set pcolor white
    set pellet-grid? false
    set wall? false
    set target? false
    ][]

  ]
end

to-report load-controller_GE [g-bin-string bits-parser]

  let p "0"
  let xy [
    ["1" "13" "131" "1313"]
    [ "ifelse(2)[333][333] " "ifelse(2)[][333] " "ifelse(2 and 2)[333][333] " "ifelse(2 and 2 and 2)[333][333] " ]
    ["wall-back? " "wall-ahead? " "wall-left? " "wall-right? "]
    [" " "turn-left " "turn-right " "move-ahead " ]
  ]
  let ini 0
  let fin bits-parser
  let int-parcer  (string-to-binary-to-integer substring g-bin-string ini fin)
  let exit true
  let posicionMinima length p
  let seleccion ""
  let error_recursive false
  while[ (member? "0" p or member? "1" p or member? "2" p or member? "3" p) and not error_recursive ]
  [
    show int-parcer
    set posicionMinima length p

    if (member? "0" p and (position "0" p < posicionMinima)) [set posicionMinima position "0" p set seleccion "0"]
    if (member? "1" p and (position "1" p < posicionMinima)) [set posicionMinima position "1" p set seleccion "1"]
    if (member? "2" p and (position "2" p < posicionMinima)) [set posicionMinima position "2" p set seleccion "2"]
    if (member? "3" p and (position "3" p < posicionMinima)) [set posicionMinima position "3" p set seleccion "3"]

    ifelse (member? "0" p and seleccion = "0")[set p replace-string p "0" (item int-parcer (item 0 xy))]
        [ifelse (member? "1" p and seleccion = "1") [set p replace-string p "1" (item int-parcer (item 1 xy))]
          [ifelse (member? "2" p and seleccion = "2") [set p replace-string p "2" (item int-parcer (item 2 xy))]
            [ifelse (member? "3" p and seleccion = "3") [set p replace-string p "3" (item int-parcer (item 3 xy))]
              []
            ]
          ]
        ]

  set ini fin
  set fin (fin + bits-parser)
    ifelse (length g-bin-string >= fin)
    [set int-parcer (string-to-binary-to-integer substring g-bin-string ini fin)]
    [
      set fin bits-parser
      set ini 0
    ]
    if(length p > 4000)[show "Recursive error" set error_recursive true wait ms / 10]
     ;show p
     ;wait ms / 1000
  ]

  report p
end
@#$#@#$#@
GRAPHICS-WINDOW
0
48
678
469
-1
-1
17.18
1
10
1
1
1
0
1
1
1
0
38
-23
0
0
0
1
ticks
30.0

BUTTON
1
10
149
43
Setup Evnironment
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
150
10
296
43
Run-Controllers
Run-Controllers
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
9
722
365
740
Genotype selected for mutation
11
0.0
1

INPUTBOX
40
474
341
670
multi_genotype
1100011100100100000010110111101101111\n0111011011100001111100101000010010110\n1101110001110010110101011110010100111\n0110000000011111000000110011011000010\n0110000000110100110100101000111010001\n0010100101011000001100101000100001101\n1101110101101001001001001111001101111\n1010100111001011001111001011000001110\n1111111001000010111100111010111011110\n1001111000001010101111101000101010101
1
1
String

SLIDER
20
807
970
840
bit-position
bit-position
1
37
1.0
1
1
NIL
HORIZONTAL

BUTTON
11
842
124
875
NIL
Mutate\n\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
9
736
992
806
47

SLIDER
6
498
39
646
Number
Number
1
10
1.0
1
1
NIL
VERTICAL

CHOOSER
12
905
122
950
parent-1
parent-1
1 3 5 7 9
0

CHOOSER
12
954
122
999
parent-2
parent-2
2 4 6 8 10
0

INPUTBOX
123
905
424
965
parent-1-genotype
NIL
1
0
String

INPUTBOX
123
975
424
1035
parent-2-genotype
NIL
1
0
String

CHOOSER
426
905
564
950
Bit_Position
Bit_Position
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
35

BUTTON
425
972
562
1032
Crossover
Crossover
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
566
906
866
966
crossover_1_genotype
NIL
1
0
String

INPUTBOX
565
972
865
1032
crossover_2_genotype
NIL
1
0
String

BUTTON
867
906
952
966
replace-1
replace parent-1 Crossover_1_Genotype
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
13
1002
121
1035
NIL
Load
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
39
675
341
708
NIL
Load_Genotype
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
700
436
1003
469
Load_Number_Robots
Load_Number_Robots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1011
50
1392
464
grammars
0 ::=   \"1\"\n         |\"13\"              \n         |\"131\"                      \n         |\"1313\"\n1 ::=   \"ifelse(2)[333][333] \"\n         |\"ifelse(2)[][333] \"\n         |\"ifelse(2 and 2)[333][333] \"\n         |\"ifelse(2 and 2 and 2)[333][333] \"\n2 ::=   \"wall-back? \"\n         |\"wall-ahead? \"\n         |\"wall-left? \"\n         |\"wall-right? \"\n3 ::=   \" \"\n         |\"turn-left \"\n         |\"turn-right \"\n         |\"move-ahead \"
1
0
String

BUTTON
364
10
451
43
Draw Wall
set tool \"Draw Wall\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
523
10
578
43
Eraser
set tool \"Eraser\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
453
10
522
43
Target
set tool \"Target\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
579
10
669
43
Draw
draw
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
867
972
951
1033
replace-2
replace parent-2 Crossover_2_Genotype
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
994
737
1134
782
Score
Score
1 2 3 4 5 6 7 8 9 10
0

BUTTON
1135
736
1247
836
Run-Controller
Run-Controller
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
993
784
1133
836
Save-Score
Save-Score
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
704
50
1003
83
ms
ms
25
1000
25.0
25
1
NIL
HORIZONTAL

INPUTBOX
701
228
1003
424
genotypes-to-run
1,2,5\n2\n3\n4\n5\n6\n7\n8\n9\n10
1
1
String

TEXTBOX
703
187
995
218
Put the genotype list to run by robot in the \nform \"1,2,4\"
11
0.0
1

INPUTBOX
345
475
1755
670
multi_phenotype
ifelse(wall-ahead? )[move-ahead  turn-right ][turn-left   ]  ifelse(wall-right?  and wall-ahead? )[move-ahead turn-right move-ahead ][turn-left move-ahead move-ahead ]  \nifelse(wall-ahead?  and wall-left?  and wall-right? )[turn-right  turn-left ][move-ahead move-ahead  ] turn-right \nifelse(wall-right? )[][ turn-left move-ahead ]  ifelse(wall-right?  and wall-ahead? )[turn-left turn-left move-ahead ][turn-right turn-left turn-left ]  \nifelse(wall-back?  and wall-back? )[ turn-left move-ahead ][move-ahead   ]  \nifelse(wall-back?  and wall-back? )[ move-ahead turn-left ][ move-ahead turn-left ]  \nifelse(wall-left?  and wall-ahead? )[turn-left turn-left turn-right ][  move-ahead ] \nifelse(wall-right? )[][turn-left turn-left turn-right ] turn-right ifelse(wall-back? )[][turn-right turn-left  ] move-ahead \nifelse(wall-left?  and wall-ahead? )[move-ahead  turn-right ][move-ahead  move-ahead ] move-ahead ifelse(wall-left? )[move-ahead   ][turn-left move-ahead move-ahead ] \nifelse(wall-right?  and wall-left?  and wall-ahead? )[  turn-right ][move-ahead move-ahead  ] move-ahead ifelse(wall-left?  and wall-right? )[turn-right move-ahead move-ahead ][move-ahead move-ahead move-ahead ] turn-right \nifelse(wall-right? )[][turn-right   ] turn-right ifelse(wall-left?  and wall-right? )[move-ahead turn-right turn-right ][ turn-right turn-right ] 
1
0
String

TEXTBOX
41
481
1750
665
NIL
11
0.0
1

TEXTBOX
130
912
418
1028
NIL
11
0.0
1

TEXTBOX
570
928
864
1026
NIL
11
0.0
1

TEXTBOX
14
885
164
903
Crossover
11
0.0
1

TEXTBOX
1020
49
1393
459
NIL
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

It is a platform to learn how the Grammatical Evolution

## HOW IT WORKS AND HOW TO USE IT

1-Generation of Genotypes and Phenotypes:
	Click on "Setup Evnironment", this generates genotypes that appear in "multi_genotype" with their respective phenotypes that appear in "multi_phenotype", these phenotypes are generated taking two bits
and passing it through the "gramers" matrix, starting from row 0 and if it remains with an incomplete phenotype, it takes again the views of the genotype from the beginning.

2-Robot configuration:
	In "genotypes-to-run" you can add lists of phenotypos to run as a sequence, for each row a robot is generated, phenotypos range from 1 to 10 and are separated by commas.

3-Editing the map:
	You can Edit with the buttons by selecting some of the "Draw Wall", "Target" and "Eraser" tools, then click on "Draw" and edit it at your discretion, to stop click on "Draw" again.

4-Robot startup:
	Click on "Run-Controllers" to start and click again to stop all robots previously configured.
	You can run a particular phenotype by loading the genotype by selecting it with "Number" and adding it with "Load_Genotype", then running with "Run-Controller".
    You can change the speed of robots with the speed bar "ms".

5-Save score:
    Se puede dar un puntaje a las gramaticas de 1 a 10 previamente esta tiene que estar seleccionada con "Number" y "Load_Genotype".

6-Mutation:
	A grammar is selected with "Number" and "Load_Genotype", then the bit to mutate is selected in "bit-position" and the "Mutate" button is pressed, this will be automatically saved in "multi_genotype".

7-Crossover:
	Two genotypos "parent-1" and "parent-2" are selected, then click on "Load", then select the bit cut position "Bit_Position" and click on "Crossover", we see how it would be in "crossover_1_genotype "and" crossover_2_genotype "and they can be replaced with the" replace-1 "and" replace-2 "buttons

## CREDITS AND REFERENCES

University   University of Limerick, Ireland
Laboratory   Biocomputational and Developmental System (BDS)
Head    Conor Ryan
Author1 Enrique Naredo
Author2 Lucas Agustin Gonzalez
Date    March 2020
Version 5.0.0 GE
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

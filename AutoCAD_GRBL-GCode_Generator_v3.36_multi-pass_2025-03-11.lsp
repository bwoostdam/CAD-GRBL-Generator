;**************************************************************************
;* Source filename: AutoCAD_GRBL-Gcode_Generator_v3.36.lsp
;**************************************************************************
;* Author         : B.W. Oostdam                                           
;* Date           : 2024-05-30                                             
;* Company        : BWO                                                    
;* Department(s)  : Development                                            
;* E-mail         : bwoostdam@gmail.com                                    
;*-------------------------------------------------------------------------
;* Modified by    : 
;* Company        : 
;* Department(s)  : 
;* E-mail         : 
;**************************************************************************
;* Language       : VLisp- , (Auto)Lisp for AutoCAD R14 and up               
;*-------------------------------------------------------------------------
;* Version number :                                                        
;*-------------------------------------------------------------------------
;* Description    : LISP-routine that generates GRBL-GCode for use with    
;*                : GRBL Laser engravers/cutters.                          
;*                : Primarily intented for cutting
;*-------------------------------------------------------------------------
;* Run-commands   : GCode                                                  
;*-------------------------------------------------------------------------
;* Param. var.    :                                                        
;*-------------------------------------------------------------------------
;* Dependent files: ...\Custom\..                                          
;*-------------------------------------------------------------------------
;* Note           : This version is based on (LW)Polylines                 
;**************************************************************************
;* Revision       : Added cutting-definention files                                                       
;* Revision date  : 2024-06-05                                                       
;* Reason         : Need to associate color with laserpower and feedspeed                                                        
;**************************************************************************
;* Revision       : System to determine the Outline of parts is introduced.
;* Revision date  : 2024-06-10
;* Reason         : Check if any of the lines has an 'inside' indication
;*                  Transparency is used to for indicate 'inside'.
;*                  (i.e. NOT 'outer-perimeters' of parts but holes inside
;*                  the perimeter of parts).
;*                  In the drawing the 'inside' shapes will be shown as
;*                  partially transparant (more vague or 'dimmed')
;**************************************************************************
;* Revision       : Completely different approach to solving G41/G42
;* Revision date  : 2024-06-14                                                       
;* Reason         : Need to have a toolpath wiht toolwidth compensation
;*                  which is lacking in GRBL1.1
;*                  CAD's own OFFSET functionality will be used instead
;*                  Also: Transparency is use in favor of color
;**************************************************************************
;* Revision       : Small improvement on working with object-transparency.
;* Revision date  : 2024-06-19                                                       
;* Reason         : Transparency is now more flexible and not
;*                  value dependent. The transparency can have any value
;*                  or it is ByLayer:  -1 or 0.
;*                  :-(   In ZWCAD it in '0',  in AutoCAD it is '-1'  :-(
;*                  For cutting-lines use: Transparency-ByLayer
;**************************************************************************
;* Revision       : Large overhaul: 2.1 --> 3.0
;* Revision date  : 2024-07-13                                                       
;* Reason         : Added Laser-Drawing layer (based on 'color not ByLayer 
;*                : and seperated all functionality into seperate functions.
;*                : Also added menu with toolbar
;**************************************************************************
;* Revision       : Major overhaul: 3.0 --> 3.3
;* Revision date  : 2025-01-17                                                       
;* Reason         : Added Multipass with, speed, depth and power
;*                : caluclations.
;*                : Code-optimization introduced 2 list-variables instead
;*                : of 1: GCode is now replaced by GCodeWri and GCodeCut
;**************************************************************************
;* Revision       : Major overhaul: 3.0 --> 3.4
;* Revision date  : 2025-01-28                                                       
;* Reason         : Reversed generating GCode or INSIDE and OUTSIDE
;*                : This results eventually in INSIDE first and OUTIDE last
;**************************************************************************

;(defun c:GCode( / SelectionSet SelectionSetCounter)
;(defun c:GCPrep( / Geometry2Delete LDF_FileName LDF_File CurDate)
(defun c:GCPrep( / Geometry2Delete LDF_File CurDate)

  ;------------------------------------------------
  ; Put requirements in place
  ;------------------------------------------------
  ; Get the name of the current drawing and use it for saving the GCODE-file
  (setq
    FileName (strcat (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)))
    CurDate (rtos (getvar "CDATE") 2 6)
    ; Break up the string into its separate parts
    YYYY (substr CurDate 1 4)
    M    (substr CurDate 5 2)
    D    (substr CurDate 7 2)
    CurDate (strcat YYYY "-" M "-" D)
    FileName (strcat FileName "_" CurDate)
    ;HH   (substr CurDate 10 2)
    ;MM   (substr CurDate 12 2)
    ;SS   (substr CurDate 14 2)
    ;CurTime (strcat HH "-" MM "-" SS)
  )

  ; Get the Laser-work-definition file with the feedspeeds en laser power
  ;(if (setq LDF_FileName (getfiled "Open the laser-work-defenition file." FileName "ldf" 192)) ; Verkrijg path\..\filename.ext dmv filedialoogbox.
  (if (setq LDF_FileName (getfiled "Open the laser-work-defenition file." (getvar "dwgprefix") "ldf" 192)) ; Verkrijg path\..\filename.ext dmv filedialoogbox.
    (progn
      (setq LDF_File (open LDF_FileName "r"))
      (ReadLDF_File LDF_File)
    )
  )

  ; 2024-06-17: This needed to change, because of change of concept
  ; Lasercutting is always 1 material-type per drawing.
  ; So there are always only 3 types of geometry:
  ; OUTSIDE-contours, INSIDE-contours and TEXT
  ; -------
  ; There is no need to go through all the layers to find geometry.
  ; Selection-Set Filtering is now used instead
  ; We can make selections based on:
  ;  1. AutoCAD: No transparency-setting (440 -1), is same as Transparency ByLayer (440 -1) ==> OUTSIDE contour
  ;     ZWCAD: No transparency-setting (440 0), is same as Transparency ByLayer (440 0) ==> OUTSIDE contour
  ;  2. Transparency-setting NOT (440  -1) ==> INSIDE contour, any other value than '-1'.
  ;  3. Type: TEXT
  ; ------
  ; Also there is no need to compare the layer color.
  ; It is only 1 material-type we are cutting or writing on.
  ; Therefor: only settings in the LDF file are needed
  ; Cutting         : power and speed
  ; Writing(Drawing): power and speed
  ; Multipass       : Z-axis support was added for thicker materials

  ; Use the values from the .LDF file (LLDefList) for the Laserpower values and the feedspeed
  ;(textscr)
  (princ "\nContents Laser-settings: \n") (princ LLDefList) (princ "\n")
  ;(getstring )

  (setq
    LaserPowerCut (nth 1 (assoc "CUT" LLDefList))
    FeedSpeedCut (nth 2 (assoc "CUT" LLDefList))
    LaserPowerWrite (nth 1 (assoc "WRITE" LLDefList))
    FeedSpeedWrite (nth 2 (assoc "WRITE" LLDefList))
  )
     
  (if (not LaserPowerCut)
    (progn
      (alert "Failed to set laser power/feedspeed... Exiting")
      (EXIT)
    )
  )
    
  (setvar "CMDECHO" 0)
  ;(command "UNDO" "Mark")
  (setvar "LUPREC" 8)
  (setvar "insunits" 0)
  ; Note: This does not change existing Spline-ellipses! --> 2024-06-05: Splines are now handled correctly during 'prep & cLean'
  (setvar "pellipse" 1) ; Set PELLIPSE Var to 1, in order to create Poly-arc-ellipses.
  (setvar "measurement" 1) ; Make all measurements METRIC, this is crucial when (exporting) DXF files and for many more systems.
  ;(setvar "CMDECHO" "1")
 
  ; Crucial requirements and settings
  (setq
    Geometry2AnalyzeOut  nil
    Geometry2AnalyzeIn   nil
    Geometry2AnalyzeText nil
    Geometry2AnalyzeDraw nil
    ToolWidth 0.12 ; Will be used for CAD's OFFSET function (Normally this would be G41/G42)
    FuzzFactor 0.0001
    ;FuzzFactor 0.00005
  )

  ; Determine if it is AutoCAD or ZWCAD: This influences the 'ByLayer'-Transparency-value.
  (if (= (strcase (substr (ver) (- (strlen (ver)) 6)  (strlen (ver)))) "(ZWCAD)")
    (progn
      (setq TransparencyByLayer (cons 440 0))
    )
    (progn ; AutoCAD
      (setq TransparencyByLayer (cons 440 -1))
    )
  )

  ;(textscr)
  ;(princ (strcat "\nPlatform: " (ver) " - TransparencyByLayer value: ")) (princ TransparencyByLayer) (princ "\n")
  ;(getstring)

  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ; Clean-up and prep the DWG: This converts ALL geometry into LWPolylines
  ; This is the most important function before we start to convert geometry
  ; to GCode.
  (CleanAndPrepDWG)
  (princ)
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
)


; ******************************************************
; GCSave: Save the resulting GCODE function
; and clean up.
; ******************************************************
(defun C:GCSave ( / GCode_FileName GCode_File *error*)
;(defun C:GCSave ( / *error* )

; Crucial intial settings!:
  (setq
    GCode    nil
    GCodeCUT nil
    GCodeWRI nil 
  )
  ;---------------------------------------
  ; This ERROR function should be inside the GCSave function.
  ; It is a local-error-catch function
  ;----------------------------------------
  (defun *error* (msg / GCode_FileName GCode_File )
    (princ "\nError in Function: GCSAVE")
    (princ (strcat "\n" msg))
    (princ "\nGCODE-filename: ") (princ GCode_Filename)
    (princ "\nGCODE-file-handle: ") (princ GCode_File)
  )

  ;----------------------------------------
  ; Get the file-name to save the GCode to.
  ;(setq
    ;FileName (strcat "P:/Tabula/Programma onderdelen/Tabula/Bestanden/Support/MapGuide-Symbol-lib/XML-Symbolen/" (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)))
    ;FileName (strcat (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)))
  ;)
  (if GCode_File
    (progn
      (alert "File possibly still open... Closing")
      (close GCode_File)
    )
    (progn
      ;(princ "\nFile is closed, continue...")
    )
  )

  (if (setq GCode_FileName (getfiled "Save the GCode to .NC file." FileName "nc;gcode;gc" 5)) ; Verkrijg path\..\filename.ext dmv filedialoogbox.
    (progn
     (setq GCode_File (open GCode_FileName "w"))
    )
  )
 
  (princ (strcat "File opened for write: " GCode_FileName))
  (princ "\nNew file-handle: ") (princ GCode_File)


  ; The GCode will now be generated and later written to file in this order:
  ; 1 DRAWING
  ; 2 TEXT
  ;-----------
  ; 3 INSIDE
  ; 4 OUTSIDE
  ; Do not change the order, there is a reason for doing it this way!


  ;DRAWING: If there is DRAWING geometry, convert it to GCode
  (if Geometry2AnalyzeDraw
    (progn
      ;(alert "Passing all collected DRAW-LWPolylines to the variable Geometry2Analyze.")
      (setq Geometry2Analyze Geometry2AnalyzeDraw)
      ; Now put a marker in the list so we know we will be CUTTING, WRITING or DRAWING
      ; When writing the GCode to file, it is used to change the laser power and feedspeed
      (AnalyzeGeometry Geometry2Analyze) ; Result: global variable GCode contains a list of GCode
      ;(princ"\nDRAWING GCodeWri length: ") (princ (sslength Geometry2Analyze))
      (princ"\nDRAWING GCodeWri length: ") (princ (length GCode))
      ;(textscr)
      ;(princ Geometry2Analyze)
      ;(getstring)

      (if (> (length GCode) 1)
        (progn
          (setq
            ;GCode is being reversed because else a G00 will be the last in the list. 
            GCodeWri (append (reverse GCode) GCodeWri)
            GCodeWri (cons "WRI" GCodeWri)
          )
          (princ "\nFinished: Generating GCode for DRAWING-toolpath.")
          ;(princ "\nGCodeWri draw: ") (princ GCodeWri)
        )
        (progn
          (alert "DRAW GCode-var is empty.")
          ;(EXIT)
        )
      )
    )
    (progn
      (princ "\nNo DRAWING-geometry found to convert...")
    )
  )


  ; TEXT: If there is TEXT geometry, convert it to GCode before saving the GCode
  (if Geometry2AnalyzeText
    (progn
      ;(alert "Passing all collected TEXT-LWPolylines to the variable Geometry2Analyze.")
      (setq Geometry2Analyze Geometry2AnalyzeText)
      ; Now put a marker in the list so we know we will be CUTTING, WRITING or DRAWING
      ; When writing the GCode to file it is used to change the laser power and feed speed
      (AnalyzeGeometry Geometry2Analyze) ; Result: global variable GCode contains a list of GCode
      ;(princ"\nTEXT GCodeWri length: ") (princ (sslength Geometry2Analyze))
      (princ"\nTEXT GCodeWri length: ") (princ (length GCode))
      ;(textscr)
      ;(princ Geometry2Analyze)
      ;(getstring)
      (if (> (length GCode) 1)
        (progn
          (setq
            ;GCode is being reversed because else a G00 will be the last in the list. 
            GCodeWri (append (reverse GCode) GCodewri)
            GCodeWri (cons "WRI" GCodeWri)
          )
          (princ "\nFinished: Generating GCode for TEXT-toolpath.")
          ;(princ "\nGCodeWri write: ") (princ GCodeWri)
        )
        (progn
          (alert "TEXT GCode-var is empty.")
        )
      )
    )
    (progn
      ;(EXIT)
      (princ "\nNo TEXT-geometry found to convert...")
    )
  )


  ; OUTSIDE: If there is OUTSIDE geometry, convert it to GCode before saving the GCode
  (if Geometry2AnalyzeOut
    (progn
      ;(alert "Passing all OUTSIDE collected LWPolylines to the variable Geometry2Analyze.")
      (setq Geometry2Analyze Geometry2AnalyzeOut)
      ; Now put a marker in the list so we know we will be CUTTING, WRITING or DRAWING
      ; When writing the GCode to file it is used to change the laser power and feed speed
      ; When the GCode list is empty (i.e. no other geometry), then put it on the front of the list
      (AnalyzeGeometry Geometry2Analyze) ; Result: global variable GCode contains a list of GCode
      ;(princ"\nOUTSIDE GCodeCut-length: ") (princ (sslength Geometry2Analyze))
      (princ"\nOUTSIDE GCodeCut-length: ") (princ (length GCode))
      ;(textscr)
      ;(princ Geometry2Analyze)
      ;(getstring)
      (if (> (length GCode) 1)
        (progn
          (setq
            ;GCode is being reversed because else a G00 will be the last in the list. 
            GCodeCut (append (reverse GCode) GCodeCut)
            GCodeCut (cons "CUT" GCodeCut)
          )
          (princ "\nFinished: Generating GCode for OUTSIDE-toolpath.")
          ;(princ "\nGCodeCut outside: ") (princ GCodeCut)
        )
        (progn
          (alert "OUTSIDE GCode-var is empty.")
          ;(exit)
        )
      )
    )
    (progn
      (princ "\nNo OUTSIDE-geometry found to convert...")
      ;(EXIT)
    )
  )

  ; INSIDE: If there is INSIDE geometry, convert it to GCode before saving the GCode
  (if Geometry2AnalyzeIn
    (progn
      ;(alert "Passing all collected INSIDE LWPolylines to the variable Geometry2Analyze.")
      (setq Geometry2Analyze Geometry2AnalyzeIn)

      ; Now put a marker in the list so we know we will be CUTTING, WRITING or DRAWING
      ; When writing the GCode to file it is used to change the laser power and feed speed
      (AnalyzeGeometry Geometry2Analyze) ; Result: global variable GCode contains a list of GCode
      ;(princ"\nINSIDE GCodeCut-length: ") (princ (sslength Geometry2Analyze))
      (princ"\nINSIDE GCodeCut-length: ") (princ (length GCode))
      ;(textscr)
      ;(princ Geometry2Analyze)
      ;(getstring)
      (if (> (length GCode) 1)
        (progn
          (setq
          ;GCode is being reversed because else a G00 will be the last in the list. 
            GCodeCut (append (reverse GCode) GCodeCut)
            GCodeCut (cons "CUT" GCodeCut)
          )
          (princ "\nFinished: Generating GCode for INSIDE-toolpath.")
          ;(princ "\nGCodeCut inside: ") (princ GCodeCut)
        )
        (progn
          (alert "INSIDE GCode-var is empty.")
          ;(exit)
        )
      )
    )
    (progn
      (princ "\nNo INSIDE-geometry found to convert...")
    )
  )

  ;(alert "About to write the GCODE to file")
  ; Start the XML code file structure - Write the header to the file
  ;(princ "\n\nStart: Write Header to GCode file.")
  ;(WriteHeaderOfGCode GCode_file)
  ;(princ "\nFinished: Write Header to GCode file.")

  ;============================================
  ; All GCODE has been generated at this point
  ; Let's write it to a file
  ;============================================
  (princ "\n**********************************")
  (princ "\n* Start: writing GCode to file.")
  (princ "\n**********************************")
  
  (if (or (> (length GCodeWri) 0) (> (length GCodeCut) 0))
    (progn
      ;(alert "Start: writing GCode to file.")
      ;(WriteBodyOfGCode GCode_file)
      ; Write the GCode out to file
      (WriteGCode GCode_File)
      (setq GCode nil)
    )
    (progn
      (alert "ERROR: No GCode to write to file... Exiting" )
      (EXIT)
    )
  )
  ;(princ "\nFinished: Write Body to GCode file.")


  ; Wrap up the GCode file structure
  ;(princ "\n\nStart: Write Footer to GCode file.")
  ;(WriteFooterOfGCode GCode_File)
  ;(princ "\n\nFinished: Write Footer to GCode file.")

  ; Close the GCode-file
  (princ "\n**********************************")
  (princ "\n* Finished: writing GCode to file.")
  (close GCode_File)
  (princ "\n* GCode file closed.")
  (princ "\n**********************************")
  
  ;(EXIT)
  
  ; Revert the drawing back to its start-state
  (princ "\nRevert the drawing back to the initial state...")
  ;(setvar "CMDECHO" 0)
  ;(getstring "\nPress a Key to proceed.")
  (command "UNDO" "Back")

  ; -------------------------------------------
  ; Cleanup the geometry that has been analyzed...
  ; -------------------------------------------
  ; Apparently, the UNDO command, does not undo everything ?
  ;(princ "\nCleanup the geometry that has been analyzed...\n")
  (setq
    sf (append (list
                 (cons -4 "<OR")
                   (cons -4 "<AND")   
                     (cons 8 "OUTSIDE-TOOLPATH")
                   (cons -4 "AND>")
                   (cons -4 "<AND")
                     (cons 8 "INSIDE-TOOLPATH")
                   (cons -4 "AND>")
                   (cons -4 "<AND")
                     (cons 8 "TEXT-TOOLPATH")
                   (cons -4 "AND>")
                 (cons -4 "OR>")
               )
       )
    Geometry2Delete (ssget "X" sf)
  )
  (if Geometry2Delete
    (progn
      (command "erase" Geometry2Delete "")
    )
  );

  (setvar "CMDECHO" 1)
  (princ "\n \nCompleted: Generating GCode.")
  (textscr)
  (princ)
)

; ************     End of GCSAVE      ***************


;****************************************************
;****************  Analyze Geometry *****************
;****************************************************
(defun AnalyzeGeometry(Geometry2Analyze / GeomType Element ElementCounter)
  ;(alert "Inside AnalyzeGeometry function.")
  ;(alert Geometry2Analyze)
  ;(textscr)
  ;(getstring "\nWaiting for key input")
  (setq GCode nil) ; Is a global variable but has to be empty every time

  (if Geometry2Analyze
    (progn
      (setq CurTime (UpdateTime))
      (princ "\nPlease wait: Analyzing... ") (princ CurTime)
      (setq ElementCounter 0)

      (repeat (sslength Geometry2Analyze)
        ;(princ "\n|")
        (setq
          Element (ssname Geometry2Analyze ElementCounter)
          ; What geometry-type is it ?
          GeomType (cdr (assoc 0 (entget Element)))
        )

        ;(princ "\nGEOMTYPE: ") (princ GeomType) (princ "\n")
        ;(princ "\n-")
        (if (= GeomType "CIRCLE")
          (progn
            (setq Element (list Element))
            (AnalyzeCircle Element)
          )
        )

        ;(princ "\n/")
        (if (= GeomType "LWPOLYLINE")
          (progn
            (setq Element (list Element))
            (AnalyzeLWPolyline Element)
          )
        )
        (setq ElementCounter (1+ ElementCounter))
      )
    )
    (progn
      (alert "*ERROR*: No geometry passed in Geometry2Analyze... Exiting")
      (EXIT)
    )
  )

  ;(alert "Completed: Converting geometry to GCode.")
  ;(princ "\nIndirectly returning resulting Gcode : ") (princ GCode) ; return result ? strange as it is the indirect calling function
  (progn GCode) ; return indirect result?
  ;)
  (princ)
)


;---------------------------------------------------------
;****************  Analyze LWPOLYLINE    *****************
;---------------------------------------------------------
(defun AnalyzeLWPolyline (GeometryElement /
                          GeometryElementInfo GCodeSpace GeometryElementName GeometryInfo SymBolCodeString
                          VertexCounter
                          LWPVertexList LWPVertex LWPVertexX LWPVertexY LWPVertexXY LWPVertexBulge
                          LWPVertexPreviousXY LWPVertexPreviousX LWPVertexPreviousY ArcInfo Radius
                          LWPFirstVertex LWPFirstVertexX LWPFirstVertexY LWPFirstVertexXY
                          CenterX CenterY CenterI CenterJ
                          GCodeActionType
                          *error* 
                         )
  ;(alert "In function: AnalyzeLWPolyline")
  ;(princ (strcat "\nFound LWPolyline: " (itoa ElementCounter) " - ")) (princ GeometryElement)

  ; Error-handler - This is supposed to be INSIDE the AnalyzeLWPolyline function. It is a localised error handler.
  (defun *error* (msg / )
    (princ "\nError in Function: AnalyzeWPolyline")
    (princ (strcat "\n" msg))
    (princ "\nError: Closing GCode file: ") (princ "GCode_file")
    (close GCode_File)
    (princ"\nReverting drawing to its start status...")
    (setvar "CMDECHO" 0)
    (command "UNDO" "Back")
    (setvar "CMDECHO" 1)  
    (princ "\nGeometry-info: ") (princ GeometryElementInfo)
    (princ "\nVertexCounter : ")  (princ VertexCounter) (princ "/") (princ (length LWPVertexList))
    ;(princ "\nVertex-list: ") (princ LWPVertexList)
    ;(princ "\nGCode: ") (princ GCode)
    (princ)
  )
  
  (setq
    VertexCounter 0
    fSweep 0
    fArc 0
    ;GCodeSpace "            "
    GCodeSpace "" 
    GeometryElementName (car GeometryElement)
    GeometryInfo (entget GeometryElementName)
    LWPVertexList nil
    ;SymBolCodeString ""
  )
  
  ;(princ "\n\nStart: Better put all vertices in a list first  :-{\n")
  (setq 
    LWPVertexList (MakeListOfVertices GeometryInfo)
    ;LWPVertexList (reverse LWPVertexList)
  )
  ;(princ "\nLWPVertexList: ") (princ LWPVertexList)
  ;(princ "\nEnd: Better put them all in a list first  :-}\n")
  
  
  (repeat (length LWPVertexList)
    (setq
      LWPVertex (car (nth VertexCounter LWPVertexList))
      LWPVertexX (nth 1 LWPVertex)
      LWPVertexY (nth 2 LWPVertex)
      LWPVertexXY (list LWPVertexX LWPVertexY)
      ;LWPVertexBulge (cdr (nth 1 (nth VertexCounter LWPVertexList)))
    )
    ;(princ "\n") (princ LWPVertex)

    (if (= VertexCounter 0)
      (progn
        ; Always put a MOVE to the starting point first, i.e. the first vertex
        (setq
          GCodeActionType "G00 "
          GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPVertexX 2 4) " Y" (rtos LWPVertexY 2 4))
          GCode (cons GCodeString GCode)
        )
      )

      (progn
        ; Watch it! If this is the first vertex, then there is no 'previous vertex'!
        (if (> VertexCounter 0)
          (progn
             ; Get the Bulge-factor from the previous vertex!
             (setq LWPVertexBulge (cdr (nth 1 (nth (- VertexCounter 1) LWPVertexList))))
          )
          (progn
            ; Get the Bulge-factor from the current vertex! Which is probably zero, but that does not matter
            (setq LWPVertexBulge (cdr (nth VertexCounter LWPVertexList)))
          )
        )
          
        (if (zerop LWPVertexBulge)
          ; The previous BulgeFactor is zero and therefor it is a straight line 
          (progn
            (setq
              GCodeActionType "G01 "
              GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPVertexX 2 4) " Y" (rtos LWPVertexY 2 4))
              GCode (cons GCodeString GCode)
            )
          )

          ; The PreviousBulgefactor is NOT zero, so there is an Arc to the current vertex 
          (progn
            ; Determine the sweep-direction - See AutoCAD Bulge documentation
            ; The sweep-direction will be used to determine G02 or G03
            ; FSweep not 'zero' therefor clockwise - CW = G02
            ; FSweep 'zero' therefor anti-clockwise - CCW = G03
            (if (minusp LWPVertexBulge) 
              (progn
                (setq fSweep 1)
              )
              (progn
                (setq fSweep 0)
              )
            )

            ; FSweep not 'zero' therefor clockwise - CW (G02)
            (if (/= fsweep 0)
              (progn
                (setq
                  GCodeActionType "G02 "
                  LWPVertexPreviousXY (cdr (nth 0 (nth (- VertexCounter 1) LWPVertexList))) ; Start of the Arc is the previous Vertex 
                  ; End of the Arc is the first Vertex of the current LWPVertexList 
                  ; Calculate the radius 
                  ArcInfo (Bulge2Arc LWPVertexPreviousXY LWPVertexXY LWPVertexBulge)
                  ; list returned = <center> <start angle> <end angle> <radius>
                  ;                  nth 0      nth 1        nth 2       nth3
                  ;---------------------------------------------------------------
                  ; The Radius is not usefull as it is always positive and will therefor
                  ; not give the offset in the correct direction in all cases.
                  ; Better to calculate the offset as: (known) CenterpointXY minus StartpointXY
                  ; gving I and J 
                  Radius (nth 3 ArcInfo)
                  ; Change this in to a IJ-construction, not R...
                  ; Always calculated from the START-XY position (i.e. current XY position)
                  LWPVertexPreviousX (nth 0 LWPVertexPreviousXY)
                  LWPVertexPreviousY (nth 1 LWPVertexPreviousXY)
                  CenterX (nth 0 (nth 0 ArcInfo))
                  CenterY (nth 1 (nth 0 ArcInfo))
                  CenterI (- CenterX LWPVertexPreviousX) ; Offset in X (relative) from startpoint
                  CenterJ (- CenterY LWPVertexPreviousY) ; Offset in Y (relative) from startpoint
                  GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPVertexX 2 4) " Y" (rtos LWPVertexY 2 4) " I" (rtos CenterI 2 4) " J" (rtos CenterJ 2 4))
                  GCode(cons GCodeString GCode)
                )
              )
              ; FSweep 'zero' therefor anti-clockwise - CCW
              (progn
                (setq
                  GCodeActionType "G03 "
                  LWPVertexPreviousXY (cdr (nth 0 (nth (- VertexCounter 1) LWPVertexList))) ; Start of the Arc is the previous Vertex 
                  ; End of the Arc is the first Vertex of the current LWPVertexList 

                  ; Calculate the radius 
                  ArcInfo (Bulge2Arc LWPVertexPreviousXY LWPVertexXY LWPVertexBulge)
                  ; list returned = <center> <start angle> <end angle> <radius>
                  ;                  nth 0      nth 1        nth 2       nth3
                  ;---------------------------------------------------------------
                  ; The Radius is not usefull as it is always positive and will therefor
                  ; not give the offset in the correct direction in all cases.
                  ; Better to calculate the offset as: (known) CenterpointXY minus StartpointXY 
                  Radius (nth 3 ArcInfo)
                  ; Change this in to a IJ-construction, not R...
                  ; Always calculated from the START-XY position (i.e. current XY position)
                  LWPVertexPreviousX (nth 0 LWPVertexPreviousXY)
                  LWPVertexPreviousY (nth 1 LWPVertexPreviousXY)
                  CenterX (nth 0 (nth 0 ArcInfo))
                  CenterY (nth 1 (nth 0 ArcInfo))
                  CenterI (- CenterX LWPVertexPreviousX)
                  CenterJ (- CenterY LWPVertexPreviousY)
                  ;GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPVertexX 2 4) " Y" (rtos LWPVertexY 2 4) " R" (rtos Radius 2 4))
                  GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPVertexX 2 4) " Y" (rtos LWPVertexY 2 4) " I" (rtos CenterI 2 4) " J" (rtos CenterJ 2 4))
                  GCode (cons GCodeString GCode)
                )
              )
            )
          )
        )
      )
    )

    ; If it is the last vertex...
    ; Does it have a non-zero BulgeFactor ?
    ; Yes: Close the polyline using an ARC to the first vertex
    (if (equal (nth VertexCounter LWPVertexList) (last LWPVertexList))
      (progn
        ;(princ "\nThis is the last vertex?") (princ VertexCounter) (princ " - ") (princ (last LWPVertexList))
        (if (not (zerop (cdr (nth 1 (nth VertexCounter LWPVertexList)))))
          (progn 
            ;(princ "\nClosing Polyline using an Arc.")
            (setq
              LWPVertexBulge (cdr (nth 1 (nth VertexCounter LWPVertexList)))
              LWPFirstVertex (car (nth 0 LWPVertexList))
              LWPFirstVertexX (nth 1 LWPFirstVertex)
              LWPFirstVertexY (nth 2 LWPFirstVertex)
              LWPFirstVertexXY (list LWPFirstVertexX LWPFirstVertexY)
            ) 

            ; Determine the sweep-direction - See AutoCAD Bulge documentation
            ; The sweep-direction will be used to determine G2 or G3
            (if (minusp LWPVertexBulge)
              (progn
                (setq fSweep 1)
              )
              (progn
                (setq fSweep 0)
              )
            )
            
            (setq
              ; Calculate the radius 
              ArcInfo (Bulge2Arc LWPVertexXY LWPFirstVertexXY LWPVertexBulge)
              ; list returned = <center> <start angle> <end angle> <radius>
              ;                  nth 0      nth 1        nth 2       nth3
              Radius (nth 3 ArcInfo)
            )

            ; FSweep not 'zero' therefor clockwise - CW
            (if (/= fsweep 0)
              (progn
                (setq
                  GCodeActionType "G02 "
                  ;GCodeString (strcat GCodeSpace GCodeActionType (rtos Radius) "," (rtos Radius) " 0 "  (itoa fArc) " " (itoa fSweep) " " (rtos LWPFirstVertexX) "," (rtos LWPFirstVertexY))
                  ;GRBL variant: G2 X_to-go-to  Y_to-go-to  Radius
                  ;GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPFirstVertexX 2 4) " Y" (rtos LWPFirstVertexY 2 4) " R" (rtos Radius 2 4))
                  ;----------------------------
                  ; Change the R-construction to IJ(K)
                  ; Always calculated from the START-XY position (i.e. current XY position)
                  LWPVertexX (nth 0 LWPVertexXY) ; current position
                  LWPVertexY (nth 1 LWPVertexXY) ; current position
                  CenterX (nth 0 (nth 0 ArcInfo))
                  CenterY (nth 1 (nth 0 ArcInfo))
                  CenterI (- CenterX LWPVertexX) ; Offset in X (relative) from startpoint
                  CenterJ (- CenterY LWPVertexY) ; Offset in Y (relative) from startpoint
                  GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPFirstVertexX 2 4) " Y" (rtos LWPFirstVertexY 2 4) " I" (rtos CenterI 2 4) " J" (rtos CenterJ 2 4))
                  GCode (cons GCodeString GCode)
                )
              )
              ; FSweep 'zero' therefor anti-clockwise - CCW
              (progn
                (setq
                  GCodeActionType "G03 "
                  ;GCodeString (strcat GCodeSpace GCodeActionType (rtos Radius) "," (rtos Radius) " 0 "  (itoa fArc) " " (itoa fSweep) " " (rtos LWPFirstVertexX) "," (rtos LWPFirstVertexY))
                  ;GRBL variant: G3 X_to-go-to  Y_to-go-to  Radius
                  ;GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPFirstVertexX 2 4 ) " Y" (rtos LWPFirstVertexY 2 4) " R" (rtos Radius 2 4))
                  ;------------------------------
                  ; Change the R-construction to IJ(K)
                  LWPVertexX (nth 0 LWPVertexXY) ; current position
                  LWPVertexY (nth 1 LWPVertexXY) ; current position
                  CenterX (nth 0 (nth 0 ArcInfo))
                  CenterY (nth 1 (nth 0 ArcInfo))
                  CenterI (- CenterX LWPVertexX) ; Offset in X (relative) from startpoint
                  CenterJ (- CenterY LWPVertexY) ; Offset in Y (relative) from startpoint
                  GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPFirstVertexX 2 4) " Y" (rtos LWPFirstVertexY 2 4) " I" (rtos CenterI 2 4) " J" (rtos CenterJ 2 4))
                  GCode (cons GCodeString GCode)
                )
              )
            )
          )
          
          ; The last vertex did not have a BulgeFactor, so the last segment is a straight line.
          ; If the LWPOLYLINE has a 'closed' indication close it. --> Close it by going back to the start-vertex
          (progn
            ;(princ "\nThe LWPOLYLINE has a 'closed' indication: ") (princ (cdr (assoc 70 GeometryInfo)))
            (setq LWPClosed (cdr (assoc 70 GeometryInfo)))
            (if (or
                  (= LWPClosed 1)
                  (= LWPClosed 129)
                )
              (progn
                ;(princ "\nClosing Polyline using a Line.")
                (setq
                  GCodeActionType "G01 "
                  LWPFirstVertex (car (nth 0 LWPVertexList))
                  LWPFirstVertexX (nth 1 LWPFirstVertex)
                  LWPFirstVertexY (nth 2 LWPFirstVertex)
                  LWPFirstVertexXY (list LWPFirstVertexX LWPFirstVertexY)
                  GCodeString (strcat GCodeSpace GCodeActionType "X" (rtos LWPFirstVertexX 2 4) " Y" (rtos LWPFirstVertexY 2 4))
                  ;GCodeString (strcat SymbolCodeString " Z")
                  GCode (cons GCodeString GCode)
                )
              )
              ; The LWPOLYLINE has no 'closed' indication. Do not close the it.
              (progn
                ;  Hmmmm something strange is happening here. --> Cause: SPLINE
                ; The polyline isn't closed but should have been. 
                ;(princ "\nThe LWPOLYLINE has no 'closed' indication: ") (princ (cdr (assoc 70 GeometryInfo)))
              )
            )
          )
        )
      )
    )
    ;(princ (strcat "\nVertexCounter: " (itoa VertexCounter) " - ")) (princ GCodeString)
   (setq VertexCounter (1+ VertexCounter))
  ); Repeat VertexList

  ;(textscr)
  ;(princ "\nResulting GCode-strings: ") (princ Gcode) (princ " - ") (princ GCodeString)
  ;(getstring)
  ;(EXIT)
  (progn GCode); return the result
)


;----------------------------------------------------
;*****             Analyze Circle               *****
;----------------------------------------------------
;** GRBL has no G-code for a fulle circle (G12 or G13)                  **
;** A circle has therefor to be in 2 halfs (i.e. ARCs of both 180 degr) --> Correction: GRBL needs to break up the circle in 4 Quadrants :-(
(defun AnalyzeCircle(GeometryElement /
                     GeometryElementInfo
                     Center CenterX CenterY
                     Radius RadiusX RadiusY
                     StartAngleQ1 StartAngleQ2 StartAngleQ3 StartAngleQ4
                     ArcStartCoordQ1 ArcEndCoordQ1
                     ArcStartCoordQ2 ArcEndCoordQ2
                     ArcStartCoordQ3 ArcEndCoordQ3
                     ArcStartCoordQ3 ArcEndCoordQ3
                     *error* )
  
  ; Error-handler - This is supposed to be INSIDE the AnalyzeCircle function. It is a localised error handler.
  (defun *error* ( msg )
    (princ "\nError in Function: AnalyzCircle")
    (princ(strcat "\n" msg))
    (princ "\nError: Closing GCode file: ") (princ GCode_File)
    (close GCode_File)
    (princ"\nReverting drawing to its start status...")
    (setvar "CMDECHO" 0)
    (command "UNDO" "Back")
    (setvar "CMDECHO" 1)
    (princ "\nGeometry-info: ") (princ GeometryElementInfo)
    (princ "\nCenter: ") (princ Center)
    (princ)
  )
  
  ;(princ (strcat "\nFound CIRCLE element " (itoa ElementCounter)))

  ; GRBL: A Circle has to consist of 2 (180 degree) arcs, the enclosed angle (360 degree) circle has to be divided in 2.
  ; CORRECTION: GRBL needs to break up the circle in 4 Quadrants :-( 
  ; A circle is alway made CCW
  (setq
    GeometryElementName (car GeometryElement)
    GeometryElementInfo (entget GeometryElementName)
    ;Centerpoint
    Center (assoc 10 GeometryElementInfo)
    CenterX (nth 1 Center)
    CenterY (nth 2 Center)
    Radius (cdr (assoc 40 GeometryElementInfo))
    RadiusX Radius
    RadiusY Radius
    StartAngleQ1 (dtr 0.0)
    StartAngleQ2 (dtr 90.0)
    StartAngleQ3 (dtr 180.0)
    StartAngleQ4 (dtr 270.0)
    ArcStartCoordQ1 (polar (list CenterX CenterY) StartAngleQ1 Radius)
    ArcEndCoordQ1 (polar (list CenterX CenterY) StartAngleQ2 Radius)
    ArcStartCoordQ2 (polar (list CenterX CenterY) StartAngleQ2 Radius)
    ArcEndCoordQ2 (polar (list CenterX CenterY) StartAngleQ3 Radius)
    ArcStartCoordQ3 (polar (list CenterX CenterY) StartAngleQ3 Radius)
    ArcEndCoordQ3 (polar (list CenterX CenterY) StartAngleQ4 Radius)
    ArcStartCoordQ4 (polar (list CenterX CenterY) StartAngleQ4 Radius)
    ArcEndCoordQ4 (polar (list CenterX CenterY) StartAngleQ1 Radius)
    ;---------------
    ; For GRBL Gcode: center of circle-arcs I and J
    ;Q1
    StartCoordQ1X (nth 0 ArcStartCoordQ1)
    StartCoordQ1Y (nth 1 ArcStartCoordQ1)
    CenterQ1I (- CenterX StartCoordQ1X)
    CenterQ1J (- CenterY StartCoordQ1Y)
    EndCoordQ1X (nth 0 ArcEndCoordQ1)
    EndCoordQ1Y (nth 1 ArcEndCoordQ1)
    ;Q2
    StartCoordQ2X EndCoordQ1X
    StartCoordQ2Y EndCoordQ1Y
    CenterQ2I (- CenterX StartCoordQ2X)
    CenterQ2J (- CenterY StartCoordQ2Y)
    EndCoordQ2X (nth 0 ArcEndCoordQ2)
    EndCoordQ2Y (nth 1 ArcEndCoordQ2)
    ;Q3
    StartCoordQ3X EndCoordQ2X
    StartCoordQ3Y EndCoordQ2Y
    CenterQ3I (- CenterX StartCoordQ3X)
    CenterQ3J (- CenterY StartCoordQ3Y)
    EndCoordQ3X (nth 0 ArcEndCoordQ3)
    EndCoordQ3Y (nth 1 ArcEndCoordQ3)
    ;Q4
    StartCoordQ4X EndCoordQ3X
    StartCoordQ4Y EndCoordQ3Y
    CenterQ4I (- CenterX StartCoordQ4X)
    CenterQ4J (- CenterY StartCoordQ4Y)
    EndCoordQ4X (nth 0 ArcEndCoordQ4)
    EndCoordQ4Y (nth 1 ArcEndCoordQ4)
   
    fArc 0
    fSweep 0
  )

 ;The centre of the circle is known: CenterX and CenterY
 ;Calculate a XY-Startpoint: CenterX + Radius = I , CenterY + Radius = Y
 (setq
    ; Always move to the start point of the shape before starting the Shape
    GCodeString (strcat "G00 X" (rtos StartCoordQ1X 2 4) " Y" (rtos StartCoordQ1Y 2 4))
    GCode (cons GCodeString GCode)
    ; Circles are drawn Anti-Clockwise (CCW): G03
    ; Circles are started directly from the CURRENT XY-position. i.e. Current XY-position is assumed to be START-position
    ;Q1
    GCodeString (strcat "G03 X" (rtos EndCoordQ1X 2 4) " Y" (rtos EndCoordQ1Y 2 4) " I" (rtos CenterQ1I 2 4) " J" (rtos CenterQ1J 2 4) " (Q1 R" (rtos Radius 2 4) ")" )
    GCode (cons GCodeString GCode)
    ;Q2
    GCodeString (strcat "G03 X" (rtos EndCoordQ2X 2 4) " Y" (rtos EndCoordQ2Y 2 4) " I" (rtos CenterQ2I 2 4) " J" (rtos CenterQ2J 2 4) " (Q2 R" (rtos Radius 2 4) ")" )
    GCode (cons GCodeString GCode)
    ;Q3
    GCodeString (strcat "G03 X" (rtos EndCoordQ3X 2 4) " Y" (rtos EndCoordQ3Y 2 4) " I" (rtos CenterQ3I 2 4) " J" (rtos CenterQ3J 2 4) " (Q3 R" (rtos Radius 2 4)  ")")
    GCode (cons GCodeString GCode)
    ;Q4    
    GCodeString (strcat "G03 X" (rtos EndCoordQ4X 2 4) " Y" (rtos EndCoordQ4Y 2 4) " I" (rtos CenterQ4I 2 4) " J" (rtos CenterQ4J 2 4) " (Q4 R" (rtos Radius 2 4)  ")")
    GCode (cons GCodeString GCode)
  )
  
  (progn Gcode) ; Return result
)


; ------------------------------------------------------------
; ----------     MAKE AND GET TEXT GEOMETRY        
; ------------------------------------------------------------
(defun MakeTextGeometry( / Geometry2AnalyzeText sf Prevlayer)
  ;(alert "About to process TEXT-geometry")
  ; Select ALL Text and put in on a seperate Layer
  ; TEXT
  ; Convert all text to POLYLINES. Not LWPolylines.
  (setq
    sf nil
    Geometry2AnalyzeText nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 0 "TEXT")
             (cons 67 0) ; Modelspace !
           (cons -4 "AND>")
         )
       )
    Geometry2AnalyzeText (ssget "X" sf)
  )

  (if Geometry2AnalyzeText
    (progn
      (setq Prevlayer (getvar "CLAYER"))
      (command "-LAYER" "M" "TEXT-TOOLPATH" "")
      (command "COPY" Geometry2AnalyzeText "" "0,0" "0,0")
      (command "_CHPROP" "P" "" "Layer" "TEXT-TOOLPATH" "") 
      (command "-LAYER" "set" PrevLayer "")
      
      (setq
        sf (append
             (list
               (cons -4 "<AND")
                 (cons 0 "TEXT")
                 (cons 8 "TEXT-TOOLPATH")
                 (cons 67 0) ; Modelspace !
               (cons -4 "AND>")
           )
         )
        Geometry2AnalyzeText (ssget "X" sf)
      )

      (if Geometry2AnalyzeText
        (progn
          ;(alert (strcat "TEXT found, total number: "  (itoa (sslength Geometry2AnalyzeText))  " - converting to POLYLINES."))
          ;(setvar "CMDECHO" 0)
          (command "_TXTEXP" Geometry2AnalyzeText "")
          ;(setvar "CMDECHO" 1)

          ; After the TXTEXP command, the text is converted into POLYLINES.
          ; This is not good enough: It must be converted to LWPolylines
          ; Remark: When using selection-set filters, 'AND' is implied
          (setq
            Geometry2AnalyzeText nil
            sf (append
                 (list
                   (cons -4 "<AND")
                     (cons 0 "POLYLINE")
                     (cons 8 "TEXT-TOOLPATH")
                     (cons 67 0) ; Modelspace !
                   (cons -4 "AND>")
                 )
               )
            Geometry2AnalyzeText (ssget "X" sf)
          )

          (if Geometry2AnalyzeText
            (progn
              ;(alert (strcat "\nTXTEP resulting POLYLINES found: " (itoa (sslength Geometry2AnalyzeText)) " - converting to LINEs."))
              (command "_EXPLODE" Geometry2AnalyzeText)
            )
            (progn
              (princ "\nNo Text resulting POLYLINES found...")
            )
          )

          ; Make ALL geometry on Layer TEXT-TOOLPATH into LWPolylines
          (MakeLWPoly "TEXT-TOOLPATH")

          (setq
            sf (append
                 (list
                   (cons -4 "<AND")
                     (cons 8 "TEXT-TOOLPATH") ; Modelspace !
                     (cons 67 0) ; Modelspace !
                   (cons -4 "AND>")
                 )
               )
            ; Example: Geometry2Analyze (ssget "X" (list (cons 8 Layer2Analyze) (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons 0 "CIRCLE") (cons -4 "OR>"))))
            Geometry2AnalyzeText (ssget "X" sf)
          )
       )
     )
    )
    (progn
      (alert "No TEXT geometry present/found.")
      ;(exit)
    )
  )

  (progn Geometry2AnalyzeText) ; return result.
)


; ------------------------------------------------------------
; ----------     MAKE AND GET INSIDE GEOMETRY        
; ------------------------------------------------------------
(defun MakeInsideGeometry( / Geometry2AnalyzeIn sf PrevLayer)
  ; Select all the geometry to convert that HAS TRANSPARENCY (i.e. INSIDE-contours of parts)
  ;(alert "About to process INSIDE-geometry")
  (setq
    sf nil
    Geometry2AnalyzeIn nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons -4 "<AND")
               (cons 67 0)    ; Modelspace !
               (cons 62 256)  ; Apparently the value (62 256) means: color-property = 'ByLayer'
             (cons -4 "AND>")
             (cons -4 "<AND")
               (cons -4 "<NOT")
                 ;(cons 440 -1) for AutoCAD or (cons 440 0) for ZWCAD
                 TransparencyByLayer ; Now a global variable. geometry that HAS Transparency --> i.e. NOT 'Transparency ByLayer'
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "DRAWING-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "OUTSIDE-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "TEXT-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 0 "TEXT")
               (cons -4 "NOT>")
             (cons -4 "AND")
           (cons -4 "AND")
         )
       )
    ; Example: Geometry2Analyze (ssget "X" (list (cons 8 Layer2Analyze) (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons 0 "CIRCLE") (cons -4 "OR>"))))
    Geometry2AnalyzeIn (ssget "X" sf)
  )
  
  ;(princ "\nGeometry2AnalzeIn: ") (princ Geometry2AnalyzeIn)
  ;(getstring "\n Hit any key...")

  (if Geometry2AnalyzeIn
    (progn
      (setq Prevlayer (getvar "CLAYER"))
      ;(alert (strcat "INSIDE-geometry found to convert:" (itoa (sslength Geometry2AnalyzeIn))))
      ; Make layer to put the geometry on
      (command "-LAYER" "M" "INSIDE-TOOLPATH" "")
      ; Copy the geometry to the same location 'onto itself'
      (command "COPY" Geometry2AnalyzeIn "" "0,0" "0,0")
      ; Change the original geometry to layer INSIDE-TOOLPATH, the copy remains on the orignal layer.
      (command "_CHPROP" Geometry2AnalyzeIn "" "Layer" "INSIDE-TOOLPATH" "") 
      (command "-LAYER" "set" PrevLayer "")

      ; MAKE geometry on layer INSIDE-TOOLPATH into LWPolylines
      ; Select all geometry on the layer INSIDE-TOOLPATH (there should be no text on this layer).
      (setq 
        sf (append
             (list
               (cons -4 "<AND")   
                 (cons 8 "INSIDE-TOOLPATH")
                 (cons 67 0) ; Modelspace !
               (cons -4 "AND>")
             )
           )
        Geometry2AnalyzeIn (ssget "X" sf)
      )
  
      (if Geometry2AnalyzeIn
        (progn
          ;(alert (strcat "Inside geometry found to convert:" (itoa (sslength Geometry2AnalyzeIn))))
          ; Make ALL geometry on Layer INSIDE-TOOLPATH into LWPolylines
          ;(alert "Going to MakeLWPOLY")
          (MakeLWPoly "INSIDE-TOOLPATH")

          ;(princ "\nGeometry2AnalyzeIn: ") (princ Geometry2AnalyzeIn)

          ;**************************
          ; NOW ALL THE ELEMENTS ON THE NEW LAYER MUST BE OFFSET TO THE INSIDE!
          ;**************************
          ; Select all changed geometry on the layer INSIDE-TOOLPATH
          (setq 
            sf (append (list (cons 8 "INSIDE-TOOLPATH")))
            Geometry2AnalyzeIn (ssget "X" sf)
          )
          (if Geometry2AnalyzeIn
            (progn 
              (setq
                Geometry2AnalyzeIn (G2AOffset Geometry2AnalyzeIn "In")
                ; Create a NEW selection-set of the NEW OFFSET-elements on the layer INSIDE-TOOLPATH 
                ; This is the final product
                sf (append
                     (list
                       (cons -4 "<AND")
                         (cons 8 "INSIDE-TOOLPATH")
                         (cons 67 0) ; Modelspace !
                       (cons -4 "AND>")
                     )
                   )
                Geometry2AnalyzeIn (ssget "X" sf)
              )
              (command "regen")
            )
          )
        )
      )
    )
    (progn
      (alert "No INSIDE geometry found.")
      ;(EXIT)
    )
  )
  (progn Geometry2AnalyzeIn) ; return the result, even if it is nil.
 )


; ------------------------------------------------------------
; -------     MAKE AND GET OUTSIDE-CONTOUR GEOMETRY    -------
; ------------------------------------------------------------
(defun MakeOutSideGeometry( / Geometry2AnalyzeOut sf PrevLayer) 
  ;(alert "About to process OUTSIDE-geometry")
  ; 2024-06-14: Copy the geometry that has NO (440 . -1 or 0) to the OUTSIDE-TOOLPATH layer.
  ;             The copies on the OUTSIDE-TOOLPATH layer will be OFFSET to the OUTSIDE using ToolWidth

  ; Select all the geometrys to convert that has NO TRANSPARENCY (i.e. OUTER-contours of parts)
  ; AND has: color-property = 'ByLayer'
  (setq
    Geometry2AnalyzeOut nil
    sf (append
         (list
           (cons -4 "<AND")   
             (cons -4 "<AND")
               (cons 67 0) ; Modelspace !
               ;(cons 440 -1) or (cons 440 0) ; Has NO transparency 'ByLayer' (value '-1' or '0' = 'ByLayer', depending on AutoCAD or ZWCAD)
               TransparencyByLayer
               (cons 62 256)       ; Apparently the value (62 256) means: color-property = 'ByLayer'
             (cons -4 "AND>")
             (cons -4 "<AND")
               (cons -4 "<NOT")
                 (cons 8 "DRAWING-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "INSIDE-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "TEXT-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 0 "TEXT")     ; geen text
               (cons -4 "NOT>")
             (cons -4 "AND>")    
           (cons -4 "AND>")
         )
       )
    
    ; Example: Geometry2Analyze (ssget "X" (list (cons 8 Layer2Analyze) (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons 0 "CIRCLE") (cons -4 "OR>"))))
    Geometry2AnalyzeOut (ssget "X" sf)
  )

  (if Geometry2AnalyzeOut
    (progn
      (setq Prevlayer (getvar "CLAYER"))
      ; Make layer to put geometry on that will be analyzed and converted to GCode
      (command "-LAYER" "M" "OUTSIDE-TOOLPATH" "")
      (command "COPY" Geometry2AnalyzeOut "" "0,0" "0,0") ; Copy the geometry to the same location 'on to itself'
      (command "_CHPROP" Geometry2AnalyzeOut "" "Layer" "OUTSIDE-TOOLPATH" "")
      (command "-LAYER" "set" PrevLayer "")

      ; MAKE geometry on layer OUTSIDE-TOOLPATH into LWPolylines
      (setq 
        sf (append
             (list
               (cons -4 "<AND")   
                 (cons 8 "OUTSIDE-TOOLPATH")
                 (cons 67 0) ; Modelspace !
               (cons -4 "AND>")
             )
           )
        Geometry2AnalyzeOut (ssget "X" sf)
      )
  
      (if Geometry2AnalyzeOut
        (progn
          ; Make ALL geometry on Layer OUTSIDE-TOOLPATH into LWPolylines
          (MakeLWPoly "OUTSIDE-TOOLPATH")

          ;(princ "\nGeometry2AnalyzeOut: ") (princ Geometry2AnalyzeOut)

          ;**************************
          ; NOW ALL THE ELEMENTS (LWPolylines) ON THE NEW LAYER MUST BE OFFSET TO THE OUTSIDE!
          ;**************************
          ; Select all changed geometry on the layer OUTSIDE-TOOLPATH
          (setq 
            sf (append (list (cons 8 "OUTSIDE-TOOLPATH")))
            Geometry2AnalyzeOut (ssget "X" sf)
          )
          (if Geometry2AnalyzeOut
            (progn 
              (setq
                Geometry2AnalyzeOut (G2AOffset Geometry2AnalyzeOut "Out")
                ; Create a NEW selection-set of the NEW OFFSET-elements on the layer OUTSIDE-TOOLPATH 
                ; This is the final product
                sf (append
                     (list
                       (cons -4 "<AND")   
                         (cons 8 "OUTSIDE-TOOLPATH")
                         (cons 67 0) ; Modelspace !
                       (cons -4 "AND>")   
                     )
                    )
                Geometry2AnalyzeOut (ssget "X" sf)
              )
              (command "regen")
            )
          )
        )
      )
    )
    (progn
      (alert "No OUTSIDE geometry present/found.")
      ;(EXIT)
    )
  )
  (progn Geometry2AnalyzeOut) ; return the result, even if it is nil.
)


; ------------------------------------------------------------
; -------     MAKE AND GET DRAWING-CONTOUR GEOMETRY    -------
; ------------------------------------------------------------
(defun MakeDrawGeometry( / Geometry2AnalyzeDraw sf PrevLayer)
  ;(alert "About to process DRAWING-geometry")
  ; 2024-06-14: Copy the geometry that has NO (440 . -1 or 0) and has 'COLOR' assigned to it
  ;             to the DRAWING-TOOLPATH layer.

  ; Select all the geometry to convert that has:
  ;  NO TRANSPARENCY 
  ;  AND has COLOR assigned to it (i.e. when color-propery IS NOT 'ByLayer')
  (setq
    Geometry2AnalyzeDraw nil
    sf (append
         (list
           (cons -4 "<AND")
               (cons 67 0) ; Modelspace !
               ;(cons 440 -1) or (cons 440 0) ; Has NO transparency 'ByLayer' (value '-1' or '0' = 'ByLayer', depending on AutoCAD or ZWCAD)
               TransparencyByLayer
             (cons -4 "AND>")
             (cons -4 "<AND")
               (cons -4 "<NOT")
                 (cons 8 "OUTSIDE-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "INSIDE-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "TEXT-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 8 "DRAWING-TOOLPATH")
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 (cons 0 "TEXT")     ; geen text
               (cons -4 "NOT>")
               (cons -4 "<NOT")
                 ; And has color assigned to it! It therefor should not have a DXF groupcode 62
                 (cons 62 256) ; apparently color 256 means: color-property = "ByLayer'
               (cons -4 "NOT>")
             (cons -4 "AND>")    
           (cons -4 "AND>")
         )
       )
    
    ; Example: Geometry2Analyze (ssget "X" (list (cons 8 Layer2Analyze) (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons 0 "CIRCLE") (cons -4 "OR>"))))
    Geometry2AnalyzeDraw (ssget "X" sf)
  )

  (if Geometry2AnalyzeDraw
    (progn
      ; Make layer to put geometry on that will be analyzed and converted to GCode
      (setq Prevlayer (getvar "CLAYER"))
      (command "-LAYER" "M" "DRAWING-TOOLPATH" "")
      (command "COPY" Geometry2AnalyzeDraw "" "0,0" "0,0") ; Copy the geometry to the same location 'on to itself'
      (command "_CHPROP" Geometry2AnalyzeDraw "" "Layer" "DRAWING-TOOLPATH" "")
      (command "-LAYER" "set" PrevLayer "")
      
      ; MAKE geometry on layer OUTSIDE-TOOLPATH into LWPolylines
      (setq 
        sf (append
             (list
               (cons -4 "<AND")   
                 (cons 8 "DRAWING-TOOLPATH")
                 (cons 67 0) ; Modelspace !
               (cons -4 "AND>")
             )
           )
        Geometry2AnalyzeDraw (ssget "X" sf)
      )
  
      (if Geometry2AnalyzeDraw
        (progn
          ; Make ALL geometry on Layer DRAWING-TOOLPATH into LWPolylines
          (MakeLWPoly "DRAWING-TOOLPATH")

          ;(princ "\nGeometry2AnalyzeDraw: ") (princ Geometry2AnalyzeDraw)

          ;**************************
          ; DRAWING Geometry does not have to be offset :-) !
          ;**************************
          ; Select all changed geometry on the layer DRAWING-TOOLPATH
          (setq 
            sf (append
                 (list
                   (cons -4 "<AND")   
                     (cons 8 "DRAWING-TOOLPATH")
                     (cons 67 0) ; Modelspace !
                   (cons -4 "AND>")
                 )
               )
            Geometry2AnalyzeDraw (ssget "X" sf)
          )
        )
      )
    )
    (progn
      (alert "No DRAWING geometry present/found.")
      ;(EXIT)
    )
  )
  (progn Geometry2AnalyzeDraw) ; return the result, even if it is nil.
)
 

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; **********************************************
; *****      CLEAN AND PREP GEOMETRY       *****
; **********************************************
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun CleanAndPrepDWG( / FuzzFactor sf SelectionSet SelectionSetCounter SelectionSetCounterDel enDist enX enY en Answer *error*) 
  ;Set an UNDO-MARK
  ;(command "undo" "mark")
  ; First of all make everything into polylines!
  ; Explode it all first though...
  ; This should order the lines into coordinate-contour-sequences !

  ; Error function. It must be inside the current defun!
  (defun *error*( msg / )
    (princ "\nError in Function: CleanAndPrepDWG")
    (princ(strcat "\n" msg))
    (setvar "CMDECHO" 1)
    (setvar "CMDDIA" 1)
  )

 ; New check 2025-02-21: Check if there is any geometry at all!
  (setq
    SelectionSet nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons -4 "<AND")
               (cons 67 0) ; Modelspace !
             (cons -4 "AND>")
           (cons -4 "AND>")
         )
       )
    SelectionSet (ssget "X" sf)
  )

  (if SelectionSet
    (progn
      (princ "\n\nStart: Prepare and clean-up the drawing...")
      (setvar "CMDECHO" 0)
      (setq InitTrue 1)
      (command "-layer" "On" "*" "Thaw" "*" "Unlock" "*" "")

      ; Use the native AutoCAD/ZWCAD OVERKILL command, to delete Double-geometry
      (princ "\n")
      (initget 1 "Yes No Y N")
      (setq Answer (getkword "Clean the drawing? (Yes or No) "))
      (if (or (= (strcase Answer) "Yes") (= (strcase Answer) "Y"))
        (progn
          (setq FuzzFactor 0.005)
          (command "_-OVERKILL" "ALL" "" "Ignore" "ALL" "Tolerance" Fuzzfactor "Partial" "yes" "End" "yes" "P" "no" "Associative" "no" "")
          (alert  "Completed: Cleaning up the drawing. \nPlease check the result!...")
        )
      )

      (setvar "CMDECHO" 1)

      ; New check 2024-06-10: System to determine the Outline of parts is introduced.
      ;                       Check if any geometry has an 'inside' indication
      ;                       Transparency is used to for indicate 'inside-contour'.
      (setq 
          sf (append
              (list
                (cons -4 "<AND")
                  (cons -4 "<AND")
                    (cons 67 0) ; Modelspace !
                  (cons -4 "AND>")
                  (cons -4 "<AND")
                    (cons -4 "<NOT")
                      ; (cons 440 -1) or (cons 440 0) ; Has transparency: Value '-1' or '0' depending on AutoCAD of ZWCAD = 'ByLayer', --> So... 'any other value than '-1' or '0'
                      TransparencyByLayer
                    (cons -4 "<NOT")
                  (cons -4 "AND>")
                (cons -4 "AND>")
              )
            )
          SelectionSet (ssget "X" sf)
      )
      
      ;(princ "\nTest for Inside geometry...")
      (if (not SelectionSet)
        (progn
          (alert "\nNone of the drawn parts have holes!\nIf you have/want holes then please use command: INSIDECONTOUR")
        )
        (progn
          ;(alert "\nYes: Inside geometry present.")
        )
      )

      ; Clean-up: ERASE LINEs equal in length or shorter than FuzzFactor
      (setq
        sf nil
        SelectionSet nil
        sf (append
            (list
              (cons -4 "<AND")
                (cons 0 "LINE")
                (cons 67 0) ; Modelspace !
              (cons -4 "AND>")
            )
          )
        ;Geometry2Analyze (ssget "X" (list (cons 8 Layer2Analyze) (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons 0 "CIRCLE") (cons -4 "OR>"))))
        SelectionSet (ssget "X" sf)
      )

      (if SelectionSet
        (progn
          (setq
            SelectionSetCounter 0
            SelectionSetCounterDel 0
          )
          (repeat (sslength SelectionSet)
            (setq
              en (entget (ssname SelectionSet SelectionSetCounter))
              enX (cdr (assoc 10 en))
              enY (cdr (assoc 11 en))
              enDist (distance enX enY)
              SelectionSetCounter (1+ SelectionSetCounter)
            )
            (if (<= enDist FuzzFactor)
              (progn
                ;(setvar "CMDECHO" 0)
                  (command "erase" en)
                ;(setvar "CMDECHO" 1)
                (setq SelectionSetCounterDel (1+ SelectionSetCounterDel))
              )
            )
          )
          (princ "\nNumber of lines to examine for possible erasure: ")  (princ (sslength SelectionSet))
        )
      )
      
      (if (> SelectionSetCounterDel 0)
        (progn
          (princ "\nNumber of erased lines: ") (princ SelectionSetCounterDel)
        )
        (progn
          (princ "\nNothing to erase...\n")
        )
      )
      
      (command "regen")
      ;(princ  "\nCompleted: Prepare and clean-up the drawing...")
      ;(alert "Finished: converting Splines."
      (princ "\nPreperation finished: converted all geometry to Polylines.")
    )

    (progn
      (alert "Error: No geometry to analyze. Nothing to do...")
    )
  )
  (princ)
)


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; -------     Get the geometries        --------
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 2024-07-17: The conversion of the geometry is now done in the GCSAVE function
; If there is any geometry to convert, it is now stored in global variables:
; Geometry2AnalyzeOut, Geometry2AnalyzeIn, Geometry2AnalyzeText, Geometry2AnalyzeDraw
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; OUTSIDE
;(defun GetOutsideGeometry ( / Geometry2AnalyzeOut)
(defun c:GetOutsideGeometry ( / sf )
  (if (not InitTrue)
    (progn
      (alert "\nPlease use the Initialize and Clean function before proceding")
    )
  )
  
  (princ "\nStart: Generating OUTSIDE geometry, please wait...")


  ; ~~ GETTING the OUTSIDE geometry ~~~
  ; Clean Up: As there could be old geometry on the OUTSIDE-toolpath-layer, lets clean that up first
  (setq
    sf nil
    Geometry2AnalyzeOut nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "OUTSIDE-TOOLPATH")
           (cons -4 "AND>")
       )
     )
    Geometry2AnalyzeOut (ssget "X" sf)
  )
  
  (if Geometry2AnalyzeOut
    (progn
      (command "erase" Geometry2AnalyzeOut "")
    )
  )

  (setq
    Geometry2AnalyzeOut nil
    Geometry2AnalyzeOut (MakeOutsideGeometry)
  )

  ;(alert (itoa (sslength Geometry2AnalyzeOut)))
  ;(progn Geometry2AnalyzeOut) ; return result
  (princ "\nFinished: Generating OUTSIDE geometry.")
  (princ)
)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; INSIDE
;(defun GetInsideGeometry ( / GeometryCounter Geometry2AnalyzeIn)
(defun c:GetInsideGeometry (/ sf )
  ; ~~ GETTING the geometry ~~~
  (if (not InitTrue)
    (progn
      (alert "\nPlease use the Initialize and Clean function before proceding")
    )
  )

  (princ "\nStart: Generating INSIDE geometry,please wait...")

  ; Clean Up: As there could be old geometry on the INSIDE-toolpath layer, lets clean that up first
  (setq
    sf nil
    Geometry2AnalyzeIn nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "INSIDE-TOOLPATH")
           (cons -4 "AND>")
       )
     )
    Geometry2AnalyzeIn (ssget "X" sf)
  )
  
  (if Geometry2AnalyzeIn
    (progn
      (command "erase" Geometry2AnalyzeIn "")
    )
  )
 
  (setq
    Geometry2AnalyzeIn nil
    Geometry2AnalyzeIn (MakeInsideGeometry)
  )
  ;(alert "Returned from making INSIDE geometry")
  ;(alert (itoa (sslength Geometry2AnalyzeIn)))
  ;(progn Geometry2AnalyzeIn) ; return result
  (princ "\nFinished: Generating INSIDE geometry.")
  (princ)
)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; TEXT
;(defun GetTextGeometry ( / GeometryCounter Geometry2AnalyzeIn Geometry2AnalyzeOut Geometry2AnalyzeText)
(defun c:GetTextGeometry ( / sf)
  ; ~~ GETTING the geometry ~~~
  (if (not InitTrue)
    (progn
      (alert "\nPlease use the Initialize and Clean function before proceding")
    )
  )

  (princ "\nStart: Generating TEXT geometry, please wait...")

  ; Clean Up: As there could be old geometry on the text-toolpath layer, lets clean that up first
  (setq
    sf nil
    Geometry2AnalyzeText nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "TEXT-TOOLPATH")
           (cons -4 "AND>")
         )
       )
    Geometry2AnalyzeText (ssget "X" sf)
  )
  
  (if Geometry2AnalyzeText
    (progn
      (command "erase" Geometry2AnalyzeText "")
    )
  )

  (setq
    Geometry2AnalyzeText nil
    Geometry2AnalyzeText (MakeTextGeometry)
  )
  ;(alert (itoa (sslength Geometry2AnalyzeText)))
  ;(progn Geometry2AnalyzeText) ; return result
  (princ "\nFinished: Generating TEXT geometry...")
  (princ)
)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; DRAWING
;(defun GetDrawGeometry ( / GeometryCounter Geometry2AnalyzeIn Geometry2AnalyzeOut Geometry2AnalyzeText)
(defun c:GetDrawGeometry ( / sf)
  ; ~~ GETTING the geometry ~~~
  (if (not InitTrue)
    (progn
      (alert "\nPlease use the Initialize and Clean function before proceding")
    )
  )

  (princ "\nStart: Generating DRAW geometry, please wait...")

  ; Clean Up: As there could be old geometry on the DRAWING-toolpath layer, lets clean that up first
  (setq
    sf nil
    Geometry2AnalyzeDraw nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "DRAWING-TOOLPATH")
           (cons -4 "AND>")
       )
     )
    Geometry2AnalyzeDraw (ssget "X" sf)
  )
  
  (if Geometry2AnalyzeDraw
    (progn
      (command "erase" Geometry2AnalyzeDraw "")
    )
  )

  (setq
    Geometry2AnalyzeDraw nil
    Geometry2AnalyzeDraw (MakeDrawGeometry)
  )
  ;(alert (itoa (sslength Geometry2AnalyzeDraw)))
  ;(progn Geometry2AnalyzeDraw) ; return result
  (princ "\nFinished: Generating DRAW geometry.")
  (princ)
)

;*******************************************************************
;* 2024-06-17: Seperated the Make LWPOLylines as a Function
;* This makes it possible to call it with a 'Layer' parameter
;* Hoping to further examine the incorrect PEDIT-Multi-Joining
;*******************************************************************
(defun MakeLWPoly ( LayerName / SelectionSet sf SplineConvertPrecision SelectionSetCounter SelectionSetNumberO)
  ; First: EXPLODE ALL Polylines and LWPolylines
  (setq
    sf nil
    SelectionSet nil
    sf (append
         (list
           ;(cons -4 "<AND") 
             (cons -4 "<AND") 
               (cons 8 LayerName)
               (cons 67 0) ; Modelspace !
             (cons -4 "AND>")
             ;(cons -4 "<AND") 
             (cons -4 "<OR")
               (cons 0 "POLYLINE")
               (cons 0 "LWPOLYLINE")
             (cons -4 "OR>") 
             ;(cons -4 "AND>")
           ;(cons -4 "AND>") 
         )
       )
    ;Geometry2Analyze (ssget "X" (list (cons 8 Layer2Analyze) (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons 0 "CIRCLE") (cons -4 "OR>"))))
    SelectionSet (ssget "X" sf)
  )
  
  ; This is in a repeat-loop because there could be 'nested' geometry
  (if SelectionSet
    (progn
      ;(alert "Polyline geometry found: EXPLODE-ing")
      (setq SelectionSetCounter 0)
      (repeat (sslength SelectionSet)
        (command "_EXPLODE" (ssname SelectionSet SelectionSetCounter))
        (setq SelectionSetCounter (1+ SelectionSetCounter))
      )
      (command "regen")
    )
    (progn
      ;(alert "No PolyLines of LWPOlylines found to explode.")
    )
  )
  

  ; Re-create the selection-set after the EXPLODE of the same Layer but SPLINEs only.
  ; -----
  ; SPLINEs: Should be converted into LWPOLYLINEs with the PEDIT command
  (setq
    sf nil
    SelectionSet nil
    sf (append
         (list
           (cons -4 "<AND")   
             (cons 8 LayerName)
           ;(cons -4 "AND>") 
           ;(cons -4 "<AND")
              (cons 0 "SPLINE")
           ; (cons -4 "AND>")
           ;  (cons -4 "<AND") 
             (cons 67 0) ; Modelspace !
           (cons -4 "AND>") 
         )
       )
    SelectionSet (ssget "X" sf)
  )
  
  (if SelectionSet
    (progn
      ;(alert (strcat "SPLINEs found - about to convert: " (itoa (sslength SelectionSet))))
      (princ (strcat "Nr. of  SPLINE(s) to convert to POLYLINES(s): " (itoa (sslength SelectionSet)))) (princ "\n")
      (setq
        SplineConvertPrecision 99
        SelectionSetCounter 0
        SelectionSetNumberOf (sslength SelectionSet)
      )
      (setvar "CMDECHO" 0)
        (setvar "PLINECONVERTMODE" 1) ; convert using ARC's instead of straight-lines
        ;(setvar "PLINECONVERTMODE" 1)  ; convert using straight-lines

        (if (getvar "trusteddomains") ; AutoCAD systemvariable
          (progn
            ;AutoCAD: tested on version 2024
            (repeat SelectionSetNumberOf
              (setq en (ssname SelectionSet SelectionSetCounter))
              (command "_PEDIT" en "Y" SplineConvertPrecision "")
              (setq SelectionSetCounter (1+ SelectionSetCounter))
              (princ "\n") (princ (itoa SelectionSetCounter))
            )
            ;(setvar "CMDECHO" 1)
          )
          (progn
            ;ZWCAD: tested on version 2022
            (repeat SelectionSetNumberOf
              (setq en (ssname SelectionSet SelectionSetCounter))
              (command "_PEDIT" en SplineConvertPrecision "")
              (setq SelectionSetCounter (1+ SelectionSetCounter))
              (princ "\n") (princ (itoa SelectionSetCounter))
            )
          )
        )

      (setvar "CMDECHO" 1)
      (princ "\nCompleted: SPLINE conversion.\n")
    )
    (progn
      ;(alert "No SPLINEs found to convert.")
    )
  )

  ; Additional check for rogue 1-segment splines
  (setq
    sf nil
    SelectionSet nil
    sf (append
         (list
           (cons -4 "<AND")   
             (cons 8 LayerName)
           ;(cons -4 "AND>") 
           ;(cons -4 "<AND")
              (cons 0 "SPLINE")
           ; (cons -4 "AND>")
           ;  (cons -4 "<AND") 
             (cons 67 0) ; Modelspace !
           (cons -4 "AND>") 
         )
       )
    SelectionSet (ssget "X" sf)
  )
  
  (if SelectionSet
    (progn
      (setq
        SelectionSetCounter 0
        SelectionSetNumberOf (sslength SelectionSet)
      )
      (alert (strcat "There are still remaining SPLINE-segments: " (itoa SelectionSetNumberOf)))
      ;(repeat SelectionSetNumberOf
        ;(setq en (ssname SelectionSet SelectionSetCounter))
        ;(princ "\n") (princ en)
        ;(command "zoom" "Object" en "")
        (command "zoom" "Object" SelectionSet "")
        ;(getstring "\nHit any key for next SPLINE...")
        ;(setq SelectionSetCounter (1+ SelectionSetCounter))
      ;)
      (exit)
    )
  )

  ; MAKE ALL geometry on the given layer into LWPolylines - but only if it is not a CIRCLE and is not TEXT!
  ; There should be not TEXT on this layer at all!
  ; !!! AND Not ALREADY a POLYLINE !!! :-)
  (princ "\nLayerName: ") (princ LayerName)
  (setq
    sf nil
    SelectionSet nil
    sf (append
         (list
           ;(cons -4 "<AND")   
             (cons -4 "<AND")   
               (cons 8 LayerName)
               (cons 67 0) ; Modelspace !
             (cons -4 "AND>")
             (cons -4 "<NOT")
               (cons 0 "CIRCLE")
             (cons -4 "NOT>")
             (cons -4 "<NOT")  
               (cons 0 "TEXT")
             (cons -4 "NOT>")
            ;(cons -4 "AND>")
         )
       )
    SelectionSet (ssget "X" sf)
  )
  
  ; Convert everything to LWPOLYLINES, exceptions: CIRCLE, TEXT
  (if SelectionSet
    (progn 
      ;(alert (strcat "About to convert all geometry to LWPolylines: " (itoa (sslength SelectionSet))))
      ; AutoCAD or ZWCAD?: They have different options with the PEDT-Multiple command.
      (if (getvar "trusteddomains") ; AutoCAD systemvariable
        (progn
          ;AutoCAD: tested on version 2024
          ;(setvar "CMDECHO" 0)
            (command "_PEDIT" "Multiple" SelectionSet "" "Y" "Join" "0.001" "")
          ;(setvar "CMDECHO" 1)
        )
        (progn
          ;ZWCAD: tested on version 2022
          ;(setvar "CMDECHO" 0)
            (command "_PEDIT" "Multiple" SelectionSet "" "J" "0.001" "")
          ;(setvar "CMDECHO" 1)
        )
      )
      (command "regen")
    )
    (progn
      ;(alert (strcat "All geometry is already of type LWPolyline\n or only of type CIRCLE or TEXT.\nLayer: " LayerName))
      ;(EXIT)
    )
  )

)


;*******************************************************************
;**********************  Write the GCode File **********************
;*******************************************************************
;-------------------------------------------------------------------
;*********** Write a HEADER to the GCode-file ******************
;(defun WriteHeaderOfGCode(GCode_File / LineNum GCounter TSpeed G00Passed Multipass NrOfPass)
(defun WriteGCode(GCode_File / LineNum GCounter TFSpeed G00Passed Multipass NrOfPass
                               GCodeAction TLaserPowerCut TFeedSpeedCut TLaserPowerWri
                               TFeedSpeedWri CurTime PassCounter
                               MaterialThickness ZHeightStep
                               GCodeElement
                               *error*)


  ; Error-handler - This is supposed to be INSIDE the WriteHeaderOfGCode function. It is a localised error handler.
  (defun *error* (msg / )
    ;(princ "\nError in Function: WriteHeaderOfGCode")
    (princ "\nError in Function: WriteGCode")
    (princ (strcat "\n" msg))
    (princ "\nError: Closing GCode file: ") (princ GCode_File)
    (close GCode_File)
    (princ"\nReverting drawing to the initial state...")
    (setvar "CMDECHO" 0)
    (command "UNDO" "Back")
    (setvar "CMDECHO" 1)
    ;(princ "\nGCodeWri: ") (princ GCodeWri)
    ;(princ "\nGCodeCut: ") (princ GCodeCut)
    (princ "\nGCounter and GCodeWri: ") (princ GCounter) (princ " - ") (princ (nth GCounter GCodeWri))
    (princ "\nGCounter and GCodeCut: ") (princ GCounter) (princ " - ") (princ (nth GCounter GCodeCut))
    (princ)
  )

  (setq
    GCounter 0
    ;GCodeReversed(reverse GCode)
    ;GCode(reverse GCode)
    TSpeed FeedSpeed
    G00Passed 0 ; Flag to know if previous line was a G00-line
  )
  (if (assoc "PASS" lldeflist)
    (progn
      (setq
        Multipass (nth 2 (assoc "PASS" lldeflist))
        MultiPass (substr Multipass 2) ;number of passes, needed for thicker materials
      )
    )
    (progn
      (setq MultiPass "1")
    )
  )

  ;----------------------------------------------------------------------------
  ; This is where it is attempted to add the multi-pass options and functions
  ;----------------------------------------------------------------------------

  ;(EXIT)
 
  ;(alert "Writing GCODE to file...")
  (setq CurTime (UpdateTime))
  (princ "\nStart: Writing GCODE-Header to file, please wait... ") (princ CurTime) 
  (setq LineNum 0)
  ;(write-line (strcat "N" (itoa LineNum) " (filename: " (vl-filename-base GCode_FileName) ")" ) GCode_File)
  (write-line (strcat "N" (itoa LineNum) " (" (vl-filename-base GCode_FileName) ");" ) GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " (" (substr (rtos (getvar "cdate")) 1 4) "-" (substr (rtos (getvar "cdate")) 5 2) "-" (substr (rtos (getvar "cdate")) 7 2) " " (substr (rtos (getvar "cdate")) 10 2) ":" (substr (rtos (getvar "cdate")) 12 2) ");" )   GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " (** Header **);") GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " G21 (All units in mm);" ) GCode_File)
  (setq LineNum (+ LineNum 5))
  ;(write-line (strcat "N" (itoa LineNum) " G91 (Realitve mode)" ) GCode_File)
  (write-line (strcat "N" (itoa LineNum) " G90 (Absolute mode);" ) GCode_File)
  ;(setq LineNum (+ LineNum 5))
  ; (write-line (strcat "N" (itoa LineNum) " G28 Z0 (Home the Z-axis)" ) GCode_File) --> This DOESN'T work. It only makes the Z go to it's Zero-position (same as G0 Z0)
  ; ----------------------------------------------------------------------------
  ; Homing the Z-axis is possible in GRBL, but you MUST ommit the Line-number!
  (write-line "$H Z" GCode_File)
  ;After Homing the Z-axis, make sure it is accepted as the Z-zero position
  ; ----------------------------------------------------------------------------
  (setq LineNum (+ LineNum 5))
  ; Do not omit the X0 Y0 values with G92
  (write-line (strcat "N" (itoa LineNum) " G92 X0 Y0 Z0;") GCode_File) ; Set current position (after homing) as Z-zero position.
  (setq LineNum (+ LineNum 5))
  ;(setq LineNum (+ LineNum 5))
  ;(write-line (strcat "N" (itoa LineNum) " M3 (Arms the laser with no regard to cornering acceleration (but stays off until a G1/2/3 move is received)") GCode_File)
  (write-line (strcat "N" (itoa LineNum) " M4 (Scales laser power according to acceleration, preventing overburn in corners. Arc-ed corners are always better);" ) GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " (** Header **);") GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " (--- Start of GCode ---);") GCode_File)
  ; Write the GCode for the Z-Axis of the Neje 4 MAX
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " G00 Z" LaserFocalHeight " F200;") GCode_File)


  (setq CurTime (UpdateTime))
  (princ "\nWriting GCODE-Body to file, please wait... ")
  (princ "\nStart: Writing the Write/Draw GCode: ") (princ CurTime)
  ; Write the WRITE-Gcode to file
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " (--- Start of WRITE - DRAW section ---);") GCode_File)


  ;-------------------------------------------------------------------------------
  ; Writing TEXT/DRAW GCodes to file
  ; Remember: WRIting and DRAwing are the same (feedspeed and power are the same)
  ;-------------------------------------------------------------------------------
  ;(alert (strcat "Length Gcode cut: " (atoi (length GcodeWri))))
  (if (> (length GCodeWri) 1) ; Don't write anything if there is no data
    (progn
      (setq
        GCounter 1 ; Do not start at '0' - i.e. Skip the first "WRI"
        LenGCodeWri (length GCodeWri)
      )

      (while (< GCounter LenGCodeWri)
        ; There are multiple 'flags' in the data, due to Drawing and Writing added into the var GCodeWRI (list)
        (if (= (strcase (substr (nth GCounter GCodeWri) 1 1)) "W")
          (progn (setq GCounter (1+ GCounter)))
        )


        ; Directly write the G00 code and move to the next line
        (if (= (substr (nth GCounter GCodeWri) 1 3) "G00")
          (progn
            (setq
              TSpeed FeedSpeed
              FeedSpeed "F2000"
              G00Passed 1
            )
            ;----------------------------
            ;(princ "\nG00 passed: ") (princ " ") (princ FeedSpeed)
            ;(princ (strcat "\nN" (itoa LineNum) " " (nth GCounter GCodeWri) " " FeedSpeed ";"))
            (write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeWri) " " FeedSpeed ";") GCode_File)
            ; Restore the original values and move to the next line
            (setq
              LaserPower LaserPowerWrite
              FeedSpeed FeedSpeedWrite
              GCounter (1+ GCounter) ; move the next line
              LineNum (+ LineNum 5)
            )
          )
          (progn
            (setq
              LaserPower LaserPowerWrite
              FeedSpeed FeedSpeedWrite
              G00Passed 0
            )
          )
        )

        ; Laserpower and feedspeed should be set.
        (if (= G00Passed 1)
          (progn
            ;(textscr)
            ;(princ "\nG00 passed: ")  (princ LaserPower) (princ " ") (princ FeedSpeed)
            ;(princ (strcat "\nN" (itoa LineNum) " " (nth GCounter GCodeWri) " " LaserPower " " FeedSpeed ";"))
            (write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeWri) " " LaserPower " " FeedSpeed ";") GCode_File)
            (setq
              LineNum (+ LineNum 5)
              G00Passed 0
            )
          )
          (progn
            ;(princ (strcat "\nN" (itoa LineNum) " " (nth GCounter GCodeWri) ";"))
            (write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeWri) ";") GCode_File)
            (setq LineNum (+ LineNum 5))
          )

        )

        (setq GCounter (1+ GCounter))
      )

      ;(setq LineNum (+ LineNum 5))
      (write-line (strcat "N" (itoa LineNum) " (--- End of WRITE - DRAW section ---);") GCode_File)
      
      (setq CurTime (UpdateTime))
      ;(alert "Finished writing WRIting GCode")
      (princ "\nFinished: Writing the Write/Draw GCode: ") (princ CurTime)
    )
  )

  ;--------------------------------------------------------------------
  ; Write the CUT-GCodes to file
  ; Repeat the number of passes with speed, depth and power adjustment
  ;--------------------------------------------------------------------
  ;(alert (strcat "Length Gcode cut: " (atoi (length GcodeCut))))
  (if (> (length GCodeCut) 1) ;Don't write anything if there is no data
    (progn
      (setq CurTime (UpdateTime))
      (princ "\nStart: Writing the CUT GCode: ") (princ CurTime)
      (setq LineNum (+ LineNum 5))
      (write-line (strcat "N" (itoa LineNum) " (--- Start of CUT section ---);") GCode_File)

      (setq
        GCounter 1 ; Do not start at 0, skip the first "CUT"
        NrOfPass (atoi Multipass)
        PassCounter 0
        ;LaserFocalHeight IS A STRING!
        ZHeight (atof LaserFocalHeight)
        MaterialThickness (abs (- -45.0 ZHeight)) ; -45.0 is the lowest Z-postion of the NEJE MAX 4 Pro. (Focalpoint is on the 'bed' in this postion)
        ZHeightStep (/ MaterialThickness (+ NrOfPass 0.0)) ; Make it a 'real or float '-number, ZHeightStep is probably fractional
      )

      (if (< NrOfPass 1) (setq NrOfPass 1)) ; prevent a 'zero'-repeat
      (princ "\nNumber of passes: ") (princ NrOfPass)
      (princ "\nLaserFocalHeight: ") (princ LaserFocalHeight) 

      (repeat NrOfPass
        ;Calculate the new feedspeed, Z-height and power for more then 1 pass
        (if (> NrOfPass 1)
          (progn
            (setq
              ; LaserPower IS A STRING!
              TLaserPowerCut (atoi (substr LaserPowerCut 2))
              ;LaserPower (fix (- TLaserPowerCut (/ TLaserPowerCut NrOfPass))) ; FIX rounds down to the lower integer
              ;Determined that is easier to not change the speed and only change the power 
              LaserPower (fix (* TLaserPowerCut 0.80)) ; FIX rounds down to the lower integer
              LaserPower (strcat "S" (itoa LaserPower))
              ; FeedSpeed IS A STRING!
              TFeedSpeedCut (atoi (substr FeedSpeedCut 2))
              ;FeedSpeed (fix (* TFeedSpeedCut NrOfPass))
              FeedSpeed TFeedSpeedCut ; determined that is easier to not change the speed and only change the power 
              FeedSpeed (strcat "F" (itoa FeedSpeed))
              PassCounter (1+ PassCounter)
            )
            (if (> PassCounter 1)
              (progn
                (setq ZHeight (- Zheight ZHeightStep)) 
                (princ "\nZHeightStep: ") (princ ZHeightStep) (princ " - ZHeight: ") (princ ZHeight)
                ;Write the Z-height to the file on a separate line
                (write-line (strcat "N" (itoa LineNum) " " "G00 Z" (rtos ZHeight) ";") GCode_File) ; convert to string while writing.
                (setq LineNum (+ LineNum 5))
              )
            )
          )
          (progn
            (setq
              LaserPower LaserPowerCut
              FeedSpeed FeedSpeedCut
            )
          )
        )

        ;(princ "\nFile-handle : ") (princ GCode_File)
        (while (< GCounter (length GCodeCut))
          ; There are multiple 'flags' in the data, due to Inside and Outside added into the var GCodeCUT (list)
          (if (= (strcase (substr (nth GCounter GCodeCut) 1 1)) "C")
            (progn (setq GCounter (1+ GCounter)))
          )

          (if (= (substr (nth GCounter GCodeCut) 1 3) "G00")
            (progn
              (setq
                ;TFSpeed FeedSpeed
                ;FeedSpeed "F2000"
                G00Passed 1
              )
              ; Directly write the G00 code and move to the next line
              ;----------------------------
              ;(princ "\nG00 passed: ") (princ " ") (princ FeedSpeed)
              ;(princ (strcat "\nN" (itoa LineNum) " " (nth GCounter GCodeCut) " " FeedSpeed ";"))
              ;(write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeCut) " " FeedSpeed ";") GCode_File)
              (write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeCut) " F2000;") GCode_File)
              ; restore the original values and move to the next line
              (setq
                ;LaserPower LaserPowerCut
                ;FeedSpeed TFSpeed
                GCounter (1+ GCounter) ; move the next Gcode in the Cutlist
                LineNum (+ LineNum 5)
              )
            )
            ; (progn
            ;   ; restore the original values 
            ;   (setq
            ;     LaserPower LaserPowerCut
            ;     FeedSpeed FeedSpeedCut
            ;     G00Passed 0
            ;   )
            ; )
          )
      
          ; This is to prevent the feedspeed being written on every line. Save processing power (speed) and total code-length
          (if (= G00Passed 1)
            (progn
              ;(princ "\nG00 passed: ")  (princ LaserPower) (princ " ") (princ FeedSpeed)
              ;(princ (strcat "\nN" (itoa LineNum) " " (nth GCounter GCodeCut) " " LaserPower " " FeedSpeed ";"))
              (write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeCut) " " LaserPower " " FeedSpeed ";") GCode_File)
              (setq 
                G00Passed 0
                LineNum (+ LineNum 5)
              )
            ) 
            (progn
              ;(princ (strcat "\nN" (itoa LineNum) " " (nth GCounter GCodeCut) ";"))
              (write-line (strcat "N" (itoa LineNum) " " (nth GCounter GCodeCut) ";") GCode_File)
              (setq 
                LineNum (+ LineNum 5)
              )
            )
          )
          (setq GCounter (1+ GCounter))
        )

        (setq GCounter 1) ; reset the counter for the next pass!!
      ); End of Repeat
      
      ;(setq LineNum (+ LineNum 5))
      (write-line (strcat "N" (itoa LineNum) " (--- End of CUT section ---);") GCode_File)

      (setq CurTime (UpdateTime))
      (princ "\nFinished: Writing the CUT GCode: ") (princ CurTime)
    )
  )

  (princ "\nFinished: Writing GCODE-Body to file.")

  ;*********** Write the FOOTER of the GCode file ******************
  (setq CurTime (UpdateTime))
  (princ "\nStart: Writing the Footer GCode: ") (princ CurTime)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " " "G40 (No tool correction);") GCode_File)
  (setq LineNum (+ LineNum 5))
  ;(write-line (strcat "N" (itoa LineNum) " " "G00 (Switch the laser OFF)") GCode_File)
  (write-line (strcat "N" (itoa LineNum) " " "M5 S0 (Switch the laser OFF);") GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " G00 Z" LaserReturnHeigt " F200;") GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " G00 X0 Y0 F1000;") GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " M30 (Reset all values and ready program for re-start);") GCode_File)
  (setq LineNum (+ LineNum 5))
  (write-line (strcat "N" (itoa LineNum) " " "(--- End of GCode ---);") GCode_File)

  (setq CurTime (UpdateTime))
  (princ "\nFinished: Writing the Footer GCode: ") (princ CurTime)
)


;*****************************************************************
;*****************************************************************
;***************** Support Library Functions *********************
;*****************************************************************
;*****************************************************************
;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes
;; the position, starting width, ending width and bulge of the
;; vertex of a supplied LWPolyline
(defun MakeListOfVertices(e)
  (if (setq e (member (assoc 10 e) e))
    (cons
      (list
        (assoc 10 e)
        ;(assoc 40 e)
        ;(assoc 41 e)
        (assoc 42 e)
      )
      (MakeListOfVertices (cdr e))
    )
  )
)

;; Bulge to Arc  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns: (<center> <start angle> <end angle> <radius>)
(defun Bulge2Arc (p1 p2 b / a c r )
  ;(princ "\nBulgeFactor: ") (princ b)
  (setq
    a (* 2 (atan b))
    r (/ (distance p1 p2) 2 (sin a))
    c (polar p1 (+ (- (/ pi 2) a) (angle p1 p2)) r)
  )
  
  ;(princ "\nRadius: ") (princ r)
  (if (minusp b)
    (progn
      (list c (angle c p2) (angle c p1) (abs r))
    )
    (progn
      (list c (angle c p1) (angle c p2) (abs r))
    )
  )
)

;******************* Radians 2 degrees ***************
(defun rtd (radians)
  (* 180.0 (/ radians pi))
)

;******************* degrees 2 Radians  **************
(defun dtr (deg)
  (* pi (/ deg 180.0))
)


;**********************   Offset the Geometry 2 Analyze ***************
(defun G2AOffset (Geometry Side / ElementCounter eninfo en VertexList VertexXList VertexYList) 
;(defun G2AOffset (Geometry Side /) 

  (if (= Side "Out")
    (progn
      ;(alert "OFFSETting Outside **")
      (setq ElementCounter 0)
      (repeat (sslength Geometry)
        (setq en (ssname Geometry ElementCounter))
        (command "_OFFSET" ToolWidth en "-10000,-10000" "")
        (command "_ERASE" en "")
        (setq ElementCounter (1+ ElementCounter))
      )
      ; ~~~~~~
      ; This stops everything after OFFSET OUTSIDE, checking visually is then possible.
      ; (EXIT) 
      ; ~~~~~~
    )
    ; Side "IN"
    (progn
      ;(alert "OFFSETting Inside")
      (princ "\n ** OFFSETting Inside **")
      (setq ElementCounter 0 )
      (repeat (sslength Geometry)
        ;(princ "\nSSLength INSIDEGeometry: ") (princ (sslength Geometry)) (princ"\n")
        ; Calculate the centroids of every closed polygon and use it to offset to the inside!
        ; Centroid calculation: Average all the X-values and Average all the Y-values
        (setq
          en (ssname Geometry ElementCounter)
          eninfo (entget en)
          VertexList (MakeListOfVertices eninfo)
          VertexCounter 0
          VertexXList '()
          VertexYList '()
          CentroidXY '()
        )
        ;(princ "\nVertexList: ") (princ VertexList)
        ;(princ "\nLength VertexList: ") (princ (length VertexList)) (princ"\n")
        ;This should work, even if it is only 1 vertex (like the centerpoint of a cirkel)
        (repeat (length VertexList)
          ;(princ"\nInside VertexXY-loop:")
          ;(princ "\nElementCounter item: ") (princ (nth ElementCounter VertexList))
          ;(princ "\nElement Coordlist: ") (princ (assoc 10 (nth ElementCounter VertexList)))
          (setq 
            VertexXList (append VertexXList (list (nth 1 (assoc 10 (nth VertexCounter VertexList)))) )
            VertexYList (append VertexYList (list (nth 2 (assoc 10 (nth VertexCounter VertexList)))) )
            VertexCounter (1+ VertexCounter)
          )
        )
        (setq
          CentroidX (/ (apply '+ VertexXList) (length VertexList))
          CentroidY (/ (apply '+ VertexYList) (length VertexList))
        )
        ; There was a bizarre situation that an polyline-ellipse would not offset on its calculated centroid
        ; but just next to the centroid it did work.
        ; So,  artificially adding a small distance to the XY-values of the calculated centroïd.
        (setq
          CentroidX (+ CentroidX ToolWidth)
          CentroidY (+ CentroidY ToolWidth)
          CentroidXY (strcat (rtos CentroidX 2 4) "," (rtos CentroidY 2 4))
        )
        ; Now we have a centroid to use with OFFSET
        (command "_OFFSET" ToolWidth en CentroidXY "")
        (command "_ERASE" en "")
        (setq ElementCounter (1+ ElementCounter))
      )

      ; ~~~~~~
      ; This stops everything after OFFSET INSIDE, checking visually is then possible.
      ;(EXIT)
      ; ~~~~~~

    )
  )
  ;(alert "Loops have finished!")
  (progn Geometry)
)


;*******************************************************************
;******               SUPPORT FUNCTIONS/COMMANDS               *****
;*******************************************************************
;-----------------------------------------------------
; ******          SET INSIDECONTOUR            *******
;-----------------------------------------------------
; Set the transparency of lines to indicate that they
; are inside the outer-perimeter. i.e. INSIDE-contours
; This is used to determine tool-correction-direction:
; It will be used to (CAD) OFFSET to the INSIDE
(defun c:GCSetInside ( / InsideSelection)
  (princ "\nSelect line(s) Inside-perimeters: ") (setq InsideSelection (ssget))
  (if InsideSelection
    (progn
      (command "._CHPROP" "P" "" "Color" "ByLayer" "TR" "65" "")
    )
    (progn
      (princ "\nNothing selected.")
    )
  )
  (princ)
)

;-----------------------------------------------------
; ******         SET  OUTSIDECONTOUR           *******
;-----------------------------------------------------
; Set the transparency of lines to indicate that they
; are lines OUTER-contour lines (i.e. perimeter)
; This is used to determine tool-correction-direction:
; It will be used to (CAD) OFFSET to the OUTSIDE
(defun c:GCSetOutside ( / OutsideSelection)
  (princ "\nSelect line(s) that are Outer-perimeters: ") (setq OutsideSelection (ssget))
  (if OutsideSelection
    (progn
      (command "._CHPROP" "P" "" "Color" "ByLayer" "TR" "ByLayer" "")
    )
    (progn
      (princ "\nNothing selected.")
    )
  )
  (princ)
)

;-----------------------------------------------------
; ******         SET  DRAWINGCONTOUR           *******
;-----------------------------------------------------
(defun c:GCSetDrawing ( / DrawSelection)
  (princ "\nSelect line(s) that are meant as 'drawing': ") (setq DrawSelection (ssget))
  (if DrawSelection
    (progn
       (command "._CHPROP" "P" "" "TR" "ByLayer" "Color" "212" "")
    )
    (progn
      (princ "\nNothing selected.")
    )
  )
  (princ)
)


;-----------------------------------------------------
; ****** Read the LDF values from the LDF_File *******
;-----------------------------------------------------
; The first non-comment-line is assumed to be the Total-heigh from bed to body-measuringpoint, Focus-length, material thickness 
(defun ReadLDF_File (LDF_File / CharCounter LDF_File LDF_Line ZAxisTotalHeight ZAxisFocalHeight ZAxisMaterialThickness)
  (setq LLDefList '() )
  
  (while (setq LDF_Line (read-line LDF_File)) ; read lines until there are no more lines
    ;(princ "\n")  (princ LDF_Line)
    (if (eq (substr LDF_Line 1 1) ";") ; If it is a 'Comment' line do nothing
      (progn
        ;(princ "Comment line") ; it is a 'Comment' line do nothing
      )
      ; Not a comment-line: analyze line
      (progn
        (setq
          CharCounter 1
          ZAxisTotalHeight ""
          ZAxisFocalHeight ""
          ZAxisMaterialThickness ""
          LLColor ""
          LLPower ""
          LLFeed ""
        )
        ; Geometry color
        (while (and 
                  (/= (substr LDF_Line CharCounter 1) ",") 
                  (< CharCounter (strlen LDF_Line))
               )
          (if (/= (substr LDF_Line CharCounter 1) " ")
            (progn
              (setq
                LLColor (strcat LLColor (substr LDF_Line CharCounter 1))
                CharCounter (1+ CharCounter)
              )
            )
            (progn
              (setq CharCounter (1+ CharCounter))
            )
          )
        )

        ; Skip the comma
        (setq CharCounter (1+ CharCounter))
        
        ; Laser Power
        (while (and 
                  (/= (substr LDF_Line CharCounter 1) ",") 
                  (< CharCounter (strlen LDF_Line))
               )
          (if (/= (substr LDF_Line CharCounter 1) " ") ; not a space
            (progn
              (setq
                LLPower (strcat LLPower (substr LDF_Line CharCounter 1))
                CharCounter (1+ CharCounter)
              )
            )
            (progn
              (setq CharCounter (1+ CharCounter))
            )
          )
        )

        ; Skip the comma
        (setq CharCounter (1+ CharCounter))

        ; Feed Speed
        (while (<= CharCounter (strlen LDF_Line))
          (if (/= (substr LDF_Line CharCounter 1) " ")
            (progn
              (setq
                LLFeed (strcat LLFeed (substr LDF_Line CharCounter 1))
                CharCounter (1+ CharCounter)
              )
            )
            (progn
              (setq CharCounter (1+ CharCounter))
            )
          )
        )
        ; If no values are read from the file, the variables still are 'empty strings'
        ; So empty file lines will result in 'empty-string-values' in the list

        ; !Completely obsolete as Colors are not being used in the code anymore!
        ; LLColor value is now either 'CUT' or 'WRITE'; 
        (if (> (strlen LLColor) 0)
          (progn
            (setq LLColor (strcase LLColor)) ; Uppercase
            
            ; Calulate the S-value from the given percentage value
            ; Max laserpower is S1000 (100%)
            (setq LLPower (strcat "S" (itoa(* (atoi LLPower) 10))))

            ; Add a 'F' to the feedspeed
            (setq
              LLFeed (strcat "F" LLFeed)
              LLDefList (append LLDefList (list (list LLColor LLPower LLFeed)))
            )
          )
        )
      )
    ) ;EndIf

  ) ;EndWhileReadLine

  ; First List-element of LLDefList contains Height-values
  ; Lowest Z-axis position on the Neje 4 MAX (E80): -45
  ; True focal distance calculation: -45 + material-thickness
  ; Remember: values are absolute
  ;----------------------------------------------------------
  ; Calibrate procedure Z-axis: 
  ; Execute following commands: 
  ; G28 Z0
  ; G0 Z-45
  ; Now focus the laser as good as you can on the bed"by mechanically shifting the lasermodule up/down
  ; and fix its position with the retaining-screws.
  ; The Z-axis is now calibrated.
  (setq
    ZAxisTotalHeight (nth 0 (car LLDefList))
    ZAxisTotalHeight (atof (substr ZAxisTotalHeight 1 (strlen ZAxisTotalHeight)))
    ZAxisFocalHeight (nth 1 (car LLDefList))
    ZAxisFocalHeight (/ (atof (substr ZAxisFocalHeight 2 (strlen ZAxisFocalHeight))) 10.0); convert to numeric and mulitply by 10.0
    ZAxisMaterialThickness (nth 2 (car LLDefList))
    ZAxisMaterialThickness (atof (substr ZAxisMaterialThickness 2 (strlen ZAxisMaterialThickness))); convert to numeric
    LaserFocalHeight (rtos (+ ZAxisTotalHeight ZAxisMaterialThickness) 2 4); add the material thickness to the negative focal distance
    LaserReturnHeigt "0.0"
  )

  (close LDF_File)
  ;(princ "\nLaser Work Definition list: ") (princ LLDefList)
  
)

;*******************************************************************
;**********************  DATE-TIME functions ********************
;*******************************************************************
(defun UpdateTime (/)
  (setq
    CurDate (rtos (getvar "CDATE") 2 6)
    ; Break up the string into its separate parts
    HH   (substr CurDate 10 2)
    MM   (substr CurDate 12 2)
    SS   (substr CurDate 14 2)
    CurTime (strcat HH ":" MM ":" SS)
  )
  (progn CurTime)
)

;*******************************************************************
;**********************  SHOW/EXAMINE functions ********************
;*******************************************************************
(defun c:GCShowOutside ( / sf ss)
  (setq
    sf nil
    ss nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "OUTSIDE-TOOLPATH")
             (cons 67 0) ; ModelSpace!
           (cons -4 "AND>")
       )
     )
    ss (ssget "X" sf)
  )

  (if ss
    (progn
      (setvar "CMDECHO" 0)
        (command ".Layer" "OFF" "*" "Yes" "On" "OUTSIDE-TOOLPATH" "Set" "OUTSIDE-TOOLPATH" "")
      (setvar "CMDECHO" 1)
    )
    (progn
      (princ "\nNo OUTSIDE toolpath to show...")
    )
  )
  (princ)
)

; ----------
(defun c:GCShowInside ( / sf ss)
  (setq
    sf nil
    ss nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "INSIDE-TOOLPATH")
             (cons 67 0) ; ModelSpace!
           (cons -4 "AND>")
       )
     )
    ss (ssget "X" sf)
  )

  (if ss
    (progn
      (setvar "CMDECHO" 0)
        (command ".Layer" "OFF" "*" "Yes" "On" "INSIDE-TOOLPATH" "Set" "INSIDE-TOOLPATH" "")
      (setvar "CMDECHO" 1)
    )
    (progn
      (princ "\nNo INSIDE toolpath to show...")
    )
  )

  (princ)
)

; ----------
(defun c:GCShowText ( / )
  (setq
    sf nil
    ss nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "TEXT-TOOLPATH")
             (cons 67 0) ; ModelSpace!
           (cons -4 "AND>")
       )
     )
    ss (ssget "X" sf)
  )

  (if ss
    (progn
      (setvar "CMDECHO" 0)
        (command ".Layer" "OFF" "*" "Yes" "On" "TEXT-TOOLPATH" "Set" "TEXT-TOOLPATH" "")
      (setvar "CMDECHO" 1)
    )
    (progn
      (princ "\nNo TEXT toolpath to show...")
    )
  )
  (princ)
)

; ----------
(defun c:GCShowDrawing ( / )
  (setq
    sf nil
    ss nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "DRAWING-TOOLPATH")
             (cons 67 0) ; ModelSpace!
           (cons -4 "AND>")
       )
     )
    ss (ssget "X" sf)
  )
  
  (if ss
    (progn
      (setvar "CMDECHO" 0)
        (command ".Layer" "OFF" "*" "Yes" "On" "DRAWING-TOOLPATH" "Set" "DRAWING-TOOLPATH" "")
      (setvar "CMDECHO" 1)
    )
    (progn
      (princ "\nNo DRAWING toolpath to show...")
    )
  )
  (princ)
)

; ----------      
(defun c:GCShowToolpaths ( / )
(setq
    sf nil
    ss nil
    sf (append
         (list
           (cons -4 "<AND")
             (cons 8 "DRAWING-TOOLPATH,TEXT-TOOLPATH,INSIDE-TOOLPATH,OUTSIDE-TOOLPATH")
              (cons 67 0) ; ModelSpace!
             (cons -4 "AND>")
         )
       )
    ss (ssget "X" sf)
  )
  
  (if ss
    (progn
      (setvar "CMDECHO" 0)
        (command ".Layer" "OFF" "*" "Yes" "")
      (command ".Layer" "On" "OUTSIDE-TOOLPATH, INSIDE-TOOLPATH, TEXT-TOOLPATH, DRAWING-TOOLPATH" "")
    )
    (progn
      (princ "\nNo Toolpath geometry to show...")
    )
  )

  (setvar "CMDECHO" 1)
  (princ)
)

; ----------
(defun c:GCShowOriginal ( / )
  (setvar "CMDECHO" 0)
    (command ".Layer" "On" "*" "SET" "0" "")
    (command ".Layer" "Off" "OUTSIDE-TOOLPATH, INSIDE-TOOLPATH, TEXT-TOOLPATH, DRAWING-TOOLPATH" "")
  (setvar "CMDECHO" 1)
  (princ)
)

; ----------
(defun c:GCShowAllLayers ( / )
  (setvar "CMDECHO" 0)
    (command ".Layer" "On" "*" "Thaw" "*" "Unlock" "*" "")
  (setvar "CMDECHO" 1)
  (princ)
)


;-----------------------
; Setttings dialogue
;-----------------------
; This is not fully operational yet.
; Should be elaborated with a file-read and a file-write
(prompt "\nType GRBLSET")
 
(defun C:GRBLSET()
 
  (setq dcl_id (load_dialog "AutoCAD_GRBL_Gcode-Generator_SettingsDialogue_V3.34.dcl"))

  (if (not (new_dialog "GRBL_Settings" dcl_id))
    (progn
      (alert "Could not load AutoCAD_GRBL_Gcode-Generator_SettingsDialogue_V3.34.dcl")
      (exit)
    )
  )
 
  (action_tile "OK"
    "(done_dialog)"
  );action_tile
 
  (start_dialog)
  (unload_dialog dcl_id)
 
  (princ)
 
);defun

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; This line is shown when loading the program.
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Before ANY operation on the drawing, put an 'UNDO - Mark' so we can go back to the original start-state of the drawing
(setvar "CMDECHO" 0)
(command "UNDO" "MARK")
(setvar "CMDECHO" 1)

(princ "\n \n *** Loaded: GCode (GRBL) Generator - v3.36")
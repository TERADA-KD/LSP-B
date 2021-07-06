;;**************************************************************************************************
;;単品機器の回路入力（挿入後背景線をカット）単品回路入力.LSP 1998/May/28 ==> 2002/NOV    By T.Terada
;;**************************************************************************************************
;; 1.ヒューズ
;; 2.ブザー
;; 3.ベル
;; 4.スナップスイッチ
;; 5.操作トランス
;;==================================================================================================
;;挿入点と記入位置
  (defun POINT ( / PX1 PX2 SS MSG)
      
     (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
        (setq MSG (strcat "\n" KIKI "の挿入点を指示："))
        (setq PC (getpoint MSG))
             (setq PX1 (list (- (car PC) 10) (- (cadr PC) 5)))
             (setq PX2 (list (+ (car PC) 10) (+ (cadr PC) 5)))
      (setq SS (ssget "C" PX1 PX2))
      (if (/= SS nil) (progn (setq ENT (ssname ss 0))))
;     (princ "ENT=")(princ ENT)
  )
;;
;;コマンドの実行 （線カット有り）
  (defun IN1 (/ PY1 PY2)
     (if (/= ENT nil)
         (progn
            (setq PY1 (list (car PC) (+ (cadr PC) 10)))
            (setq P12 (list (car PC) (- (cadr PC) 100)))
            (command "BREAK" PY1 "F" PC P12)
         )
         (princ "\n線のカットはしません");else  ;縦線が存しないなら 
     )
     (command "COLOR" 2)
     (command "INSERT" SYM PC "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MK "1" "" "")
     (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(prompt "\n入力完了")(redraw)
  )
;;コマンドの実行 （線カット無し）
  (defun IN1X (/ PY1 PY2)
     (command "COLOR" 2)
     (command "INSERT" SYM PC "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MK "1" "" "")
     (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(prompt "\n入力完了")(redraw)
  )
;;==================================================================================================
;;1.ヒューズ
;;==================================================================================================
  (defun C:FUSE( / DEF1 DEF_BAK1 DCL_ID WHAT_NEXT SYM PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 MK KIKI ENT
                   DEF2 DEF_BAK2 DT SK)
      (setvar "CMDECHO" 1);(setq *error* *err1*)
      (TEXTSCR)
      (princ "\n")(princ "\n")
      (prompt "\n            【 消弧剤入りヒューズを使用しなければいけない個所 】")(princ "\n")
      (prompt "\n    １．官庁・民間共、ＱＢ・配電盤のＴｒ二次の低圧回路に直接接続する補助回路")
      (prompt "\n    ２．官庁向け分電盤・制御盤の主幹一次に直接接続する補助回路（電源表示灯など）")
      (prompt "\n    ３．官庁向け分電盤・制御盤で主幹がなく直接バーなどに接続する回路")
      (princ "\n")(princ "\n")
      (setq PNA "F")(setq KIKI "ヒューズ")(setq T2 "" T4 "" T5 "" T6 "" T7 "" T8 "" T9 "")
;   DCLﾌｧｲﾙの読み込み
      (setq DEF1_BAK DEF1)(setq DEF2_BAK DEF2)
      (if (not DEF1)(setq DEF1 "A"))(if (not DEF2)(setq DEF2 "X"))
      (setq DCL_ID (load_dialog "X:/DCL/FUSE.DCL"))
      (setq WHAT_NEXT 4)
      (while (>= WHAT_NEXT 4)
         (if (null (new_dialog "SFM" DCL_ID) )(exit))
          
;     図枠の選択用ダイアログボックス
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "1" "(setq MK \"22\")");坂詰
         (action_tile "2" "(setq MK \"21\")");エコー
         (action_tile "3" "(setq MK \"4\" )");富士
         (action_tile "4" "(setq MK \"42\")");キムデン
;         (if (and (= MK "4")(= MK "42"))(mode_tile "Y" 1))
         (act_radioset '("A" "B") DEF1)
         (act_radioset '("X" "Y") DEF2)
         (action_tile "A" "(setq DEF1 $key)");縦
         (action_tile "B" "(setq DEF1 $key)");横
         (action_tile "X" "(setq DEF2 $key)");消弧剤無
         (action_tile "Y" "(setq DEF2 $key)");消弧剤入
         
         (setq WHAT_NEXT (start_dialog))
      )
         (unload_dialog DCL_ID)
         
         (cond ((= WHAT_NEXT 0)(setq DEF1 DEF_BAK1)(princ))((= WHAT_NEXT 1) DEF1))
         (cond ((= WHAT_NEXT 0)(setq DEF2 DEF_BAK2)(princ))((= WHAT_NEXT 1) DEF2))
         (cond ((= DEF1 "A") (setq DT "T"))((= DEF1 "B") (setq DT "Y")))
         (cond ((= DEF2 "X") (setq T4 "")) ((or (/= MK "4")(= DEF2 "Y")) (setq T4 "消弧剤入")))
         
;
;  分岐 DCLﾌｧｲﾙの読み込み
      (if (= MK "22");坂詰
          (progn 
             (GRAPHSCR)
             (setq DCL_ID (load_dialog "X:/DCL/FUSE.DCL"))
             (if (not (new_dialog "SF22" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"FHC-15N\")(setq T3 \"1A\")")
             (action_tile "12" "(setq T1 \"FHC-15N\")(setq T3 \"2A\")")
             (action_tile "13" "(setq T1 \"FHC-15N\")(setq T3 \"3A\")")
             (action_tile "14" "(setq T1 \"FHC-15N\")(setq T3 \"5A\")")
             (action_tile "15" "(setq T1 \"FHC-15N\")(setq T3 \"10A\")")
             (action_tile "16" "(setq T1 \"FHC-15N\")(setq T3 \"15A\")")
             (action_tile "21" "(setq T1 \"FHC-15D\")(setq T3 \"1A\")")
             (action_tile "22" "(setq T1 \"FHC-15D\")(setq T3 \"2A\")")
             (action_tile "23" "(setq T1 \"FHC-15D\")(setq T3 \"3A\")")
             (action_tile "24" "(setq T1 \"FHC-15D\")(setq T3 \"5A\")")
             (action_tile "25" "(setq T1 \"FHC-15D\")(setq T3 \"10A\")")
             (action_tile "26" "(setq T1 \"FHC-15D\")(setq T3 \"15A\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
       (if (= MK "21");エコー
          (progn 
             (GRAPHSCR)
             (setq DCL_ID (load_dialog "X:/DCL/FUSE.DCL"))
             (if (not (new_dialog "SF21" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"FH-001AF\")(setq T3 \"1A\")")
             (action_tile "12" "(setq T1 \"FH-001AF\")(setq T3 \"2A\")")
             (action_tile "13" "(setq T1 \"FH-001AF\")(setq T3 \"3A\")")
             (action_tile "14" "(setq T1 \"FH-001AF\")(setq T3 \"5A\")")
             (action_tile "15" "(setq T1 \"FH-001AF\")(setq T3 \"10A\")")
             (action_tile "16" "(setq T1 \"FH-001AF\")(setq T3 \"15A\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
       (if (= MK "4");富士;栓型の場合
          (progn 
             (GRAPHSCR)
             (setq DCL_ID (load_dialog "X:/DCL/FUSE.DCL"))
             (if (not (new_dialog "SF4" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"AFaC-3\") (setq T3 \"3A\") (setq FR \"AFa30XA\")")
             (action_tile "12" "(setq T1 \"AFaC-5\") (setq T3 \"5A\") (setq FR \"AFa30XA\")")
             (action_tile "13" "(setq T1 \"AFaC-10\")(setq T3 \"10A\")(setq FR \"AFa30XA\")")
             (action_tile "14" "(setq T1 \"AFaC-15\")(setq T3 \"15A\")(setq FR \"AFa30XA\")")
             (action_tile "15" "(setq T1 \"AFaC-20\")(setq T3 \"20A\")(setq FR \"AFa30XA\")")
             (action_tile "16" "(setq T1 \"AFaC-30\")(setq T3 \"30A\")(setq FR \"AFa30XA\")")
             (action_tile "21" "(setq T1 \"BaC-3\")  (setq T3 \"3A\") (setq FR \"Ba-30用絶縁物\")")
             (action_tile "22" "(setq T1 \"BaC-5\")  (setq T3 \"5A\") (setq FR \"Ba-30用絶縁物\")")
             (action_tile "23" "(setq T1 \"BaC-10\") (setq T3 \"10A\")(setq FR \"Ba-30用絶縁物\")")
             (action_tile "24" "(setq T1 \"BaC-15\") (setq T3 \"15A\")(setq FR \"Ba-30用絶縁物\")")
             (action_tile "25" "(setq T1 \"BaC-20\") (setq T3 \"20A\")(setq FR \"Ba-30用絶縁物\")")
             (action_tile "26" "(setq T1 \"BaC-30\") (setq T3 \"30A\")(setq FR \"Ba-30用絶縁物\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
          )
       )
      (if (= MK "42");キムデン 二種耐熱型
          (progn 
             (GRAPHSCR)
             (setq DCL_ID (load_dialog "X:/DCL/FUSE.DCL"))
             (if (not (new_dialog "SF222" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"F-10BK\")(setq T3 \"1A\")")
             (action_tile "12" "(setq T1 \"F-10BK\")(setq T3 \"2A\")")
             (action_tile "13" "(setq T1 \"F-10BK\")(setq T3 \"3A\")")
             (action_tile "14" "(setq T1 \"F-10BK\")(setq T3 \"5A\")")
             (action_tile "15" "(setq T1 \"F-10BK\")(setq T3 \"10A\")")

             (action_tile "21" "(setq T1 \"F-10BK2\")(setq T3 \"1A\")")
             (action_tile "22" "(setq T1 \"F-10BK2\")(setq T3 \"2A\")")
             (action_tile "23" "(setq T1 \"F-10BK2\")(setq T3 \"3A\")")
             (action_tile "24" "(setq T1 \"F-10BK2\")(setq T3 \"5A\")")
             (action_tile "25" "(setq T1 \"F-10BK2\")(setq T3 \"10A\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
    (POINT)
    (if (= DT "T")(setq SYM "SFV"))
    (if (= DT "Y")(setq SYM "SFH"))
     
    (setq PNo (getstring "\nﾋｭｰｽﾞ番号を指示:(例：-1)"))
              (cond ((= PNo "") (setq PNo "-1")))
    (setq PNo (strcat PNA PNo))
    (if (= DT "T")(setq PC2 (list (car PC)(+ (cadr PC) 50))))
    (if (= DT "Y")(setq PC2 (list (- (car PC) 40)(cadr PC))))
    ;栓型の場合
    (if (= MK "4")
        (progn (if (= DT "T")(setq SYM2 "SSKT"))
               (if (= DT "Y")(setq SYM2 "SSKTH"))
               (command "INSERT" SYM2 PC2 "1" "1" "0" "F" PNO FR "" "" "" "" "" "" "" "" MK "1" "" "")
        )
    )
    ;耐熱型の場合
    (if (= MK "42")(setq T4 "消弧剤入"))
    (if (and (= MK "42")(= T1 "F-10BK2"))
        (progn (if (= DT "T")(setq SYM2 "SSKT"))
               (if (= DT "Y")(setq SYM2 "SSKTH"))
               (command "INSERT" SYM2 PC2 "1" "1" "0" "F" PNO "FC-10BK2" "" "" "" "" "" "" "" "" MK "1" "" "")
        )
    )
    (IN1X)
    (GC)
  )
;;==================================================================================================
;;2.ブザー
;;==================================================================================================
  (defun C:BZA( / PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 MK KIKI ENT)
      (setq *error* *err1*)
      (setq PNA "BZ")
      (setq KIKI "ブザー")
      (setq SYM "SBZ")
      (setq T3 "")(setq T4 "")(setq T5 "")(setq T6 "")(setq T7 "")(setq T8 "")(setq T9 "")
;   DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/DCL/SBZ.DCL"))
      (if (not (new_dialog "BZM" DCL_ID) )(EXIT))
       
;
;  図枠の選択用ダイアログボックス
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq MK \"15\")");松下
      (action_tile "2" "(setq MK \"20\")");鎌田（ハロー）
      (action_tile "3" "(setq MK \"9\" )");和泉
      (action_tile "4" "(setq MK \"4\" )");富士
      (start_dialog)
      (unload_dialog DCL_ID)
      (princ)
;
;  分岐 DCLﾌｧｲﾙの読み込み
      (if (= MK "15");松下
          (progn 
             (setq DCL_ID (load_dialog "X:/DCL/SBZ.DCL"))
             (if (not (new_dialog "BZ15" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"EA4031B\")(setq T2 \"AC100V\")")
             (action_tile "12" "(setq T1 \"EA4032B\")(setq T2 \"AC200V\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
       (if (= MK "9");和泉
          (progn 
             (setq DCL_ID (load_dialog "X:/DCL/SBZ.DCL"))
             (if (not (new_dialog "BZ9" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"AZ11N\")(setq T2 \"AC100V\")")
             (action_tile "12" "(setq T1 \"AZ12N\")(setq T2 \"AC200V\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
       (if (= MK "20");鎌田（ハロー）
          (progn 
             (setq DCL_ID (load_dialog "X:/DCL/SBZ.DCL"))
             (if (not (new_dialog "BZ20" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"207\")(setq T2 \"AC100V\")")
             (action_tile "12" "(setq T1 \"VR-24\")(setq T2 \"AC24V\")")
             (action_tile "13" "(setq T1 \"VR-24D\")(setq T2 \"DC24V\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
       (if (= MK "4");富士
          (progn 
             (setq DCL_ID (load_dialog "X:/DCL/SBZ.DCL"))
             (if (not (new_dialog "BZ4" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"DR30B6-HB\")(setq T2 \"AC100V\")")
             (action_tile "12" "(setq T1 \"DR30B6-MB\")(setq T2 \"AC200V\")")
             (action_tile "13" "(setq T1 \"DR30B6-EB\")(setq T2 \"DC24V\")")
             (action_tile "14" "(setq T1 \"DR30B5-HB\")(setq T2 \"AC100V\")")
             (action_tile "15" "(setq T1 \"DR30B5-MB\")(setq T2 \"AC200V\")")
             (action_tile "16" "(setq T1 \"DR30B5-EB\")(setq T2 \"AC/DC24V\")")
             (action_tile "16" "(setq T1 \"DR30B0-HB\")(setq T2 \"AC100V\")")
             (action_tile "16" "(setq T1 \"DR30B0-MB\")(setq T2 \"AC200V\")")
             
             (action_tile "21" "(setq T1 \"DR22B5-HB\")(setq T2 \"AC100V\")")
             (action_tile "22" "(setq T1 \"DR22B5-MB\")(setq T2 \"AC200V\")")
             (action_tile "23" "(setq T1 \"DR22B5-EB\")(setq T2 \"AC/DC24V\")")
             (action_tile "24" "(setq T1 \"DR22B3-HB\")(setq T2 \"AC100V\")")
             (action_tile "25" "(setq T1 \"DR22B3-MB\")(setq T2 \"AC200V\")")
             (action_tile "26" "(setq T1 \"DR22B3-EB\")(setq T2 \"AC/DC24V\")")
             
            (start_dialog)
            (unload_dialog DCL_ID)
            (princ)
         )
       )
    (POINT)
     
    (setq PNo (getstring "\nブザー番号を指示:(例：-1)"))
              (cond ((= PNo "") (setq PNo "-1")))
    (setq PNo (strcat PNA PNo))
    (IN1)
    (GC)
  )
;;==================================================================================================
;;3.ベル
;;==================================================================================================
  (defun C:BLL( / PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 MK KIKI ENT)
      (setq *error* *err1*)
      (setq PNA "BL")
      (setq KIKI "ベル")
      (setq SYM "SBL")
      (setq MK "20")
      (setq T1 "1036")(setq T2 "")
      (setq T3 "DC6V")(setq T4 "")(setq T5 "")(setq T6 "")(setq T7 "")(setq T8 "")(setq T9 "")
      (POINT)
       
      (setq PNo (getstring "\nベル番号を指示:(例：-1)"))
              (cond ((= PNo "") (setq PNo "-1")))
      (setq PNo (strcat PNA PNo))
      (IN1)
    (GC)
  )
;;==================================================================================================
;;4.スナップスイッチ    
;;==================================================================================================
  (defun C:SSW ( / PNA PNo SYM T1 T2 T3 T4 T5 T6 T7 T8 T9 MK KIKI ENT)
      (setq *error* *err1*)
      (setq PNA "SS")
      (setq KIKI "スナップSW")
      (setq T4 "")(setq T5 "")(setq T6 "")(setq T7 "")(setq T8 "")(setq T9 "")
;   DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/DCL/SSW.DCL"))
      (if (not (new_dialog "SS" DCL_ID) )(EXIT))
       
;
;  図枠の選択用ダイアログボックス
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq MK \"15\")");松下
      (action_tile "2" "(setq MK \"10\")");日本開閉器
      (start_dialog)
      (unload_dialog DCL_ID)
      (princ)
;
;  分岐 DCLﾌｧｲﾙの読み込み
      (if (= MK "15");松下
          (progn 
             (setq DCL_ID (load_dialog "X:/DCL/SSW.DCL"))
             (if (not (new_dialog "SS15" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "11" "(setq T1 \"WD1021F\")(setq T2 \"単極\")(setq T3 \"ON-OFF\")")
             (action_tile "12" "(setq T1 \"WD1122\")(setq T2 \"単極\")(setq T3 \"ON-OFF-ON\")")
             (action_tile "13" "(setq T1 \"WD1221\")(setq T2 \"２極\")(setq T3 \"ON-OFF\")")
             (action_tile "14" "(setq T1 \"WD1322\")(setq T2 \"２極\")(setq T3 \"ON-OFF-ON\")")
             
             (action_tile "15" "(setq T1 \"WD1011\")(setq T2 \"単極\")(setq T3 \"ON-OFF\")")
             (action_tile "16" "(setq T1 \"WD1112\")(setq T2 \"単極\")(setq T3 \"ON-OFF-ON\")")
             (action_tile "17" "(setq T1 \"WD1211\")(setq T2 \"２極\")(setq T3 \"ON-OFF\")")
             (action_tile "18" "(setq T1 \"WD1312\")(setq T2 \"２極\")(setq T3 \"ON-OFF-ON\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
       (if (= MK "10");日開
          (progn 
             (setq DCL_ID (load_dialog "X:/DCL/SSW.DCL"))
             (if (not (new_dialog "SS10" DCL_ID) )(EXIT))
              
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "21" "(setq T1 \"S-301T\")(setq T2 \"単極\")(setq T3 \"ON-OFF\")")
             (action_tile "22" "(setq T1 \"S-303T\")(setq T2 \"単極\")(setq T3 \"ON-OFF-ON\")")
             (action_tile "23" "(setq T1 \"S-331T\")(setq T2 \"２極\")(setq T3 \"ON-OFF\")")
             (action_tile "24" "(setq T1 \"S-333T\")(setq T2 \"２極\")(setq T3 \"ON-OFF-ON\")")
             
             (action_tile "25" "(setq T1 \"S-301\")(setq T2 \"単極\")(setq T3 \"ON-OFF\")")
             (action_tile "26" "(setq T1 \"S-303\")(setq T2 \"単極\")(setq T3 \"ON-OFF-ON\")")
             (action_tile "27" "(setq T1 \"S-331\")(setq T2 \"２極\")(setq T3 \"ON-OFF\")")
             (action_tile "28" "(setq T1 \"S-333\")(setq T2 \"２極\")(setq T3 \"ON-OFF-ON\")")
             (start_dialog)
             (unload_dialog DCL_ID)
             (princ)
         )
       )
    (if (and (= UD "U")(= T3 "ON-OFF"))(setq SYM "SSS2U"))
    (if (and (= UD "D")(= T3 "ON-OFF"))(setq SYM "SSS2D"))
    (if (and (= UD "U")(= T3 "ON-OFF-ON"))(setq SYM "SSS3U"))
    (if (and (= UD "D")(= T3 "ON-OFF-ON"))(setq SYM "SSS3D"))
    
    (POINT)
     
    (setq PNo (getstring "\nS.SW番号を指示:(例：-1)"))
              (cond ((= PNo "") (setq PNo "-1")))
    (setq PNo (strcat PNA PNo))
    (IN1)
    (GC)
  )
;;==================================================================================================
;;5.操作トランス
;;==================================================================================================
  (defun C:TRL ( / PNA PNo SYM T1 T2 T3 T4 T5 T6 T7 T8 T9 MK KIKI V H ENT)
      (setq *error* *err1*)
      (setq PNA "TRL" KIKI "操作トランス" SYM "20" MK "20" T2 "" T5 "" T6 "" T7 "" T8 "" T9 "")
      (TRL_DCL)
      (if (= T1 nil)(progn (ALERT "トランスが選択されていません")(TRL_DCL)))
       
      (setq PNo (getstring "\nトランス番号を指示:(例：-1)"))
      (cond ((= PNo "") (setq PNo "-1")))(setq PNo (strcat PNA PNo))
      (prompt "操作トランスの挿入点を指示：")
      (command "INSERT" SYM pause "1" "1" "0" PNA PNO T1 T2 T3 T4 T5 T6 T7 T8 T9 MK "1" "" "")
      (GC)
 )
;;-------------------------------------------------------------------------------------------------
 (defun TRL_DCL ( / );   DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/DCL/OPTR.DCL"))
      (if (not (new_dialog "TROP1" DCL_ID) )(EXIT))
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq T1 \"WL-50A\")  (setq T4 \"50VA\")(setq T3 \"220/110V\")")
      (action_tile "2" "(setq T1 \"WTP-50A\") (setq T4 \"50VA\")(setq T3 \"200/100V\")")
      (action_tile "3" "(setq T1 \"WTP-50B\") (setq T4 \"50VA\")(setq T3 \"200/24V\")")
      (action_tile "4" "(setq T1 \"WTP-50C\") (setq T4 \"50VA\")(setq T3 \"100/24V\")")
      (action_tile "5" "(setq T1 \"WTP-100A\")(setq T4 \"100VA\")(setq T3 \"200/100V\")")
      (action_tile "6" "(setq T1 \"WTP-100B\")(setq T4 \"100VA\")(setq T3 \"200/24V\")")
      (action_tile "7" "(setq T1 \"WTP-100C\")(setq T4 \"100VA\")(setq T3 \"100/24V\")")
      (action_tile "8" "(setq T1 \"WTP-150A\")(setq T4 \"150VA\")(setq T3 \"200/100V\")")
      (action_tile "9" "(setq T1 \"WTP-150B\")(setq T4 \"150VA\")(setq T3 \"200/24V\")")
      (action_tile "10" "(setq T1 \"WTP-150C\")(setq T4 \"150VA\")(setq T3 \"100/24V\")")
      (action_tile "11" "(setq T1 \"WTP-200A\")(setq T4 \"200VA\")(setq T3 \"200/100V\")")
      (action_tile "12" "(setq T1 \"WTP-200B\")(setq T4 \"200VA\")(setq T3 \"200/24V\")")
      (action_tile "13" "(setq T1 \"WTP-200C\")(setq T4 \"200VA\")(setq T3 \"100/24V\")")
      (action_tile "14" "(setq T1 \"WTP-300A\")(setq T4 \"300VA\")(setq T3 \"200/100V\")")
      (action_tile "15" "(setq T1 \"WTP-300B\")(setq T4 \"300VA\")(setq T3 \"200/24V\")")
      
      (action_tile "16" "(setq T1 \"WTP-300C\")(setq T4 \"300VA\")(setq T3 \"100/24V\")")
      (action_tile "17" "(setq T1 \"WTP-500A\")(setq T4 \"500VA\")(setq T3 \"200/100V\")")
      (action_tile "18" "(setq T1 \"WTP-500B\")(setq T4 \"500VA\")(setq T3 \"200/24V\")")
      (action_tile "19" "(setq T1 \"WTP-500C\")(setq T4 \"500VA\")(setq T3 \"100/24V\")")
      (action_tile "20" "(setq T1 \"WTP-750A\")(setq T4 \"750VA\")(setq T3 \"200/100V\")")
      (action_tile "21" "(setq T1 \"WTP-750B\")(setq T4 \"750VA\")(setq T3 \"200/24V\")")
      (action_tile "22" "(setq T1 \"WTP-750C\")(setq T4 \"750VA\")(setq T3 \"100/24V\")")
      (action_tile "23" "(setq T1 \"WTP-1000A\")(setq T4 \"1kVA\")(setq T3 \"200/100V\")")
      (action_tile "24" "(setq T1 \"WTP-1000B\")(setq T4 \"1kVA\")(setq T3 \"200/24V\")")
      (action_tile "25" "(setq T1 \"WTP-1000C\")(setq T4 \"1kVA\")(setq T3 \"100/24V\")")
      (action_tile "26" "(setq T1 \"WTP-1500A\")(setq T4 \"1.5kVA\")(setq T3 \"200/100V\")")
      (action_tile "27" "(setq T1 \"WTP-1500B\")(setq T4 \"1.5kVA\")(setq T3 \"200/24V\")")
      (action_tile "28" "(setq T1 \"WTP-1500C\")(setq T4 \"1.5kVA\")(setq T3 \"100/24V\")")
      (action_tile "29" "(setq T1 \"WTP-2000A\")(setq T4 \"2kVA\")(setq T3 \"200/100V\")")
      (action_tile "30" "(setq T1 \"WTP-2000B\")(setq T4 \"2kVA\")(setq T3 \"200/24V\")")
      (action_tile "31" "(setq K1 \"WTP-2000C\")(setq T4 \"2kVA\")(setq T3 \"100/24V\")")
      (setq SYM "TROP-V");初期値
      (action_tile "V"  "(setq SYM \"TROP-V\")")
      (action_tile "H"  "(setq SYM \"TROP-H\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
   )

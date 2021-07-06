;;=================================================================================================
;;各接点の入力を行うLISP挿入後背景線をカット  接点入力.LSP 1998/May/28 ==> 2002/NOV     By T.Terada
;;=================================================================================================
;; * 共通関数  挿入点と記入位置  POINT1
;; * 共通関数  コマンドの実行    IN01
;; * 共通関数  横向き接点の実行  IN02
;; 1.リレー接点         Ａ接点       C:AC   SAC.DWG
;; 2.リレー接点         Ｂ接点       C:BC   SBC.DWG
;; 3.リレー接点         Ａ接点横向き C:ACL  SACH.DWG
;; 4.リレー接点         Ｂ接点横向き C:ACL  SBCH.DWG
;; 5.操作回路用2Pﾌﾞﾚｰｶｰ 横向き       C:SH1P SH1P.DWG（部品属性無し）
;; 6.タイマ－接点       Ａ接点       C:ATC  SATC1 SATC3 SAAR SATC2 SASC SATH.DWG
;; 7.タイマ－接点       Ｂ接点       C:BTC  SBTC1 SBTC3 SBAR SBTC2 SBSC SBTH.DWG
;; 8.スナップＳＷ       ＡＢ接点     C:ABC  SBAC.DWG
;; 9.スナップＳＷ       ＢＡ接点     C:BAC  SABC.DWG
;;10.サ－マル接点       Ａ接点       C:ATH  SATR.DWG
;;11.サ－マル接点       Ｂ接点       C:BTH  SBTR.DWG
;;12.コイル           （属性無）     C:XRY  XX.DWG
;;13.入出線             下向き       C:SEN1 KD.DCL/SE2
;;14.入出線             上向き       C:SEN2 KD.DCL/SEN1
;;15.SLCの回路入力   部品表属性無し  C:SSLC1 SPL1S.DWG    SPLS.DWG        
(princ "\n接点入力をロードしました")
;;=================================================================================================
;;挿入点と記入位置
;;-------------------------------------------------------------------------------------------------
  (defun POINT1 ( / PX1 PX2 SS)
;      
     (setvar "CMDECHO" 0)
     (setvar "BLIPMODE" 0)
        (setq MSG (strcat "\n" KIKI "の挿入点を指示："))
        (setq PC (getpoint MSG))
             (setq PX1 (list (- (car PC) 10) (- (cadr PC) 5)))
             (setq PX2 (list (+ (car PC) 10) (+ (cadr PC) 5)))
      (setq SS (ssget "C" PX1 PX2))
      (if (/= SS nil) (progn (setq ENT (ssname ss 0))))
  )
;;-------------------------------------------------------------------------------------------------
;;コマンドの実行
;;-------------------------------------------------------------------------------------------------
  (defun IN01 ( / PY1 PY2)
     (if (/= ENT nil)
         (progn
            (setq PY1 (list (car PC) (+ (cadr PC) 10)))
            (setq P12 (list (car PC) (- (cadr PC) 100)))
            (command "BREAK" PY1 "F" PC P12)
         )
         (prompt "\n線のカットはしません");else  ;縦線が存しないなら 
     )
     (command "COLOR" 2)
     (command "INSERT" SYM PC "1" "1" "0" T1 T2)(command "COLOR" 4)
;;後始末
         (setvar "CMDECHO" 1)
         (setvar "BLIPMODE" 1)
         (setq P1 nil P2 nil PC nil PY1 nil P12 nil)
         (prompt "\n入力完了")
         (redraw)
  )
;;-------------------------------------------------------------------------------------------------
;;横向き接点の実行
;;-------------------------------------------------------------------------------------------------
  (defun IN02()
      (setq PX1 (list (- (car PC) 5) (- (cadr PC) 10)))
      (setq PX2 (list (+ (car PC) 5) (+ (cadr PC) 10)))
      (setq SS (ssget "C" PX1 PX2))
      (if (/= SS nil) (progn (setq ENT (ssname ss 0))))
      (if (/= ENT nil)
         (progn
            (setq PY1 (list (- (car PC) 10) (cadr PC)))
            (setq P12 (list (+ (car PC) 100)(cadr PC)))
            (command "BREAK" PY1 "F" PC P12)
         )
         (prompt "\n線のカットはしません");else  ;縦線が存しないなら 
     )
      (command "INSERT" SYM PC "" "" "" T1 T2)
         (setvar "CMDECHO" 1)
         (setvar "BLIPMODE" 1)
         (setq P1 nil P2 nil PC nil PY1 nil P12 nil)
         (prompt "\n入力完了")
         (redraw)
  )
;;=================================================================================================
;;1.Ａ接点    C:AC
;;=================================================================================================
  (defun C:AC ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nＡ接点")
    (POINT1)
    (setq SYM "SAC")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：AX)"))
             (cond ((= T1 "") (setq T1 "AX")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;2.Ｂ接点
;;=================================================================================================
  (defun C:BC ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nＢ接点")
    (POINT1)
    (setq SYM "SBC")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：AX)"))
             (cond ((= T1 "") (setq T1 "AX")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;3.Ａ接点 横向き
;;=================================================================================================
  (defun C:ACL ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nＡ接点 横向き")
    (POINT1)
    (setq SYM "SACH")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：AX)"))
             (cond ((= T1 "") (setq T1 "AX")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN02)
    (GC)
  )
;;=================================================================================================
;;4.Ｂ接点 横向き
;;=================================================================================================
  (defun C:BCL ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nＢ接点 横向き")
    (POINT1)
    (setq SYM "SBCH")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：AX)"))
             (cond ((= T1 "") (setq T1 "AX")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN02)
    (GC)
  )
;;=================================================================================================
;;5.操作回路用2Pﾌﾞﾚｰｶｰ 横向き（部品属性無し）
;;=================================================================================================
  (defun C:SH1P ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nﾌﾞﾚｰｶｰ 横向き")
    (POINT1)
    (setq SYM "SH1P")(setq PNA "MCB") 
    (setq T1 (getstring "\n１:ブレーカー番号を指示:(例：-1)"))(cond ((= T1 "") (setq T1 "-1")))
    (setq T2 (getstring "\n２:ブレーカートリップ値を指示:(例：3A)"))(cond ((= T2 "") (setq T2 "3A")))
    (setq T1 (strcat PNA T1))
    (IN02)
    (GC)
  )
;;=================================================================================================
;;6.タイマ－Ａ接点
;;=================================================================================================
  (defun C:ATC ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
;   DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/DCL/TM.DCL"))
      (if (not (new_dialog "TMX" DCL_ID) )(EXIT))
;       
      (setq DMC (getvar "dimscale"))
;  図枠の選択用ダイアログボックス
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"SATC1\")(setq KIKI \"タイマー\")")  ;ﾀｲﾏｰ
      (action_tile "2" "(setq SYM \"SATC1\")(setq KIKI \"24Hﾀｲﾏｰ\")")   ;24Hﾀｲﾏｰ
      (action_tile "3" "(setq SYM \"SATC3\")(setq KIKI \"フリッカー\")");ﾌﾘｯｶｰ
      (action_tile "4" "(setq SYM \"SAAR\") (setq KIKI \"自動復帰\")")  ;自動復帰
      (action_tile "5" "(setq SYM \"SATC2\")(setq KIKI \"ｵﾌﾃﾞｨﾚｰ\")")   ;ｵﾌﾃﾞｨﾚｰ
      (action_tile "6" "(setq SYM \"SASC\") (setq KIKI \"リミット\")")  ;ﾘﾐｯﾄ
      (action_tile "7" "(setq SYM \"SATH\") (setq KIKI \"サーモ\")")    ;ｻｰﾓ
         (start_dialog)
         (unload_dialog DCL_ID)
      (princ)
    (POINT1)
;     
    (setq T1 (getstring "\n１:接点記号を指示:(例：TM)"))
             (cond ((= T1 "") (setq T1 "TM")))
;     
    (setq T2 (getstring "\n２:接点番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;7.タイマ－Ｂ接点
;;=================================================================================================
  (defun C:BTC ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
;   DCLﾌｧｲﾙの読み込み
      (setq DCL_ID (load_dialog "X:/DCL/TM.DCL"))
      (if (not (new_dialog "TMX" DCL_ID) )(EXIT))
;       
      (setq DMC (getvar "dimscale"))
;  図枠の選択用ダイアログボックス
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"SBTC1\")(setq KIKI \"タイマー\")")  ;ﾀｲﾏｰ
      (action_tile "2" "(setq SYM \"SBTC1\")(setq KIKI \"24Hﾀｲﾏｰ\")")   ;24Hﾀｲﾏｰ
      (action_tile "3" "(setq SYM \"SBTC3\")(setq KIKI \"タイマー\")")  ;ﾌﾘｯｶｰ
      (action_tile "4" "(setq SYM \"SBAR\") (setq KIKI \"自動復帰\")")  ;自動復帰
      (action_tile "5" "(setq SYM \"SBTC2\")(setq KIKI \"ｵﾌﾃﾞｨﾚｰ\")")   ;ｵﾌﾃﾞｨﾚｰ
      (action_tile "6" "(setq SYM \"SBSC\") (setq KIKI \"リミット\")")  ;ﾘﾐｯﾄ
      (action_tile "7" "(setq SYM \"SBTH\") (setq KIKI \"サーモ\")")    ;ｻｰﾓ
         (start_dialog)
         (unload_dialog DCL_ID)
      (princ)
    (POINT1)
;     
    (setq T1 (getstring "\n１:接点記号を指示:(例：TM)"))
             (cond ((= T1 "") (setq T1 "TM")))
;     
    (setq T2 (getstring "\n２:接点番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;8.ＡＢ接点
;;=================================================================================================
  (defun C:ABC ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nＡＢ接点")
    (POINT1)
    (setq SYM "SBAC")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：AX)"))
             (cond ((= T1 "") (setq T1 "AX")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;9.ＢＡ接点
;;=================================================================================================
  (defun C:BAC ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nＢＡ接点")
    (POINT1)
    (setq SYM "SABC")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：AX)"))
             (cond ((= T1 "") (setq T1 "AX")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;10.サーマル接点
;;=================================================================================================
  (defun C:ATH ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nサーマル接点")
    (POINT1)
    (setq SYM "SATR")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：49)"))
             (cond ((= T1 "") (setq T1 "49")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;11.サーマル接点
;;=================================================================================================
  (defun C:BTH ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nサーマル接点")
    (POINT1)
    (setq SYM "SBTR")
;     
    (setq T1 (getstring "\n１:リレ－記号を指示:(例：49)"))
             (cond ((= T1 "") (setq T1 "49")))
;     
    (setq T2 (getstring "\n２:リレ－番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;12.マグネット等のコイル（属性無し）
;;=================================================================================================
  (defun C:XRY ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)
    (setq KIKI "\nコイル（属性無し）")
    (POINT1)
    (setq SYM "XX")
;     
    (setq T1 (getstring "\n１:機器記号を指示:(例：52X)"))
             (cond ((= T1 "") (setq T1 "52")))
;     
    (setq T2 (getstring "\n２:機器番号を指示:(例：-1)"))
             (cond ((= T2 "") (setq T2 "-1")))
    (IN01)
    (GC)
  )
;;=================================================================================================
;;13.入出線　下向き
;;=================================================================================================
  (defun C:SEN1 ( / SYM PC NAM)
;      (setq *error* *err1*)
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 0)
      (setq DCL_ID (load_dialog "X:/DCL/INOUT.DCL"))
      (if (not (new_dialog "SEN" DCL_ID) )(EXIT))
;  図枠の選択用ダイアログボックス
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"SEN2\")(setq NAM \"入線\")")
      (action_tile "2" "(setq SYM \"SEN2\")(setq NAM \"出線\")")
         (start_dialog)
         (unload_dialog DCL_ID)
      (princ)(princ "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" NAM)
      (GC)
  )
;;=================================================================================================
;;14.入出線　上向き
;;=================================================================================================
  (defun C:SEN2 ( / SYM PC NAM)
      (setq *error* *err1*)
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 0)
      (setq DCL_ID (load_dialog "X:/DCL/INOUT.DCL")) 
      (if (not (new_dialog "SEN" DCL_ID) )(EXIT))
;  図枠の選択用ダイアログボックス
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "1" "(setq SYM \"SEN1\")(setq NAM \"入線\")")
      (action_tile "2" "(setq SYM \"SEN1\")(setq NAM \"出線\")")
         (start_dialog)
         (unload_dialog DCL_ID)
      (princ)(princ "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" NAM)
    (GC)
  )

;;=================================================================================================
;;15.SLCの回路入力      SPL1S.DWG    SPLS.DWG        部品表属性無し
;;=================================================================================================
  (defun C:SSLC1 ( / SYM T1 T2 PC P1 P2 P12 PY1 ENT)
    (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (setq DCL_ID (load_dialog "X:/DCL/SSLC1.DCL"))
      (if (not (new_dialog "SSLC1" DCL_ID) )(EXIT))
      (action_tile "cancel" "(done_dialog)(exit)")
      (action_tile "T"  "(setq SYM \"SPL1S\")(setq KIKI \"集合表示灯\")")
      (action_tile "N"  "(setq SYM \"SPLS\") (setq KIKI \"集合表示灯\")")
      (action_tile "CR" "(setq N3 \"赤\")(setq N1 \"RL\")")
      (action_tile "CG" "(setq N3 \"緑\")(setq N1 \"GL\")")
      (action_tile "CO" "(setq N3 \"橙\")(setq N1 \"OL\")")
      (action_tile "CY" "(setq N3 \"黄\")(setq N1 \"YL\")")
         (start_dialog)(unload_dialog DCL_ID)(princ)
;       
      (if (= SYM "SPL1S")
          (progn
             (setq MSG (strcat "\n" KIKI "の挿入点を指示："))
             (setq PC (getpoint MSG))
             (setq PX1 (list (- (car PC) 10) (- (cadr PC) 5)))
             (setq PX2 (list (+ (car PC) 10) (+ (cadr PC) 5)))
             (setq SS (ssget "C" PX1 PX2))
             (if (/= SS nil) (progn (setq ENT (ssname ss 0))))
             (if (/= ENT nil)
                 (progn
                    (setq PY1 (list (car PC) (+ (cadr PC) 10)))
                    (setq P12 (list (car PC) (- (cadr PC) 100)))
                    (command "BREAK" PY1 "F" PC P12)
                 )
                 (prompt "\n線のカットはしません");else  ;縦線が存しないなら 
             )
          )
          (setq PC (getpoint (strcat "\n" KIKI "の挿入点を指示：")))
      )
      (setq N2 (getstring "\nランプ番号を指示:(例：-1)"))
                             (cond ((= N2 "") (setq N2 "-1")))
      (command "COLOR" 2)
      (command "INSERT" SYM PC "1" "1" "0" N1 N2 N3 "" "" "")
;;後始末
         (setvar "CMDECHO" 1)
         (setvar "BLIPMODE" 1)
         (setq P1 nil P2 nil PC nil PY1 nil P12 nil)
         (redraw)(GC)
  )
;;=================================================================================================
;;(setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
(prompt "\n接点入力.LSP....OK")
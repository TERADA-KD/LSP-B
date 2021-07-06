;;機器外形情報ファイルから機器外形図を配置。 D_IN.LSP   2003/JUL   By T.Terada
;;
(defun C:D_IN (;/ DAT F n PC PBr PNF P52PCT PAM PTB PSC PCu PWH PBZ PPL PCS PSS PRY PTM PPB PTr 
               ;  PR1 PR2 PR3 PR4 PR5 PXX PCP KG En SYM NUM k No H FDAT DATALL NAM1 NAM2 PFX LN NEN
              )


;＊プログラム変更の事！！！！！！！！
;   １．Ｚ２に記入される記号は機器番号とする。
;   ２．Ｚ２は空きにして、ＰＮｏを使用する。
;   ３．上記で動作するように変更する事。
;   ４．Ｍ＿ＩＮ、ＭＡＩＮ、ＢＵＮ－ＭＣＢも変更の事。

   (setq *error* *err1*)
   (SETVAR "CMDECHO" 0)
;;保存するファイル名の取得と設定 
   (setq FDAT nil)                   ;盤毎の外形ﾃﾞｰﾀｰﾌｧｲﾙ名を収納
   (setq DATALL nil)                 ;各図面の外形ﾃﾞｰﾀｰを１つのﾌｧｲﾙに統合
   (setq DAT nil)
   (setq NAM1 (getvar "dwgname"))
   (if (= NAM1 "Drawing1.dwg")           ;図番が Drawing1の場合。実験用
       (progn
          (princ "\nDrawingの場合")
          (setq PFX "X:/Ddat/")
          (setq NAM2 "Drawing1.DAT")
          (setq FDAT (dos_find NAM2 PFX))
          (setq En (length FDAT))
       )
       (progn ;else                      ;正規の図番の場合
          (setq LN  (strlen NAM1))
          (setq NEN (substr NAM1 1 1))
          (setq PFX (strcat "X:/Ddat/" NEN "/"))
          (setq NAM1 (substr NAM1 3 (- LN 8)))
          (setq NAM2 (strcat NAM1 "*" ".DAT"))  ;  外形情報ファイル
          (setq FDAT (dos_find NAM2 PFX))
          (setq En (length FDAT))
       )
   )
;;盤単位での全外形データーを取得
   (setq i 0)
   (repeat En ;;ファイルの読み込み
       (setq FNAM (nth i FDAT))      ; (princ "\nFNAM=")(princ FNAM)
       (setq F  (open FNAM "r"))
       (while (/= (read-line F) nil)(setq DAT (read (read-line F))))
       (close F)
       (setq DATALL (append DATALL DAT))
       (setq i (+ i 1))
   )
   (prompt "\n  全外形情報、読込完了...")
;  (princ "\nﾃﾞｰﾀｰ=")(princ DATALL)
;;回路図のシンボルから外形情報を呼びだす。
    
   (if (= (getvar "USERI5") 0)(setq PC (getpoint "\n  挿入点を指示... ")))
   (if (= (getvar "USERI5") 1)(setq PC '(900 2200)))
   
;;回路図と関係付ける外形機器の配置基点
   (setq PBr PC)                                       ;Br
                                                       ;MM 動力盤用主幹ﾌﾞﾚｰｶｰ
                                                       ;BM 分電盤用主幹ﾌﾞﾚｰｶｰ
   (setq PNF (list (+ (car PC) 600)(cadr PC)))         ;NF 動力盤用ﾌﾞﾚｰｶｰ
   (setq P52 (list (+ (car PC) 600)(- (cadr PC) 200))) ;52,06,42
   (setq P49 (list (+ (car PC) 600)(- (cadr PC) 400))) ;49
   (setq PCT (list (+ (car PC) 600)(- (cadr PC) 600))) ;CT
   (setq PAM (list (+ (car PC) 600)(- (cadr PC) 800))) ;AM,VM
   (setq PTB (list (+ (car PC) 600)(- (cadr PC) 900))) ;TB
   (setq PPL (list (+ (car PC) 600)(- (cadr PC) 1000)));PL or RL or GL or WL or OL or YL
   (setq PCS (list (+ (car PC) 600)(- (cadr PC) 1100)));CS
   (setq PPB (list (+ (car PC) 600)(- (cadr PC) 1200)));PB
   (setq PSC (list (+ (car PC) 600)(- (cadr PC) 1400)));SC
   ;Cu
   ;WH
   ;RY,5X=>52X,4X=>49X,AX,CX,FX,TX,X-,3X=>33X,AL=>ALX,AT=>ATX,AS=>ASX,2X=>27X,8X=>84X
   ;TM
   ;Tr
   ;33 液面
   (setq PR1 (list (car PC)(- (cadr PC) 500)))         ;R1 RRY
   ;R2 RTr
   ;R3 T/U
   ;R4 6AT/U
   ;R5 接点T/Uの配置基点 伝送ﾕﾆｯﾄ
   ;BZ
   ;SS
   
   
   ;XX その他機器
   (setq i 0)
   (setq PCP (cadr PBr))
   (setq KG 1)(setq k 1)
   (setq En (length DatALL))
   (repeat En
       (setq SYM (car (nth i DatALL)))       ;(princ "\nSYM=")(princ SYM)
       (setq NUM (car (cdr (nth i DatALL)))) ;(princ "\nNUM=")(princ NUM)
       (setq k (rem KG 2))
;       (princ "\nK=")(princ k)
;--------------------------------------------------------------------------------------------
       (princ "\nNUM =")(princ NUM)
       (setq NN (strlen NUM)) (setq N1 1) (setq TXT nil)
       (while (/= TXT "-")
          (setq TXT (substr NUM N1 1)) (setq N1 (1+ N1))
       )
       (setq N2 (- N1 2))(setq N3 (+ N2 1))
       (setq PFX (substr NUM 1  N2));機器番号
       (setq SFX (substr NUM N3 NN));回路番号
;       (princ "\nNUM =")(princ NUM)(princ "\nPFX =")(princ PFX)(princ "\nSFX =")(princ SFX)
   ;--------------------------------------------------------------------------------------------
       (if (or (= PFX "Br")(= PFX "R1")(= PFX "R2"));分電盤の分岐ﾌﾞﾚｰｶｰ・ﾘﾓｺﾝﾘﾚｰ・ﾘﾓｺﾝﾄﾗﾝｽ
           (progn
              (if (or (= PFX "R1")(= PFX "R2"))
                  (command "INSERT" SYM (list (car PBr)(- PCP 100)) "1" "1" "0" "" "" "" "" "" "")
                  (command "INSERT" SYM (list (car PBr) PCP) "1" "1" "0" "" "" "" "" "" "")
              )
             ;;属性からﾌﾞﾚｰｶｰの高さを取得
              (setq n 0)(setq AA nil)(setq BB nil)(setq CC nil)(setq DD nil)
              (setq AA (ssget "L" ))
              (while (setq BB (ssname AA n))
                  (setq CC (entnext BB)) (setq DD (entget CC))  ; 図形リスト
                      (while (/= (cdr (assoc 0 DD)) "SEQEND")   ; SEQENDまで繰り返し
                          (if (and (= (cdr (assoc 0 DD)) "ATTRIB")(= (cdr (assoc 2 DD)) "D4"))
                              (setq H (cdr (assoc 1 DD)))       ; ﾌﾞﾚｰｶｰの高さを得る
                          );if
                          (setq CC (entnext CC)  DD (entget CC))
                      );while
                      (setq n (1+ n))
               );while
              (setq H (atof H))
              (if (= PFX "Br")
                  (progn
                     (if (= k 1)(command "INSERT" "CH-L" (list (car PBr) PCP) "1" "1" "0"))
                     (if (= k 0)(command "INSERT" "CH-R" (list (car PBr) PCP) "1" "1" "0"))
                  )
              )
              (if (= k 0)(setq PCP (- PCP H)))
           );progn
       );if
       (setq i (+ i 1))
       (setq KG (+ KG 1))
      ;--------------------------------------------------------------------------------------------
       (if (or (= PFX "MCB")(= PFX "ELB"));ﾌﾞﾚｰｶｰ
           (progn
              (command "INSERT" SYM PNF "1" "1" "0" PFX SFX "" "" "" "")
              (command "INSERT" "NP1-1" (list (car PNF)(+ (cadr PNF) 35)) "1" "1" "0")
              (setq PNF (list (+ (car PNF) 100)(cadr PNF)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (or (= PFX "MC")(= PFX "52")(= PFX "42")(= PFX "06"));MC 52 42 6
           (progn
              (command "INSERT" SYM P52 "1" "1" "0" PFX SFX "" "" "" "")
              (setq P52 (list (+ (car P52) 100)(cadr P52)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "49");49
           (progn
              (command "INSERT" SYM P49 "1" "1" "0" PFX SFX "" "" "" "")
              (setq P49 (list (+ (car P49) 100)(cadr P49)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "CT");CT
           (progn
              (command "INSERT" SYM PCT "1" "1" "0" PFX SFX "" "" "" "")
              (setq PCT (list (+ (car PCT) 100)(cadr PCT)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "AM");AM
           (progn
              (command "INSERT" SYM PAM "1" "1" "0" PFX SFX "" "" "" "")
              (command "COLOR" 2);黄色
              (command "TEXT" "S" "M0" "C" (list (car PAM)(- (cadr PAM) 0)) "35" "0" "A")
              (command "COLOR" 4);水色
              (setq PAM (list (+ (car PAM) 100)(cadr PAM)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "TB");TB
           (progn
              (command "INSERT" SYM PTB "1" "1" "0" PFX SFX "" "" "" "")
              (setq PTB (list (+ (car PTB) 100)(cadr PTB)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (or (= PFX "PL")(= PFX "WL")(= PFX "GL")(= PFX "RL")(= PFX "OL")(= PFX "YL"));PL
           (progn
              (command "INSERT" SYM PPL "1" "1" "0" PFX SFX "" "" "" "")
              (setq PPL (list (+ (car PPL) 100)(cadr PPL)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "CS");CS
           (progn
              (command "INSERT" SYM PCS "1" "1" "0" PFX SFX "" "" "" "")
              (setq PCS (list (+ (car PCS) 100)(cadr PCS)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "PB");PB
           (progn
              (command "INSERT" SYM PPB "1" "1" "0" PFX SFX "" "" "" "")
              (setq PPB (list (+ (car PPB) 100)(cadr PPB)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
       (if (= PFX "SC");SC
           (progn
              (command "INSERT" SYM PSC "1" "1" "0" PFX SFX "" "" "" "")
              (setq PSC (list (+ (car PSC) 100)(cadr PSC)))
           );progn
       );if
      ;--------------------------------------------------------------------------------------------
   );repeat
   (prompt "\n  作図完了...")
   (command "redraw")
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0) ;Access自動作図のフラグ
)

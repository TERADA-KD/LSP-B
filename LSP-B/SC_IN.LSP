;;高圧盤用、コンデンサー回路入力 標準図の属性を書換え    SC_IN.LSP  2003/DEC/31  By T.Terada
;;
;;C++ダイアログから取得する変数
;; 回路番号          SCN :
;; ｺﾝﾃﾞﾝｻｰ容量       Var :
;; パーセント        PAR : 6% = "6%", 13% = "13%"
;; 主  幹            MAIN: "VCS", "LBS", "PC"
;; ＶＣＳタイプ      MTP : "KOTEI", "DRAW"
;; 回路電圧          HV  : 6600V = "V6", 3300V = "V3"
;; 周波数            HZ  : 60Hz  = "60Hz" 50Hz = "50Hz"
;; ＳＣメーカー      SCMK: ｼｽﾞｷ  = "19" , ﾆﾁｺﾝ  = "57"
;; ＳＲメーカー      SRMK: ｼｽﾞｷ  = "19" , ﾆﾁｺﾝ  = "57", 電気計器 = "107"
;; オプション       OP1-3: ﾘﾐｯﾄ  = "L"
;; 支給品かどうか    SHI : Y = "*"  N = ""

(defun C:SC_IN ( / AA BB CC DD ENT n DATATT ZOKU N2 MN GYO RETU SEL1 SEL DAT KLBS KPC KCT HSq Sq STD
                   SCN Var PAR MAIN MTP HV HZ SCMK SRMK OP1 OP2 OP3 SHI DAT
                   LBSNo PFFNo PCNo VCSNo VCFNo SCNo SRNo KPF YPF YPC KPFF KVCS DVCS YSC YSR KSSC KSSR
                   KNSC KNSR KDSR P1 P2 P3 P31 P32 P33 FP1 FP2 FP3 FP4
               )
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
  (setq DAT nil SCN nil Var nil PAR nil MAIN nil MTP nil HV nil SCMK nil SRMK nil
                                                                 OP1 nil OP2 nil OP3 nil SHI nil)
  (startapp "VBeep" "")
  (C:MKR)              ;;基本メーカーを調べる
  (CSCH)               ;;C++ 入力用ダイアログを呼びだし

;;メーカー番号の決定
    (if (= MAKER "0")(setq MN "1")) (if (= MAKER "1")(setq MN "4"))
    (if (= MAKER "2")(setq MN "1")) (if (= MAKER "3")(setq MN "4"))

;標準図の決定とEXCELの読込む"行"を決定
    (if (or  (= MAIN "LBS")(= MAIN "PC"))  (progn (setq STD "*ST-SC1")(setq GYO 31)))
    (if (and (= MAIN "VCS")(= MTP "KOTEI"))(progn (setq STD "*ST-SC2")(setq GYO 31)))
    (if (and (= MAIN "VCS")(= MTP "DRAW" ))(progn (setq STD "*ST-SC3")(setq GYO 31)))

;;回路電圧と周波数でEXCELの読込む"行"を決定
    (if (and (= HV "V6")(= HZ "60Hz"))(setq GYO 10))  (if (and (= HV "V3")(= HZ "60Hz")) (setq GYO 40))
    (if (and (= HV "V6")(= HZ "50Hz"))(setq GYO 70))  (if (and (= HV "V3")(= HZ "50Hz")) (setq GYO 100))

;;SC容量でEXCELの読込む"列"を決定
    (if (or (= MAKER "0")(= MAKER "2"));三菱の場合
        (progn (if (= Var   "10KVar")(setq RETU "D")) (if (= Var   "15KVar")(setq RETU "E"))
               (if (= Var   "20KVar")(setq RETU "F")) (if (= Var   "25KVar")(setq RETU "G"))
               (if (= Var   "30KVar")(setq RETU "H")) (if (= Var   "50KVar")(setq RETU "I"))
               (if (= Var   "75KVar")(setq RETU "J")) (if (= Var  "100KVar")(setq RETU "K"))
               (if (= Var  "150KVar")(setq RETU "L")) (if (= Var  "200KVar")(setq RETU "M"))
               (if (= Var  "250KVar")(setq RETU "N")) (if (= Var  "300KVar")(setq RETU "O"))
               (if (= Var  "400KVar")(setq RETU "P")) (if (= Var  "500KVar")(setq RETU "Q"))  );end progn
    )
    (if (or (= MAKER "1")(= MAKER "3"));富士の場合
        (progn (if (= Var   "10KVar")(setq RETU "AD")) (if (= Var   "15KVar")(setq RETU "AE"))
               (if (= Var   "20KVar")(setq RETU "AF")) (if (= Var   "25KVar")(setq RETU "AG"))
               (if (= Var   "30KVar")(setq RETU "AH")) (if (= Var   "50KVar")(setq RETU "AI"))
               (if (= Var   "75KVar")(setq RETU "AJ")) (if (= Var  "100KVar")(setq RETU "AK"))
               (if (= Var  "150KVar")(setq RETU "AL")) (if (= Var  "200KVar")(setq RETU "AM"))
               (if (= Var  "250KVar")(setq RETU "AN")) (if (= Var  "300KVar")(setq RETU "AO"))
               (if (= Var  "400KVar")(setq RETU "AP")) (if (= Var  "500KVar")(setq RETU "AQ"))  );end progn
    );endif

 ;EXCELデーターの読込
    (xls_open "X:/XLS/ＳＣ回路容量表" "r")
       (repeat 25                             ;読み込む行の数
          (setq SEL1 (strcat RETU (itoa GYO)))
          (setq SEL (xls_getval SEL1))
          (setq DAT (append DAT (list SEL)))
          (setq GYO (+ GYO 1))
       )
;   (princ "\nDAT=")(princ DAT)(prompt "\n")
    (xls_close)

;;-------------------------------------------------------------------------------------------------
;;読込だEXCELデーターを変数にセット
 ;LBS
    (if (or (= MAIN "LBS")(= MAIN "PC"))
        (progn (setq KLBS (nth 0 DAT))(setq KPF (nth 2 DAT))(setq YPF (nth 3 DAT)))
        (progn (setq KLBS "")(setq KPF "")(setq YPF ""))
    )
 ;PC
    (if (= MAIN "PC") (progn (setq KPC (nth 4 DAT))(setq YPC (nth 5 DAT))))
 ;PFF + 固定VCSの場合
    (if (and (= MAIN "VCS")(= MTP "KOTEI"))
        (progn (setq KPFF (nth 1 DAT))(setq KPF (nth 2 DAT))(setq YPF (nth 3 DAT))(setq KVCS (nth 6 DAT)))
        (setq KVCS "")
    )
 ;引出VCSの場合
    (if (and (= MAIN "VCS")(= MTP "DRAW"))
        (setq DVCS (nth 7 DAT))
        (setq DVCS "")
    )
 ;高圧母線サイズ
    (setq HSq (nth 10 DAT))
 ;SC,SR 6%の選定
    (if (= PAR "6%")
        (progn (setq YSC (nth 12 DAT))(setq YSR (nth 11 DAT))
               (if (= SCMK "S")(progn (setq KSSC (nth 16 DAT))(setq KSSR (nth 15 DAT))));シズキ
               (if (= SCMK "N")(progn (setq KNSC (nth 18 DAT))(setq KNSR (nth 17 DAT))));ニチコン
               (if (= SCMK "D") (setq KDSR (nth 19 DAT)))
        )
    )
 ;SC,SR 13%の選定
    (if (= PAR "13%")
        (progn (setq YSC (nth 14 DAT))(setq YSR (nth 13 DAT))
               (if (= SCMK "S")(progn (setq KSSC (nth 21 DAT))(setq KSSR (nth 20 DAT))));シズキ
               (if (= SCMK "N")(progn (setq KNSC (nth 23 DAT))(setq KNSR (nth 22 DAT))));ニチコン
               (if (= SCMK "D") (setq KDSR (nth 24 DAT)))
        )
    )
;;属性値の設定
 ;機器番号
    (setq LBSNo (strcat "LBS-" SCN)) (setq PFFNo (strcat "PFF-" SCN)) (setq PFNo  (strcat "PF-" SCN))
    (setq PCNo  (strcat "PC-"  SCN)) (setq VCSNo (strcat "VCS-" SCN)) (setq VCFNo (strcat "F-"  SCN))
    (setq SCNo  (strcat "SC-"  SCN)) (setq SRNo  (strcat "SR-"  SCN)) (setq PCFNo  (strcat "PCF-"  SCN))

;;挿入図形の位置計算
    (ST1)(prompt "\n挿入点を指示... ")(command "INSERT" STD pause "1" "0")
    (setq P1 (getvar "LASTPOINT"))
    (setq P2 (list (car P1) (- (cadr P1) 150)))         ;PFの挿入点
    (setq P3 (list (car P1) (- (cadr P1) 300)))         ;PFの終点
       (setq P31 (list (- (car P3) 40) (cadr P3)))      ;LBSの補助線 1
       (setq P32 (list (- (car P3) 100)(cadr P3)))      ;LBSの補助線 2
       (setq P33 (list (car P32)(- (cadr P3) 150)))     ;LBSの補助線 3

;;標準図の図形選択範囲
  (setq FP1 (list (- (car P1) 150) (+ (cadr P1) 0)))   ;フェンス左上
  (setq FP2 (list (- (car P1) 150) (- (cadr P1) 1600)));フェンス左下
  (setq FP3 (list (+ (car P1) 350) (- (cadr P1) 1600)));フェンス右下
  (setq FP4 (list (+ (car P1) 350) (+ (cadr P1) 0)))   ;フェンス右上

;;属性の編集
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
        ;;属性の書込データーをセット                       ;書込みたい属性値を一旦DATATTに書き込む。
        (if (= (cdr (assoc 2 ENT)) "QLBS1") ;1.LBSの属性値 
            (setq DATATT (list "LBS" LBSNo KLBS "" "200A" "7.2/3.6KV" "" "" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QPF2")  ;2.PFの属性値 
            (setq DATATT (list "PF" PFNo KPF "7.2KV" YPF "40KA" "" "" "" "" "" MN "3" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QPFF1") ;3.PFFの属性値 
            (setq DATATT (list "PFF" PFFNo KPFF "7.2KV" "" "" "" "" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QVCS1") ;4.VCSの属性値 
            (setq DATATT (list "VCS" VCSNo KVCS "1000?" "200A" "" "常時励磁" "AC100V" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QSRH")  ;5.SRの属性値 
            (setq DATATT (list "SR" SRNo YSR PAR "243V" SHI HZ "" "" "" "" SRMK "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QSCH")  ;6.SCの属性値 
            (setq DATATT (list "SC" SCNo YSC PAR "7020V" SHI HZ "" "" "" "" SCMK "1" "" "")))
        
        ;;属性の書込対象となるシンボル
        (if (or (= (cdr (assoc 2 ENT)) "QLBS1")  (= (cdr (assoc 2 ENT)) "QPF2")
                (= (cdr (assoc 2 ENT)) "QPFF1")  (= (cdr (assoc 2 ENT)) "QVCS1")
                (= (cdr (assoc 2 ENT)) "QSRH")   (= (cdr (assoc 2 ENT)) "QSCH")
            );or
            (progn
;              (princ "ENT=")(princ (cdr (assoc 2 ENT)))
               (setq N2 1)
               (setq CC (entnext BB)  DD (entget CC))
               (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
                         (if (= N2 1) (setq ZOKU "PNA"))(if (= N2 2) (setq ZOKU "PNO"))
                         (if (= N2 3) (setq ZOKU "T1")) (if (= N2 4) (setq ZOKU "T2"))
                         (if (= N2 5) (setq ZOKU "T3")) (if (= N2 6) (setq ZOKU "T4"))
                         (if (= N2 7) (setq ZOKU "T5")) (if (= N2 8) (setq ZOKU "T6"))
                         (if (= N2 9) (setq ZOKU "T7")) (if (= N2 10)(setq ZOKU "T8"))
                         (if (= N2 11)(setq ZOKU "T9")) (if (= N2 12)(setq ZOKU "MN"))
                         (if (= N2 13)(setq ZOKU "QT")) (if (= N2 14)(setq ZOKU "Z1"))
                         (if (= N2 15)(setq ZOKU "Z2"))
                         (if (= (cdr (assoc 2 DD)) ZOKU)
                             (entmod (subst (cons 1 (nth (1- N2) DATATT))(assoc 1 DD) DD));属性を変更
                         );if
                      );progn
                  );if
                  (setq N2 (1+ N2))
                  (setq CC (entnext CC) DD (entget CC))
               );while
            );progn
        );if
        ;;線サイズの変更
        (if (= (cdr (assoc 2 ENT)) "QYAH")
            (progn
;               (princ "\nYA=")(princ (cdr (assoc 2 ENT)))
               (if (= (cdr (assoc 2 ENT)) "QYAH")(setq Sq HSq))
               (setq CC (entnext BB)  DD (entget CC))
               (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
                         (setq ZOKU "N1")
                         (if (= (cdr (assoc 2 DD)) ZOKU)
                             (entmod (subst (cons 1 Sq)(assoc 1 DD) DD));属性を変更
                         );if
                      );progn
                  );if
                  (setq CC (entnext CC) DD (entget CC))
               );while
            );progn
        );if
        
        ;;主幹がＰＣの場合 不要なシンボルを消去する
        (if (and (= MAIN "PC")(= STD "*ST-SC1")(= (cdr (assoc 2 ENT)) "QLBS1"))(entdel BB)); LBSを消す
        (if (and (= MAIN "PC")(= STD "*ST-SC1")(= (cdr (assoc 2 ENT)) "QPF2")) (entdel BB)); PFを 消す

        (setq n (1+ n))
   );while
;;==================================================================================================
;;主幹がPCの場合
    (if (and (= MAIN "PC")(= STD "*ST-SC1")); 主幹がＰＣの場合
        (progn
          (command "INSERT" "QPC"  P2 "1" "1" "0"
                            "PC"  PCNo   KPC "" "7.2KV" "" "" "" "" "" "" "11" "3" "" "")
          (command "INSERT" "QPF2" P2 "1" "1" "0"
                            "PCF" PCFNo "FT" "7.2KV" YPC "1.5KA" "" "" "" "" "" "11" "3" "" "")
        )
    )
;;主幹がLBSの場合
    (if (and (= MAIN "LBS")(= STD "*ST-SC1"))
        (progn
           (command "PLINE" P31 "W" "0" "" P32 P33 "")
           (command "INSERT" "KOUTEN" P31 "1" "1" "0")
        )
    )
;;======================================================================================================
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (redraw)
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
   (prompt "\nＳＣ回路、作図完了！！")
)
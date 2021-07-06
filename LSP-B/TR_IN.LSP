;;低圧配電盤用、トランス回路入力（単線図） 標準図の属性を書換え    TR_IN.LSP  2003/DEC/3  By T.Terada
;;
;;C++ダイアログから取得する変数
;; TRN:ﾄﾗﾝｽ番号    KVA:ﾄﾗﾝｽ容量
;; G6 :回路電圧    V3=3300V, V6=6600V
;; G8 :高圧主幹    L=LBS, P=PC, N=無
;; G12:相・電圧    33W2=3P3W 210V, 33W4=3P3W 440V, 34W4=3P4W 440V, 13W=1P3W, LST=低圧ｽｺｯﾄ, HST=高圧ｽｺｯﾄ
;; G1 :Trメーカー  50=支給品, 1=三菱, 4=富士, 99=その他
;; G10:冷却方式    OIL=油例, MLD=モールド
;; G4 :シャント    N=無,Y=有
;; G2 :ﾄﾗﾝｽの種類  T1=低損, T2=高効率, T3=超高効率, T4=アモルファス
;; G9 :オプション  C1=防振ゴム, C2=ﾀﾞｲﾔﾙ温度計, C3=混触防止板, C4=車輪
;; G14:ﾒｰﾀｰの種類  K12=120角, KW=広角, KS=切替付, KM=ﾏﾙﾁﾒｰﾀｰ
;; G3:ＷＨＭ       WH0=無, WH1=普通, WH2=ﾊﾟﾙｽ付, WH3=電子式, WH4=ｺﾝﾊﾟｸﾄ
;; G5 :サーマル    N=無,Y=有
;; G7 :ＤＭＡ      N=無,Y=有
(defun C:TR_IN ( / n AA BB CC DD ENT DAT DATATT ZOKU N2 Sq STD MN GYO RETU
                   TRN G1 G2 G3 G4 G5 G6 G7 G8 G9 G10 G12 KVA
                   SEL1 SEL DAT KAM KVM KWH KLBS KPC KCT AMS VMS KSH HSq  LSq STSq KLGR KZCT ZCTS KTH KVT
                   KF TH FS REI TYP OP TrPNo LBSPNo PFPNo PCPNo CTPNo AMPNo ASPNo MMPNo DMAPNo VMPNo VSPNo
                   FPNo WHPNo SHTPNo ZCTLPNo THPNo KVTPNo PW VV NN FN FP1 FP2 FP3 FP4 P1 P2 
                   P3 P31 P32 P33 P4 P5 P6 P7 P8 P81 P82 P83 P84 P85 P9 P91 P92 P93 P94 P95 P10 P11 P12
               )
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
  (setq DAT nil TRN nil KVA nil G1 nil G2 nil G4 nil G5 nil G6 nil G7 nil G8 nil G9 nil G10 nil G12 nil G14 nil)
   
  (C:MKR)              ;;基本メーカーを調べる
  (CTr1)               ;;C++ 入力用ダイアログを呼びだし
  (setq LG "N")        ;;LGR無しの設定（Ｃのダイアログには設定無し）
;;メーカー番号の決定
    (if (= MAKER "0")(setq MN "1")) (if (= MAKER "1")(setq MN "4"))
    (if (= MAKER "2")(setq MN "1")) (if (= MAKER "3")(setq MN "4"))

;標準図とトランスシンボルの決定
    (if (= G12 "33W2")(progn (setq STD "*ST-TRS01")(setq SYMTr "QTRH33")))
    (if (= G12 "33W4")(progn (setq STD "*ST-TRS01")(setq SYMTr "QTRH34")))
    (if (= G12 "34W4")(progn (setq STD "*ST-TRS01")(setq SYMTr "QTRH34")))
    (if (= G12 "13W" )(progn (setq STD "*ST-TRS04")(setq SYMTr "QTRH13")))
    (if (= G12 "HST" )(progn (setq STD "*ST-TRS05")(setq SYMTr "QTRHST")))
    (if (= G12 "LST" )(progn (setq STD "*ST-TRS06")(setq SYMTr "QTRHST")))

;;回路電圧とトランスの相・電圧でEXCELの読込む"行"を決定
    (if (and (= G6 "V6")(= G12 "33W2"))(setq GYO 31))  (if (and (= G6 "V3")(= G12 "33W2")) (setq GYO 391))
    (if (and (= G6 "V6")(= G12 "33W4"))(setq GYO 91))  (if (and (= G6 "V3")(= G12 "33W4")) (setq GYO 451))
    (if (and (= G6 "V6")(= G12 "34W4"))(setq GYO 151)) (if (and (= G6 "V3")(= G12 "34W4")) (setq GYO 511))
    (if (and (= G6 "V6")(= G12 "13W" ))(setq GYO 211)) (if (and (= G6 "V3")(= G12 "13W" )) (setq GYO 571))
    (if (and (= G6 "V6")(= G12 "HST" ))(setq GYO 271)) (if (and (= G6 "V3")(= G12 "HST" )) (setq GYO 631))
    (if (and (= G6 "V6")(= G12 "LST" ))(setq GYO 331))

;;トランス容量でEXCELの読込む"列"を決定
    (if (or (= MAKER "0")(= MAKER "2"));三菱の場合
        (progn (if (= KVA   "20KVA")(setq RETU "D")) (if (= KVA   "30KVA")(setq RETU "E"))
               (if (= KVA   "50KVA")(setq RETU "F")) (if (= KVA   "75KVA")(setq RETU "G"))
               (if (= KVA  "100KVA")(setq RETU "H")) (if (= KVA  "150KVA")(setq RETU "I"))
               (if (= KVA  "200KVA")(setq RETU "J")) (if (= KVA  "300KVA")(setq RETU "K"))
               (if (= KVA  "500KVA")(setq RETU "L")) (if (= KVA  "750KVA")(setq RETU "M"))
               (if (= KVA "1000KVA")(setq RETU "N")) (if (= KVA "1500KVA")(setq RETU "O"))    );end progn
    )
    (if (or (= MAKER "1")(= MAKER "3"));富士の場合
        (progn (if (= KVA   "20KVA")(setq RETU "AD")) (if (= KVA   "30KVA")(setq RETU "AE"))
               (if (= KVA   "50KVA")(setq RETU "AF")) (if (= KVA   "75KVA")(setq RETU "AG"))
               (if (= KVA  "100KVA")(setq RETU "AH")) (if (= KVA  "150KVA")(setq RETU "AI"))
               (if (= KVA  "200KVA")(setq RETU "AJ")) (if (= KVA  "300KVA")(setq RETU "AK"))
               (if (= KVA  "500KVA")(setq RETU "AL")) (if (= KVA  "750KVA")(setq RETU "AM"))
               (if (= KVA "1000KVA")(setq RETU "AN")) (if (= KVA "1500KVA")(setq RETU "AO"))  );end progn
    );endif

 ;EXCELデーターの読込
    (xls_open "X:/XLS/トランス回路容量表" "r")
       (repeat 33                             ;読み込む行の数
          (setq SEL1 (strcat RETU (itoa GYO)))
          (setq SEL (xls_getval SEL1))
          (setq DAT (append DAT (list SEL)))
          (setq GYO (+ GYO 1))
       )
;   (princ "\nDAT=")(princ DAT)(prompt "\n")
    (xls_close)

;;読込だEXCELデーターを変数にセット
 ;AM VM
    (if (and (= G14 "K12")(= G7 "N")) (progn (setq KAM (nth 7 DAT))(setq KVM (nth 13 DAT)) ));120角
    (if (and (= G14  "KW")(= G7 "N")) (progn (setq KAM (nth 8 DAT))(setq KVM (nth 14 DAT)) ));広角
    (if (and (= G14  "KS")(= G7 "N")) (progn (setq KAM (nth 9 DAT))(setq KVM (nth 15 DAT)) ));切替付
    (if (and (= G14  "KW")(= G7 "Y")) (progn (setq KAM (nth 11 DAT))(setq KVM (nth 14 DAT)) ));DMA
    (if (and (= G14  "KM")(= G7 "Y")) (progn (setq KAM (nth 10 DAT))(setq KVM "") ));ﾏﾙﾁﾒｰﾀｰ
    (if (and (= G14  "KM")(= G7 "N")) (progn (setq KAM (nth 10 DAT))(setq KVM "") ));ﾏﾙﾁﾒｰﾀｰ
 ;WHM
    (if (= G3 "WH0")(setq KWH ""));無
    (if (= G3 "WH1")(setq KWH (nth 17 DAT)));普通型
    (if (= G3 "WH2")(setq KWH (nth 18 DAT)));ﾊﾟﾙｽ付
    (if (= G3 "WH3")(setq KWH (nth 19 DAT)));電子式
    (if (= G3 "WH4")(setq KWH (nth 20 DAT)));ｺﾝﾊﾟｸﾄ
 ;OTHER
    (setq KLBS (nth 0 DAT)) (setq KPF (nth 1 DAT)) (setq PFS (nth 2 DAT))   ;LBS
    (setq KPC  (nth 3 DAT)) (setq PCS (nth 4 DAT))                          ;PC
    (setq KCT  (nth 5 DAT)) (setq CTR (nth 6 DAT))                          ;CT
    (setq AMS  (nth 12 DAT))                                                ;AMｽｹｰﾙ
    (setq VMS  (nth 16 DAT))                                                ;VMｽｹｰﾙ
    (setq KSH  (nth 21 DAT))                                                ;SHUNT
    (setq HSq  (nth 22 DAT))(setq LSq (nth 23 DAT))(setq STSq (nth 24 DAT)) ;線ｻｲｽﾞ
    (setq KLGR (nth 25 DAT))(setq KZCT (nth 26 DAT))(setq ZCTS (nth 27 DAT));LGR
    (setq KTH  (nth 28 DAT))(setq TH (nth 29 DAT))                          ;CT
    (setq KVT  (nth 30 DAT))                                                ;AMｽｹｰﾙ
    (setq KF   (nth 31 DAT))                                                ;VM-FUSE
    (setq FS   (nth 32 DAT))                                                ;Fｽｹｰﾙ

;;ダイアログ条件の設定
 ;冷却方式
    (if (= G10 "OIL")(setq REI "油例式"))
    (if (= G10 "MLD")(setq REI "モールド式"))
 ;トランスの種類
    (if (= G2 "T1")(setq TYP "低損"))
    (if (= G2 "T2")(setq TYP "高効率"))
    (if (= G2 "T3")(setq TYP "超高効率"))
    (if (= G2 "T4")(setq TYP "アモルファス"))
 ;オプション 修正の事！！！！！！！！！！！！！！！！！！！！！！！！！！
    (setq OP "")
 
 ;サーマル
 
 
;;属性値の設定
 ;機器番号
    (setq TrPNo (strcat "Tr-" TRN))    (setq AMPNo (strcat "AM-" TRN)) (setq VSPNo (strcat "VS-" TRN))
    (setq LBSPNo(strcat "LBS-" TRN))   (setq ASPNo (strcat "AS-" TRN)) (setq FPNo  (strcat "F-" TRN))
    (setq PFPNo (strcat "PF-" TRN))    (setq MMPNo (strcat "MM-" TRN)) (setq WHPNo (strcat "WH-" TRN))
    (setq PCPNo (strcat "PC-" TRN))    (setq DMAPNo(strcat "DMA-" TRN))(setq SHTPNo(strcat "SHT-" TRN))
    (setq CTPNo (strcat "CT-" TRN))    (setq VMPNo (strcat "VM-" TRN)) (setq LGRPNo(strcat "LGR-" TRN))
    (setq ZCTLPNo (strcat "ZCTL-" TRN))(setq THPNo (strcat "TH-" TRN)) (setq KVTPNo (strcat "VT400-" TRN))
    (setq PCFPNo (strcat "PCF-" TRN))

;;挿入図形の位置計算
    (ST1)(prompt "\n挿入点を指示... ")(command "INSERT" STD pause "1" "0")
    (setq P1 (getvar "LASTPOINT"))
    (setq P2 (list (car P1) (- (cadr P1) 125)))         ;PFの挿入点
    (setq P3 (list (car P1) (- (cadr P1) 300)))         ;PFの終点
       (setq P31 (list (- (car P3) 40) (cadr P3)))      ;LBSの補助線 1
       (setq P32 (list (- (car P3) 100)(cadr P3)))      ;LBSの補助線 2
       (setq P33 (list (car P32)(- (cadr P3) 150)))     ;LBSの補助線 3
    (setq P4 (list (car P1) (- (cadr P1) 550)))         ;Trの挿入点
    (setq P5 (list (car P1) (- (cadr P1) 850)))         ;Trの終点,SHUNTの始点
    (setq P6 (list (car P1) (- (cadr P1) 1000)))        ;SHUNTの終点
    (setq P7 (list (car P1) (- (cadr P1) 1700)))        ;母線の終点
    (setq P8 (list (car P1) (- (cadr P1) 1200)))        ;VM-母線点
       (setq P81 (list (+ (car P8) 50) (cadr P8)))      ;Fの挿入点
       (setq P82 (list (+ (car P8) 425) (cadr P8)))     ;VSの挿入点
       (setq P83 (list (+ (car P82) 40) (cadr P8)))     ;ﾏﾙﾁﾒｰﾀｰの補助線 1
       (setq P84 (list (car P83)(- (cadr P8) 210)))     ;ﾏﾙﾁﾒｰﾀｰの補助線 2
       (setq P85 (list (+ (car P8) 150) (cadr P8)))     ;WHMの挿入点
    (setq P9 (list (car P1) (- (cadr P1) 1450)))        ;AM-母線点
       (setq P92 (list (+ (car P9) 425) (cadr P9)))     ;ASの挿入点
       (setq P93 (list (+ (car P9) 550) (cadr P9)))     ;AMの挿入点 
       (setq P94 (list (+ (car P9) 260) (cadr P9)))     ;WHの左端
       (setq P95 (list (+ (car P9)  75) (cadr P9)))     ;CTの右端
    (setq P10 (list (car P1) (- (cadr P1) 325)))        ;低圧－線ｻｲｽﾞ
    (setq P11 (list (car P1) (- (cadr P1) 1180)))       ;高圧－線ｻｲｽﾞ
    (setq P12 (list (- (car P1) 100)(- (cadr P1) 900))) ;LGRの挿入点
    
 ;相・電圧と回路条件
    (if (= G12 "33W2")(progn (setq PW "3φ3W")   (setq VV "6600/210V")    (setq NN "3")(setq FN "2")))
    (if (= G12 "33W4")(progn (setq PW "3φ3W")   (setq VV "6600/440V")    (setq NN "3")(setq FN "2")))
    (if (= G12 "34W4")(progn (setq PW "3φ4W")   (setq VV "6600/440V")    (setq NN "3")(setq FN "2")))
    (if (= G12 "13W") (progn (setq PW "1φ3W")   (setq VV "6600/210-105V")(setq NN "2")(setq FN "1")))
    (if (= G12 "LST") (progn (setq PW "スコット")(setq VV "210/105V X 2") (setq NN "3")(setq FN "2")))
    (if (= G12 "HST") (progn (setq PW "スコット")(setq VV "6600/105V X 2")(setq NN "3")(setq FN "2")))
;;======================================================================================================

;;標準図の図形選択範囲
  (setq FP1 (list (- (car P1) 400) (+ (cadr P1) 0)))   ;フェンス左上
  (setq FP2 (list (- (car P1) 400) (- (cadr P1) 1700)));フェンス左下
  (setq FP3 (list (+ (car P1) 900) (- (cadr P1) 1700)));フェンス右下
  (setq FP4 (list (+ (car P1) 900) (+ (cadr P1) 0)))   ;フェンス右上
  
;  (princ "\nPFS=")(princ PFS)
;;属性の編集
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
        
        ;;属性の書込データーをセット                       ;書込みたい属性値を一旦DATATTに書き込む。
        (if (= (cdr (assoc 2 ENT)) SYMTr)
            (setq DATATT (list "TR" TRPNo PW VV KVA "" REI TYP "" "" OP G1 "1" "ABC" "XYZ")))
        (if (= (cdr (assoc 2 ENT)) "QVM")
            (setq DATATT (list "VM" VMPNo KVM "" VMS "" "" "" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QAM")
            (setq DATATT (list "AM" AMPNo KAM "" AMS "" "" "" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QCTL")
            (setq DATATT (list "CT" CTPNo KCT "" CTR "" "" "" "" "" "" MN "2" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QZCTL")
            (setq DATATT (list "ZCT" ZCTLPNo KZCT "" "" "" "" "" "" "" "" "5" "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QLBS1")
            (setq DATATT (list "LBS" LBSPNo KLBS "" "200A" "7.2KV" "" "" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QPF2")
            (setq DATATT (list "PF" PFPNo KPF "7.2KV" PFS "40KA" "" "" "" "" "" MN NN "" "")))
        (if (= (cdr (assoc 2 ENT)) "QWHMK")
            (setq DATATT (list "WHM" WHPNo KWH "" CTR "" "" "" "" "" "" MN "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QVS")
            (setq DATATT (list "VS" VSPNo "BHN-V" "" "銘板N-7" "" "" "" "" "" "" "14" "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QAS")
            (setq DATATT (list "AS" ASPNo "BHN-A" "" "銘板N-1" "" "" "" "" "" "" "14" "1" "" "")))
        (if (= (cdr (assoc 2 ENT)) "QSHT")
            (setq DATATT (list "SHT" SHTPNo KSH "" "" "" "" "" "" "" "" "74" NN "" "")))
        (if (= (cdr (assoc 2 ENT)) "QTHR")
            (setq DATATT (list "THR" THPNo KTH "" TH "" "" "" "" "" "" "4" NN "" "")))
        
        ;;属性の書込対象となるシンボル
        (if (or (= (cdr (assoc 2 ENT)) SYMTR)  (= (cdr (assoc 2 ENT)) "QWHMK")
                (= (cdr (assoc 2 ENT)) "QAM")  (= (cdr (assoc 2 ENT)) "QVM")
                (= (cdr (assoc 2 ENT)) "QVS")  (= (cdr (assoc 2 ENT)) "QAS")
                (= (cdr (assoc 2 ENT)) "QCTL") (= (cdr (assoc 2 ENT)) "QZCTL")
                (= (cdr (assoc 2 ENT)) "QLBS1")(= (cdr (assoc 2 ENT)) "QPF2")
                (= (cdr (assoc 2 ENT)) "QSHT") (= (cdr (assoc 2 ENT)) "QTHR")
            );or
            (progn
;              (princ "\nENT=")(princ (cdr (assoc 2 ENT)))
;              (princ "\nDAT=")(princ DATATT)
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
        (if (or (= (cdr (assoc 2 ENT)) "QYAH")(= (cdr (assoc 2 ENT)) "QYAL"))
            (progn
               (if (= (cdr (assoc 2 ENT)) "QYAH")(setq Sq HSq))
               (if (= (cdr (assoc 2 ENT)) "QYAL")(setq Sq LSq))
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
      ;;必要に応じて不要なシンボルを消去する
        ;;主幹がＰＣ又は無い場合
        (if (and (or (= G8 "P")(= G8 "N"))(= (cdr (assoc 2 ENT)) "QLBS1"))(entdel BB)); LBSを消す
        (if (and (or (= G8 "P")(= G8 "N"))(= (cdr (assoc 2 ENT)) "QPF2")) (entdel BB)); PFを 消す
        
        ;;切替付きメーターの場合
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "QVM"))(entdel BB)); VMを消す
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "QAM"))(entdel BB)); AMを消す
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "QVS"))(entdel BB)); VSを消す
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "QAS"))(entdel BB)); ASを消す
        
        ;;マルチメーターの場合
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "QVM"))(entdel BB)); VMを消す
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "QVS"))(entdel BB)); VSを消す
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "QAM"))(entdel BB)); AMを消す
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "QAS"))(entdel BB)); ASを消す
        
        ;;シャントが無い場合
        (if (and (= G4 "N")(= (cdr (assoc 2 ENT)) "QSHT"))(entdel BB))    ; SHUNTを消す
        
        ;;ＷＨＭが無い場合
        (if (and (= G3 "WH0")(= (cdr (assoc 2 ENT)) "QWHMK"))(entdel BB)) ; WHMを消す
        
        ;;サーマルが無い場合
        (if (and (= G5 "N")(= (cdr (assoc 2 ENT)) "QTHR"))(entdel BB))    ; WHMを消す
        
        ;;ＬＧＲを消す
        (if (and (= LG "N")(= (cdr (assoc 2 ENT)) "QLGR"))(entdel BB))    ; ＬＧＲを消す
        
        (setq n (1+ n)) 
   );while
;;--------------------------------------------------------------------------------------------------
;;サーマルとＷＨＭが無くて、ﾏﾙﾁﾒｰﾀｰじゃない場合
        (if (and (= G3 "WH0")(= G5 "N"))
            (command "PLINE" P95 "W" "0" "" P92 "")
            (progn ; else ;ＷＨＭが無い場合
              (if (and (= G3 "WH0")(/= G5 "N"))(command "PLINE" P94 "W" "0" "" P92 "")) 
                          ;サーマルが無い場合
              (if (and (/= G3 "WH0")(= G5 "N"))(command "PLINE" P95 "W" "0" "" P94 "")) 
            )
        )

;;主幹がPC or 無い場合
    (if (= G8 "P"); 主幹がＰＣの場合
        (progn
          (command "INSERT" "QPC"  P2 "1" "1" "0"
                            "PC"  PCPNo   KPC "" "7.2KV" "" "" "" "" "" "" "11" NN "" "")
          (command "INSERT" "QPF2" P2 "1" "1" "0"
                            "PCF" PCFPNo "FT" "7.2KV" PCS "1.5KA" "" "" "" "" "" "11" NN "" "")
        )
    )

    (if (= G8 "N")(command "PLINE" P1 "W" "10" "" P4 ""))

    (if (= G8 "L")
        (progn
           (command "PLINE" P31 "W" "0" "" P32 P33 "")
           (command "INSERT" "KOUTEN" P31 "1" "1" "0")
        )
    )
;;シャント
    (if (= G4 "N")(command "PLINE" P5 "W" "10" "" P6 ""));SHUNT 無し

;;切替付きメーターの場合
     (if (= G14 "KS"); VM切替付きを挿入
         (command "INSERT" "QVMS" P82 "1" "1" "0" "VM" VMPNo KVM "" VMS "" "" "" "" "" "" MN "1" "" ""))
     (if (= G14 "KS"); AM切替付きを挿入
         (command "INSERT" "QAMS" P92 "1" "1" "0" "AM" AMPNo KAM "" AMS "" "" "" "" "" "" MN "1" "" ""))

;;マルチメーターの場合
     (if (= G14 "KM")
         (progn
            (command "PLINE" P82 "W" "0" "" P83 P84 "")      ; 補助線
            (command "INSERT" "QMM"  P92 "1" "1" "0"
                              "MM" MMPNo KAM "電圧・電流" "" "" "" "" "" "" "" MN "1" "" "")
         )
     )
;;ＬＧＲへの矢印挿入
     (if (= LG "N") (command "INSERT" "QZCTL1"  P5 "1" "1" "0"))
     
;;スコット回路は未完


;;======================================================================================================
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (redraw)
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
   (prompt "\n作図完了！！")
)
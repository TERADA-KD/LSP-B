;;低圧配電盤用、トランス回路入力（３線図） 標準図の属性を書換え   3TR_IN.LSP  2004/JAN/1  By T.Terada
;;
;;C++ダイアログから取得する変数
;; TRN:ﾄﾗﾝｽ番号
;; KVA:ﾄﾗﾝｽ容量
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
(defun C:3TR_IN ( / n AA BB CC DD ENT DAT DATATT ZOKU N2 Sq STD MN GYO RETU
                    TRN KVA G1 G2 G3 G4 G5 G6 G7 G8 G9 G10 G12 G14
                    SEL1 SEL DAT KAM KVM KWH KLBS KPC KCT AMS VMS KSH HSq  LSq STSq KLGR KZCT ZCTS KTH KVT
                    KF TH FS REI TYP OP TrPNo LBSPNo PFPNo PCPNo CTPNo AMPNo ASPNo MMPNo DMAPNo VMPNo
                    VSPNo FPNo WHPNo SHTPNo ZCTLPNo THPNo KVTPNo PW VV NN FN FP1 FP2 FP3 FP4 P1 P2 
                    P3 P31 P32 P33 P4 P5 P6 P7 P8 P81 P82 P83 P84 P85 P9 P91 P92 P93 P94 P95 P10 P11 P12
                )
  (setq *error* *err1*)(setvar "CMDECHO" 0)
  (setq DAT nil TRN nil KVA nil G1 nil G2 nil G4 nil G5 nil G6 nil G7 nil G8 nil G9 nil G10 nil G12 nil G14 nil)
   
  (C:MKR)              ;;基本メーカーを調べる
  (CTr1)               ;;C++ 入力用ダイアログを呼びだし
  (setvar "USERS5" "A");;C:SQ3Wを自動モードで使う
  (setq LG "N")        ;;LGR無しの設定（Ｃのダイアログには設定無し）
  (command "COLOR" 4)
;;メーカー番号の決定
    (if (= MAKER "0")(setq MN "1")) (if (= MAKER "1")(setq MN "4"))
    (if (= MAKER "2")(setq MN "1")) (if (= MAKER "3")(setq MN "4"))

;標準図とトランスシンボルの決定
    (setq STD "*ST-トランス３線標準図")
    (if (= G12 "33W2")(progn (setq SYMTr "Q3TRH33")(setq SYMPC "Q3PC3")(setq SYMPF "Q3PF3")))
    (if (= G12 "33W4")(progn (setq SYMTr "Q3TRH34")(setq SYMPC "Q3PC3")(setq SYMPF "Q3PF3")))
    (if (= G12 "34W4")(progn (setq SYMTr "Q3TRH34")(setq SYMPC "Q3PC3")(setq SYMPF "Q3PF3")))
    (if (= G12 "13W" )(progn (setq SYMTr "Q3TRH13")(setq SYMPC "Q3PC2")(setq SYMPF "Q3PF2")))
;    (if (= G12 "HST" )(progn (setq STD "*ST-TR3M5")(setq SYMTr "Q3TRHST")))
;    (if (= G12 "LST" )(progn (setq STD "*ST-TR3M6")(setq SYMTr "Q3TRHST")))

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
    (if (and (= G14 "K12")(= G7 "N")) (progn (setq KAM (nth 7 DAT)) (setq KVM (nth 13 DAT)) ));120角
    (if (and (= G14  "KW")(= G7 "N")) (progn (setq KAM (nth 8 DAT)) (setq KVM (nth 14 DAT)) ));広角
    (if (and (= G14  "KS")(= G7 "N")) (progn (setq KAM (nth 9 DAT)) (setq KVM (nth 15 DAT)) ));切替付
    (if (and (= G14  "KW")(= G7 "Y")) (progn (setq KAM (nth 11 DAT))(setq KVM (nth 14 DAT)) ));DMA
    (if (and (= G14  "KM")(= G7 "Y")) (setq KAM (nth 10 DAT)) );ﾏﾙﾁﾒｰﾀｰ
    (if (and (= G14  "KM")(= G7 "N")) (setq KAM (nth 10 DAT)) );ﾏﾙﾁﾒｰﾀｰ
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
    (setq KTH  (nth 28 DAT))(setq TH (nth 29 DAT))                          ;THR
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
    (setq ZCTLPNo (strcat "ZCTL-" TRN))(setq THPNo (strcat "THR-" TRN)) (setq KVTPNo (strcat "VT400-" TRN))
    (setq PCFPNo (strcat "PCF-" TRN))

;;挿入図形の位置計算
    (ST1)(prompt "\n挿入点を指示")
    (command "INSERT" STD pause "1" "0")
    (setq PC1 (getvar "LASTPOINT"))
    (setq PC2 (list (car PC1) (- (cadr PC1) 125)))          ;PFの挿入点
    (setq PC3 (list (car PC1) (- (cadr PC1) 200)))          ;PFの終点
       (setq PC31 (list (- (car PC3) 40) (cadr PC3)))       ;LBSの補助線 1
       (setq PC32 (list (- (car PC3) 100)(cadr PC3)))       ;LBSの補助線 2
       (setq PC33 (list (car PC32)(- (cadr PC3) 150)))      ;LBSの補助線 3
    (setq PC4 (list (car PC1) (- (cadr PC1) 300)))          ;Trの挿入点
    (setq PC5 (list (car PC1) (- (cadr PC1) 600)))          ;Trの終点,SHUNTの始点
    (setq PC6 (list (car PC1) (- (cadr PC1) 1000)))         ;SHUNTの終点
    (setq PC7 (list (car PC1) (- (cadr PC1) 1700)))         ;母線の終点
    (setq PC8 (list (car PC1) (- (cadr PC1) 1100)))         ;VM-母線点
       (setq PC81 (list (+ (car PC8) 800)(+ (cadr PC8) 50)));AMSの挿入点
       (setq PC82 (list (+ (car PC8) 425) (cadr PC8)))      ;VSの挿入点
       (setq PC83 (list (+ (car PC82) 40) (cadr PC8)))      ;ﾏﾙﾁﾒｰﾀｰの補助線 1
       (setq PC84 (list (car PC83)(- (cadr PC8) 210)))      ;ﾏﾙﾁﾒｰﾀｰの補助線 2
       (setq PC85 (list (+ (car PC8) 150) (cadr PC8)))      ;WHMの挿入点
    (setq PC9 (list (car PC1) (- (cadr PC1) 1700)))         ;AM-母線点
       (setq PC91 (list (+ (car PC9) 800)(+ (cadr PC9) 50)));AMSの挿入点
       (setq PC92 (list (+ (car PC9) 100)(- (cadr PC9) 50)));THRの挿入点
       (setq PC93 (list (+ (car PC9) 300)(- (cadr PC9) 50)));WHMの挿入点 
       (setq PC94 (list (+ (car PC9) 700)(- (cadr PC9) 50)));WHMの右端
       (setq PC95 (list (+ (car PC9)  75) (cadr PC9)))      ;CTの右端
    (setq PC10 (list (car PC1) (- (cadr PC1) 325)))         ;低圧－線ｻｲｽﾞ
    (setq PC11 (list (car PC1) (- (cadr PC1) 1180)))        ;高圧－線ｻｲｽﾞ
    (setq PC12 (list (- (car PC1) 100)(- (cadr PC1) 900)))  ;LGRの挿入点
    
 ;相・電圧と回路条件
    (if (= G12 "33W2")(progn (setq PW "3φ3W")   (setq VV "6600/210V")    (setq NN "3")(setq FN "2")))
    (if (= G12 "33W4")(progn (setq PW "3φ3W")   (setq VV "6600/440V")    (setq NN "3")(setq FN "2")))
    (if (= G12 "34W4")(progn (setq PW "3φ4W")   (setq VV "6600/440V")    (setq NN "3")(setq FN "2")))
    (if (= G12 "13W") (progn (setq PW "1φ3W")   (setq VV "6600/210-105V")(setq NN "2")(setq FN "1")))
    (if (= G12 "LST") (progn (setq PW "スコット")(setq VV "210/105V X 2") (setq NN "3")(setq FN "2")))
    (if (= G12 "HST") (progn (setq PW "スコット")(setq VV "6600/105V X 2")(setq NN "3")(setq FN "2")))
;;======================================================================================================
;;標準図の図形選択範囲
  (setq FP1 (list (- (car PC1) 750)  (+ (cadr PC1) 50)))  ;フェンス左上
  (setq FP2 (list (- (car PC1) 750)  (- (cadr PC1) 2000)));フェンス左下
  (setq FP3 (list (+ (car PC1) 1600) (- (cadr PC1) 2000)));フェンス右下
  (setq FP4 (list (+ (car PC1) 1600) (+ (cadr PC1) 50)))  ;フェンス右上
  
;;属性の編集
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
        ;;属性の書込データーをセット                       ;書込みたい属性値を一旦DATATTに書き込む。
        (if (= (cdr (assoc 2 ENT)) "Q3VM")
                    (setq DATATT (list "VM" VMPNo KVM "" VMS "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3AM")
                    (setq DATATT (list "AM" AMPNo KAM "" AMS "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3CTH")
                    (setq DATATT (list "CT" CTPNo KCT "" CTR "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3ZCTL")
                    (setq DATATT (list "ZCTL" ZCTLPNo KZCT "" "" "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3LBS1")
                    (setq DATATT (list "LBS" LBSPNo KLBS "" "200A" "7.2/3.6KV" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3PF1")
                    (setq DATATT (list "PF" PFPNo KPF "7.2KV" PFS "40KA" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3WHM2")
                    (setq DATATT (list "WHM" WHPNo KWH "" CTR "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3VS2")
                    (setq DATATT (list "VS" VSPNo "BHN-V" "" "" "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3AS2")
                    (setq DATATT (list "AS" ASPNo "BHN-A" "" "" "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3THR2")
                    (setq DATATT (list "THR" THPNo KTH "" TH "" "" "" "" "" "")))
        (if (= (cdr (assoc 2 ENT)) "Q3SHT")
                    (setq DATATT (list "SHT" SHTPNo KSH "" "" "" "" "" "" "" "")))
            
        ;;属性の書込対象となるシンボル
        (if (and (/= G14 "KM")
                 (or (= (cdr (assoc 2 ENT)) "Q3WHM2")(= (cdr (assoc 2 ENT)) "Q3THR2")
                     (= (cdr (assoc 2 ENT)) "Q3AM")  (= (cdr (assoc 2 ENT)) "Q3VM")
                     (= (cdr (assoc 2 ENT)) "Q3VS2") (= (cdr (assoc 2 ENT)) "Q3AS2")
                     (= (cdr (assoc 2 ENT)) "Q3CTH") (= (cdr (assoc 2 ENT)) "Q3ZCTL")
                     (= (cdr (assoc 2 ENT)) "Q3LBS1")(= (cdr (assoc 2 ENT)) "Q3PF1")
                     (= (cdr (assoc 2 ENT)) "Q3SHT")
                 );or
            );and
            (progn
;              (princ "ENT=")(princ (cdr (assoc 2 ENT)))
               (setq N2 1)
               (setq CC (entnext BB)  DD (entget CC))
               (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                  (if (= (cdr (assoc 0 DD)) "ATTRIB")
                      (progn
                         (if (= N2 1) (setq ZOKU "QNA"))(if (= N2 2) (setq ZOKU "QNO"))
                         (if (= N2 3) (setq ZOKU "Q1")) (if (= N2 4) (setq ZOKU "Q2"))
                         (if (= N2 5) (setq ZOKU "Q3")) (if (= N2 6) (setq ZOKU "Q4"))
                         (if (= N2 7) (setq ZOKU "Q5")) (if (= N2 8) (setq ZOKU "Q6"))
                         (if (= N2 9) (setq ZOKU "Q7")) (if (= N2 10)(setq ZOKU "Q8"))
                         (if (= N2 11)(setq ZOKU "Q9"))
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
      ;;必要に応じて不要なシンボルを消去する
        ;;トランスを一旦消去
        (if (= (cdr (assoc 2 ENT)) "Q3TRH13")(entdel BB))
        (if (= (cdr (assoc 2 ENT)) "Q3PF2")(entdel BB))
        
        ;;主幹がＰＣ又は無い場合
        (if (and (or (= G8 "P")(= G8 "N"))(= (cdr (assoc 2 ENT)) "Q3LBS1"))(entdel BB)); LBSを消す
        
        ;;切替付きメーターの場合
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "Q3VM"))(entdel BB)) ; VMを消す
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "Q3AM"))(entdel BB)) ; AMを消す
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "Q3VS"))(entdel BB)); VSを消す
        (if (and (= G14 "KS")(= (cdr (assoc 2 ENT)) "Q3AS"))(entdel BB)); ASを消す
        
        ;;マルチメーターの場合
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "Q3VM"))(entdel BB)); VMを消す
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "Q3VS"))(entdel BB)); VSを消す
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "Q3AM"))(entdel BB)); AMを消す
        (if (and (= G14 "KM")(= (cdr (assoc 2 ENT)) "Q3AS"))(entdel BB)); ASを消す
        
        ;;シャントが無い場合
        (if (and (= G4 "N")(= (cdr (assoc 2 ENT)) "Q3SHT"))(entdel BB))    ; SHUNTを消す
        
        ;;ＷＨＭが無い場合
        (if (and (= G3 "WH0")(= (cdr (assoc 2 ENT)) "Q3WHM2"))(entdel BB)) ; WHMを消す
        
        ;;サーマルが無い場合
        (if (and (= G5 "N")(= (cdr (assoc 2 ENT)) "Q3THR2"))(entdel BB))   ; サーマルを消す
        
        ;;ＬＧＲを消す
        (if (and (= LG "N")(= (cdr (assoc 2 ENT)) "Q3LGR"))(entdel BB))    ; ＬＧＲを消す
        
        (setq n (1+ n)) 
   );while
;;-------------------------------------------------------------------------------------------------
;;必要に応じて不要な文字を消去する
  (setq AA nil BB nil ENT nil)
  (setq n 0)
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(0 . "TEXT"))))
  (while (setq BB  (ssname AA n))
         (setq ENT (entget BB))

           ;;サーマルが無い場合
           (if (and (= G3 "WH0")(= (cdr (assoc 1 ENT)) "T1C12"))(entdel BB))
           (if (and (= G3 "WH0")(= (cdr (assoc 1 ENT)) "T1C32"))(entdel BB))
           
           ;;ＷＨＭが無い場合
           (if (and (= G5 "N")(= (cdr (assoc 1 ENT)) "T1C11"))(entdel BB))
           (if (and (= G5 "N")(= (cdr (assoc 1 ENT)) "T1C21"))(entdel BB))
           (if (and (= G5 "N")(= (cdr (assoc 1 ENT)) "T1C31"))(entdel BB))

           ;;切替付きメーターの場合
           (if (and (= G14 "KS")(= (cdr (assoc 1 ENT)) "T1VM1"))(entdel BB))
           (if (and (= G14 "KS")(= (cdr (assoc 1 ENT)) "T1VM2"))(entdel BB))
           (if (and (= G14 "KS")(= (cdr (assoc 1 ENT)) "T1AM1"))(entdel BB))
;           (if (and (= G14 "KS")(= (cdr (assoc 1 ENT)) "Q3AS"))(entdel BB))

         (setq n (1+ n)) 
   );while
;;-------------------------------------------------------------------------------------------------
;;回路に応じてトランスを挿入
        (command "INSERT" SYMTr  PC4 "1" "1" "0" "Tr" TrPNo PW VV KVA "" REI TYP "" "" OP)
        (if (/= SYMTr "Q3TRH13")(command "PLINE" PC3 "W" "0" "" PC4 ""))

;;サーマルとＷＨＭが無くて、ﾏﾙﾁﾒｰﾀｰじゃない場合
;        (if (and (= G3 "WH0")(= G5 "N"))
;            (command "PLINE" PC95 "W" "0" "" PC92 "")
;            (progn ; else ;ＷＨＭが無い場合
;              (if (and (= G3 "WH0")(/= G5 "N"))(command "PLINE" PC94 "W" "0" "" PC92 "")) 
;                          ;サーマルが無い場合
;              (if (and (/= G3 "WH0")(= G5 "N"))(command "PLINE" PC95 "W" "0" "" PC94 "")) 
;            )
;        )

;;主幹がＰＣの場合
    (if (= G8 "P")
        (progn
          (command "INSERT" SYMPC PC1 "1" "1" "0" "PC" PCPNo KPC "" "7.2KV" "" "" "" "" "" "")
          (command "INSERT" SYMPF PC1 "1" "1" "0" "PCF" PCFPNo "FT" "7.2KV" PCS "1.5KA" "" "" "" "" "")
        )
    )
;;主幹が無い場合
    (if (= G8 "N")(progn (setq P1 PC1) (setq P2 (list (car PC1) (- (cadr PC1) 300))) (C:SQ3W)))

;;主幹がＬＢＳの場合
    (if (= G8 "L")
        (progn
           (command "INSERT" SYMPF PC1 "1" "1" "0" "PF" PFPNo KPF "7.2KV" PFS "40KA" "" "" "" "" "")
        )
    )
;;シャントが 無い場合
    (if (= G4 "N")
        (progn
           (setq P1 (list (car PC1) (- (cadr PC1) 700)))         ;Trの終点,SHUNTの始点
           (setq P2 (list (car PC1) (- (cadr PC1) 850)))         ;SHUNTの終点
           (C:SQ3W)
        )
    )

;;WHM & THR が無い場合
    (if (and (= G5 "N")(= G3 "WH0"))
        (progn 
           (setq P1 PC92) (setq P2 PC94) (C:SQ3W)
        )
        (progn 
           (if (and (= G5 "Y")(= G3 "WH0"))(progn (setq P1 PC93) (setq P2 PC94) (C:SQ3W)));;WHMが無い場合
           (if (and (= G5 "N")(or (= G3 "WH1")(= G3 "WH2")(= G3 "WH3")(= G3 "WH4")))
               (progn (setq P1 PC92) (setq P2 PC93) (C:SQ3W))                             ;;THRが無い場合
           )
        )
    )
;;切替付きメーターの場合
     (if (= G14 "KS"); VM切替付きを挿入
         (command "INSERT" "Q3VMS" PC81 "1" "1" "0" "VM" VMPNo KVM "" VMS "" "" "" "" "" ""))
     (if (= G14 "KS"); AM切替付きを挿入
         (progn
            (command "INSERT" "Q3AMS" PC91 "1" "1" "0" "AM" AMPNo KAM "" AMS "" "" "" "" "" "")
            (command "INSERT" "*Q3AVMSL" PC8 "1" "0")
         )
     )

;;マルチメーターの場合
     (if (= G14 "KM")
         (progn
            (command "PLINE" PC82 "W" "0" "" PC83 PC84 "")      ; 補助線
            (command "INSERT" "Q3MM"  PC92 "1" "1" "0" "MM" MMPNo KAM "電圧・電流" "" "" "" "" "" "" "")
         )
     )
;;ＬＧＲへの矢印挿入
     (if (= LG "N") (command "INSERT" "Q3ZCTL1" PC5 "1" "1" "0"))
     
;;スコット回路は未完


;;======================================================================================================
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (redraw)
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
   (setvar "USERS5" "A");;C:SQ3Wを手動モードに戻す
   (prompt "\n作図完了！！")
)
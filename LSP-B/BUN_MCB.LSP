;;==================================================================================================
;;分電盤用、分岐ブレーカー入力  With BUN_MCB.CPP  2003/JUL/25 =>05/JUN/20改     By T.Terada
;;CBUN_MCB.CPPからもらう変数    "X:/XLS/分岐ＭＣＢ.XLS" ==> 分岐ＭＣＢ改訂版.XLS参照
;;MMK=ﾒｰｶｰ                              ;;;
;;PNA=部品記号   :"MCB" or "ELB"
;;CB=ﾌﾞﾚｰｶｰの種類:"SLM1" .........."SP3"
;;LR=左 or 右        ;Bn=入力数            ;BNo=開始番号
;;==================================================================================================
(defun C:BUN_MCB (/ DAT RETU GYO GY SEL SEL1 N1 N2 N3 T1 T2 PX BV CUR CUN CUT DL DR SYML SYMR CSR CSN CST NN LR2
               T1L T1R NOL NOR Bnn NL NR BNoL BNoR LP PCP PCT CuX PCu PCD PMN PMR PMT PMRE PMNE PMTE KG QT
               BHAN)
;変数の初期化
(setq DAT "")(setq RERTU "")(setq GYO "")(setq GY "")(setq SEL "")(setq SEL1 "")(setq N1 "")(setq N2 "")(setq N3 "")
(setq T1 "")(setq T2 "")(setq PX "")(setq BV "")(setq CUR "")(setq CUN "")(setq CUT "")(setq DL "")(setq DR "")(setq SYML "")
(setq CSR "")(setq CSN "")(setq CST "")(setq NN "")(setq LR2 "")(setq T1L "")(setq T1R "")(setq NOL "")(setq NOR "")(setq Bnn "")
(setq NL "")(setq NR "")(setq BNoL "")(setq BNoR "")(setq LP "")(setq PCP "")(setq PCT "")(setq CuX "")(setq PCu "")(setq PCD "")
(setq PMN "")(setq PMR "")(setq PMT "")(setq PMRE "")(setq PMNE "")(setq PMTE "")(setq PMNE "")(setq KG "")(setq QT "")(setq BHAN "")

   (setq *error* *err1*)
   (SETVAR "CMDECHO" 0)
    (CBUN_MCB);C++
(if (= CB "")(exit))
;;ﾒｰｶｰの設定
   (if (= MMK "1")(setq MK "1")) ;三菱
   (if (= MMK "2")(setq MK "4")) ;富士
   (if (= MMK "3")(setq MK "7")) ;寺崎
   (if (= MMK "4")(setq MK "39"));河村
   (if (= MMK "6")(setq MK "37"));日東
;;ﾒｰｶｰ名とﾌﾞﾚｰｶｰの型でEXCELを読込んでﾃﾞｰﾀｰをDATに収納
    (setq DAT nil)
    (setq RETU 67); 67 = ascciの"C"
 ;ﾌﾞﾚｰｶｰの型で行を指定
    (if (= CB "SLM1")  (setq GY "1"))  ;ｽﾘﾑ100V
    (if (= CB "SLM1E") (setq GY "2"))  ;ｽﾘﾑ100V(2E) 
    (if (= CB "SLM2")  (setq GY "3"))  ;ｽﾘﾑ200V     
    (if (= CB "SB11")  (setq GY "4"))  ;SB1-100V    河村
    (if (= CB "SB11E") (setq GY "5"))  ;SB1-100V(2E)河村
    (if (= CB "SB12")  (setq GY "6"))  ;SB1-200V    河村
    (if (and (= CB "SB21")(= PNA "ELB"))(setq GY "7"))  ;SB2-100V(ELB) 日東
    (if (and (= CB "SB21")(= PNA "MCB"))(setq GY "8"))  ;SB2-100V(MCB) 日東
    (if (= CB "SB21E") (setq GY "9"))  ;SB2-100V(2E)  日東
    (if (= CB "SB22")  (setq GY "10")) ;SB2-200V      日東
    (if (= CB "KB23")  (setq GY "11")) ;KC 2P 30AF
    (if (= CB "KB33")  (setq GY "12")) ;KC 3P 30AF
    (if (= CB "KB25")  (setq GY "13")) ;KC 2P 50AF
    (if (= CB "KB35")  (setq GY "14")) ;KC 3P 50AF
    (if (= CB "KB310") (setq GY "15")) ;KC 3P 100AF
    (if (= CB "BH1P")  (setq GY "16")) ;BH-K 1P
    (if (= CB "BH2P")  (setq GY "17")) ;BH-K 2P
    (if (= CB "BH3P")  (setq GY "18")) ;BH-K 3P
    (if (= CB "BH21")  (setq GY "19")) ;BH-K100 2P
    (if (= CB "BH31")  (setq GY "20")) ;BH-K100 3P
    (if (= CB "SP1")   (setq GY "21")) ;スペース1
    (if (= CB "SP2")   (setq GY "22")) ;スペース2
    (if (= CB "SP3")   (setq GY "23")) ;スペース3
    (if (= CB "NSB11") (progn (setq GY "24")(setq BHAN "NO"))) ;*内外用に追加 内外 PS-B 100V 2P1E 選択時
    (if (= CB "NSB12") (progn (setq GY "25")(setq BHAN "NO"))) ;*内外用に追加 内外 PS-B 100V 2P2E 選択時
    (if (= CB "NSB20") (progn (setq GY "26")(setq BHAN "NO"))) ;*内外用に追加 内外 PS-B 200V 2P2E 選択時
    (if (= CB "SPN")   (progn (setq GY "27")(setq BHAN "NO"))) ;*内外用に追加 内外 スペース 
    (if (= CB "NAISB11") (setq GY "101")) ;*内外用に追加 内外 安全ブレーカー
    (if (= CB "NAISB12") (setq GY "102")) ;*内外用に追加 内外 安全ブレーカー
    (if (= CB "NAISB20") (setq GY "103")) ;*内外用に追加 内外 安全ブレーカー
    

 ;ﾒｰｶｰによる行番号の設定
    (if (or (= CB "NSB11")(= CB "NSB12")(= CB "NSB20")(= CB "NAISB11")(= CB "NAISB12")(= CB "NAISB20")(= CB "SPN"))(setq MK "36"))  ;*内外用に追加 メーカ番号を内外用に設定する
    ;*(princ "\nCB=")(princ CB)
    ;*(princ "\nGY@1=")(princ GY)
    (setq GYO (itoa (- (+ (atoi GY) (* (atoi MMK) 30)) 20)))
    (if (= CB "NAISB11")(setq GYO "101")) ;*内外用に追加 内外 安全ブレーカー
    (if (= CB "NAISB12")(setq GYO "102")) ;*内外用に追加 内外 安全ブレーカー
    (if (= CB "NAISB20")(setq GYO "103")) ;*内外用に追加 内外 安全ブレーカー
    ;*(princ "\nGYO=")(princ GYO)
    ;*(princ "\nGY@2=")(princ GY)
    ;*(princ "\nMMK=")(princ MMK)
    ;*(princ "\nLR=")(princ LR)
    (princ "\nBHAN=")(princ BHAN)
 ;ﾃﾞｰﾀｰの読込
    (setq N 0)
    (xls_open "X:/XLS/分岐ＭＣＢ改訂版" "r");*読み込むファイルを指定
       (repeat 30                              ;読み込む列の数
          (if (>= N 24)(setq SEL1 (strcat "A" (chr RETU) GYO)) (setq SEL1 (strcat (chr RETU) GYO)))
          (setq SEL  (xls_getval SEL1))
          (setq DAT  (append DAT (list SEL)))
          (setq RETU (+ RETU 1))
          (if (>= RETU 91)(setq RETU 65))
          (setq N (+ N 1))
         ;*(princ "\nN=")(princ N)(princ "SEL1=")(princ SEL1)(princ "\nDAT=")(princ DAT)
       )
    (xls_close)

;;読込だEXCELﾃﾞｰﾀｰを変数にｾｯﾄ
    (if (= PNA "MCB")(setq N1 1 N2 8 N3 10))
    (if (= PNA "ELB")(setq N1 2 N2 9 N3 11))
    (if (= PNA "ELB")(setq T4 "30mA")(setq T4 ""))
        (setq T1   (nth N1 DAT)) ;型式
        ;*(princ "\nT1=")(princ T1)
        (setq PX   (nth 3 DAT))  ;極数
        (setq BV   (nth 4 DAT))  ;電圧
        (setq CUR  (nth 5 DAT))  ;バー型名 分岐ﾊﾞｰR
        (setq CUN  (nth 6 DAT))  ;バー型名 分岐ﾊﾞｰN
        (setq CUT  (nth 7 DAT))  ;バー型名 分岐ﾊﾞｰT
        (setq SYML (nth N2 DAT)) ;ｼﾝﾎﾞﾙ名 ﾌﾞﾚｰｶｰ左
        (setq SYMR (nth N3 DAT)) ;ｼﾝﾎﾞﾙ名 ﾌﾞﾚｰｶｰ右
        (setq CSR  (nth 12 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰR
        (setq CSN  (nth 13 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰN
        (setq CST  (nth 14 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰT
        (setq BMK  (nth 15 DAT)) ;分岐ﾊﾞｰのﾒｰｶｰ番号
        (setq DL   (nth 16 DAT)) ;関連付ける外形ｼﾝﾎﾞﾙ名 L
        (setq DR   (nth 17 DAT)) ;関連付ける外形ｼﾝﾎﾞﾙ名 R
        (setq CURL (nth 18 DAT)) ;バー型名 分岐ﾊﾞｰR 片側のみの左側
        (setq CUNL (nth 19 DAT)) ;バー型名 分岐ﾊﾞｰN 片側のみの左側
        (setq CUTL (nth 20 DAT)) ;バー型名 分岐ﾊﾞｰT 片側のみの左側
        (setq CURR (nth 21 DAT)) ;バー型名 分岐ﾊﾞｰR 片側のみの右側
        (setq CUNR (nth 22 DAT)) ;バー型名 分岐ﾊﾞｰN 片側のみの右側
        (setq CUTR (nth 23 DAT)) ;バー型名 分岐ﾊﾞｰT 片側のみの右側
        (setq CSRL (nth 24 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰR 片側のみの左側
        (setq CSNL (nth 25 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰN 片側のみの左側
        (setq CSTL (nth 26 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰT 片側のみの左側
        (setq CSRR (nth 27 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰR 片側のみの右側
        (setq CSNR (nth 28 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰN 片側のみの右側
        (setq CSTR (nth 29 DAT)) ;ｼﾝﾎﾞﾙ名 分岐ﾊﾞｰT 片側のみの右側
        (if (= PNA "MCB")(setq T1L (strcat T1 "R"))) ;*内外用に追加 内外 PS-B MCB 2P1E 100V 左用型式
        (if (= PNA "MCB")(setq T1R (strcat T1 "T"))) ;*内外用に追加 内外 PS-B MCB 2P1E 100V 右用型式
        (if (= PNA "ELB")(setq T1L T1)) ;*内外用に追加 内外 PS-B ELB 2P1E 100V 左用型式
        (if (= PNA "ELB")(setq T1R T1)) ;*内外用に追加 内外 PS-B ELB 2P1E 100V 右用型式
        (if (or (= CB "SP1")(= CB "SP2")(= CB "SP3")(= CB "SPN"))(setq T4 "")) ;*ELBのスペースを選択した場合は感度電流を表示させない。
        ;*(princ "\nT1L=")(princ T1L)
        ;*(princ "\nT1R=")(princ T1R)
        ;*(princ "\nBV=")(princ BV)
        ;*(princ "\nT1=")(princ T1)
    ;;型式が設定されていないところは実行しない
    (if (= T1 "")(progn (prompt "\nカタログに存在しません")(exit)))

     (prompt "\nX:/XLS/分岐ＭＣＢ改訂版.XLSを参照:")
    (setq P1 (getpoint "  挿入点を指示... "))
    (command "COLOR" 2)(setq PNo (strcat PNA "-" BNo))(setq T2 "20AT")
    (if (or (= CB "BH21")(= CB "BH31")(= CB "KB310"))(setq T2 "100AT"))      ;100AFのﾄﾘｯﾌﾟは100ATにする。
    (if (or (= CB "SP1")(= CB "SP2")(= CB "SP3"))(progn (setq T2 "")(setq NN "0"))(setq NN "1"));SPは"",数量1
    (if (and (= PNA "MCB")(or (= CB "SLM1")(= CB "SLM1E")(= CB "SLM2")))(setq PNA "CMCB")) ;スリムの場合
    (if (and (= PNA "ELB")(or (= CB "SLM1")(= CB "SLM1E")(= CB "SLM2")))(setq PNA "CELB")) ;スリムの場合
    ;*(if (and (= PNA "MCB")(or (= CB "SB11")(= CB "SB11E")(= CB "SB12")))(setq PNA "SMCB")) ;SBの場合 （河村）内外追加によりコメントアウト
    ;*(if (and (= PNA "ELB")(or (= CB "SB11")(= CB "SB11E")(= CB "SB12")))(setq PNA "SELB")) ;ESBの場合（河村）内外追加によりコメントアウト
    (if (and (= PNA "MCB")(or (= CB "SB11")(= CB "SB11E")(= CB "SB12")(= CB "NSB11")(= CB "NSB12")(= CB "NSB20")(= CB "SPN")))(setq PNA "SMCB"))  ;*部品記号をSMCBへ
    (if (and (= PNA "ELB")(or (= CB "SB11")(= CB "SB11E")(= CB "SB12")(= CB "NSB11")(= CB "NSB12")(= CB "NSB20")(= CB "SPN")))(setq PNA "SELB"))  ;*部品記号をSELBへ
    (if (and (= PNA "MCB")(or (= CB "SB21")(= CB "SB21E")(= CB "SB22")))(setq PNA "SB"))   ;SBの場合 （日東）
    (if (and (= PNA "ELB")(or (= CB "SB21")(= CB "SB21E")(= CB "SB22")))(setq PNA "ESB"))  ;ESBの場合（日東）
    (if (and (= PNA "MCB")(or (= CB "NAISB11")(= CB "NAISB12")(= CB "NAISB20")))(setq PNA "SB"))  ;*内外用に追加 内外 安全ブレーカー用
    (if (and (= PNA "ELB")(or (= CB "NAISB11")(= CB "NAISB12")(= CB "NAISB20")))(setq PNA "ESB")) ;*内外用に追加 内外 安全ブレーカー用
    (if (or (= CB "SB11")(= CB "SB12")(= CB "SB11E"))(setq MK "39"))         ;SB-河村
    (if (or (= CB "SB21")(= CB "SB22")(= CB "SB21E"))(setq MK "37"))         ;SB-日東
    (if (= CB "SPN")(setq PNA "SMCB"))

;;ﾌﾞﾚｰｶｰ複数個入力の場合  位置計算
   (setq NOL (atoi BNo))                            ;ﾌﾞﾚｰｶｰ番号 L
   (setq NOR (+ NOL 1))                             ;ﾌﾞﾚｰｶｰ番号 R
   ;*(princ "\nNOR@@@=")(princ NOR)
   (setq NOLD (strcat "Br" BNo))                    ;Z2に渡すﾌﾞﾚｰｶｰ番号 L
   (setq NORD (strcat "Br" (itoa (+ NOL 1))))       ;Z2に渡すﾌﾞﾚｰｶｰ番号 R
   (setq Bnn (atoi BN))                             ;BN = 入力数
   (setq NL (FIX (+ 0.5 (/ Bnn 2.0))))              ;左の分岐数
   (setq NR (FIX (/ Bnn 2)))                        ;右の分岐数
   (setq BNoL (strcat PNA "-" (rtos NOL)))          ;記入するﾌﾞﾚｰｶｰ番号 L
   (setq BNoR (strcat PNA "-" (rtos NOR)))          ;記入するﾌﾞﾚｰｶｰ番号 R
   ;*(princ "\nBNoR=")(princ BNoR)
   (setq LP (FIX (/ Bnn 2)))                        ;ﾙｰﾌﾟ回数
   (setq PCP (cadr P1))                             ;ﾌﾞﾚｰｶｰの挿入点   Y軸
   (setq PCT (- (cadr P1) 5))                       ;負荷名称の挿入点 Y軸
   (setq PCV (- (cadr P1) 70))                      ;電圧記入の挿入点 Y軸
;;メインバーの設定
   (setq PMN (list (car P1)(+ (cadr P1) 50)))       ;ﾒｲﾝCu の描画基点 N
   (setq PMR (list (- (car PMN) 50)(cadr PMN)))     ;ﾒｲﾝCu の描画基点 R
   (setq PMT (list (+ (car PMN) 50)(cadr PMN)))     ;ﾒｲﾝCu の描画基点 T
;;分岐バーの入力     分岐バーのｼﾝﾎﾞﾙの挿入点が2Pと3Pで異なっているので挿入点を変更
   (if (or (= PX "1P")(= PX "3P"))(setq PIN 50)(setq PIN 25));1,3P=50,2P=25
   (setq CuX (FIX (/ Bnn 2)))                       ;入力数
   (setq PCu (list (car P1)(- (cadr P1) PIN)))      ;分岐バーの挿入基点
   (setq PCD PCu)                                   ;初回のみ
   (setq PCDR (list (car PCu)(- (cadr PCu) 25)))
   (setq PCDT (list (car PCu)(+ (cadr PCu) 25)))
   (setq KG (cl_evenp BNN));   (princ "\nKG = ")(princ (cl_evenp BNN))

;;ﾌﾞﾚｰｶｰ複数個力
   ;SBのバーの個数
   (if (or (= CB "SB11")(= CB "SB11E")(= CB "SB12")(= CB "SB21")(= CB "SB21E")(= CB "SB22")(= CB "NAISB11")(= CB "NAISB12")(= CB "NAISB20"))
       (setq QT "2")(setq QT "1");else
   )
   ;*(princ "\nBV=")(princ BV)
;ｽﾘﾑ・SB 200Vの場合 -------------------------------------------------------------------------------------------
   (if (and (= LR "M")(= BV "200V")(or (= PX "2P1E")(= PX "2P2E")))
     (while (>= LP 1)
        (setq BPT 100);BPT=Y方向のﾌﾞﾚｰｶｰﾋﾟｯﾁ
        ;*(princ "\nｽﾘﾑ SB 200V=")
        (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
        (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD)
        (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)   ;*ブレーカー番号のテキスト表示。左側
        (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" NOR)   ;*ブレーカー番号のテキスト表示。右側
        (command "COLOR" 6);ピンク
        (command "TEXT" "S" "M0" (list (- (car P1) 500) PCV) "20" "0" BV)
        (command "TEXT" "S" "M0" (list (+ (car P1) 550) PCV) "20" "0" BV)(command "COLOR" 2);黄色
       ;分岐ﾊﾞｰの入力
        ;*(if (/= MK "36")  ;*ブレーカーが内外製以外の場合は、分岐バーを挿入する。
          (if (/= BHAN "NO") ;*ブレーカーが内外PS-B以外を選択した場合は、分岐バーを挿入する。
            (progn
               (command "INSERT" CSR PCD "1" "1" "0" "Cu" "" CUR "" "" "" "" "" "" "" "" BMK QT "" "")  ;*分岐バーを挿入する R相 
               (command "INSERT" CST PCD "1" "1" "0" "Cu" "" CUT "" "" "" "" "" "" "" "" BMK QT "" "")  ;*分岐バーを挿入する T相
            );*progn
        );*if
        (setq LP   (1- LP));(princ "\nLP = ")(princ LP)
        (setq PCP  (- PCP BPT)) (setq PCT (- PCT BPT)) (setq PCV (- PCV BPT))
        (setq NOL  (+ NOL 2))   (setq NOR (+ NOR 2))
        (setq NOLD (strcat "Br" (itoa NOL)))(setq NORD (strcat "Br" (itoa NOR)))
        (setq BNoL (strcat PNA "-" (rtos NOL))) (setq BNoR (strcat PNA "-" (rtos NOR)))
        (setq PCD  (list (car PCD)(- (cadr PCD) BPT)))
      );while
   );if
   ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= BV "200V")(= KG nil)(or (= PX "2P1E")(= PX "2P2E")))(SLMSB200L))

;ｽﾘﾑ・SB 100Vの場合 -------------------------------------------------------------------------------------------
   (if (and (= LR "M")(= BV "100V")(or (= PX "2P1E")(= PX "2P2E")))
      (while (>= LP 1)
        (setq BPT 100) ;BPT=Y方向のﾌﾞﾚｰｶｰﾋﾟｯﾁ
        ;*(princ "\nｽﾘﾑ SB 200V=")
        ;*(princ "\nT1L@@=")(princ T1L)
        ;*(princ "\nT1R@@=")(princ T1R)
        (if (= CB "NSB11") ;*内外の100V 2P1Eを選択した場合
            (progn
               (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1L PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用に追加 左側用型式挿入
               (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1R PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用に追加 右側用型式挿入
            );progn
            (progn
               (if (= CB "NSB12") ;*内外の100V 2P1Eを選択した場合
                   (progn
                      ;*(command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1L PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用に追加 左側用型式挿入
                      ;*(command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1R PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用に追加 右側用型式挿入
                      (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)  ;*内外用に追加 左側挿入
                      (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX T2 T4 "" "" "" "" "" MK NN DL NORD)  ;*内外用に追加 右側挿入
                   );progn
                   (progn
                      (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用に追加 左側挿入
                      (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用に追加 右側挿入
                   );progn
               );if
            );progn                       
        );if
        (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)    ;*ブレーカー番号のテキスト表示。左側
        (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" NOR)    ;*ブレーカー番号のテキスト表示。右側
       ;分岐ﾊﾞｰの入力
        ;*(if (/= MK "36")  ;*ブレーカーが内外製以外の場合は、分岐バーを挿入する。
        (if (/= BHAN "NO")  ;*ブレーカーが内外PS-B以外を選択した場合は、分岐バーを挿入する。
            (progn
               (command "INSERT" CSR PCD "1" "1" "0" "Cu" "" CUR "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する R相
               (command "INSERT" CSN PCD "1" "1" "0" "Cu" "" CUN "" "" "" "" "" "" "" "" BMK QT "" "")  ;*分岐バーを挿入する N相
               (command "INSERT" CST PCD "1" "1" "0" "Cu" "" CUT "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する T相
            );progn
        );if
        (setq LP (1- LP))
        (setq PCP  (- PCP BPT)) (setq PCT (- PCT BPT)) (setq PCV (- PCV BPT))
        (setq NOL  (+ NOL 2))   (setq NOR (+ NOR 2))
        (setq NOLD (strcat "Br" (itoa NOL)))(setq NORD (strcat "Br" (itoa NOR)))
        (setq BNoL (strcat PNA "-" (rtos NOL))) (setq BNoR (strcat PNA "-" (rtos NOR)))
        (setq PCD  (list (car PCD)(- (cadr PCD) BPT)))
      );while
   );if
   ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= BV "100V")(= KG nil)(or (= PX "2P1E")(= PX "2P2E")))(SLMSB100L))

;BH-K 1Pの場合 --------------------------------------------------------------------------------
   (setq LR2 0)
   (if (and (= LR "M")(= PX "1P"))
      (while (>= LP 1)
         (setq BPT 100)
         ;*(princ "\nBH-K 1P=")
         (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
         (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD)
         (if (/= CB "SP1")  ;*スペース1Pを選択した場合は、ブレーカー番号のテキスト表示をさせない為のif文
             (progn
                (command "TEXT" "S" "M0" (list (- (car P1) 625) (+ PCT 25)) "20" "0" NOL)  ;*ブレーカー番号のテキスト表示。左側 
                (command "TEXT" "S" "M0" (list (+ (car P1) 425) (+ PCT 25)) "20" "0" NOR)  ;*ブレーカー番号のテキスト表示。右側
             );progn
         );if
        ;1P分岐ﾊﾞｰの入力
         (setq LR2 (rem LR2 2));  (princ "\nLR2=")(princ LR2);  LR2=0 => L,  LR2=1 => R
         (if (/= CB "SP1")
             (progn  (if (= LR2 0)
                         (command "INSERT" CSR (list (car P1) (- PCP 50)) "1" "1" "0" "Cu" "" CUR "" "" "" "" "" "" "" "" BMK "1" "" "")
                     ):if
                     (if (= LR2 1)
                         (command "INSERT" CST (list (car P1) (+ PCP 50)) "1" "1" "0" "Cu" "" CUT "" "" "" "" "" "" "" "" BMK "1" "" "")
                     );if
              );progn
         );if
         (setq LR2 (+ LR2 1))
         (setq LP (- LP 1))
         (setq PCP (- PCP BPT))
         (setq PCT (- PCT BPT)) (setq PCV (- PCV BPT))
         (setq NOL (+ NOL 2))(setq NOR (+ NOR 2))
         (setq NOLD (strcat "Br" (itoa NOL)))(setq NORD (strcat "Br" (itoa NOR)))
         (setq BNoL (strcat PNA "-" (rtos NOL)))(setq BNoR (strcat PNA "-" (rtos NOR)))
       );while
   );if
  ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= PX "1P")(= KG nil))(BH1PL))

;KC BHK 2P 200Vの場合 --------------------------------------------------------------------------------
   (if (and (= LR "M")(= PX "2P"))
      (while (>= LP 1)
        (setq BPT 100)      ;BPT=Y方向のﾌﾞﾚｰｶｰﾋﾟｯﾁ
        ;*(princ "\nKC BHK 2P 200V=")
        (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
        (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD)
        (if (/= CB "SP2")  ;*スペース2Pを選択した場合は、ブレーカー番号のテキスト表示をさせない為のif文
            (progn
               (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)  ;*ブレーカー番号のテキスト表示。左側
               (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" NOR)  ;*ブレーカー番号のテキスト表示。右側
            );progn
        );if
        (command "COLOR" 6);ピンク
        (command "TEXT" "S" "M0" (list (- (car P1) 500) PCV) "20" "0" BV)
        (command "TEXT" "S" "M0" (list (+ (car P1) 550) PCV) "20" "0" BV)(command "COLOR" 2);黄色
       ;分岐ﾊﾞｰの入力
        (if (/= CB "SP2")
            (progn
               (command "INSERT" CSR PCDR "1" "1" "0" "Cu" "" CUR "" "" "" "" "" "" "" "" BMK "1" "" "")  ;*分岐バーを挿入する R相
               (command "INSERT" CST PCDT "1" "1" "0" "Cu" "" CUT "" "" "" "" "" "" "" "" BMK "1" "" "")  ;*分岐バーを挿入する T相
            );progn
        );if
        (setq LP   (1- LP))
        (setq PCP  (- PCP BPT)) (setq PCT (- PCT BPT)) (setq PCV (- PCV BPT))
        (setq NOL  (+ NOL 2))   (setq NOR (+ NOR 2))
        (setq NOLD (strcat "Br" (itoa NOL)))(setq NORD (strcat "Br" (itoa NOR)))
        (setq BNoL (strcat PNA "-" (rtos NOL)))(setq BNoR (strcat PNA "-" (rtos NOR)))
        (setq PCDR (list (car PCDR)(- (cadr PCDR) 100)))
        (setq PCDT (list (car PCDT)(- (cadr PCDT) 100)))
      );while
   );if
   ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= PX "2P")(= KG nil))(BHKC2P200L))

;KC BH-K 3Pの場合 --------------------------------------------------------------------------------
   (if (and (= LR "M")(= PX "3P"))
      (while (>= LP 1)
         (setq BPT 150)                               ;BPT=Y方向のﾌﾞﾚｰｶｰﾋﾟｯﾁ
         (princ "KC BH-K 3P=") 
         (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
         (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD)
         (if (/= CB "SP3")  ;*スペース3Ｐを選択した場合は、ブレーカー番号のテキスト表示をさせない為のif文
             (progn
                (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)  ;*ブレーカー番号のテキスト表示。左側
                (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" NOR)  ;*ブレーカー番号のテキスト表示。右側
             );progn
         );if
        ;分岐ﾊﾞｰの入力
         (if (/= CB "SP3")
             (progn
                (command "INSERT" CSR PCD "1" "1" "0" "Cu" "" CUR "" "" "" "" "" "" "" "" BMK "1" "" "")
                (command "INSERT" CSN PCD "1" "1" "0" "Cu" "" CUN "" "" "" "" "" "" "" "" BMK QT "" "")
                (command "INSERT" CST PCD "1" "1" "0" "Cu" "" CUT "" "" "" "" "" "" "" "" BMK "1" "" "")
             );pogn
          );if
          (setq LP   (1- LP))
          (setq PCP  (- PCP BPT)) (setq PCT (- PCT BPT)) (setq PCV (- PCV BPT))
          (setq NOL  (+ NOL 2))   (setq NOR (+ NOR 2))
          (setq NOLD (strcat "Br" (itoa NOL)))(setq NORD (strcat "Br" (itoa NOR)))
          (setq BNoL (strcat PNA "-" (rtos NOL))) (setq BNoR (strcat PNA "-" (rtos NOR)))
          (setq PCD  (list (car PCD)(- (cadr PCD) BPT)))
      );while
   )
  ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= PX "3P")(= KG nil))(BHKC3PL))

  ;内外用のスペースが入る場合 --------------------------------------------------------------------------------
   (if (= PX "")
       (while (>= LP 1)
           (setq BPT 100)
           (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX "" "" "" "" "" "" "" MK NN DL NOLD) ;内外スペース左挿入
           (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX "" "" "" "" "" "" "" MK NN DR NORD) ;内外スペース右挿入
           (setq LP   (1- LP))
           (setq NOL  (+ NOL 2))   (setq NOR (+ NOR 2))
           (setq BNoL (strcat PNA "-" (rtos NOL))) (setq BNoR (strcat PNA "-" (rtos NOR)))
           (setq PCP  (- PCP BPT)) (setq PCT (- PCT BPT)) (setq PCV (- PCV BPT))
       );while
   );if
   ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= PX "")(= KG nil))(MSP2PL-NL))

;;左１個入力の場合 --------------------------------------------------------------------------------
   (if (and (= LR "L")(= BV "200V")(or (= PX "2P1E")(= PX "2P2E")))(SLMSB200L)); ｽﾘﾑ・SB 200Vの場合
   (if (and (= LR "L")(= BV "100V")(or (= PX "2P1E")(= PX "2P2E")))(SLMSB100L)); ｽﾘﾑ・SB 100Vの場合
   (if (and (= LR "L")(= PX "1P"))(BH1PL))                                     ; BH-K    1Pの場合
   (if (and (= LR "L")(= PX "2P"))(BHKC2P200L))                                ; KC BHK  2Pの場合(200V)
   (if (and (= LR "L")(= PX "3P"))(BHKC3PL))                                   ; KC BHK  3Pの場合
   (if (and (= LR "L")(= PX ""))(MSP2PL-NL))                                    ; 内外スペース左の場合
;;右１個入力の場合 --------------------------------------------------------------------------------
   (if (and (= LR "R")(= BV "200V")(or (= PX "2P1E")(= PX "2P2E")))(SLMSB200R)); ｽﾘﾑ・SB 200Vの場合
   (if (and (= LR "R")(= BV "100V")(or (= PX "2P1E")(= PX "2P2E")))(SLMSB100R)); ｽﾘﾑ・SB 100Vの場合
   (if (and (= LR "R")(= PX "1P")) (BH1PR))                                    ; BH-K    1Pの場合
   (if (and (= LR "R")(= PX "2P")) (BHKC2P200R))                               ; KC BHK  2Pの場合(200V)
   (if (and (= LR "R")(= PX "3P")) (BHKC3PR))                                  ; KC BHK  3Pの場合
   (if (and (= LR "R")(= PX ""))(setq BNoR BNoL))                              ; 内外スペース右の場合
   (if (and (= LR "R")(= PX ""))(MSP2PR-NR))                                   ; 内外スペース右の場合
;;--------------------------------------------------------------------------------------------------
;;ﾒｲﾝﾊﾞｰの描画

;;--------------------------------------------------------------------------------------------------
;;終了処理
   (command "COLOR" "BYLAYER")(command "REDRAW")(setvar "CMDECHO" 1)
   (prompt "\n作図完了...")
)
;;--------------------------------------------------------------------------------------------------
;;左１個追加する。
(defun SLMSB200L () ;;ｽﾘﾑ・SB 200Vの場合 
    (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)   ;*ブレーカー番号のテキスト表示しないようにする。左側
    (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
       ;*(if (/= MK "36")  ;*ブレーカーが内外製以外の場合は、分岐バーを挿入する
       (if (/= BHAN "NO")  ;*ブレーカーが内外PS-B以外の場合は、分岐バーを挿入する
           (progn
              (command "INSERT" CSRL (list (car P1) (- PCP 25)) "1" "1" "0" "Cu" "" CURL "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する R相
              (command "INSERT" CSTL (list (car P1) (- PCP 25)) "1" "1" "0" "Cu" "" CUTL "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する T相
           );progn
       );if
    (command "COLOR" 6)(command "TEXT" "S" "M0" (list (- (car P1) 500) PCV) "20" "0" BV)(command "COLOR" 2);黄色
)
(defun SLMSB100L () ;;ｽﾘﾑ・SB 100Vの場合
    (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)     ;*ブレーカー番号のテキスト表示。右側
    ;*(command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
    (if (= CB "NSB11") ;*内外の100V 2P1Eを選択した場合
        (progn
           (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1L PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用に追加 左用型式挿入
        );progn
        (progn
           (if (= CB "NSB12") ;*内外の100V　2P2Eを選択した場合
               (progn
                  ;*(command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1L PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用に追加 左用型式挿入
                  (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用・2P2E用に変更 左用に変更
               );progn
               (progn
                  (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD) ;*内外用に追加 左側挿入
               );progn
           );if
        );progn
    );if          
       ;*(if (/= MK "36")  ;*ブレーカーが内外製以外の場合は、分岐バーを挿入する
       (if (/= BHAN "NO")  ;*ブレーカーが内外PS-B以外の場合は、分岐バーを挿入する
           (progn
              (command "INSERT" CSRL (list (car P1) (- PCP 25)) "1" "1" "0" "Cu" "" CURL "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する R相
              (command "INSERT" CSNL (list (car P1) (- PCP 25)) "1" "1" "0" "Cu" "" CUNL "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する T相
           );progn
       );if
)
(defun BH1PL ()  ;; BHK 1Pの場合
    (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
    (if (/= CB "SP1")
        (progn
           (command "TEXT" "S" "M0" (list (- (car P1) 625) (+ PCT 25)) "20" "0" NOL) ;*ブレーカー番号のテキスト表示。左側
           (command "INSERT" CSRL (list (car P1) (- PCP 50)) "1" "1" "0" "Cu" "" CURL "" "" "" "" "" "" "" "" BMK "1" "" "")
        );progn
    );if
)
(defun BHKC2P200L ()  ;;KC BHK 2P 200Vの場合
    (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
    (if (/= CB "SP2")
        (progn
           (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)  ;*ブレーカー番号のテキスト表示。左側
           (command "INSERT" CSRL (list (car P1) (- PCP 50)) "1" "1" "0" "Cu" "" CURL "" "" "" "" "" "" "" "" BMK "1" "" "")
           (command "INSERT" CSTL (list (car P1) PCP) "1" "1" "0" "Cu" "" CUTL "" "" "" "" "" "" "" "" BMK "1" "" "")
        );progn
    );if
    (command "COLOR" 6)(command "TEXT" "S" "M0" (list (- (car P1) 500) PCV) "20" "0" BV)(command "COLOR" 2);黄色
)
(defun BHKC3PL ()  ;;KC BHK 3Pの場合
    (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
    (if (/= CB "SP3")  
        (progn
           (command "TEXT" "S" "M0" (list (- (car P1) 625) PCT) "20" "0" NOL)  :*ブレーカー番号のテキスト表示。左側
           (command "INSERT" CSRL (list (car P1) (- PCP 50)) "1" "1" "0" "Cu" "" CURL "" "" "" "" "" "" "" "" BMK "1" "" "")
           (command "INSERT" CSNL (list (car P1) (- PCP 50)) "1" "1" "0" "Cu" "" CUNL "" "" "" "" "" "" "" "" BMK "1" "" "")
           (command "INSERT" CSTL (list (car P1) (- PCP 50)) "1" "1" "0" "Cu" "" CUTL "" "" "" "" "" "" "" "" BMK "1" "" "")
       );progn
    );if
)
(defun MSP2PL-NL ()
    (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX "" "" "" "" "" "" "" MK NN DL NOLD) ;内外スペース左挿入
)

;;--------------------------------------------------------------------------------------------------
;;右１個追加する。
(defun SLMSB200R () ;;ｽﾘﾑ・SB 200Vの場合
   (command "INSERT" SYMR P1 "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
      ;*(if (/= MK "36")  ;*ブレーカーが内外製以外の場合は、分岐バーを挿入する
      (if (/= BHAN "NO")  ;*ブレーカーが内外PS-B以外は、分岐場－を挿入する
          (progn
             (command "INSERT" CSRR PCD "1" "1" "0" "Cu" "" CURR "" "" "" "" "" "" "" "" BMK "1" "" "")  ;*分岐バーを挿入する R相
             (command "INSERT" CSTR PCD "1" "1" "0" "Cu" "" CUTR "" "" "" "" "" "" "" "" BMK "1" "" "")  ;*分岐バーを挿入する T相
          );progn
      );if
   (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" BNo)   ;*ブレーカー番号のテキスト表示。右側
   (command "COLOR" 6)(command "TEXT" "S" "M0" (list (+ (car P1) 550) PCV) "20" "0" BV)(command "COLOR" 2);黄色
)
(defun SLMSB100R () ;;ｽﾘﾑ・SB 100Vの場合
   ;*(command "INSERT" SYMR P1 "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD) 
   (if (= CB "NSB11") ;*内外100V 2P1Eを選択した場合
       (progn
          (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoL T1R PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用に追加 右用型式挿入
       );progn
       (progn
          (if (= CB "NSB12") ;*内外100V 2P2Eを選択した場合
              (progn
                 ;*(command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoL T1R PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用に追加 右用型式挿入
                 (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用・2P2E用に変更 右用に変更
              );progn
              (progn
                 (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DR NORD) ;*内外用に追加 右用型式挿入
              );progn
          );if
       );progn
   );if
      ;*(if (/= MK "36")  ;*ブレーカーが内外製以外の場合は、分岐バーを挿入する
      (if (/= BHAN "NO")  ;*ブレーカーが内外PS-B以外の場合は、分岐バーを挿入する
          (progn
             (command "INSERT" CSNR PCD "1" "1" "0" "Cu" "" CUNR "" "" "" "" "" "" "" "" BMK "1" "" "")  ;*分岐バーを挿入する N相
             (command "INSERT" CSTR PCD "1" "1" "0" "Cu" "" CUTR "" "" "" "" "" "" "" "" BMK "1" "" "")  ;*分岐バーを挿入する T相 
          );progn
      );if
   (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" BNo)   ;*ブレーカー番号のテキスト表示。右側
)
(defun BH1PR ()     ;;BHK 1Pの場合
   (setq PCD (list (car PCD)(+ (cadr PCD) 100)))
   (command "INSERT" SYMR P1 "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
   (if (/= CB "SP1")  ;*スペースの場合は、分岐バーとブレーカー番号のテキスト表示をしない為のif文
       (progn
          (command "INSERT" CSTR PCD "1" "1" "0" "Cu" "" CUTR "" "" "" "" "" "" "" "" BMK "1" "" "") ;*分岐バーを挿入する T層
          (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" BNo)   ;*ブレーカー番号のテキスト表示。右側
       );progn
   );if
)
(defun BHKC2P200R ();;KC BHK 2P 200Vの場合
   (command "INSERT" SYMR P1 "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
   (if (/= CB "SP2")
       (progn
          (setq PCD (list (car PCD)(- (cadr PCD) 25)))
          (command "INSERT" CSRR PCD "1" "1" "0" "Cu" "" CURR "" "" "" "" "" "" "" "" BMK "1" "" "")
          (setq PCD (list (car PCD)(+ (cadr PCD) 50)))
          (command "INSERT" CSTR PCD "1" "1" "0" "Cu" "" CUTR "" "" "" "" "" "" "" "" BMK "1" "" "")
          (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" BNo)   ;*ブレーカー番号のテキスト表示。右側
       );progn
   );if
   (command "COLOR" 6)(command "TEXT" "S" "M0" (list (+ (car P1) 550) PCV) "20" "0" BV)(command "COLOR" 2);黄色
)
(defun BHKC3PR ()   ;;KC BHK 3Pの場合
   (command "INSERT" SYMR P1 "1" "1" "0" PNA BNoL T1 PX T2 T4 "" "" "" "" "" MK NN DL NOLD)
   (if (/= CB "SP3")
       (progn
          (command "TEXT" "S" "M0" (list (+ (car P1) 425) PCT) "20" "0" BNo)   ;*ブレーカー番号のテキスト表示。右側
          (command "INSERT" CSRR PCD "1" "1" "0" "Cu" "" CURR "" "" "" "" "" "" "" "" BMK "1" "" "")
          (command "INSERT" CSNR PCD "1" "1" "0" "Cu" "" CUNR "" "" "" "" "" "" "" "" BMK "1" "" "")
          (command "INSERT" CSTR PCD "1" "1" "0" "Cu" "" CUTR "" "" "" "" "" "" "" "" BMK "1" "" "")
       );progn
   );if
)
(defun MSP2PR-NR ()
   (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA BNoR T1 PX "" "" "" "" "" "" "" MK NN DR NORD) ;内外スペース右挿入
)
;;--------------------------------------------------------------------------------------------------
(defun cl_evenp ( num )  (zerop (logand num 1))) ; num：整数 , numが偶数ならT 奇数ならNIL

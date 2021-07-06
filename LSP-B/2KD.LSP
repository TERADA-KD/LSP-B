;;ＬＩＳＰ関数              KD.LSP             2002/SEP By T.Terada
;;関数定義されたLISPはすべてこのファイルに収納しICAD.LSPでロードする。
;;  機  能                                          関  数  名    旧LISP名
;;================================================  ==========    ============
;; 1.エラー処理                                     *err1*
;; 2.共通関数
;; 2-1  ラジオボタン用の set_tile 関数（規定値設定）act_radioset
;; 2-2  スライダーとエディットボックス用の関数      SLD_ac,EDB_ac
;; 2-3  制御用ラインを手動モードに戻す              C:手動
;; 2-4  偶数・奇数の判定                            cl_evenp      num：整数 , numが偶数ならT 奇数ならnil
;; 2-5  PURGEでBLOCKを削除                          C:PG          PG.LSP
;; 3.LISPの開始・終了                               C:ST1,C:END1
;; 4.文字を横書きにする                             C:TM0         TM0.LSP
;; 5.文字を縦書きにする                             C:M0          M0.LSP
;; 6.表紙記入                                       C:HYOUSHI     図枠記入.LSP
;; 7.基準図枠サイズの取得                           C:SZ
;; 8.ドーナッツ（交点）直径１０                     C:DOUNT
;; 9.交点を挿入                                     C:KOUTEN
;;10.属性のみのシンボル"ZOKU"を挿入                 C:ZOKU
;;11.寸法記入                                       C:DIMT       
;;12.ハッチパターンの記入（ＡＮＳＩ３１）           C:HT2,3       HT2.LSP
;;13.文字記入（左揃え,中央揃え,左揃右上線上,両端揃）C:ATXT        A-TXT ATXT-C ATXT-RL ATXT-A.LSP
;;14.線の描画         細線 太線 極太線              C:SPL0,C:WPL6,C:WPL10
;;15.既存線の線幅変更 細線 太線 極太線              C:EDW0,C:EDW6,C:EDW10
;;16.ストレッチ－Ｃ                                 C:C-STR
;;17.交点、切断LISP   垂直カット                    C:CUT-V       CUT-L.LSP
;;18.交点、切断LISP   水平カット                    C:CUT-H       CUT-L.LSP
;;19.ＭＯＶＥ－窓                                   C:WMOVE       W-MCE.LSP
;;20.ＣＯＰＹ－窓                                   C:WCOPY       W-MCE.LSP
;;21.ＥＲＡＳＥ－窓                                 C:WERASE      W-MCE.LSP
;;22.自動連番プログラム                             C:ANUM        A-NUM.LSP
;;23.文字列移動。基点指定省略                       C:TMOVE       TMOVE.LSP
;;24.文字列COPY。基点指定省略                       C:TCOPY       TCOPY2.LSP
;;25.文字列を編集                                   C:TED         TED.LSP
;;26.四角形の描画                                   C:4KAKU.VBI   4KAKU1.LSP
;;27.リレーの外形を描く                             C:DRY.DLL     DMY.LSP      DRY.DLLからDMYをLOAD
;;28.図枠変更                                       C:CH-WAKU     CH-WAKU.LSP
;;29.２点を指定してBREAK                            C:BRKF        BreakF.LSP
;;30.板金・塗装覧の追加                             C:INS-C       AnC.dwg
;;31.属性文字の大きさを変える                       C:ATTSIZE     ATTMOVE.LSP for IntelliCAD IZAWAさん作を流用
;;32.属性文字を移動する                             C:ATTMOVE     ATTMOVE.LSP
;;33.１次幹線の入力                                 C:RST,RNT,RN  幹線入力.LSP
;;34.アクリカバーと留めビス、描画                   C:BCOV        BCOV.LSP
;;35.ＮＴ－縦型  外形入力プログラム                 C:NT-V        NT-V.LSP
;;36.ＮＴ－横型  外形入力プログラム                 C:NT-H        NT-H.LSP
;;37.雲マークを描きます。                           C:KUMO        KUMO.LSP
;;38.制御回路の線番号変更、文字ｻｲｽﾞ15のみ数字を変更 C:SENBAN      線番号変更.LSP
;;39.単位精度（小数点以下の桁表示）                 C:SEIDO       単位精度.LSP
;;40.集合表示灯（富士電機用）外形図を描く           C:FSLC        集合表示灯－富士.LSP
;;41.コメント付き引出線を書く                       C:HIKI        引出線.LSP 
;;42.部品属性,銘板用ﾌｧｲﾙ,ＣＨ用ﾌｧｲﾙを出力           C:ASAVE       ASAVE.LSP  
;;43.図面保存･終了ﾌﾟﾛｸﾞﾗﾑ for Icad                  C:END-I       END-I.LSP 
;;44-1.ﾘﾓｺﾝﾘﾚｰ の回路入力。                         C:RRY         RRY.LSP      RRY.DCL
;;44-2.ﾘﾓｺﾝﾄﾗﾝｽの回路入力。                         C:RTr         RTr.LSP      RTr.DCL
;;45.縦型端子表作成                                 C:TBV         端子表.LSP    
;;46.横型端子表作成                                 C:TBH         端子表.LSP
;;47.コメント線とコメントを描く                     C:COMENT      端子表.LSP
;;48.ツイスト線を書く                               C:TW          TW.LSP 
;;49.ブロック内の属性値をダイアログで変更する。     C:ATTCH       ATTCH.LSP  <======= EattEditに置き換え、故に不要 空き
;;50.ブロック図形を分解して文字情報は元のままに残す C:B2T         BLtoTXT.LSP  分解２
;;51.基本ﾌﾞﾛｯｸを挿入して任意のｻｲｽﾞの外形図を作成。  C:GAIKEI      GAIKEI.LSP
;;52.部品表用ｼﾝﾎﾞﾙの属性値(PNO:機器番号)を一括変更。C:CH-PNO      CH-PNO.LSP
;;53.線結合 LINE・PLINEを結合する。                 C:JL C:JPL    JPL.LSP
;;54.簡易銘板表を作成                               C:NP-TXT      NP-TXT.LSP
;;55.電線サイズ記入                                 C:SEN-SIZE    SEN-SIZE.LSP    Tool.LSP
;;56.ＮＴ－ＳＷ  外形入力                           C:NTSW        NT-V,NT-H.LSP   Tool.LSP
;;57.ＫＤ標準画層に戻す                             C:STD-LA      画層標準.LSP    Tool.LSP
;;58.フリ－アングル描画                             C:FREEANG     FREEANG.LSP     Tool.LSP
;;59.標準図入力用ダイアログボックス                 C:STD         STD2.LSP        STD.DCL
;;60.Ｔｒの外形入力                                 C:D_TR        D_TR.LSP        D_TRSCSR.LSP
;;61.ＳＣの外形入力                                 C:D_SC        D_SC.LSP        D_TRSCSR.LSP
;;62.ＳＲの外形入力                                 C:D_SR        D_SR.LSP        D_TRSCSR.LSP
;;63.ブスバーサポートの入力。                       C:CUZ         CUZ.LSP
;;64.バーサイズ記入プログラム                       C:BARSZ       BAR-SIZE.LSP
;;65.突き合せＢＯＸの上面図、描画                   C:BOX_U       BOX_U.LSP       BOX_U.DCL
;;66.ALL-INを使ったシンボル挿入と属性編集           C:ALL-IN      C:ALL-ATT
;;67.外部接点記号の入力（操作回路）                 C:OUT-C       OUT-C.LSP
;;68.開口図 正面 水平方向                           C:KAI_FH      開口図.LSP
;;69.開口図 正面 垂直方向                           C:KAI_FV      開口図.LSP
;;70.図枠の日付更新                                 C:DATEUP      DATEUP.LSP
;;71.キュービクル用チャンネルバース                 C:QCHANB      QB-CHANB.LSP
;;72.-------各種変更.LSP----------------------------
;;72-1.制御回路用の属性 N1をまとめて変更            C:N1          N1.LSP
;;72-2.制御回路用の属性 N2をまとめて変更            C:N2          N2.LSP
;;72-3.画層の変更リスプ                             C:CHLA        CHANGE01.LSP
;;72-4.線種の変更リスプ                             C:CHLT        CHANGE01.LSP
;;72-5.色の変更リスプ                               C:CHCO        CHANGE01.LSP
;;72-6.既、記入文字サイズの変更                     C:CHTS        CHANGE01.LSP
;;72-7.既、記入寸法サイスの変更                     C:CHDIMTXT    CHANGE01.LSP
;;72-8.一括文字サイズ変更                           C:CHTXT       CHTXT.LSP
;;72-9.記入文字（記入文字）サイズの変更             C:CHTSZ       CHTSZ.LSP
;;72-X.スナップ値の変更                             C:CHSNAP      SNAP.LSP
;;73.図面が終了しないEND-I                          C:SAVE-I
;;74.回路図用コンセントの入力                       C:CONS        CONS.LSP        DCLは回路・外形、共用
;;75.外形図用コンセントの入力                       C:COND        COND.LSP        DCLは回路・外形、共用
;;76.架台の作図                                     C:KADAI       架台.LSP
;;77.文字整列                                       C:TXRAY       TXRAY.LSP
;;78.セパレーターの作図                             C:SEPA        SEPA.LSP
;;79.現在層と表示画層をＫＤ標準画層に戻します。     C:STD-LA      画層標準.LSP
;;80.Ｔ／Ｕ関係機器の回路作図                       C:TU          TU.LSP
;;81.主幹用ランプ回路の作図                         C:MPL         MPL.LSP
;;82.３線図ブレーカーの入力                         C:S3MCB       S3MCB.LSP
;;83.数字混りの文字列(TEXT)の数字部分を加減算する   C:AddNum2     AddMun2.LSP     IZAWAさん作を流用
;;84.配線条数を線分(LWPOLYLINEorLINE)上に挿入します C:PickWire    PickWire.LSP    IZAWAさん作を流用
;;85.ＱＢ・配電盤用ブレーカーの単線図一覧入力       C:QBMCBL      QBMCBL.LSP
;;86.４角形を描く                                   C:4KAKU2      4KAU2.LSP
;;87.Ｓ＿ＭＥＮＵ                                   C:S_MENU      S_MENU.LSP
;;88.１Ｐ端子台                                     C:STB         １Ｐ端子台.LSP
;;89.ダイオードの入力                               C:SDI         ダイオードの入力.LSP
;;90.協約型の外形入力                               C:DBr2        DBr2.LSP        DBr2.DCL
;;91.盤一覧表作成リスプ                             C:BanList1～6 BanList.LSP
;;                                                  C:NpAuto1～3  BanList.LSP
;;92.PickWire:条線を描く。(LWPOLYLINEorLINE)        C:PickW       PickWire.LSP
;;93.３Ｐ端子台の回路入力                           C:STB_M       STB_INN.LSP     DTB.DCL共用
;;94.窓ガラスを書く                                 C:MADO        MADO.LSP
;;95.主幹ブレーカーの外形図を入力                   C:DMCB_IN     DMCB_IN-NEW.LSP + DMCB_IN.CCP
;;96.動力主回路の入力                               C:M_IN        M_IN.LSP + M-SYU.CCP 
;;97.マグネット主回路の選定                         C:S_MC        S_MC.LSP        S_MC.DCL 
;;98-1.寸法線を手動入力した場合に線の色を変更する   C:DIMX        DIMX.LSP
;;98-2.寸法線の尺度を一括で変更する　　　　　　　   C:DIMST       DIMST.LSP
;;99.  分岐Ｃｕバーの回路入力                       C:BrCuS       BrCuS.LSP BrCu.DCL DCLは回路・外形 共用
;;100. 分岐Ｃｕバーの外形入力                       C:BrCuD       BrCuD.LSP BrCu.DCL DCLは回路・外形 共用
;;101. ブレーカー マグネットオプション選定          C:KIBMOP
;;102. 操作変圧器用ブレーカー選定                   C:SOUSATORA 
;;103. 排煙ファン回路図の入力                       C:HAIENFAN
;;104. 排煙ファン外形図の入力                       C:HAIENGAI
;;105. 内外ブレーカーの外形入力                     C:NAIBGAI
;;106. 箱体内機メーカー選択用                       C:HAKONAIM
;;107. 箱体外形メーカー選択用                       C:HAKOGAIM
;;108. 動力分電盤銅バー外形図                       C:CuBrGAI
;;109. 可逆型マグネットの入力                       C:KAGYAKU
;;110. ＰＶ ＳＰＤ（回路図・外形図兼用）            C:SPD
;;111. ＰＶ コメント一覧                            C:PVCOMME
;;112. ＰＶ ３線図ブレーカーの入力                  C:PVMCB
;;113. 圧着端子の入力                               C:ATCH
;;114. 河村ＱＢ図面の自動変換                       C:KWQB


;;==================================================================================================
;;LISPからEXCELを起動し、セルのデーターを取得する   2020/9/9   T.Terada
;;==================================================================================================
;ファイル名とセルの予約例
;(setq FILE "X:/XLS/SEPA-SEL.xlsx");ファイル名に漢字は使えません。拡張子はXLSXとする事。
;(setq CELL "C12")                 ;シート名は必ず"SHEET1"とする事。 

(defun REX (/ sheet)
	(setvar "CMDECHO" 0)
	(setq excelApplication (vlax-get-or-create-object "Excel.Application"))
	(vlax-put-property excelApplication 'Visible :vlax-false);excelを表示しない

;既存のブックを開きます。
	(setq workbooks (vlax-get-property excelApplication 'Workbooks));アクティブなブックを取得
	(setq workbook (vlax-invoke-method workbooks 'Open FILE))
	(setq workbook (vlax-get-property excelApplication 'ActiveWorkbook))
	(vlax-invoke-method workbook 'Activate)                         ;ブックをアクティブにする
	(setq window (vlax-get-property (vlax-get-property workbook 'Windows) 'Item 1))

;ワークシートの読込み。
	(setq sheets (vlax-get-property workbook 'Worksheets))
	(vlax-invoke-method (vlax-get-property sheets 'Item "Sheet1") 'Activate)
;;;	(setq sheet (vlax-get-object excelApplication 'ActiveSheet))  ；アクティブなシートを得る
	(setq sheet (vlax-get-property excelApplication 'ActiveSheet))  ；アクティブなシートを得る
;	(princ "\nsheet = ")(princ sheet)

;セルの読込み。
	(setq rangeObj (vlax-get-property sheet 'Range CELL))
	(setq value2 (vlax-get-property rangeObj 'Value))
	(setq value (vlax-variant-value value2))
	(princ "\nvalue = ")(princ value)

;EXCEL終了処理	
    (vlax-invoke-method workbook 'Close)
    (vlax-invoke-method workbooks 'Close)
	(vlax-invoke-method excelApplication 'Quit);EXCEL終了
	(vlax-release-object excelApplication)     ;VLAの開放
	(vlax-object-released-p excelApplication)
	(princ "\nEXCEL終了")
)


;;=================================================================================================
;; シンボルを挿入するだけ。     S_MENU用、VBAから呼出専用      IN1.LSP        2020/8/19  By.T.Terada
;;=================================================================================================
 (defun C:IN1(/);（属性無し）（トランス外形入力）TYP = S
 	  (princ "\n挿入点を指示：")
      (command "INSERT" SYM1 pause "1" "1" "0")
 )
;--------------------------------------------------------------------------------------------------
 (defun C:IN2(/);（*insert 属性無し）TYP = T
      (princ "\n挿入点を指示：")
      (setq SYM1 (strcat "*" SYM1))(command "INSERT" SYM1 pause "1" "0")
 )
;--------------------------------------------------------------------------------------------------
 (defun C:IN3(/);（部品属性有り）TYP = I
      (princ "\n挿入点を指示：")
      (command "INSERT" SYM1 pause "1" "1" "0" PNA "" T1 T2 T3 T4 "" "" "" "" "" MKN "1" "" "")
 )
;--------------------------------------------------------------------------------------------------
 (defun C:IN4(/);（外型属性有り）TYP = G
      (princ "\n挿入点を指示：")
      (command "INSERT" SYM1 pause "1" "1" "0" PNA T1 T2 T3 T4 "")
 )
;--------------------------------------------------------------------------------------------------
 (defun C:CLD(/);（雲マークを描く）
       (command "RevCloud")
 )
;;==================================================================================================
;; 1.エラー処理     だらだらも出さない関数、エラーメッセージは出す。
;;==================================================================================================
;;(setq *error* *err1*) <=== この行を各リスプの関数定義の下に書く
(defun *err1* ( msg )
   (setq *error* nil)
   (if (or (= msg "関数はキャンセルされました") (= msg "quit/exit による中止"))
      (progn
         (princ "\n")
      )
      (progn
;         (princ (strcat "\nＫＤエラーです！！  処理を中止します！:" msg))
         (princ "\nエラーです！処理を中止します:")(princ)
         (EXIT)
      )
   )
)
(defun *err2* ( msg )
   (setq *error* nil)
   (princ)
   (setvar "USERS5" "M");SLIを手動モードに戻す
   (setvar "CMDECHO" 1)
   (princ (strcat "\nUSERS5を戻して、処理を中止します！:" msg))
)
(defun *err3* ( )
   (setq *error* nil)
   (princ "\n")
)
;;==================================================================================================
;; 2.共通関数
;;==================================================================================================
;;2-1.(act_radioset 引数1 引数2) はラジオボタン用の set_tile 関数。
;;      引数1:DCLの radio_button の key のリスト、引数2:action_tile で得た値
(defun act_radioset(KEY_LIST DATA /)(foreach n KEY_LIST (if (= n DATA)(set_tile n "1"))))
;;--------------------------------------------------------------------------------------------------
;;2-2  Slider Sub  スライダーとエディットボックス用の関数
(defun SLD_ac(val why)(if (>= why 1)(set_tile "EDB" val)));ｽﾗｲﾀﾞｰの値をｴﾃﾞｨｯﾄBOXにセット(中間結果を表示)
(defun EDB_ac(val why)(if (>= why 1)(set_tile "SLD" val)));ｴﾃﾞｨｯﾄBOXの値をｽﾗｲﾀﾞｰにセット(中間結果を表示)
;---------------------------------------------------------------------------------------------------------
;;2-3  制御用ラインを手動モードに戻す              C:手動
(defun C:手動() 
    (progn (setvar "USERS5" "M")(princ "手動モードに変更されました。"))
)
;;--------------------------------------------------------------------------------------------------
;;2-4  num：整数 , numが偶数ならT 奇数ならnil
(defun cl_evenp ( num )  (zerop (logand num 1)))
;;--------------------------------------------------------------------------------------------------
;;2-5  PURGEで使用していないBLOCKを削除                 PG.LSP
(defun C:PG () (command "PURGE" "B" ""))
;;==================================================================================================
;; 3.LISPの開始と終了
;;==================================================================================================
(defun ST1 ()
  (setvar "CMDECHO" 0)
)

(defun END1 ()
  (setvar "CMDECHO" 1)
  (redraw)
)
(defun C:ST1 ()
  (setvar "CMDECHO" 0)
)
(defun C:END1 ()
  (setvar "CMDECHO" 1)
  (redraw)
)
;;==================================================================================================
;; 4.文字を縦書きにする       TM0.LSP      '98/AUG/05  By T.Terada
;;==================================================================================================
(defun C:TM0(/)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (command "-STYLE" "TM0" "" "" "" "" "" "" "")
   (princ "\n < 縦書きに変更しました。>")
   (setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 5.文字横書をきにする       M0.LSP      '98/AUG/05  By T.Terada
;;==================================================================================================
(defun C:M0(/)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (command "-STYLE" "M0" "" "" "" "" "" "" "")
   (princ "\n < 横書きに戻しました。>")
   (setvar "CMDECHO" 1)
)

;;==================================================================================================
;;6.表紙記入
;;==================================================================================================
(defun C:HYOUSHI ( / DWG NOU TYU NAI)
    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(command "COLOR" 2)
    (setq DWG (GETVAR "DWGNAME"))
    (setq DWG (SUBSTR DWG 1 5));;製番
    (if (= (getvar "USERI5") 1)
        (progn
            (setq NOU NOUNYU);;納入先名
            (setq TYU TYUMON);;注文先名
            (setq NAI NAIYOU);;内容
        )
    )
    (if (= (getvar "USERI5") 0)
        (progn
             (setq NOU (getstring "\n  納入先名を入力:"))
             (setq TYU (getstring "\n  注文先名を入力:"))
             (setq NAI (getstring "\n  内容を入力:"))
        )
    )

    ;;製番を全角にする
    (setq DWG (strcase DWG));;製番を大文字にする
    (setq cnt 1)
    (setq zenDWG "")
    (while (<= cnt 5)
        (setq mySTR (substr DWG cnt 1))
        (cond
            ((= mySTR "0")(setq mySTR "０"))
            ((= mySTR "1")(setq mySTR "１"))
            ((= mySTR "2")(setq mySTR "２"))
            ((= mySTR "3")(setq mySTR "３"))
            ((= mySTR "4")(setq mySTR "４"))
            ((= mySTR "5")(setq mySTR "５"))
            ((= mySTR "6")(setq mySTR "６"))
            ((= mySTR "7")(setq mySTR "７"))
            ((= mySTR "8")(setq mySTR "８"))
            ((= mySTR "9")(setq mySTR "９"))
            ((= mySTR "0")(setq mySTR "０"))
            ((= mySTR "A")(setq mySTR "Ａ"))
            ((= mySTR "B")(setq mySTR "Ｂ"))
            ((= mySTR "C")(setq mySTR "Ｃ"))
            ((= mySTR "D")(setq mySTR "Ｄ"))
            ((= mySTR "E")(setq mySTR "Ｅ"))
            ((= mySTR "F")(setq mySTR "Ｆ"))
            ((= mySTR "G")(setq mySTR "Ｇ"))
            ((= mySTR "H")(setq mySTR "Ｈ"))
            ((= mySTR "I")(setq mySTR "Ｉ"))
            ((= mySTR "J")(setq mySTR "Ｊ"))
            ((= mySTR "K")(setq mySTR "Ｋ"))
            ((= mySTR "L")(setq mySTR "Ｌ"))
            ((= mySTR "M")(setq mySTR "Ｍ"))
            ((= mySTR "N")(setq mySTR "Ｎ"))
            ((= mySTR "O")(setq mySTR "Ｏ"))
            ((= mySTR "P")(setq mySTR "Ｐ"))
            ((= mySTR "Q")(setq mySTR "Ｑ"))
            ((= mySTR "R")(setq mySTR "Ｒ"))
            ((= mySTR "S")(setq mySTR "Ｓ"))
            ((= mySTR "T")(setq mySTR "Ｔ"))
            ((= mySTR "U")(setq mySTR "Ｕ"))
            ((= mySTR "V")(setq mySTR "Ｖ"))
            ((= mySTR "W")(setq mySTR "Ｗ"))
            ((= mySTR "X")(setq mySTR "Ｘ"))
            ((= mySTR "Y")(setq mySTR "Ｙ"))
            ((= mySTR "Z")(setq mySTR "Ｚ"))
            ((= mySTR "-")(setq mySTR "－"))
        )
        ;;アスキーコードを使ったやり方だがchrは全角変換ができない
        ;;(setq myASC (ascii mySTR))
        ;;(setq myASC (- myASC 32225))
        ;;(setq mySTR (chr myASC))
        
        (setq zenDWG (strcat zenDWG mySTR))
        (setq cnt (+ cnt 1))
    )
    (command "TEXT" "S" "M0" "1700,1825" "60" "0" NOU)
    (command "TEXT" "S" "M0" "1700,1625" "60" "0" TYU)
    (command "TEXT" "S" "M0" "1775,1425" "60" "0" NAI)
    (command "TEXT" "S" "M0" "2025,1150" "50" "0" zenDWG)
    (setvar "USERI5" 0)(setvar "BLIPMODE" 1)(setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 7.基準図枠サイズの取得
;;==================================================================================================
(defun C:SZ (/ LMAX)
  (setq *error* *err1*)
  (setq SZ nil)
  (setq LMAX (getvar "limmax"))  ;現図面の図面範囲
     (progn
        (if (= (car LMAX) 2100)(setq SZ "A5"));;A5追加20130204大田
        (if (= (car LMAX) 2970)(setq SZ "A4"))
        (if (= (car LMAX) 4200)(setq SZ "A3"))
        (if (= (car LMAX) 5940)(setq SZ "A2"))
        (if (= (car LMAX) 8400)(setq SZ "A1"))
        (if (= (car LMAX) 11880)(setq SZ "A0"))
     )
  (princ "\n現在の図枠は <")(princ SZ)(princ ">です。")(princ)
)
;;==================================================================================================
;; 8.ドーナッツ（交点）直径は図枠サイズで変更      9.KOUTEN は削除
;;==================================================================================================
(defun C:DX (/ DD)
	(C:SZ)
	(if (= SZ "A0")(setq DD "30"))
	(if (= SZ "A1")(setq DD "20"))
	(if (= SZ "A2")(setq DD "15"))
	(if (= SZ "A3")(setq DD "10"))
    (command "color" "2")(command "donut" "0" DD)
)
;;==================================================================================================
;; 10.属性のみのシンボル"ZOKU"を挿入
;;==================================================================================================
(defun C:ZOKU(/ PC)
   (setq *error* *err1*)
   (princ "図形選択で「ここをクリック」を選んで編集して下さい。")
   (setq PC (getpoint "\n挿入点を指示... "))
   (command "INSERT" "ZOKU" PC "1" "1" "0" "" "" "ここをクリック" "" "" "" "" "" "" "" "" "" "1" "" "")
   (command "EATTEDIT")
)
;;==================================================================================================
;;11.寸法記入                                       C:DIMT
;;==================================================================================================
(defun C:DIMT ()
   (setq *error* *err1*)
   (command "COLOR" 2)(command "DIMLINEAR" pause pause pause)(command "COLOR" 4)
)
;;==================================================================================================
;; 12.ハッチパターンの記入（ＡＮＳＩ３１）   HT2.LSP  通常はHT3を使用の事。
;;==================================================================================================
(defun C:HT2( / HPNAME HP);For ICAD5
    (setvar "CMDECHO" 0)
    (command "COLOR" 3)  ;;緑色
    (setvar "HPNAME" "ANSI31")
    (command "LINETYPE" "S" "CONTINUOUS" "") ;;実線

    HP = 212 ;;A0～A5以外の場合
    (C:SZ)
    (cond
        ((= SZ "A0")(setq HP 424))
        ((= SZ "A1")(setq HP 300))
        ((= SZ "A2")(setq HP 212))
        ((= SZ "A3")(setq HP 150))
        ((= SZ "A4")(setq HP 106))
        ((= SZ "A5")(setq HP 75))
    )
    ;;(princ "\nHP=")(princ HP)
    
    (command "-BHATCH" "P" "ANSI31" HP "0" pause "")
    (redraw)
    (command "COLOR" 4) ;;水色
    (setvar "CMDECHO" 1)
)

(defun C:HT3( / HPNAME HP);For IJ-CAD2000
    (setvar "CMDECHO" 0)
    (command "COLOR" 3)  ;;緑色
    (setvar "HPNAME" "ANSI31")
    (command "LINETYPE" "S" "CONTINUOUS" "") ;;実線

;;    HP = 212 ;;A0～A5以外の場合
    (C:SZ)
    (cond
        ((= SZ "A0")(setq HP 424));12
        ((= SZ "A1")(setq HP 300));10
        ((= SZ "A2")(setq HP 212));8
        ((= SZ "A3")(setq HP 150));5
        ((= SZ "A4")(setq HP 106));3
        ((= SZ "A5")(setq HP 75));
    )
    (princ "\nHP=")(princ HP)(princ "\n")
    (command "HATCH" "ANSI31" HP "0" pause "")
    (redraw)
    (command "COLOR" 4) ;;水色
    (setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 12-2.図枠サイズによるハッチパターンの設定（ＡＮＳＩ３１）   SHT.LSP  通常はHT3を使用の事。
;;==================================================================================================
(defun C:SHT(/)
    (C:SZ)
    (cond
        ((= SZ "A0")(setq HP 12));12
        ((= SZ "A1")(setq HP 300));10
        ((= SZ "A2")(setq HP 212));8
        ((= SZ "A3")(setq HP 150));5
        ((= SZ "A4")(setq HP 106));3
        ((= SZ "A5")(setq HP 75));2
    )
    (princ "\nHP=")(princ HP)(princ "\n")
)
;;==================================================================================================
;; 13.連続　文字記入プログラム  旧A-TXT.LSPをまとめる。2004/4/10  左揃え,中央揃え,左揃え、線上5mm,両端揃え
;;==================================================================================================
(defun C:ATXT ( / TYP TX P01 P02 P1X P1Y)
   (setq *error* *err1*)
   (C:ST1)
;   (initget "L C S W")
   (initget "L:左寄せ C:中央揃 S:左寄の線上5mm W:両端揃 L:Enter")
   (setq TYP (getkword "\n文字記入方式 /L /C /S /W <Enter=左寄せ> : "))
   (COND ((= TYP "L:左寄せ")         (setq TYP "L"))
         ((= TYP "C:中央揃")       (setq TYP "C"))
         ((= TYP "S:左寄の線上5mm")(setq TYP "S"))
         ((= TYP "W:両端揃")       (setq TYP "W"))
         ((= TYP "L:Enter")          (setq TYP "L"))
         ((= TYP nil)                (setq TYP "L"))
   )
;   (if (= TYP nil)(setq TYP "L"))
   (command "COLOR" 2);黄色
   (if (= TYP "L")
       (progn
          (setq TX (getstring "\n左揃えです。  文字を入力 /終了=ﾘﾀｰﾝ:"))
          (while (/= TX "")
              
             (setq P01 (getpoint "\n配置点を指示..."))
             (cond ((/= P01 nil) (command "TEXT" P01 "" "0" TX)))
             (setq TX (getstring "\n文字を入力 /終了=ﾘﾀｰﾝ:"))
          )
       )
   )
   (if (= TYP "C")
       (progn
          (setq TX (getstring "\n中央揃えです。   文字を入力 /終了=ﾘﾀｰﾝ:"))
          (while (/= TX "")
              
             (setq P01 (getpoint "\n配置点を指示(中央)："))
             (cond ((/= P01 nil) (command "TEXT" "S" "M0" "C" P01 "" "0" TX)))
              
             (setq TX (getstring "\n文字を入力 /終了=ﾘﾀｰﾝ:"))
          )
       )
   )
   (if (= TYP "S")
       (progn
          (setq TX (getstring "\n左揃え、線上5mmに記入します。    文字を入力 /終了=ﾘﾀｰﾝ:"))
          (while (/= TX "")
              
             (setq P01 (getpoint "\n配置点を指示..."))
             (setq P1X (+ (car P01) 5))
             (setq P1Y (+ (cadr P01) +5))
             (setq P02 (list P1X P1Y)) 
             (cond ((/= P02 nil) (command "TEXT" P02 "" "0" TX)))
              
             (setq TX (getstring "\n文字を入力 /終了=ﾘﾀｰﾝ:"))
          )
       )
   )
   (if (= TYP "W")
       (progn
          (princ "\n両端揃えで記入します。  連続入力は出来ません") 
          (command "TEXT" "A")
       )
   )
   (command "COLOR" 4)
   ;(IMEMODE nil)
   (C:END1)
)
;;==================================================================================================
;; 連続、文字記入プログラム		左寄せ専用  ATXL.lsp
;;==================================================================================================
(defun C:ATXL ( / TYP TX P01 P02 P1X P1Y)
   (setq *error* *err1*)
   (C:ST1)
   (command "COLOR" 2);黄色
   (setq TYP "L")
       (progn
          (setq TX (getstring "\n左揃えです。  文字を入力 /終了=ﾘﾀｰﾝ:"))
          (while (/= TX "")
             (setq P01 (getpoint "\n配置点を指示..."))
             (cond ((/= P01 nil) (command "TEXT" P01 "" "0" TX)))
             (setq TX (getstring "\n文字を入力 /終了=ﾘﾀｰﾝ:"))
          )
       )
)
;;==================================================================================================
;; 14.線の描画
;;==================================================================================================
;;細  線     PLINE W = 0
  (defun C:PL0 ( / P1)(C:ST1)(setq P1 (getpoint "\nどこから？:"))(command "PLINE" P1 "W" "0" "0")(C:END1))

;;太  線     A3で W = 6
  (defun C:PL6 ( / P1 W)
  (C:ST1)(C:SZ)
  (cond
        ((= SZ "A0")(setq W 14))
        ((= SZ "A1")(setq W 11))
        ((= SZ "A2")(setq W 8))
        ((= SZ "A3")(setq W 6))
  )
  (princ "\n線の太さ=")(princ W)(princ "\n")
  (setq P1 (getpoint "\nどこから？:"))(command "PLINE" P1 "W" W W)
  (C:END1)
  )
;;--------------------------------------------------------------------------------------------------
;;極太線     A3で W = 10
  (defun C:PL10 ( / P1 W)
  (C:ST1)(C:SZ)
  (cond
        ((= SZ "A0")(setq W 18))
        ((= SZ "A1")(setq W 15))
        ((= SZ "A2")(setq W 12))
        ((= SZ "A3")(setq W 10))
  )
  (princ "\n線の太さ=")(princ W)(princ "\n")
  (setq P1 (getpoint "\nどこから？:"))(command "PLINE" P1 "W" W W)
  (C:END1)
  )

;;==================================================================================================
;; 15.既存線の線幅変更
;;==================================================================================================
;;細  線     PLINE W = 0
  (defun C:EDW0 () (setq *error* *err1*) (command "PEDIT" pause "W" "0" ""))

;;太  線     A3で W = 6
  (defun C:EDW6 () (setq *error* *err1*) 
  	(C:SZ)
  	(cond
        ((= SZ "A0")(setq W 14))
        ((= SZ "A1")(setq W 11))
        ((= SZ "A2")(setq W 8))
        ((= SZ "A3")(setq W 6))
    )
    (command "PEDIT" pause "W" W "")
    (princ "\n線の太さ=")(princ W)(princ "\n")
  )

;;極太線     A3で = 10
  (defun C:EDW10() (setq *error* *err1*)
  	(C:SZ)
  	(cond
        ((= SZ "A0")(setq W 18))
        ((= SZ "A1")(setq W 15))
        ((= SZ "A2")(setq W 12))
        ((= SZ "A3")(setq W 10))
    )
    (command "PEDIT" pause "W" W "")
    (princ "\n線の太さ=")(princ W)(princ "\n")
  )
;;==================================================================================================
;; 16.ストレッチ－Ｃ                           1998/Mar/28     By T.Terada
;;==================================================================================================
(defun C:C-STR () (command "STRETCH" "C"))

;;==================================================================================================
;; 回路図用、外部端子〇垂直カット  :CCUT-V.LSP                           2020/OCT/8     By T.Terada
;;==================================================================================================
 (defun C:CCUT-V(/ PC CUT1 CUT2 CUT)
   (setq *error* *err1*)(setvar "CMDECHO" 0)
   (setq LMAX (getvar "limmax"));図面ザイズによってカットサイズを変える
     (progn
        (if (= (car LMAX) 2970) (setq CUT 10));A4
        (if (= (car LMAX) 4200) (setq CUT 10));A3 ;;カットサイズは2005/6/7設計指示
        (if (= (car LMAX) 5940) (setq CUT 12));A2
        (if (= (car LMAX) 8400) (setq CUT 14));A1
        (if (= (car LMAX) 11880)(setq CUT 18));A0
     )
          (setq PC (getpoint "\n  端子挿入点を指示："))
          (setq CUT1 (list (car PC)(- (cadr PC) CUT)))
          (setq CUT2 (list (car PC)(+ (cadr PC) CUT)))
      (command "OSNAP" "OFF")
   (command "BREAK" CUT1 "F" CUT2 CUT1) (redraw)
   (command "circle" PC CUT)
   (redraw)(setvar "CMDECHO" 1)
 )
;;==================================================================================================
;; 17.交点-切断LISP   垂直カット  :CUT-L.LSP                           1998/Mar/28     By T.Terada
;;==================================================================================================
 (defun C:CUT-V(/ PC CUT1 CUT2 CUT)
   (setq *error* *err1*)(setvar "CMDECHO" 0)
   (setq LMAX (getvar "limmax"));図面ザイズによってカットサイズを変える
     (progn
        (if (= (car LMAX) 2970) (setq CUT 10));A4
        (if (= (car LMAX) 4200) (setq CUT 10));A3 ;;カットサイズは2005/6/7設計指示
        (if (= (car LMAX) 5940) (setq CUT 10));A2
        (if (= (car LMAX) 8400) (setq CUT 15));A1
        (if (= (car LMAX) 11880)(setq CUT 20));A0
     )
     (command "OSNAP" "APP")
           
          (setq PC (getpoint "\n    垂直カットする、交点を指示："))
          (setq CUT1 (list (car PC)(- (cadr PC) CUT)))
          (setq CUT2 (list (car PC)(+ (cadr PC) CUT)))
      (command "OSNAP" "OFF")
   (command "BREAK" CUT1 "F" CUT2 CUT1) (redraw)
   (setvar "CMDECHO" 1)
 )
;;==================================================================================================
;; 18.交点-切断LISP   水平カット                                       1998/Mar/28     By T.Terada
;;==================================================================================================
 (defun C:CUT-H (/ PC CUT1 CUT2)
   (setq *error* *err1*)(setvar "CMDECHO" 0)
   (setq LMAX (getvar "limmax"));図面ザイズによってカットサイズを変える
     (progn
        (if (= (car LMAX) 2970) (setq CUT 10));A4
        (if (= (car LMAX) 4200) (setq CUT 10));A3 ;;カットサイズは2005/6/7設計指示
        (if (= (car LMAX) 5940) (setq CUT 10));A2
        (if (= (car LMAX) 8400) (setq CUT 15));A1
        (if (= (car LMAX) 11880)(setq CUT 20));A0
     )
     (command "OSNAP" "APP");INT
           
          (setq PC (getpoint "\n    水平カットする、交点を指示："))
          (setq CUT1 (list (- (car PC) CUT)(cadr PC)))
          (setq CUT2 (list (+ (car PC) CUT)(cadr PC)))
      (command "OSNAP" "OFF")
   (command "BREAK" CUT1 "F" CUT2 CUT1) (redraw)
   (setvar "CMDECHO" 1)
 )
;;==================================================================================================
;;窓選択    19:WMOVE 20:WCOPY 21:WERASE
;;==================================================================================================
(defun C:WMOVE () (command "MOVE" "W" pause pause ""))  ;ＭＯＶＥ－窓

(defun C:WCOPY () (command "COPY" "W" pause pause ""))  ;ＣＯＰＹ－窓

(defun C:WERASE () (command "ERASE" "W" pause pause ""));ＥＲＡＳＥ－窓
;;==================================================================================================
;; 22.自動連番プログラム   A-NUM.LSP
;;==================================================================================================
(defun C:ANUM (/ IN P01)
   (setq *error* *err1*)(setvar "CMDECHO" 0)(command "COLOR" 2);黄色
    (setq IN (getint "\n何番から始めますか？:"))
    (setq P01 (getpoint "\n配置点を指示 /終了=ﾘﾀｰﾝ ..."))
      (while (/= P01 nil)
         (command "TEXT" P01 "" "" IN)
         (setq IN (1+ IN))
          (setq P01 (getpoint "\n配置点を指示 /終了=ﾘﾀｰﾝ ..."))
      )
   (command "REDRAW")(command "COLOR" 4)(setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 23.TMOVE.LSP 2000.2.6 create by f.izawa
;;==================================================================================================
;文字列を移動するだけ。基点指定の手間が省くのが目的。
(defun c:tmove()
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
  (setq ent (entsel "\n移動文字列を選択:" ))
  (if (/= ent nil)
    (progn
        (setq ename (car ent))
        (setq edata (entget ename))
        (if (= (cdr(assoc 0 edata)) "TEXT")
          (progn
            (setq pos (cdr(assoc 11 edata)))
            (if (equal pos '(0.0 0.0 0.0))(setq pos (cdr(assoc 10 edata))))
            (command "move" "single" ename pos )
          )
        )
      
    )
  )
  (setq *error* old_error)
  (princ)
)
;;==================================================================================================
;; 24.TCOPY2.LSP 2000.3.3 create by f.izawa  2002.8.4 update
;;==================================================================================================
;文字列をコピー後、文字編集を行う。for IntelliCAD
(defun c:tcopy( / my_error icad_esc old_error esel ename ent pos loop p)
   (defun my_error(msg)
      (command) (setq *error* old_error) (princ)
   )
   
   (setvar "CMDECHO" 0)
   (setq old_error *error*)
   (setq *error* my_error)
   (if (setq esel (entsel "\nコピー元の文字列を選択 (ESC=終了) : " ))
       (progn
          (setq ename (car esel) ent (entget ename))
             (if (= (cdr (assoc 0 ent)) "TEXT")
                 (progn
                    (if (= (+ (cdr (assoc 72 ent))(cdr (assoc 73 ent))) 0)
                        (setq pos (cdr (assoc 10 ent)))
                        (setq pos (cdr (assoc 11 ent)))
                    )
                    (setq loop T)
                    (while loop
                        (command "copy" "single" ename pos);; pause)
                            (if (setq p (getpoint "\nコピー先を指示 <Enter=終了> : "))
                                (progn (command p) (command "DDEDIT" (entlast)))
                                (progn (command)   (setq loop nil))
                            )
                    );while
                 );progn
             );if
       )
   )
   (setq *error* old_error)
   (princ)
)
(princ)
;;==================================================================================================
; 25.文字列を変更する。       TED.LSP / TED.DCL     2003/JUN   By T.Terada 現在は、未使用です。
;;==================================================================================================
(defun C:TED ( / AA BB CC DD TXT TXT2 ENT);
;   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (princ "\n変更するオブジェクトを指示:")
   (if (null (setq AA (ssget (list '(0 . "TEXT")))));;
             (setq AA (ssget "x" (list '(0 . "TEXT"))));;
   );;
   (if (null (setq AA (ssget (list '(0 . "TEXT")))))
             (exit)
   )

   (setq BB (ssname AA 0))
   (setq ENT (entget BB))
   (setq TXT (cdr (assoc 1 ENT)));文字列取得
;;ダイアログの表示
  (setvar "FILEDIA" 1)
  (setq dcl_id (load_dialog "X:/LSP-I/2KD.DCL"))
     (if (not (new_dialog "KD_TED" dcl_id))(exit))
     (action_tile "cancel" "(done_dialog)(setvar \"FILEDIA\" 1)(exit)")
     (set_tile "edit_1" TXT) ;属性値
;;タイルの値を読込
  (action_tile "edit_1" "(setq TXT2 (get_tile \"edit_1\"))")
        (start_dialog)          ;入力待ちの状態
        (unload_dialog dcl_id)
        (princ)
;;属性変更
  (entmod (subst (cons 1 TXT2)(assoc 1 ENT) ENT))
     (command "regen")
    ;(IMEMODE nil)
  (princ "\n属性変更終了")
)
;;==================================================================================================
;; 26.各種四角形の描画  4KAKU.VBI + 4KAKU.LSP        2003/Feb ==>2020/Oct    by T.Terada
;;==================================================================================================
(defun C:4KAKU ( / P1 P2 P0 W0 H0 W2 H2)
    (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
	(princ "\nPX = ") (princ PX) (princ)
	(princ "\nSEN = ")(princ SEN)(princ)
    
    (if (= PX nil)(exit))
    (if (= SEN "S")(command "LINETYPE" "S" "CONTINUOUS" ""))
    (if (= SEN "T")(command "LINETYPE" "S" "DASHED" ""))
;;フリーBOXの場合
 (if (= PX "FB")
    (progn 
        (setq P1 (getpoint "\n 始点を指示...."))
        (setq P2 (getpoint "\n 対角の１点を指示...."))
        (command "rectangle" P1 P2)
        (princ)
    )

    ;;入力項目
    (progn
        (setq P0 (getpoint "\n 挿入点を指示...."))
        (setq W0 (getreal "\n横幅を指示してください．:"))
        (setq H0 (getreal "\n高さを指示してください．:"))
        (setq W2 (/ W0 2)); W0の1/2
        (setq H2 (/ H0 2)); H0の1/2

    ;;各位置の計算
       (setq UL1 P0)
       (setq UL2 (list (+ (car P0) W0) (- (cadr P0) H0)))
       (setq UC1 (list (- (car P0) W2) (cadr P0)))
       (setq UC2 (list (+ (car UC1) W0) (- (cadr UC1) H0)))
       (setq UR1 (list (- (car P0) W0) (cadr P0)))
       (setq UR2 (list (+ (car UR1) W0) (- (cadr UR1) H0)))
       (setq CL1 (list (car P0) (+ (cadr P0) H2)))
       (setq CL2 (list (+ (car CL1) W0) (- (cadr CL1) H0)))
       (setq CC1 (list (- (car P0) W2) (+ (cadr P0) H2)))
       (setq CC2 (list (+ (car CC1) W0) (- (cadr CC1) H0)))
       (setq CR1 (list (- (car P0) W0) (+ (cadr P0) H2)))
       (setq CR2 (list (+ (car CR1) W0) (- (cadr CR1) H0)))
       (setq DL1 (list (car P0) (+ (cadr P0) H0)))
       (setq DL2 (list (+ (car DL1) W0) (- (cadr DL1) H0)))
       (setq DC1 (list (- (car P0) W2) (+ (cadr P0) H0)))
       (setq DC2 (list (+ (car DC1) W0) (- (cadr DC1) H0)))
       (setq DR1 (list (- (car P0) W0) (+ (cadr P0) H0)))
       (setq DR2 P0)
    ;;描画
       (if (= PX "UL")(command "rectangle" UL1 UL2))
       (if (= PX "UC")(command "rectangle" UC1 UC2))
       (if (= PX "UR")(command "rectangle" UR1 UR2))
       (if (= PX "CL")(command "rectangle" CL1 CL2))
       (if (= PX "CC")(command "rectangle" CC1 CC2))
       (if (= PX "CR")(command "rectangle" CR1 CR2))
       (if (= PX "DL")(command "rectangle" DL1 DL2))
       (if (= PX "DC")(command "rectangle" DC1 DC2))
       (if (= PX "DR")(command "rectangle" DR1 DR2))
    );end progn
 );end if
       (command "LINETYPE" "S" "CONTINUOUS" "")(command "REDRAW")
       (setq PX nil SEN nil)
)

;;==================================================================================================
;; 28.図枠変更LISP  CH-WAKU.LSP    2003/JUL改 ﾃﾞｰﾀｰを属性で処理  2020/OCT IJCAD対応      By T.Terada
;;        図枠は右記で構成される       AnB.dwg:図枠本体
;;                                     A3W.dwg:件名・盤名称・製番等の記入属性
;;                                     A3C.dwg:板金・塗装覧の図枠と属性
;;                                     A3R.dwg:不要 旧来歴覧
;;==================================================================================================
(defun C:CH-WAKU( / n n2 Rn Rnn RX AA BB CC DD ENT DAT DAT1 DAT2 DAT3 SYM SYMD 
                    ATTName ATTDat RDAT LMAX ZS RP;図面範囲
                    SZ SZW SZR SZC                ;変更元の用紙サイズ
                    SZ2 SZ2W SW2R SZ2C SZ2K       ;目的の用紙サイズ
                    SZ1                           ;標準図枠以外の図面範囲が設定
                )                                 ;されている時の新用紙ｻｲｽﾞ
  (setq *error* *err1*)
  (setvar "CMDECHO" 1)
  (setq LMAX (getvar "limmax"))  ;現図面の図面範囲
     (progn
        (princ "\nLMAX=")(princ LMAX);状態表示
        (if (= (car LMAX) 2100) (setq SZ "A5"));A5追加20130204
        (if (= (car LMAX) 2970) (setq SZ "A4"))
        (if (= (car LMAX) 4200) (setq SZ "A3"))
        (if (= (car LMAX) 5940) (setq SZ "A2"))
        (if (= (car LMAX) 8400) (setq SZ "A1"))
        (if (= (car LMAX) 11880)(setq SZ "A0"))
     )
	 (setq SZI SZ);最初の図枠
            (setq SZW (strcat SZ "W"));変更元の件名記入覧
            (setq SZC (strcat SZ "C"));変更元の板金・塗装覧
            (setq SZR (strcat SZ "R"));変更元の来歴記入覧   （不要）
            (setq SZB (strcat SZ "B"));変更元の図枠内部ブロック（属性無し）

;(princ "\nSZW=")(princ SZW)(princ "\nSZC=")(princ SZC);状態表示
;(princ "\nSZR=")(princ SZR)(princ "\nSZB=")(princ SZB);状態表示
;(princ "\nSZ=")(princ SZ);;;(princ "\nSZ2=")(princ SZ2);状態表示
;(princ "\nLMAX")(princ LMAX);状態表示

;;-----------------------------------------------------------------------------
;; 標準図枠以外の図面範囲が設定されている時の処理
      (if (= SZ nil)          ;用紙ｻｲｽﾞが記入されていない時の処理
          (progn 
             (setq dcl_id (load_dialog "X:/LSP-I/2KD.DCL"))
             (if (not (new_dialog "SET_WAKU" dcl_id) )(exit))
             (mode_tile "accept" 1)  ;;; OKﾎﾞﾀﾝをOFF
             ;図枠の選択用ダイアログボックス
             (action_tile "cancel" "(done_dialog)(setvar \"FILEDIA\" 1)(exit)")
             (action_tile "S_A0" "(setq SZ1 \"A0\")(mode_tile \"accept\" 0)")
             (action_tile "S_A1" "(setq SZ1 \"A1\")(mode_tile \"accept\" 0)")
             (action_tile "S_A2" "(setq SZ1 \"A2\")(mode_tile \"accept\" 0)")
             (action_tile "S_A3" "(setq SZ1 \"A3\")(mode_tile \"accept\" 0)")
             (action_tile "S_A4" "(setq SZ1 \"A4\")(mode_tile \"accept\" 0)");A4追加20130204
             (action_tile "S_A5" "(setq SZ1 \"A5\")(mode_tile \"accept\" 0)");A5追加20130204

;(princ "\nSZW=")(princ SZW)(princ "\nSZC=")(princ SZC);状態表示
;(princ "\nSZR=")(princ SZR)(princ "\nSZB=")(princ SZB);状態表示
;(princ "\nSZ=")(princ SZ)(princ "\nSZ2=")(princ SZ2);状態表示
;(princ "\nLMAX")(princ LMAX);状態表示

             (start_dialog)(unload_dialog dcl_id)(princ)
                (cond ;;新しい図面範囲を設定
                      ((= SZ1 "A0")(command "LIMMAX" "11880,8400"))
                      ((= SZ1 "A1")(command "LIMMAX"  "8400,5940"))
                      ((= SZ1 "A2")(command "LIMMAX"  "5940,4200"))
                      ((= SZ1 "A3")(command "LIMMAX"  "4200,2970"))
                      ((= SZ1 "A4")(command "LIMMAX"  "2970,2100"));A4追加20130204
                      ((= SZ1 "A5")(command "LIMMAX"  "2100,1485"));A5追加20130204
                )
          );progn
    );end if
;;-----------------------------------------------------------------------------
;;用紙サイズの変更
  (if (/= SZ nil)          ;用紙ｻｲｽﾞが記入されている場合の処理
      (progn
           (setq dcl_id (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "CH_WAKU" dcl_id) )(exit))
          (mode_tile "accept" 1)                                 ; OKﾎﾞﾀﾝをOFF
          ;図枠の選択用ダイアログボックス
          (action_tile "cancel" "(done_dialog)(setvar \"FILEDIA\" 1)(exit)")
          (action_tile "KD_A0" "(setq SZ2 \"A0\")
                 (command \"LIMMAX\" \"11880,8400\")(mode_tile \"accept\" 0)")
          (action_tile "KD_A1" "(setq SZ2 \"A1\")
                 (command \"LIMMAX\" \"8400,5940\")(mode_tile \"accept\" 0)")
          (action_tile "KD_A2" "(setq SZ2 \"A2\")
                 (command \"LIMMAX\" \"5940,4200\")(mode_tile \"accept\" 0)")
          (action_tile "KD_A3" "(setq SZ2 \"A3\")
                 (command \"LIMMAX\" \"4200,2970\")(mode_tile \"accept\" 0)")
          (action_tile "KD_A4" "(setq SZ2 \"A4\")
                 (command \"LIMMAX\" \"2970,2100\")(mode_tile \"accept\" 0)");A4追加20130204
          (action_tile "KD_A5" "(setq SZ2 \"A5\")
                 (command \"LIMMAX\" \"2100,1485\")(mode_tile \"accept\" 0)");A5追加20130204
          (set_tile "SZ" SZ)
          (start_dialog)(unload_dialog dcl_id)(princ)
      );progn
    );end if
;;--------------------------------------------------------------------------------------------------
;;図面内の各図枠ブロック内の属性値を取得する
;ﾌﾞﾛｯｸA3C(板金・塗装属性)が 挿入されているかをﾁｪｯｸ
    (princ "\nSZC =")(princ SZC)
    (setq n 0)(setq Ry nil Rn nil)
    (setq AA (ssget "X" (list '(66 . 1))))
    (while
       (setq BB (ssname AA n));          (princ "\nBB =")(princ BB)
       (setq ENT (entget BB)) ;          (princ "\nENT =")(princ ENT)
           (if (= (cdr (assoc 2 ENT)) SZC)
               (progn (setq Ry 3))               ;A3Cが有る時
           )
           (if (= Ry 3)(setq Ryn 3))
           (setq n (+ n 1))
    );while
    (if (/= (cdr (assoc 2 ENT)) SZC)(progn (setq Rn 2)))
(princ "\nRy =")(princ Ry)(princ "\nRn =")(princ Rn)
    (if (> Ry Rn)(setq RP 3)(setq RP 2))
    (if (= Ry nil)(setq RP 1))
(princ "\nRP =")(princ RP)
;;--------------------------------------------------------------------------------------------------
;各図枠覧の属性を読出し
    (setq DAT1 nil DAT2 nil DAT3 nil) (setq AA nil BB nil CC nil DD nil ENT nil)
    (setq RX 1)         ;ﾘﾋﾟｰﾄの初期値
    (repeat RP
       (setq n 0)
       (setq DAT nil)
       (if (= RX 1)(setq SYM SZW));件名記入覧
;       (if (= RX 2)(setq SYM SZR));来歴記入覧
       (if (= RX 3)(setq SYM SZC));板金・塗装覧
       (setq AA (ssget "X" (list '(66 . 1))));;66 = 挿入ブロック
       (while
           (setq BB (ssname AA n))
           (setq ENT (entget BB))
           (setq CC (entnext BB))  ; 次の図形名
           (setq DD (entget CC))   ; 図形リスト
           (if (= (cdr (assoc 2 ENT)) SYM)
               (progn
                 ;属性名称と入力値を取得 === ATTRIB
                  (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                         (if (= (cdr (assoc 0 DD)) "ATTRIB")
                             (progn
                                (setq DAT
                                   (append DAT (list (list (cdr (assoc 2 DD))
                                                           (cdr (assoc 1 DD)))))
                                );setq
                                (if (= RX 1)(setq DAT1 DAT));図枠記入覧のﾃﾞｰﾀｰ
;                                (if (= RX 2)(setq DAT2 DAT));来歴記入覧のﾃﾞｰﾀｰ
                                (if (= RX 3)(setq DAT3 DAT));板金,塗装覧のﾃﾞｰﾀｰ
                             );progn
                         );if
                         (setq CC (entnext CC) ; 次の図形名
                               DD (entget CC)  ; 図形リスト
                         )
                 );while
                 (entdel BB)
               );progn
           );if
          (setq n (1+ n))
       );while
       (setq RX (1+ RX))
    );repeat

;(princ "\nDAT1=")(princ DAT1)(princ "\nDAT2=")(princ DAT2)(princ "\nDAT3=")(princ DAT3)
;(princ "\nSZW=")(princ SZW)(princ "\nSZC=")(princ SZC)
;(princ "\nSZR=")(princ SZR)(princ "\nSZB=")(princ SZB)
;(princ "\nSZ=")(princ SZ)(princ "\nSZ2=")(princ SZ2)
;(princ "\nLMAX")(princ LMAX)

;;--------------------------------------------------------------------------------------------------
;;図枠の入れ替え
    (setq SZ2W (strcat SZ2 "W")) ;目的の図枠
;    (setq SZ2R (strcat SZ2 "R"));目的の図枠
    (setq SZ2C (strcat SZ2 "C")) ;目的の図枠
    (setq SZ2K (strcat "*" SZ2 "N"));目的の図枠 *A2N.dwg
;;;;;;;;    (command "ERASE" "PRO" "N" SZB "")            ; 元の図枠を削除(A3B等）
;;--------------------------------------------------------------------------------------------------
;ﾌﾞﾛｯｸA3B（属性無し）を消す
    (setq n 0)
    (setq AA (ssget "X" (list '(0 . "insert"))))
    (while (setq BB (ssname AA n))
           (setq ENT (entget BB));(princ "\nBB = ")(princ BB)
           (if (= (cdr (assoc 2 ENT)) SZB);A3Bを消す
               (entdel BB)
           )
           (setq n (+ n 1))
    );while
    (princ "\n元図枠消しました")
;;--------------------------------------------------------------------------------------------------
;;(princ "\nSZ2K=")(princ SZ2K)
    (command "INSERT" SZ2K (list 0 0) "1" "0")     ; 目的の図枠を挿入
    (if (= RP 3)(command "INSERT" SZ2C (list 0 0) "1" "1" "0" "" "" "" "" "" "" "" "" "" ""))

;;件名覧に表示される図面ｻｲｽﾞを目的の図面ｻｲｽﾞに変更
    (if (= SZ2 "A0") (setq ZS '("ZW8" "A0")))
    (if (= SZ2 "A1") (setq ZS '("ZW8" "A1")))
    (if (= SZ2 "A2") (setq ZS '("ZW8" "A2")))
    (if (= SZ2 "A3") (setq ZS '("ZW8" "A3")))
    (if (= SZ2 "A4") (setq ZS '("ZW8" "A4")));A4追加20130204
    (if (= SZ2 "A5") (setq ZS '("ZW8" "A5")));A5追加20130204

;(princ "\nDAT1 =")(princ DAT1)(princ "\nZS=")(princ ZS)

    (setq DAT1 (subst ZS (nth 7 DAT1) DAT1))<===この行が不動作?
(princ "\nDAT1 = ")(princ DAT1)
;------------------------------------------------------------------------------
;各図枠覧の属性書き換え
    (setq RX 1);ﾘﾋﾟｰﾄの初期値
    (setq AA nil BB nil CC nil DD nil ENT nil)
    (princ "\nRP = ")(princ RP)
    (repeat RP
       (setq n 0 n2 0 DAT nil)
       (if (= RX 1)(progn (setq SYMD SZ2W)(setq RDAT DAT1))) ;件名記入覧
;       (if (= RX 2)(progn (setq SYMD SZ2R)(setq RDAT DAT2)));来歴記入覧
       (if (= RX 3)(progn (setq SYMD SZ2C)(setq RDAT DAT3))) ;板金・塗装覧
       (setq AA (ssget "X" (list '(66 . 1))))
       (while 
           (setq BB (ssname AA n))
           (setq ENT (entget BB))
           (while BB
              (setq CC (entnext BB))
              (setq DD (entget CC))
              (if (= (cdr (assoc 2 ENT)) SYMD)
                  (progn
                      (setq ATTName (cdr (assoc 2 DD)))   ;属性名称をセット
                      (setq ATTDat (cadr (nth n2 RDAT)))  ;属性値をセット
                      (if (= (cdr (assoc 2 DD)) ATTName)
                          (entmod                         ;属性変更
                             (subst(cons 1 ATTDat)
                                   (assoc 1 DD)
                                   DD
                             );subst
                          );entmod
                      );if
                     (setq n2 (1+ n2))
                   );progn
              );if
             (if (= (cdr (assoc 0 DD)) "SEQEND")(setq BB nil)(setq BB CC))
         );while
         (setq n (1+ n))
       );while
       (setq RX (1+ RX))
    );repeat
(princ "loop end")
;;-----------------------------------------------------------------------------
    (C:SZ);図枠別、設定値の変更
    (cond ((= SZ "A0")(setvar "TEXTSIZE" 60)(setvar "LTSCALE" 150)(setvar "DIMSCALE" 200))
          ((= SZ "A1")(setvar "TEXTSIZE" 40)(setvar "LTSCALE" 110)(setvar "DIMSCALE" 160))
          ((= SZ "A2")(setvar "TEXTSIZE" 30)(setvar "LTSCALE" 75) (setvar "DIMSCALE" 120))
          ((= SZ "A3")(setvar "TEXTSIZE" 20)(setvar "LTSCALE" 55) (setvar "DIMSCALE" 90)) 
          ((= SZ "A4")(setvar "TEXTSIZE" 15)(setvar "LTSCALE" 40) (setvar "DIMSCALE" 55)) ;A4追加20130204
          ((= SZ "A5")(setvar "TEXTSIZE" 10)(setvar "LTSCALE" 30) (setvar "DIMSCALE" 40)) ;A5追加20130204
    )
;;-----------------------------------------------------------------------------
;;  グリッドの変更　20130207  (condはifとほぼ同じ）
    (cond ((= SZ "A0")(command "GRID" 200));
          ((or (= SZ "A1")(= SZ "A2")(= SZ "A3"))(command "GRID" 100));
          ((or (= SZ "A4")(= SZ "A5"))(command "GRID" 50));
    )
;;-----------------------------------------------------------------------------
    (command "ZOOM" "E")
;;    (princ "\nグリッド=")  (princ (getvar "GRIDUNIT"));状態表示
;;    (princ "\n文字高さ=")  (princ (getvar "TEXTSIZE"));状態表示
;;    (princ "\n点線尺度=")  (princ (getvar "LTSCALE")) ;状態表示
;;    (princ "\nﾊｯﾁﾝｸﾞ尺度=")(princ (getvar "HPSCALE")) ;状態表示
;;    (princ "\n寸法尺度=")  (princ (getvar "DIMSCALE"));状態表示

    (princ "\n 用紙サイズを ")(princ SZI)(princ "から")
    (princ SZ2)(princ " に変更しました。")
    (setvar "CMDECHO" 1)
);defunEnd

;;==================================================================================================
;; 29.BreakF.LSP    ２点を指定してBREAK      2002/NOV     BY T.Terada
;;==================================================================================================
(defun C:BRKF(/ P1 P2)
    (setvar "CMDECHO" 0)
    (princ "\n<<２点を指定して線を切断>>")
    (setq P1 (getpoint "\n始点を指示:"))
    (setq P2 (getpoint "\n終点を指示:"))
    (command "BREAK" P1 P2)(redraw)
    (setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 30.板金・塗装覧の追加.LISP  For I-CAD     2002/oct   By T.Terada
;;==================================================================================================
(defun C:INS-C ( / SZ1 SZ2 SZC SS SN ENAME EDATA)
   (setq *error* *err1*)(setvar "CMDECHO" 0)
   (C:SZ)
   ;;ブロック AnW.DWG(物件名等)の有無：有りで一旦AnWを削除する。
   (setq SZC (strcat SZ "C")) (setq SZP (strcat SZ "-OP"))
   (setq i 0)
   (setq SS (ssget "X"))
   (setq SN (sslength SS))
   (while (< i SN)
      (setq ENAME (ssname SS i))
      (setq EDATA (entget ENAME))
         (if (= (cdr(assoc 2 EDATA)) SZC) (progn (entdel ENAME)))
         (if (= (cdr(assoc 2 EDATA)) SZP) (progn (entdel ENAME)))
         (setq i(+ i 1))
   )
   (princ "現在の図枠 = ")(princ SZ)
   (setq SZ2 (strcat SZ "C"))
   (command "insert" SZ2 "0,0" "" "" "" "鋼板1.6" "鋼板1.6" "" "" "" "5分艶" "" "" "" "5分艶")
;   (princ "板金・塗装覧 = ")(princ SZ2)
;   (command "EATTEDIT")
	(princ "\n  注意）板金・塗装覧の内容は標準です！ 物件の仕様に適合しているか確認して下さい！！!")(princ)
   (setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 31.属性の文字サイズ変更
;;==================================================================================================
(defun C:ATTH ()
    (setvar "CMDECHO" 0)
    (princ "\n変更したい文字を選択")
    (princ "\n新しい文字ｻｲｽﾞを入力（例：30）：")
    (command "attedit" "y" "*" "*" "*" pause "" "h" pause "")
    (setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 32.属性の表示位置変更
;;==================================================================================================
(defun C:ATTP ()
	(setvar "CMDECHO" 0)
    (command "attedit" "y" "*" "*" "*" pause "" "p" pause "")
    (setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 33.１次幹線の入力  RST,RNT,RN
;;==================================================================================================
;;幹線入力
;;挿入点と記入位置
  (defun PT3 ( / )
     (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
  )
;;RST -----------------------------------------------------------------------------
  (defun C:RST ( / SYM T1 T2 T3 T4 PC MSG)
    (setq KIKI "\n３φ３Ｗ")
    (PT3)
    (setq SYM "RST")
     
    (setq T1 (getstring "\n幹線番号を指示:(例：M1)"))
             (cond ((= T1 "") (setq T1 "M1")))
    (setq T2 "3φ3W 210V")
     
    (setq T3 (getstring "\n回路番号を指示:(例：1)"))
             (cond ((= T3 "") (setq T3 "1")))
     
    (setq T4 (getstring "\n幹線サイズを指示:(例：38)"))
             (cond ((= T4 "") (setq T4 "38")))
             (setq T4 (strcat "CV-T " T4 "Sqe"))
    (setq MSG (strcat "幹線 " KIKI "   挿入点を指示："))(princ MSG)
    (command "INSERT" SYM pause "1" "1" "0" T1 T2 T3 T4 "" "" "")
    (GC)(setvar "CMDECHO" 1)
  )
;;RNT -----------------------------------------------------------------------------
  (defun C:RNT ( / SYM T1 T2 T3 T4 PC MSG)
    (setq KIKI "\n１φ３Ｗ")
    (PT3)
    (setq SYM "RNT")
     
    (setq T1 (getstring "\n幹線番号を指示:(例：L1)"))
             (cond ((= T1 "") (setq T1 "L1")))
    (setq T2 "1φ3W 210/105V")
     
    (setq T3 (getstring "\n回路番号を指示:(例：1)"))
             (cond ((= T3 "") (setq T3 "1")))
     
    (setq T4 (getstring "\n幹線サイズを指示:(例：38)"))
             (cond ((= T4 "") (setq T4 "38")))
             (setq T4 (strcat "CV-T " T4 "Sqe"))
    (setq MSG (strcat "幹線 " KIKI "   挿入点を指示："))(princ MSG)
    (command "INSERT" SYM pause "1" "1" "0" T1 T2 T3 T4 "" "" "")
    (GC)(setvar "CMDECHO" 1)
  )
;;RN -----------------------------------------------------------------------------
  (defun C:RN ( / SYM T1 T2 T3 T4 PC MSG)
    (setq KIKI "\n１φ２Ｗ")
    (PT3)
    (setq SYM "RN")
     
    (setq T1 (getstring "\n幹線番号を指示:(例：L1)"))
             (cond ((= T1 "") (setq T1 "L1")))
    (setq T2 "1φ2W 105V")
     
    (setq T3 (getstring "\n回路番号を指示:(例：1)"))
             (cond ((= T3 "") (setq T3 "1")))
     
    (setq T4 (getstring "\n幹線サイズを指示:(例：38)"))
             (cond ((= T4 "") (setq T4 "38")))
             (setq T4 (strcat "CV " T4 "Sqe-2C"))
    (setq MSG (strcat "幹線 " KIKI "   挿入点を指示："))(princ MSG)
    (command "INSERT" SYM pause "1" "1" "0" T1 T2 T3 T4 "" "")
    (GC)(setvar "CMDECHO" 1)
  )
;;TN -----------------------------------------------------------------------------
  (defun C:TN ( / SYM T1 T2 T3 T4 PC MSG)
    (setq KIKI "\n１φ２Ｗ")
    (PT3)
    (setq SYM "TN")
     
    (setq T1 (getstring "\n幹線番号を指示:(例：L1)"))
             (cond ((= T1 "") (setq T1 "L1")))
    (setq T2 "1φ2W 105V")
     
    (setq T3 (getstring "\n回路番号を指示:(例：1)"))
             (cond ((= T3 "") (setq T3 "1")))
     
    (setq T4 (getstring "\n幹線サイズを指示:(例：38)"))
             (cond ((= T4 "") (setq T4 "38")))
             (setq T4 (strcat "CV " T4 "Sqe-2C"))
    (setq MSG (strcat "幹線 " KIKI "   挿入点を指示："))(princ MSG)
    (command "INSERT" SYM pause "1" "1" "0" T1 T2 T3 T4 "" "")
    (GC)(setvar "CMDECHO" 1)
  )
;;=================================================================================================
;;34. アクリカバーと留めビス、描画              BCOV.LSP    2005/6/26  By T.terada
;;=================================================================================================
(defun C:BCOV ( / P1 P2 WW PC1 PC2 PC3 PX PMX1 PMX2 PMY1 PMY2 P1X P2X P3X P4X TPX XX YY)
      (setq *error* *err1*);ハッチの尺度はA3で5とする。
      (setvar "CMDECHO" 1)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "BCOV" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq WW \"125\")(setq WW2  62)(setq WW3 63)")
          (action_tile "2" "(setq WW \"135\")(setq WW2  67)(setq WW3 68)")
          (action_tile "3" "(setq WW \"165\")(setq WW2  82)(setq WW3 83)")
          (action_tile "4" "(setq WW \"205\")(setq WW2 102)(setq WW3 103)")
          (action_tile "5" "(setq WW \"FF\")")
       (start_dialog)(unload_dialog DCL_ID)(princ)(princ "\nWW=")(princ WW)
       (command "COLOR" "3")
       (if (/= WW "FF")
           (progn
              (setq WW (atoi WW))
               (setq PC1 (getpoint "\n 上端を指示...."))
               (if (/= PC1 nil)(setq PC2 (getpoint PC1 "\n 下端を指示（1000mm以下）")))
              (setq P1 (list (- (car PC1) WW2)(cadr PC1)))
              (setq P2 (list (+ (car PC2) WW3)(cadr PC2)))
              (command "rectangle" P1 P2)
              (setq PX (list (car PC1)(- (cadr PC1) 50)));-Bする内側の点
              (setq TPX (list (car PC1)(+ (cadr PC1) 30)));"樹脂カバー"を記入する点
              (setq ZP1 (list (- (car P1) 50)(+ (cadr P1) 50))) ;;ZOOMしないとハッチでエラーが発生することがある
              (setq ZP2 (list (+ (car P2) 50)(- (cadr P2) 50)))
           )
           (progn ;else ﾌﾘｰの場合
               (setq P1 (getpoint "\n 上端を指示...."))
               (if (/= P1 nil)(setq P2 (getpoint "\n 対角の１点を指示....")))
              (command "rectangle" P1 P2)
              (setq PX (list (+ (car P1) 50)(- (cadr P1) 50)))  ;-BHATCHする内側の点
              (setq XX (abs (/ (- (car  P2) (car  P1)) 2)))
              (setq YY (abs (/ (- (cadr P2) (cadr P1)) 2)))
              (setq ZP1 (list (- (car P1) 50)(+ (cadr P1) 50))) ;;ZOOMしないとハッチでエラーが発生することがある
              (setq ZP2 (list (+ (car P2) 50)(- (cadr P2) 50)))
           )
       )

;       (princ "\nZP1=")(princ ZP1)(princ "\nZP2=")(princ ZP2)
       (if (> (car P1) (car P2)) (setq PMX1 -))
       (if (< (car P1) (car P2)) (setq PMX1 +))
       (if (> (cadr P1)(cadr P2))(setq PMY1 -))
       (if (< (cadr P1)(cadr P2))(setq PMY1 +))
       (if (> (car P1) (car P2)) (setq PMX2 +))
       (if (< (car P1) (car P2)) (setq PMX2 -))
       (if (> (cadr P1)(cadr P2))(setq PMY2 +))
       (if (< (cadr P1)(cadr P2))(setq PMY2 -))
       (setq P1X (list (PMX1 (car P1) 15)(PMY1 (cadr P1) 15)))
       (setq P2X (list (PMX2 (car P2) 15)(PMY2 (cadr P2) 15)))
       (setq P3X (list (car P1X)(cadr P2X)))
       (setq P4X (list (car P2X)(cadr P1X)))
       (C:SHT)
       (command "COLOR" "2")
       (command "_DONUT" "0" "8" P1X P2X P3X P4X "");(command "_DONUT" P2X P2X "")
       ;;(command "DONUT" P3X "")(command "DONUT" P4X "")
       (if (= WW "FF")(setq TPX (list (PMX1 (car P1) XX)(PMY2 (cadr P2) YY))));ﾌﾘｰの"樹脂カバー"を記入する点
       (command "TEXT" "S" "M0" "C" TPX "20" "0" "樹脂カバー")
       (command "COLOR" "3")
       (princ "\nPX = ")(princ PX)
       (command "-HATCH" "P" "ANSI31" HP "0" PX "");(command "-HATCH" "P" "ANSI31" HP "0" "S" "" P1 PX "")
       (command "COLOR" "2")
       (if (/= WW "FF")
           (progn
              (C:SZ)
              (if (= SZ "A3")(progn (setq PC3 (list 3000 200))(command "INSERT" "*COV" PC3 "1" "0")))
              (if (= SZ "A2")(progn (setq PC3 (list 4000 225))(command "INSERT" "*COV" PC3 "1.5" "0")))
              (if (= SZ "A1")(progn (setq PC3 (list 5600 300))(command "INSERT" "*COV" PC3 "2" "0")))
              (if (= SZ "A0")(progn (setq PC3 (list 7800 450))(command "INSERT" "*COV" PC3 "2.5" "0")))
           )
       )
       (command "ZOOM" "E")
       (redraw)(setvar "CMDECHO" 1)
)
;;==================================================================================================
;; 35.ＮＴ－横型 入力プログラム   NT-H.LSP
;;==================================================================================================
(defun C:NT-H( / PC PN L LL PYU PX1 PX2 PYD P1 P2 P3 P4 P5 AT)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0) 
   (setq PC (getpoint "\n挿入点を指示（上端の中心点）:")) 
   (setq PN (getint "\nＮＴは何Ｐですか？:"))
   (command "LINETYPE" "S" "DASHED" "")
     (setq L(+ (* PN 11) 30))
     (setq LL(/ L 2))
     (setq PYU (cadr PC))
     (setq PX1 (- (car PC) LL))
     (setq PX2 (+ (car PC) LL))
     (setq PYD (- (cadr PC) 50))
     (setq P1 (list PX1 PYU))
     (setq P2 (list PX2 PYU))
     (setq P3 (list PX2 PYD))
     (setq P4 (list PX1 PYD))
     (setq P5 (list PX1 PYU))
   (command "PLINE" P1 P2 P3 P4 P5 C)
   (command "LINETYPE" "S" "CONTINUOUS" "")
     (setq AT (list (car PC) (+ 5 PYD)))
   (command "TEXT" "S" "M0" "C" AT "30" (list (car PC) (+ 5 PYD)) "N T")
   (setvar "CMDECHO" 1)
   (setvar "BLIPMODE" 1)
)
;;==================================================================================================
;; 36.ＮＴ－縦型 入力プログラム  NT-V.LSP
;;==================================================================================================
(defun C:NT-V( / PC PN L LL PY1 PY2 PXL PXR P1 P2 P3 P4 P5 AT)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0) 
   (setq PC (getpoint "\n挿入点を指示（左端の中心点）:")) 
   (setq PN (getint "\nＮＴは何Ｐですか？:"))
   (command "LINETYPE" "S" "DASHED" "")
     (setq L(+ (* PN 11) 30))
     (setq LL(/ L 2))
     (setq PY1 (- (cadr PC) LL))
     (setq PY2 (+ (cadr PC) LL))
     (setq PXL (car PC))
     (setq PXR (+ (car PC) 50))
     (setq P1 (list PXL PY1))
     (setq P2 (list PXR PY1))
     (setq P3 (list PXR PY2))
     (setq P4 (list PXL PY2))
     (setq P5 (list PXL PY1))
   (command "PLINE" P1 P2 P3 P4 P5 C)
   (command "LINETYPE" "S" "CONTINUOUS" "")
      (setq AT (list (+ 25 (car PC)) (cadr PC)))
   (command "TEXT" "S" "TM0" "C" AT "30" 270 "N T")
   (setvar "TEXTSTYLE" "M0")
   (setvar "CMDECHO" 1)
   (setvar "BLIPMODE" 1)
)
;;==================================================================================================
;; 37.雲マークを描きます。KUMO.lsp 2001.9.26 ;注意：ｷｬﾝｾﾙした場合、ｼｽﾃﾑ変数PLINETYPEが２で終了する。
;;==================================================================================================
(defun c:kumo( / plinetyp lst ent item)
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
  (setq plinetyp (getvar "PLINETYPE"))
  (setvar "PLINETYPE" 2)
  (command "ORTHOMODE" "OFF")
   
  (princ "\n左まわりに通過点を指示して下さい。Enter=終了 C=閉鎖終了 : ")
  (command "PLINE")
  (while (wcmatch (getvar "CMDNAMES") "*LINE*");;ICAD="POLYLINE"
         (command pause)
  )
	
  (setvar "PLINETYPE" plinetyp)
  (setvar "CMDECHO" 1)
	
  (setq ent (entget (entlast)))
  (if (= (cdr (assoc 1 ent) "LWPOLYLINE"))
	(progn
	  (foreach item ent
		(if (= (car item) 42)
		  (setq item (cons 42 0.65))
		)
		(setq lst (append lst (list item)))
	  )
	  (entmod lst)
      )
  )
  (princ)
(command "ORTHOMODE" "ON")
(princ "\nKUMO:雲マークを描きます。")
(princ)
)
;;==================================================================================================
;; 38.線番号変更.LSP    文字サイズ１５の数字を変更
;;==================================================================================================

(defun C:SENBAN( / EN01 IND01 W01 PC1 PC2 UP01 LEN01 ENT01 COM01 HIGH01 DAT01)
 (setq *error* *err1*)
 (setvar "cmdecho" 0)
 (setq W01 nil) (setq EN01 nil) (setq IND01 0)
 (princ "制御回路の線番号変更、文字ｻｲｽﾞ15のみ数字を加算します。")
 (princ "\n変更したい線番の")
 (if (= (getvar "USERI5") 0)
     (setq W01 (ssget))
 )
 (if (= (getvar "USERI5") 1)
     (progn
        (setq PC1(list PCX PCY))   ;選択範囲始点
        (setq PC2(list PCX2 PCY2)) ;   〃   終点
        (setq W01 (ssget "W" PC1 PC2))
     )
 )
 (cond ((/= W01 nil)
       (if (= (getvar "USERI5") 0)
           (setq UP01 (getint "\n増やす数を入力:"))
       )
       (if (= (getvar "USERI5") 1)
           (setq UP01 sen_cnt)
       )
       (cond ((/= UP01 nil)
             (setq LEN01 (sslength W01))
             (repeat LEN01
                (setq EN01 (ssname W01 IND01))
                (cond ((/= EN01 nil)
                      (setq ENT01 (entget en01))
                      (setq COM01  (cdr (assoc 0 ENT01)))
                      (setq HIGH01 (cdr (assoc 40 ENT01)))
                         (cond ((and (= COM01 "TEXT") (= HIGH01 15.0))
                            (setq DAT01 (cdr (assoc 1 ENT01)))
                            (setq DAT01 (itoa (+ (atoi DAT01) UP01)))
                            (setq ENT01 (subst (cons 1 DAT01) (assoc 1 ENT01) ENT01))
                            (entmod ENT01)
               ))
       ))
       (setq EN01 nil) (setq IND01 (+ IND01 1))
   )))))
   
 (setvar "USERI5" 0)
 (setvar "cmdecho" 1)
)
;;==================================================================================================
;; 39.単位精度（小数点以下の桁表示） 単位精度.LSP       1998/Dec/25  By T.Terada
;;==================================================================================================
(defun C:SEIDO (/)
   (setq *error* *err1*)
    
;; DCLﾌｧｲﾙの読み込み
   (SETQ DCL_ID (LOAD_DIALOG "X:/LSP-i/2KD.DCL"))
   (IF (NOT (NEW_DIALOG "SEIDO" DCL_ID) )(EXIT))
;;   (MODE_TILE "accept" 1)  ;;; OKﾎﾞﾀﾝをOFF
;;
;;図枠の選択用ダイアログボックス
        (ACTION_TILE "cancel" "(DONE_DIALOG)(EXIT)")
	(ACTION_TILE "0" "(SETVAR \"LUPREC\" 0)")
	(ACTION_TILE "1" "(SETVAR \"LUPREC\" 1)")
	(ACTION_TILE "2" "(SETVAR \"LUPREC\" 2)")
	(ACTION_TILE "3" "(SETVAR \"LUPREC\" 3)")
	(ACTION_TILE "4" "(SETVAR \"LUPREC\" 4)")
        (START_DIALOG)
        (UNLOAD_DIALOG DCL_ID)
        (PRINC)
)
;;==================================================================================================
;; 40.集合表示灯（富士電機用）外形図を描く（別途、和泉用は 集合表示灯－和泉.LSP）
;;==================================================================================================
(defun C:FSLC( / kw xcnt ycnt xx yy w pos x y xxx yyy ystr xstr T1 T2 T3 T4 T7 T8 VV)
   (setq *error* *err1*)(setvar "CMDECHO" 0) 
   (setq txths 18.0)
   (initget "30 40")  
   (if (not (setq kw (getkword "\n何角 /30 /40 <Enter=30>: ")))
       (setq kw "30")
   )
    (if (not (setq xcnt (getint "\n横の個数 <Enter=3>: ")))(setq xcnt 3))
    (if (not (setq ycnt (getint "\n縦の個数 <Enter=1>: ")))(setq ycnt 1))
   (cond 
      ((= kw "30")(setq xx 30.0 yy 30.0 w 6.0))
      ((= kw "40")(setq xx 40.0 yy 40.0 w 8.0))
   )
   (if (setq pos (getpoint "\n中心点を指示: "))
       (progn
          (setq x (car pos) y (cadr pos))
          (command "PLINE"
             (list (+ x (/ (* xx xcnt) 2.0) w)(+ y (/ (* yy ycnt) 2.0) w))
             (list (- x (/ (* xx xcnt) 2.0) w)(+ y (/ (* yy ycnt) 2.0) w))
             (list (- x (/ (* xx xcnt) 2.0) w)(- y (/ (* yy ycnt) 2.0) w))
             (list (+ x (/ (* xx xcnt) 2.0) w)(- y (/ (* yy ycnt) 2.0) w) ) "C")
          (command "PLINE"
             (list (+ x (/ (* xx xcnt) 2.0))(+ y (/ (* yy ycnt) 2.0)))
             (list (- x (/ (* xx xcnt) 2.0))(+ y (/ (* yy ycnt) 2.0)))
             (list (- x (/ (* xx xcnt) 2.0))(- y (/ (* yy ycnt) 2.0)))
             (list (+ x (/ (* xx xcnt) 2.0))(- y (/ (* yy ycnt) 2.0))) "C")
          (setq yyy 0.0)
          (repeat (- ycnt 1)
             (setq yyy (- yyy yy))
             (command "PLINE" 
                 (list (+ x (/ (* xx xcnt) 2.0))(+ y (/ (* yy ycnt) 2.0) yyy))
                 (list (- x (/ (* xx xcnt) 2.0))(+ y (/ (* yy ycnt) 2.0) yyy))"")
          ) 
          (setq xxx 0.0)
             (repeat (- xcnt 1)
                 (setq xxx (- xxx xx))
                 (command "PLINE" 
                    (list (+ x (/ (* xx xcnt) 2.0) xxx)(+ y (/ (* yy ycnt) 2.0)))
                    (list (+ x (/ (* xx xcnt) 2.0) xxx)(- y (/ (* yy ycnt) 2.0)))"")
             )
          (setq Xstr (itoa Xcnt))
          (if (< (strlen Xstr) 2)(setq Xstr (strcat "0" Xstr)))
          (setq Ystr (itoa Ycnt))
          (if (< (strlen Ystr) 2)(setq Ystr (strcat "0" Ystr)))
;文字記入
          (setq PX (list x  (- y (/ (* yy ycnt) 2.0))))
          (DCL_DSLC2)
          (setq T1 (strcat "AP" kw "F-" Ystr Xstr "-" VV "-S"))
          (setq T2 "色指定無" T4 "LED球")
          (if (= kw "30")(setq T7 "角30×30mm"))
          (if (= kw "40")(setq T7 "角40×40mm"))
          (setq T8 "R分圧式")
          (command "INSERT" "ZOKU_SLC" PX "1" "1" "0" "SLC" "" T1 T2 T3 T4 "" "" T7 T8 "" "4" "1" "" "")
          (setq Xstr (itoa (+ (* (atoi kw) Xcnt) 5)))
          (setq Ystr (itoa (+ (* (atoi kw) Ycnt) 5)))
          (command "color" "2")
          (command "TEXT" "J" "MC" (list x (- y (/ (* yy ycnt) 2.0) w txths)) (/ txths 2.0) 0.0
                   (strcat "(CutSize:" Xstr "x" Ystr ")")
          )
          (command "color" "4")
      )
   )
  (setvar "CMDECHO" 1)
  (redraw)
)
;---------------------------------------------------------------------------------------------------
  (defun DCL_DSLC2(/)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "DSLC2" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "E3" "(setq VV \"E3\")(setq T3  \"AC/DV24V\")     (setq T8 \"全電圧式\")")
          (action_tile "H3" "(setq VV \"H3\")(setq T3  \"AC/DC100-110V\")(setq T8 \"R分圧式\")")
          (action_tile "M3" "(setq VV \"M3\")(setq T3  \"AC200-220V\")   (setq T8 \"CR分圧式\")")
          (start_dialog) (unload_dialog DCL_ID) (princ)
  )
;;==================================================================================================
;; 41.コメント付き引出線を書く    引出線.LSP         2005/Feb  by T.Terada
;;==================================================================================================
(defun C:HIKI( / P1 P2 P3 P3U TX)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
      (progn
         (command "ORTHO" "OFF")(command "COLOR" 2)
          (setq P1 (getpoint "\n引出線の始点を指示:"))
          (if (/= P1 nil)(setq P2 (getpoint P1"\n引出線の終点を指示:")))
         (command "ORTHO" "ON")
          (if (/= P2 nil)(setq P3 (getpoint P2"\nコメントの末尾を指示:")))
      )
      (if (/= P3 nil)(command "PLINE" P1 P2 P3 ""))

       (setq TX  (getstring "\nコメント入力:"))
                             (setq P3U (list (car P3)(+ (cadr P3) 5)))
      (if (< (car P1) (car P2))
         (command "TEXT" "R" P3U "" "0" TX)
         (command "TEXT" P3U "" "0" TX)    ;else
      )
      (command "COLOR" 4)(setvar "CMDECHO" 1)
      (redraw)
   (princ "\n終了")
)
;;==================================================================================================
;; 42.部品属性,銘板用ﾌｧｲﾙ,ＣＨ用ﾌｧｲﾙを出力   ASAVE.LSP        1998/Dec/11変更            By T.Terada
;;==================================================================================================
;; DRV-ME  = C: <========== ICAD.LSPで定義されてます。
;; DRV-LAN = X: <========== ICAD.LSPで定義されてます。
;;

;;==================================================================================================
;; 43.図面保存･終了ﾌﾟﾛｸﾞﾗﾑ for Icad      END-I.LSP       2003/JUL/03   By T.Terada
;;==================================================================================================
;; 1) 定義された属性値から部品表をIPLﾌｧｲﾙに出力します。                                
;; 2) 現図面の回路図から外形図情報を読み出して外形図情報ﾌｧｲﾙに出力します。                               
;; 3) 現図面を保存して終了します。
;; 4) \\LSP-i\\ATT-OUT.TXTは属性書出しの定義ファイルです。
;; 5) DOSで使用するので \\ は / に置換えない事！ 
;;--------------------------------------------------------------------------------------------------------
(defun C:END-I(/ DAT AA BB CC DD n Z1D Z2D F NAME01 NAME02 NAME03 NAME04 LN NEN DWG_NAME FNAME NDWG)
   (setq *error* *err1*)
   (setvar "cmdecho" 0)
   
   ;;先に全図面の保存をしておく
   (command "SAVEALL")
   
   ;;部品表属性ﾌｧｲﾙ出力 ----保存するファイル名の取得と設定----------------------------------------------
   (setq NDWG (length (mdi_get_all)))
   
   (STARTAPP "X:/図面サーバー圧縮.exe" "");;開いている図面の製番を取得する為に、最初に起動する
   
   (while (>= NDWG 1)
      (setq NAME01 (getvar "dwgname"))
      (princ "\nDWGNAME =")(princ NAME01)
      (princ "\nDWGNo =")(princ NDWG)
            
      (if (/= "Drawing" (substr NAME01 1 7))
         (progn
            (setq NEN (substr NAME01 1 1))
            (setq LN (strlen NAME01))
            (setq NAME01 (substr NAME01 3 (- LN 6)))
            (setq NAME02 (strcat "Z:" "\\BUHIN-I\\" NEN "\\" NAME01 ".TXT"))
            (setq NAME03 (strcat "Z:" "\\BUHIN-I\\" NEN "\\" NAME01 ".IPL"));IPL=Icad Parts List  部品表ファイル
            (setq NAME04 (strcat "X:/DDAT/" NEN "/" NAME01 ".DAT"))         ;外形情報ファイル
            ;;(princ "\nNAME01 =")(princ NAME01)
            ;;(princ "\nNAME02 =")(princ NAME02)
            ;;(princ "\nNAME03 =")(princ NAME03)

            (setq AA (ssget "x" (list '(66 . 1))))      ;属性の付いた図形だけ。"INSERT"ではダメ。
            (if (/= AA nil)
               (progn
                  ;;部品表ファイルの保管                                    ********.IPL IPL=Icad Parts List
                  (command "ATTEXT" "c" "X:\\LSP-i\\ATT-OUT.TXT" NAME02)
                  (princ "\n  部品表  書出完了...")
               )
               (progn
                  (setq F (open NAME02 "w"))
                  (close F)
               )
            )
            (dos_copy NAME02 NAME03)                                   ; DOS Lib
            (dos_delete NAME02)

            ;;図面保管 -----------------------------------------------------------------------------------------
            (if (= (getvar "USERI5") 0)                  ;Access自動作図のフラグ（初期値はOFF）
               (progn (setvar "CMDECHO" 1)(command "ZOOM" "E")(command "QSAVE")(command "WCLOSE"))
            )
            (if (= (getvar "USERI5") 1)                  ;Access自動作図のフラグ（初期値はOFF）
               (progn (setvar "CMDECHO" 1)(setvar "USERI5" 0)(command "QSAVE")) ;;QSAVEは最後にすること
            )
         )
      )
      
      (mdi_next)
      (setq NDWG (1- NDWG))
   );End While
   
   (command "EXIT")
)
;;==================================================================================================
;;44-1.ﾘﾓｺﾝﾘﾚｰの回路入力。      RRY.DCL  RRY.LSP  2003/SEP  2005/3/3 C++からDCLに変更    By T.Terada
;;==================================================================================================
(defun C:RRY(/ P1 T1 T2 T3 Z1L Z1R SYML SYMR PNA PNoL PNoR DEF DEF_BAK RRY LR WHAT_NEXT BN KG LP BPT)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (DCL_RRY)
   ;;入力忘れの処理
      (if (= RRY nil)(progn (ALERT "リモコンが選択されていません")  (DCL_RRY)))
      (if (= LR  nil)(progn (ALERT "入力位置が選択されていません")  (DCL_RRY)))
      (cond ((= WHAT_NEXT 0)(setq DEF DEF_BAK)(princ))((= WHAT_NEXT 1) DEF))
      (cond ((= DEF "1") (setq T3 "AC24V" ))
            ((= DEF "2") (setq T3 "AC100V"))
            ((= DEF "3") (setq T3 "AC200V"))
      )
;;-------------------------------------------------------------------------------------------------
;;単数個の入力
   (if (= RRY "1E")
       (progn (setq T1 "WR6165")(setq T2 "2P1E")(setq PNA "RRY")
              (if (= LR "L") (progn (setq SYML "MRRY65L")(setq Z1L "DRRY1PTL015")))
              (if (= LR "R") (progn (setq SYMR "MRRY65R")(setq Z1R "DRRY1PTR015")))
       )
   )
   (if (= RRY "2E")
       (progn (setq T1 "WR6166") (setq T2 "2P2E") (setq PNA "RRY")
              (if (= LR "L") (progn (setq SYML "MRRY66L")(setq Z1L "DRRY1PTL015")))
              (if (= LR "R") (progn (setq SYMR "MRRY66R")(setq Z1R "DRRY1PTR015")))
       )
   )
   (if (= LR "L")
       (progn  (setq PNo (getint "\nRRY番号を入力:(ﾘﾀｰﾝで:101)"))
              (if (= PNo nil)(setq PNo 101))(setq PNo (strcat "RRY-" (itoa PNo)))
               (setq P1 (getpoint "  挿入点を指示... "))
              (command "INSERT" SYML P1 "1" "1" "0" PNA PNo T1 T2 T3 "" "" "" "" "" "" "15" "1" Z1L PNo)
       )
   )
   (if (= LR "R")
       (progn  (setq PNo (getint "\nRRY番号を入力:(ﾘﾀｰﾝで:102)"))
              (if (= PNo nil)(setq PNo 102))(setq PNo (strcat "RRY-" (itoa PNo)))
               (setq P1 (getpoint "  挿入点を指示... "))
              (command "INSERT" SYMR P1 "1" "1" "0" PNA PNo T1 T2 T3 "" "" "" "" "" "" "15" "1" Z1R PNo)
       )
   )
;;-------------------------------------------------------------------------------------------------
;;複数個の入力
   (if (= LR "M")
       (progn 
          (if (= RRY "1E")
              (progn (setq SYML "MRRY65L")(setq SYMR "MRRY65R")(setq Z1L "DRRY1PTL015")(setq Z1R "DRRY1PTR015"))
          )
          (if (= RRY "2E")
              (progn (setq SYML "MRRY66L")(setq SYMR "MRRY66R")(setq Z1L "DRRY1PTL015")(setq Z1R "DRRY1PTR015"))
          )
           (setq PNo (getint "\nRRY番号を入力:(ﾘﾀｰﾝで:101)"))
              (if (= PNo nil)(setq PNo 101))
               (setq BN (getint "個数を入力："))
               (setq P1 (getpoint "\n挿入点を指示... "))
              (setq KG (cl_evenp BN));   (princ "\nKG = ")(princ (cl_evenp BN))
              (setq PCP (cadr P1))
              (if (= KG nil)(setq LP (/ (- BN 1) 2))(setq LP (/ BN 2)))
          (while (>= LP 1)
             (setq BPT 100)
             (setq PNoL (strcat "RRY-" (itoa PNo)))(setq PNo (+ PNo 1))
             (setq PNoR (strcat "RRY-" (itoa PNo)))
             (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA PNoL T1 T2 T3 "" "" "" "" "" "" "15" "1" Z1L PNoL)
             (command "INSERT" SYMR (list (car P1) PCP) "1" "1" "0" PNA PNoR T1 T2 T3 "" "" "" "" "" "" "15" "1" Z1R PNoR)
             (setq LP (- LP 1))
             (setq PNo (+ PNo 1))
             (setq PCP (- PCP BPT))
          );while
       );progn
   );if
  ;左右の分岐数が等しくない時、最後に左１個追加する。
   (if (and (= LR "M")(= KG nil))
       (progn
          (setq PNoL (strcat "RRY-" (itoa PNo)))(setq PNo (+ PNo 1))
          (command "INSERT" SYML (list (car P1) PCP) "1" "1" "0" PNA PNoL T1 T2 T3 "" "" "" "" "" "" "15" "1" Z1L PNoL)
       )
   )
   (gc)
   (setvar "CMDECHO" 1)
)
;---------------------------------------------------------------------------------------------------------
(defun DCL_RRY (/)
   (setq DEF_BAK DEF) (if (not DEF)(setq DEF "1"))
   (setq DCL_ID (load_dialog "X:/LSP-i/2KD.DCL"))
   (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "RRY" DCL_ID) )(exit))
          
         (act_radioset '("1" "2" "3") DEF)
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "1E"  "(setq RRY \"1E\")")
         (action_tile "2E"  "(setq RRY \"2E\")")
         (action_tile "1" "(setq DEF $key)")
         (action_tile "2" "(setq DEF $key)")
         (action_tile "3" "(setq DEF $key)")
         (action_tile "M" "(setq LR \"M\")")
         (action_tile "L" "(setq LR \"L\")")
         (action_tile "R" "(setq LR \"R\")")
         (action_tile "cancel" "(done_dialog 0)(exit)")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;; 44-2.ﾘﾓｺﾝﾄﾗﾝｽの回路入力。             RTr.DCL  RTr.LSP  2003/SEP   By T.Terada
;;==================================================================================================
(defun C:RTr (/ PNA PNo T1 T2 T3 Z1 SYM LR DEF DEF_BAK WHAT_NEXT)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)
   (DCL_RTr)(princ "\nDEF=")(princ DEF)
      (if (= LR nil)(progn (ALERT "入力位置が選択されていません")(DCL_RTr)))
      (cond ((= WHAT_NEXT 0)(setq DEF DEF_BAK)(princ))((= WHAT_NEXT 1) DEF))
      (cond ((= DEF "100")(setq T1 "WR2301")(setq T2 "")(setq T3 "AC100/24V"))
            ((= DEF "200")(setq T1 "WR2311")(setq T2 "")(setq T3 "AC200/24V"))
      )
      (setq PNA "RTr")
      (if (= LR "L")(progn (setq SYM "MRTRL")(setq Z1 "DRTRTL015")))
      (if (= LR "R")(progn (setq SYM "MRTRR")(setq Z1 "DRTRTR015")))
       (setq PNo (getstring "\nRTr番号を指示:(例：1)"))
      (if (= PNo "")(setq PNo "1"))(setq PNo (strcat "RTr-" PNo))
       (princ "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" PNA PNO T1 T2 T3 "" "" "" "" "" "" "15" "1" Z1 "")
      (gc)(setvar "CMDECHO" 1)
)
;---------------------------------------------------------------------------------------------------------
(defun DCL_RTr (/)
   (setq DEF_BAK DEF) (if (not DEF)(setq DEF "100"))
   (setq DCL_ID (load_dialog "X:/LSP-i/2KD.DCL"))
   (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "RTr" DCL_ID) )(exit))
          
         (act_radioset '("100" "200") DEF)
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "100" "(setq DEF $key)")
         (action_tile "200" "(setq DEF $key)")
         (action_tile "L"   "(setq LR \"L\")")
         (action_tile "R"   "(setq LR \"R\")")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;; 45.縦型端子表作成             C:TBV      端子表.LSP    2004/3/19    By T.TERADA
;;==================================================================================================
;;縦型端子表作成プログラム  TB-V.LSP   
(defun C:TBV ( / TBW TBH PL PL1 PL2 PR1 PR2 PC1 PC2 IN IN2 TX TX2)
    (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
    (setvar "TEXTSIZE" 30)
     (princ "\n縦に端子表を描きます")

    (DCL_TBVH)


    (if (not (setq TBW (getint "\n端子表の横幅 <Enter=200>: ")))(setq TBW 200))
    (setq TBH 50)
   
    (if (= INP "UVW")
        (progn
            (if (not (setq IN (getint "\n何番から入力しますか？<Enter=1>: ")))(setq IN 1))
            (if (not (setq SUU (getint "\n入力する個数<Enter=10>: ")))(setq SUU 10))
            (setq SUU (+ SUU (- IN 1)))
        )
        (progn
            (setq IN 1)
            (setq SUU 1000)
        )
    )
     
     (setq PL  (getpoint "\n端子の左上を指示:"))
    (setq PL1 (list (car PL)(+ (cadr PL) TBH)))
    (setq PR1 (list (+ (car PL) TBW)(- (cadr PL1) TBH)))
    (setq PC1 (list (+ (car PL) 25)(+ (cadr PL) 10)))
    (setq IN2 1)

    (if (= INP "UVW")
        (progn
            (setq TX "")
            (setq TX2 "W")
        )
        (progn
             (setq TX (getstring "\n 端子番号を入力 (終了= X):"))
            (setq TX2 "")
        )
    )

    (while (and (/= TX "X") (/= TX "x"))
        (if (= INP "UVW")
            (progn
                (repeat 3
                    ;;枠を描く
                    (setq PIN (* TBH IN2))
                    (setq PL2 (list (car PL1)(- (cadr PL1) PIN)))
                    (setq PR2 (list (car PR1)(- (cadr PR1) PIN)))
                    (setq PC2 (list (car PC1)(- (cadr PC1) PIN)))
                    (command "COLOR" 4)(command "RECTANGLE" PL2 PR2)
                    (setq IN2 (+ IN2 1))

                    (cond
                        ((= TX2 "W")(setq TX2 "U"))
                        ((= TX2 "U")(setq TX2 "V"))
                        ((= TX2 "V")(setq TX2 "W"))
                        (t nil)
                    )
                    ;;(princ "\nTX2=")(princ TX2)(princ IN)

                    (setq TX (strcat TX2 (itoa IN)))
                    (command "COLOR" 2)(command "TEXT" PC2 "" "0" TX)
                )
            )
            (progn
                ;;枠を描く
                (setq PIN (* TBH IN2))
                (setq PL2 (list (car PL1)(- (cadr PL1) PIN)))
                (setq PR2 (list (car PR1)(- (cadr PR1) PIN)))
                (setq PC2 (list (car PC1)(- (cadr PC1) PIN)))
                (command "COLOR" 4)(command "RECTANGLE" PL2 PR2)
                (setq IN2 (+ IN2 1))

                (command "COLOR" 2)(command "TEXT" PC2 "" "0" TX)
            )
        )

        (setq IN (+ IN 1))

        (if (= INP "UVW")
            (progn
                (if (> IN SUU)(setq TX "X"))
            )
            (progn
                 (setq TX (getstring "\n 端子番号を入力 (終了= X):"))
            )
        )
    )
    (command "COLOR" 4)(setvar "TEXTSIZE" 20)
    (setvar "BLIPMODE" 1)(setvar "CMDECHO" 1)(command "REDRAW")
)
;---------------------------------------------------------------------------------------------------------
(defun DCL_TBVH (/)
    (setq INP "NOR")
    (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
    (princ "\nDCL_ID=")(princ DCL_ID)
    ;(new_dialog "TBVH" DCL_ID "")
    (if (not (new_dialog "X:/LSP-I/2KD.DCL/TBVH" DCL_ID))(exit))
    (action_tile "cancel" "(done_dialog)(exit)")
    (action_tile "NOR" "(setq INP $key)")
    (action_tile "UVW" "(setq INP $key)")
    (start_dialog) (unload_dialog DCL_ID) (princ)
)
;;==================================================================================================
;; 46.横型端子表作成             C:TBH
;;==================================================================================================
;;縦型端子表作成プログラム  TB-V.LSP   
(defun C:TBH ( / TBW TBH PL PL1 PL2 PR1 PR2 PC1 PC2 IN IN2 TX TX2)
    (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
    (setvar "TEXTSIZE" 30)
     (princ "\n横に端子表を描きます")

    (DCL_TBVH)
    ;;(princ "\nINP=")(princ INP)

    (if (not (setq TBW (getint "\n端子表の横幅 <Enter=200>: ")))(setq TBW 200))
    (setq TBH 50)
   
     (if (= INP "UVW")
        (progn
            (if (not (setq IN (getint "\n何番から入力しますか？<Enter=1>: ")))(setq IN 1))
            (if (not (setq SUU (getint "\n入力する個数<Enter=10>: ")))(setq SUU 10))
            (setq SUU (+ SUU (- IN 1)))
        )
        (progn
            (setq IN 1)
            (setq SUU 1000)
        )
    )
     
     (setq PL  (getpoint "\n端子の左上を指示:"))
    (setq PL1 (list (- (car PL) TBH)(cadr PL)))
    (setq PR1 (list (+ (car PL1) TBH)(- (cadr PL1) TBW)))
    (setq PC1 (list (+ (car PL1) 40)(- (cadr PL1) (- TBW 25))))
    (setq IN2 1)
    
    (if (= INP "UVW")
        (progn
            (setq TX "")
            (setq TX2 "W")
        )
        (progn
             (setq TX (getstring "\n 端子番号を入力 (終了= X):"))
            (setq TX2 "")
        )
    )

    (while (and (/= TX "X") (/= TX "x"))
        (if (= INP "UVW")
            (progn
                (repeat 3
                    ;;枠を描く
                    (setq PIN (* TBH IN2))
                    (setq PL2 (list (+ (car PL1) PIN)(cadr PL1)))
                    (setq PR2 (list (+ (car PR1) PIN)(cadr PR1)))
                    (setq PC2 (list (+ (car PC1) PIN)(cadr PC1)))
                   
                    (command "COLOR" 4)(command "RECTANGLE" PL2 PR2)
                    (setq IN2 (+ IN2 1))

                    (cond
                        ((= TX2 "W")(setq TX2 "U"))
                        ((= TX2 "U")(setq TX2 "V"))
                        ((= TX2 "V")(setq TX2 "W"))
                        (t nil)
                    )
                    ;;(princ "\nTX2=")(princ TX2)(princ IN)

                    (setq TX (strcat TX2 (itoa IN)))
                    (command "COLOR" 2)(command "TEXT" PC2 "" "90" TX)
                )
            )
            (progn
                ;;枠を描く
                (setq PIN (* TBH IN2))
                (setq PL2 (list (+ (car PL1) PIN)(cadr PL1)))
                (setq PR2 (list (+ (car PR1) PIN)(cadr PR1)))
                (setq PC2 (list (+ (car PC1) PIN)(cadr PC1)))
                (command "COLOR" 4)(command "RECTANGLE" PL2 PR2)
                (setq IN2 (+ IN2 1))

                (command "COLOR" 2)(command "TEXT" PC2 "" "90" TX)
            )
        )

        (setq IN (+ IN 1))

        (if (= INP "UVW")
            (progn
                (if (> IN SUU)(setq TX "X"))
            )
            (progn
                 (setq TX (getstring "\n 端子番号を入力 (終了= X):"))
            )
        )
    )
    (command "COLOR" 4)(setvar "TEXTSIZE" 20)
    (setvar "BLIPMODE" 1)(setvar "CMDECHO" 1)(command "REDRAW")
)
;;================================================================================================
;; 47.既存の端子表に、コメントと括弧線を描く       端子表.LSP       2020/7/17改
;;================================================================================================
(defun C:COMENT (/ C1 PX1 PX2 CM PCM)
;   (setq *error* *err1*)
   (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
   (princ "\n既存の端子表に、コメントと括弧線を描く")
   (setq VT (getstring "\n端子表の向きは？ 縦方向/Ｖ , 横方向 / Ｈ "))(princ "\n")
   (if (or (= VT "V")(= VT "v"))(progn (setq DST (getstring "\nコメントを描くのは、左/Ｌ or 右/Ｒ"))))
   (if (or (= VT "H")(= VT "h"))(progn (setq DST (getstring "\nコメントを描くのは、上/Ｕ or 下/Ｄ"))))

   (if (or (or (= DST "L")(= DST "l"))(or (= DST "R")(= DST "r")))
   	   (setq C1 "垂直方向の上")
   )
   (if (or (or (= DST "U")(= DST "u"))(or (= DST "D")(= DST "d")))
   	   (setq C1 "水平方向の左")
   )

   (setq CM (getstring "\n コメントを入力:"))
   (setq PX1 (getpoint "\n括弧の始点を指示:")) 
   (if (/= PX1 nil) (setq PX2 (getpoint PX1 "\n   終点を指示:")))
   (command "COLOR" 2)
   (if (or (= DST "R")(= DST "r"))(COMR))
   (if (or (= DST "L")(= DST "l"))(COML))
   (if (or (= DST "D")(= DST "d"))(COMD))
   (if (or (= DST "U")(= DST "u"))(COMU))
   (command "COLOR" 4)(setvar "BLIPMODE" 1)(setvar "CMDECHO" 1)(redraw)(gc)

;---------------------------------------------------------------------------------------------------
;縦方向－左
;PCMは文字の挿入位置を示す。
 (defun COML (/ PC1 PC2 PC3 PC4)
   (setq PC1 (list (- (car PX1) 10)(cadr PX1)))
   (setq PC2 (list (- (car PX1) 30)(- (cadr PX1) 20)))
   (setq PC3 (list (- (car PX2) 30)(+ (cadr PX2) 20)))
   (setq PC4 (list (- (car PX2) 10)(cadr PX2)))
   (setq PCM (list (- (car PX1) 80)(- (- (cadr PX1) (/ (- (cadr PX1)(cadr PX2)) 2)) 15)))
   (command "PLINE" PC1 PC2 PC3 PC4 "")
   (command "TEXT" "J" "R" PCM "30" "0" CM)
 )

;縦方向－右
 (defun COMR (/ PC1 PC2 PC3 PC4)
   (setq PC1 (list (+ (car PX1) 10)(cadr PX1)))
   (setq PC2 (list (+ (car PX1) 30)(- (cadr PX1) 20)))
   (setq PC3 (list (+ (car PX2) 30)(+ (cadr PX2) 20)))
   (setq PC4 (list (+ (car PX2) 10)(cadr PX2)))
   (setq PCM (list (+ (car PX1) 50)(- (- (cadr PX1) (/ (- (cadr PX1)(cadr PX2)) 2)) 15)))
   (command "PLINE" PC1 PC2 PC3 PC4 "")
   (command "TEXT" PCM "30" "0" CM)
 )
;横方向－下
 (defun COMD (/ PC1 PC2 PC3 PC4)
   (setq PC1 (list (car PX1)(- (cadr PX1) 10)))
   (setq PC2 (list (+ (car PX1) 20)(- (cadr PX1) 30)))
   (setq PC3 (list (- (car PX2) 20)(- (cadr PX2) 30)))
   (setq PC4 (list (car PX2)(- (cadr PX2) 10)))
   (setq PCM (list (- (car PX1) (/ (- (car PX1)(car PX2)) 2))(- (cadr PX1) 80)))
   (command "PLINE" PC1 PC2 PC3 PC4 "")
   (command "TEXT" "C" PCM "30" "0" CM)
 )
;横方向－上
 (defun COMU (/ PC1 PC2 PC3 PC4)
   (setq PC1 (list (car PX1)(+ (cadr PX1) 10)))
   (setq PC2 (list (+ (car PX1) 20)(+ (cadr PX1) 30)))
   (setq PC3 (list (- (car PX2) 20)(+ (cadr PX2) 30)))
   (setq PC4 (list (car PX2)(+ (cadr PX2) 10)))
   (setq PCM (list (- (car PX1) (/ (- (car PX1)(car PX2)) 2))(+ (cadr PX1) 50)))
   (command "PLINE" PC1 PC2 PC3 PC4 "")
   (command "TEXT" "C" PCM "30" "0" CM)
 )
);End DEF

;;==================================================================================================
;; 48.ツイスト線を書く    TW.LSP     2004/NOV  by T.Terada ;IZAWAさんのTW.LSPを改造
;;==================================================================================================
(defun c:tw( / P1 P2 P11 P12
	my_error loop linelist cnt osm old_error frad fradnew esel ename ent
	ename1 ename2 ent1 ent2 p1a p1b p2a p2b p1pa p1pb p2pa p2pb dist1 dist2 fdist
	twpa twpb twang ang ang1 ang1sa stang1 nextang1 stang2 nextang2
	tw1pa pw1pb tw2pa tw2pb ename_ dist elast1 elast2 pset ped pst1)
(setvar "CMDECHO" 1)
	(defun my_error(msg)
		(if linelist 
			(progn
				(redraw (car linelist) 4)
				(if (= (length linelist)2)(redraw (cadr linelist) 4))
			)
		)
		(setvar "FILLETRAD" frad)
		(setvar "OSMODE" osm)
	)
	(defun senjou_ten(p0 p1 p2)
		(inters p1 p2 p0 (polar p0 (+ (angle p1 p2) (/ pi 2.0)) 100.0) nil)
	)
	(defun tyu_ten(p1 p2)
		(list (/ (+ (car p1)(car p2)) 2.0)(/ (+ (cadr p1)(cadr p2)) 2.0)
			(/ (+ (caddr p1)(caddr p2)) 2.0))
	)

	(setq loop T linelist nil cnt 1)
	(setq osm (getvar "OSMODE"))
	(setvar "CMDECHO" 1)
	(setvar "OSMODE" 0)
	(setq old_error *error*)
	(setq *error* my_error)

;;TWの傾き
        (setq ang (/ pi 3.0));;60度

;;フィレット半径をセット
        (setq frad (getvar "FILLETRAD"))
;        (setq fradnew 0.04)
        (setvar "FILLETRAD" 4)
;=========================================================================================
   (if (= (getvar "USERI5") 0)
       (progn
           
          (setq P1 (getpoint "\n始点を指示:"))
           
          (if (/= P1 nil)(setq P2 (getpoint P1 "\n終点を指示")))
       )
   )
   (if (= (getvar "USERI5") 1) (progn (setq P1 ACP1)(setq P2 ACP2)))
   (if (/= P2 nil)
       (progn (setvar "CMDECHO" 1)(command "COLOR" 4)(command "LINE" P1 P2 ""))
   )
   (setq DW "50")
   (setq PX (- (car P1) (car P2)))
   (if (/= PX 0)
       (progn
          (setq P11 (list (car P1) (- (cadr P1) 50)));offset側　横線の場合
          (setq P12 (list (car P2) (- (cadr P1) 50)))
       )
       (progn
          (setq P11 (list (+ (car P1) 50) (cadr P1)));offset側　縦線の場合
          (setq P12 (list (+ (car P1) 50) (cadr P2)))
       )
   )
   (command "OFFSET" DW "L" P11 "")
;;=====================================================================================
;(exit)
  (setq n 0)
  (setq AA (ssget "CP" (list P1 P2 P11 P12) (list '(0 . "LINE"))))
  (while (setq ENAME (ssname AA n))
     (setq ENT (entget ENAME))
     (if (= (cdr (assoc 0 ent)) "LINE")
         (progn
             (setq linelist (append (list ename) linelist))
;             (redraw ename 3);;ハイライト表示
             (setq cnt (1+ cnt));;カウンタアップ
         )
         (princ "\nLINEではありません．")
     )
     (setq n (+ n 1))
  )
	(if (> cnt 2)
		;;１本目のハイライトを戻す
		(redraw (car linelist) 4)
	)
;;
	(if p2
		(progn
			(setq ename1 (car linelist) ename2 (cadr linelist))
			(setq ent1 (entget ename1) ent2 (entget ename2))
			(setq p1a (cdr (assoc 10 ent1)) p1b (cdr (assoc 11 ent1)))
			(setq p2a (cdr (assoc 10 ent2)) p2b (cdr (assoc 11 ent2)))
			
			;;始点、終点を線分１上に変換
			(setq p1pa (senjou_ten p1 p1a p1b) p1pb (senjou_ten p2 p1a p1b))
			;;始点、終点を線分２上に変換
			(setq p2pa (senjou_ten p1 p2a p2b) p2pb (senjou_ten p2 p2a p2b))
			
			;;丸め半径のチェック
			(setq dist1 (/ (distance p1pa p2pa) (/ 10.0 3.0)))
			(setq dist2 (/ (distance p1pb p2pb) (/ 10.0 3.0)))
			(if (< dist1 dist2)
				(setq fdist dist1)
				(setq fdist dist2)
			)
			(setq loop T)
			(while loop
				(if (>= fradnew fdist)
					(progn
						(princ "\n丸め半径が大きすぎます．")
						(if (setq fradnew (getreal (strcat "\nより小さい丸め半径(実寸)を入力 <Enter=" (rtos fdist) "> : ")))
							(if (< fradnew fdist)
								(setq loop nil)
							)
							(setq fradnew fdist loop nil)
						)
						(if (not loop)(setvar "FILLETRAD" fradnew))
						
					)
					(setq loop nil)
				)
			)
			;;TWを描く仮想中心線
			(setq twpa (tyu_ten p1pa p2pa) twpb (tyu_ten p1pb p2pb))
			;;仮想中心線の角度
			(setq twang (+ (angle twpa twpb)))
			;;開始点－終了点の距離
			(setq dist (distance twpa twpb))
			
			;;指定距離が短か過ぎる時
			(if (< (distance twpa twpb)(distance p1pa p2pa))
				(setq dist (distance p1pa p2pa)
					twpb (polar twpa twang dist)
					p1pb (polar p1pa (angle p1pa p1pb) (distance p1pa p1pb))
					p2pb (polar p2pa (angle p2pa p2pb) (distance p2pa p2pb)))
			)
			
			;;実際の傾き
			(setq ang1 (angle twpa p1pa))
			;;TWを描く方向（角度）を決める
			(setq ang1sa (- ang1 twang))
			(if (< ang1sa 0.0)(setq ang1sa (+ ang1sa (* pi 2.0))))
			(if (< ang1sa pi)
				(setq stang1 (+ twang ang) nextang1 (- twang ang)
					stang2 (- twang ang) nextang2 (+ twang ang))
				(setq stang1 (- twang ang) nextang1 (+ twang ang)
					stang2 (+ twang ang) nextang2 (- twang ang))
			)
			(setq cnt 0)
			(repeat 2
				(setq ename (nth cnt linelist))
				;;描く方向を変える
				(if (= cnt 0)
					(setq tw1pa p1pa tw1pb p1pb tw2pa p2pa tw2pb p2pb
						stang stang1 nextang nextang1) 
					(setq tw1pa p2pa tw1pb p2pb tw2pa p1pa tw2pb p1pb
						stang stang2 nextang nextang2) 
				)
				(command "break" ename tw1pa tw1pb);
				(setq ename_ (entlast))
				(setq ent (entget ename));;エンティティーを取り直す
				(if (equal ename ename_)
					(setq ename_ nil)
					(if (or (equal (cdr (assoc 10 ent)) tw1pb 0.0001)(equal (cdr (assoc 11 ent)) tw1pb 0.0001))
						(setq temp ename ename ename_ ename_ temp)
					)
				)
				;;ループのための開始点
				(setq ped tw1pa)
				(setq elast2 ename)
				;;-------------------------------------------
				(setq loop T)
				(while loop
					(setq pst ped pst1 pst)
					(setq ped (inters tw2pa tw2pb pst (polar pst nextang 100.0) nil))
					(command "LINE" pst ped "")
					(setq elast1 (entlast))
					(if elast2
						(command "FILLET" elast1 elast2)
					)
					(setq pst ped)
					(setq ped (inters tw1pa tw1pb pst (polar pst stang 100.0) nil))
					(command "LINE" pst ped "")
					(setq elast2 (entlast)) 
					(command "FILLET" elast1 elast2)
					(if (< (- dist (distance tw1pa ped))(/ (distance pst1 ped)2.0))
						(setq loop nil)
					)
				)
				;;------------------------------------
				(if ename_
					(command "FILLET" elast2 ename_)
				)
				(command)
				(setq cnt (1+ cnt))
			)
		)
	)
	(setq *error* old_error)
	(setvar "OSMODE" osm)
	(setvar "FILLETRAD" frad)
	(redraw)
	(princ "\n描画完了。")
	(princ)
)

;;==================================================================================================
;; 50.ブロック図形を分解して文字情報は元のままに残す。  C:B2T   BLtoTXT.LSP   hanafusa作を流用・改造
;;==================================================================================================
(defun C:B2T ( / subst2 attrib-get-data attdef->text err en ent data lay col lt lw ss i modify val)
  (setq *error* *err1*)
  ; (subst)の複数版
  (defun subst2 (nl ent / )
    (if nl
      (subst2 (cdr nl)
        (if (assoc (caar nl) ent)
          (subst (car nl) (assoc (caar nl) ent) ent)
          (append ent (list (car nl)))
        )
      )
      ent
    )
  )
  ;-----------------------------------------------------------------------
  ;ATTRIB図形の調査をサブルーチン化
  (defun attrib-get-data (en / ent ret)
    (setq ent (entget en))
    (if (= (cdr (assoc 0 ent)) "INSERT")
      (while (/= (cdr (assoc 0 ent)) "SEQEND") ; SEQENDが現れるまで繰り返し
        (setq en  (entnext en) ; 次の図形名
              ent (entget en)  ; 図形リスト
        )
        ; 図形がATTRIBなら名称と入力値を取得...
        ; (("名称1" . "値1")("名称2" . "値2")...("名称n" . "値n"))みたいなリストを作っとく
        (if (= (cdr (assoc 0 ent)) "ATTRIB")
          (setq ret (append ret (list (cons (cdr (assoc 2 ent)) (cdr (assoc 1 ent))))))
        )
      )
    )
    ret
  )
  ;-----------------------------------------------------------------------
  ;ATTDEF->TEXTをサブルーチン化
  (defun attdef->text (ent txt / en lt col lw)
;    (princ "\nENT=")(princ ent)
    (setq en (cdr (assoc -1 ent)))                                    ; ATTDEF図形の図形名
    (if (not (setq lt  (assoc 6 ent)))   (setq lt  '(6 . "ByLayer"))) ; 線種
    (if (not (setq col (assoc 62 ent)))  (setq col '(62 . 2)))      ; 色
    (if (not (setq lw  (assoc 370 ent))) (setq lw  '(370 . -1)))      ; 太さ
    (setq col '(62 . 2));;;;                                      <== ここで無理矢理　色を黄色に変更
    ; TEXT図形の作成に成功したらATTDEFを削除
    (if (entmakex  (list '(0 . "text") (assoc 8 ent) col lt lw (cons 1 txt)
                          (assoc 10 ent) (assoc 11 ent) (assoc 7 ent)
                          (assoc 40 ent) (assoc 41 ent) (assoc 50 ent) (assoc 51 ent)
                          (assoc 72 ent) (cons 73 (cdr (assoc 74 ent))) (assoc 210 ent)
                   )
        )
      (entdel en)
    )
  )
  ;-----------------------------------------------------------------------
  ; エラーコールバック
  (defun err ( / *err*)
     (defun *err* (s)
        (setvar "cmdecho" 1)(setq *error* nil)(princ s)
     )
     (setq *error* *err*)
  )
  ;-----------------------------------------------------------------------
  ; メイン
  (err)
  (setvar "cmdecho" 0)
  ; 属性付きのブロックを選択
  (if (and (setq en  (car (entsel "\nブロックを指示: "))) ; 図形選択
           (setq ent (entget en))                         ; 図形リスト
           (= (cdr (assoc 0 ent)) "INSERT")               ; INSERT図形かどうか
      )
    (if (= (cdr (assoc 66 ent)) 1)  ; ブロックが属性を持ってるかどうか
      (progn
        ;-----------------------------------------------------------------------
        ; 後で使いたいからINSERT図形の情報を控えとく
        (setq lay (assoc 8 ent))                                          ; 画層
        (if (not (setq lt  (assoc 6 ent)))   (setq lt  '(6 . "ByLayer"))) ; 線種
        (if (not (setq col (assoc 62 ent)))  (setq col '(62 . 2)))      ; 色
        (if (not (setq lw  (assoc 370 ent))) (setq lw  '(370 . -1)))      ; 太さ
        ;-----------------------------------------------------------------------
        ; 分解後は入力値はリセットされてまうから先に入力値を調べとく...
        (setq data (attrib-get-data en))
;        (princ "\nDATA=")(princ data)
        ;-----------------------------------------------------------------------
        ; 分解
        (command "_.explode" en)
        ;-----------------------------------------------------------------------
        ; 分解後にできた図形の編集
        (if (setq ss (ssget "p"))  ; 分解後にできた図形の選択セット
          (repeat (setq i (sslength ss))
            (setq en  (ssname ss (setq i (1- i))) ; 編集する図形の図形名
                  ent (entget en)                 ; 編集する図形の図形リスト
            )
            ;-----------------------------------------------------------
            ; 0画層やByBlockを処理しない場合は必要ない
            ; ↓↓↓ ここから ↓↓↓
            (setq modify nil)       ; 編集項目の初期化
            ; 分解後の図形の画層が「0」なら、挿入画層に
            (if (= (cdr (assoc 8 ent)) "0")
              (setq modify (append modify (list lay)))
            )
            ; 分解後の図形の線種が「ByBlock」なら挿入時の線種に
            (if (= (cdr (assoc 6 ent)) "ByBlock")
              (setq modify (append modify (list lt)))
            )
            ; 分解後の図形の色が0(ByBlock)なら挿入時の色に
;            (princ "\nCOL=")(princ col)
            (if (= (cdr (assoc 62 ent)) 0)
                (setq modify (append modify (list col)));;;;;;;;;;;;;;;
            )
            ; 分解後の図形の線の太さが-2(ByBlock)なら挿入時の太さに
            (if (= (cdr (assoc 370 ent)) -2)
              (setq modify (append modify (list lw)))
            )
            ; 変更したい情報があれば(変数modifyが空でなければ)図形を更新
            (if modify
              (setq ent (entmod (subst2 modify ent))) ; 変更して図形リストを更新
            )
            ; ↑↑↑ ここまで ↑↑↑
            ;-----------------------------------------------------------
            ; ATTDEF図形からTEXT図形に変換...
            (if (= (cdr (assoc 0 ent)) "ATTDEF")
              (progn
                ; 分解前に取得しておいたdataから名称が同じものの入力値を拾う
                (setq val (cdr (assoc (cdr (assoc 2 ent)) data)))
                ; 入力値が空の文字列""だったら、TEXT図形に変更した場合見えなくなってしまうので
                ; どうにかしといた方がよい...とりあえずATTDEFのまま
                (if (/= val "")
                  (attdef->text ent val)
                )
              )
            )
          )
        )
      )
      (print "\n属性をもってない")
    )
    (princ "\n選択ミス")
  )
  (setvar "cmdecho" 0)
  (setq *error* nil)
  (redraw)
  (princ "\nブロックを文字に分解、完了！")(princ)
)
;;==================================================================================================
;; 51.基本ブロックを挿入して任意のサイズの外形図を作成する。   GAIKEI.LSP   2004/Mar     By T.Terada
;;==================================================================================================
(defun C:GAIKEI( / D1 D2 D3 D4 D5 D6 W0 W1 H0 H1 SEN KITEN SYM PC n TXS TXW AA BB CC DD ENT BNAM)
;   (setq *error* *err1*)
   (setvar "CMDECHO" 1)(setvar "BLIPMODE" 0)
   (CGAIKEI)
    
   (setq W0 (atoi D3))(setq H0 (atoi D4))
   (setq W1 (/ W0 100.00))
   (setq H1 (/ H0 100.00))
   (if (and (= SEN "J")(= KITEN "US")) (setq SYM "ST-G1" ));上センター  実線
   (if (and (= SEN "T")(= KITEN "US")) (setq SYM "ST-G1T"));上センター  点線
   (if (and (= SEN "J")(= KITEN "LS")) (setq SYM "ST-G2" ));左センター  実線
   (if (and (= SEN "T")(= KITEN "LS")) (setq SYM "ST-G2T"));左センター  点線
   (if (and (= SEN "J")(= KITEN "RUS"))(setq SYM "ST-G3" ));右上　　　  実線
   (if (and (= SEN "T")(= KITEN "RUS"))(setq SYM "ST-G3T"));右上　　　  点線
   (if (and (= SEN "J")(= KITEN "RS")) (setq SYM "ST-G4" ));右センター  実線
   (if (and (= SEN "T")(= KITEN "RS")) (setq SYM "ST-G4T"));右センター  点線
   
   (setq PC (getpoint "\n挿入点を指示... "))
   (if (= MUKI "SYO") (command "INSERT" SYM PC W1 H1 "0" D1 "" D3 D4 "" ""));向きが正面
   (if (= MUKI "SOKU")(command "INSERT" SYM PC W1 H1 "0" "" "" D3 D4 "" ""));向きが側面
   (setq AA (ssget "L"))
   
;;文字サイズ変更
  (setq n 0)
  (setq TXS 20.0000)
  (setq TXW 1.0000)
  (while (setq BB (ssname AA n))
     (setq ENT (entget BB))
     (setq BNAM (cdr (assoc 2 ENT)));シンボル名取得
     (while BB
        (setq CC (entnext BB))
        (setq DD (entget CC))
        ;(princ "\nDD=")(princ DD)
           (if (= (cdr (assoc 0 DD)) "ATTRIB")
               (progn
                  (entmod (subst (cons 40 TXS)(assoc 40 DD) DD));属性変更
                  (entmod (subst (cons 41 TXW)(assoc 41 DD) DD));属性変更
               )
           );if
;           (princ "\nDD2=")(princ DD)
           (if (= (cdr (assoc 0 DD)) "SEQEND")(setq BB nil)(setq BB CC))
     );while
     (setq n (1+ n))
  );while
  (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
)

;;==================================================================================================
;; 52.部品表用ｼﾝﾎﾞﾙの属性値（PNO:機器番号）をまとめて変更する。 CH-PNO.LSP  2003/SEP   By T.Terada
;;==================================================================================================
(defun C:CH-PNO(/ n NoNew AA BB CC DD PC1 PC2 ATT LN LN1 TL NEWATT)
   (setq *error* *err1*)
   (setq n 0)
   (setvar "CMDECHO" 0)
    
   (if (= (getvar "USERI5") 0)
      (progn
         (setq NoNew (getstring "\n回路番号を入力(例: 12)："))
         ;;(princ "\n変更するオブジェクトを指示:<ﾘﾀｰﾝ=全て>")
         ;;(if (null (setq AA (ssget (list '(66 . 1)))))
         ;;          (setq AA (ssget "x" (list '(66 . 1))))
         ;;)
         (princ "\n変更するオブジェクトを指示:")
         (if (null (setq AA (ssget (list '(66 . 1)))))
                   (exit)
         )
      )
   )
   (if (= (getvar "USERI5") 1)
      (progn
         (setq NoNew zoku_cnt)         ;回路番号入力（文字で設定 例："12"）
         (setq PC1(list PCX PCY))      ;選択範囲始点
         (setq PC2(list PCX2 PCY2))    ;   〃   終点
         ;(princ "\nPC1=")(princ PC1);(princ "\nPC2=")(princ PC2);(princ "\n")
         (setq AA (ssget "W" PC1 PC2))
      )
   )
   (while (setq BB (ssname AA n))
       (setq CC (entnext BB) DD (entget CC))
          (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
             (if (= (cdr (assoc 0 DD)) "ATTRIB")
                 (progn
                    (if (= (cdr (assoc 2 DD)) "PNO")
                        (progn
                           (setq ATT (cdr (assoc 1 DD)))
                           (if (= ATT "") (setq ATT "AAA")) ;;機器番号未入力の場合を追加09/12/04
                           ;;(princ "\nATT=")(princ ATT)
                           (setq LN (strlen ATT) LN1 (strlen ATT))
                           ;;(princ "\nLN=") (princ LN)

                           ;;末尾が0の場合におかしくなるので修正09/01/29
                           (setq TL (substr ATT LN 1))
                           ;;(princ "\nTL=") (princ TL)

                           (setq NEWATT "")
                                                                                                                                     
                           (if (or (/= (atoi TL) 0)(= TL "0")) ;"-"があって、末尾が数字の場合
                               (progn
                                  (if (= (substr ATT 2 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 2) NoNew))
                                  )
                                  (if (= (substr ATT 3 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 3) NoNew))
                                  )
                                  (if (= (substr ATT 4 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 4) NoNew))
                                  )
                                  (if (= (substr ATT 5 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 5) NoNew))
                                  )
                                  (if (= (substr ATT 2 1) "E")
                                      (setq NEWATT (strcat NoNew (substr ATT 2 2)))
                                  )
                                  ;(princ "\nNEWATT1=")(princ NEWATT)
                               )
                               (progn ;末尾が英文字の場合

                                  ;"-"が無い場合
                                  ;(if (or (/= (substr ATT 2 1) "-")(/= (substr ATT 3 1) "-")
                                  ;        (/= (substr ATT 4 1) "-")(/= (substr ATT 5 1) "-"))
                                  ;    (setq NEWATT (strcat NoNew TL))
                                  ;)

                                  ;"-"が有る場合
                                  (if (= (substr ATT 2 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 2) NoNew TL))
                                  )
                                  (if (= (substr ATT 3 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 3) NoNew TL))
                                  )
                                  (if (= (substr ATT 4 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 4) NoNew TL))
                                  )
                                  (if (= (substr ATT 5 1) "-")
                                      (setq NEWATT (strcat (substr ATT 1 5) NoNew TL))
                                  )
                                  ;(princ "\nNEWATT2=")(princ NEWATT)
                               )
                           )
                           (if (/= NEWATT "")
                               (progn
                                  (entmod (subst (cons 1 NEWATT)(assoc 1 DD) DD))
                               )
                               (progn
                                  ;;(princ "\n変更できませんでした")
                               )
                           )
                        )
                    )
                 )
             )
;            (princ "\nDD=")(princ DD)
             (setq CC (entnext CC) DD (entget CC))
          );while
       (setq n (1+ n))
   );while
   (command "REGEN")
   (setvar "CMDECHO" 1)(setvar "USERI5" 0)
)

;;==================================================================================================
;; 53.線結合 LINE・PLINEを結合する。  C:JL C:JPL  JPL.LSP
;; Try Block Trim ブロックトリム by f.izawa Ver.2001.10.13をLINE・PLINE結合のみに改悪
;;==================================================================================================
;LINEを結合
(defun c:JL( / ss)
	(setq *error* *err1*)
	 
	(princ "\n結合する線分を選択 : " )
	(if (setq ss (ssget (list '(0 . "LINE"))))
		(line_joint ss)
	)
	(princ)
)
;PLINEを結合
(defun c:JPL( / ss i j entname1 entname2)
	(setq *error* *err1*)
	 
	(princ "\n結合するPLINE線分を選択 : " )
	(if (setq ss (ssget (list '(0 . "LWPOLYLINE"))))
		(progn
			(setq i 0)
			(repeat (1- (sslength ss))
				(setq entname1 (ssname ss i))
				(setq j (1+ i))
				(repeat (- (sslength ss) i 1)
					(setq entname2 (ssname ss j))
					(if (and (entget entname1)(entget entname2))
						(setq entname2 (pline_joint entname2 entname1))
					)
					(setq j (1+ j))
				)
				(setq i (1+ i))
			)
		)
		(line_joint ss)
	)
	(princ)
)

;LWPOLYLINEの直線部分を結合
(defun pline_joint(entname1 entname2 / ent1 ent2 loop i cnt)
	(defun pline_code1042(ent / item res10 res42)
		(foreach item ent
			(if (or (= (car item) 10))
				(setq res10 (append res10 (list (cdr item)))))
			(if (= (car item) 40)(setq code40 (cdr item)))
			(if (= (car item) 41)(setq code41 (cdr item)))
			(if (= (car item) 42)
				(setq res42 (append res42 (list (list code40 code41 (cdr item))))))
		)
		(list res10 res42);
	)
	
	(defun pline_start(ent cnt / i loop item temp)
		(setq i 0 loop T)
		(while loop 
			(setq item (nth i ent))
			(if (= (car item) 10)(setq loop nil))
			(if loop 
				(if (= (car item) 90)
					(setq temp (append temp (list (cons 90 cnt))))
					(setq temp (append temp (list item)))
				)
			)
			(setq i(1+ i))
		)
		temp;
	)
	(defun pline_end(ent / i loop item temp)
		(setq ent (reverse ent))
		(setq i 0 loop T)
		(while loop 
			(setq item (nth i ent))
			(if (= (car item) 42)(setq loop nil))
			(if loop 
				(setq temp (append temp (list item)))
			)
			(setq i(1+ i))
		)
		(reverse temp)
	)
	;;code42を反転
	(defun reverse_code42(code42 / i temp)
		(setq code42 (reverse code42))
		(setq code42 (append (cdr code42)(list (car code42))))
		(setq i 0)
		(foreach item code42
			(setq temp (append temp (list (list (car item)(cadr item)(* (caddr item) -1.0)))))
		)
		temp;
	) 
	(defun append_1042(plst code10 code42 st / i item)
		(setq i st)
		(repeat (1- (length code10))
			(setq item (nth i code42))
			(setq plst (append plst (list (cons 10 (nth i code10)))))
			(setq plst (append plst (list (cons 40 (car item)))))
			(setq plst (append plst (list (cons 41 (cadr item)))))
			(setq plst (append plst (list (cons 42 (caddr item)))))
			(setq i (1+ i))
		)
		plst;
	)
	
	(setq flag T ans nil ret nil)
	;(setq entname1 (car (entsel "\n１本目のPLINEを選択 : ")))
	(setq ent1 (entget entname1))
	;(setq entname2 (car (entsel "\n２本目のPLINEを選択 : ")))
	(setq ent2 (entget entname2))
	(if (equal entname1 entname2)
		(progn
			(setq flag nil)
			(princ "\n同じ図形です")
		)
	)
	(if (or (/= (cdr (assoc 0 ent1)) "LWPOLYLINE")(/= (cdr (assoc 0 ent2)) "LWPOLYLINE"))
		(progn
			(setq flag nil)
			(princ "\nLWPOLYLINEではありません")
		)
	
	) 
	(if flag
		(progn
			(setq code101 (car	(pline_code1042 ent1)) code102 (car	 (pline_code1042 ent2)))
			(setq code421 (cadr (pline_code1042 ent1)) code422 (cadr (pline_code1042 ent2)))
			;;最初と最後を得る
			(setq st1 (list (car code101)(cadr code101)))
			(setq ed1 (list (car (reverse code101)) (cadr (reverse code101))))
			(setq st2 (list (car code102)(cadr code102)))
			(setq ed2 (list (car (reverse code102))(cadr (reverse code102))))
			(cond
				((and (is_douitu (car st1)(cadr st1)(car st2)(cadr st2))
					(< (distance (car st1)(car st2))(distance (cadr st1)(cadr st2))))
						(setq ans (list 0 0)))
				((and (is_douitu (car ed1)(cadr ed1)(car st2)(cadr st2))
					(< (distance (car ed1)(car st2))(distance (cadr ed1)(cadr st2))))
						(setq ans (list 1 0)))
				((and (is_douitu (car st1)(cadr st1)(car ed2)(cadr ed2))
					(< (distance (car st1)(car ed2))(distance (cadr st1)(cadr ed2))))
						(setq ans (list 0 1)))
				((and (is_douitu (car ed1)(cadr ed1)(car ed2)(cadr ed2))
					(< (distance (car ed1)(car ed2))(distance (cadr ed1)(cadr ed2))))
						(setq ans (list 1 1)))
			)
		)
	)
	;;同一線分上で要素が１本ずつの時
	(if (and ans (= (length code101) 2)(= (length code102) 2))
		(progn
			(if (> (distance (car st1)(car st2))(distance (car st1)(cadr st2)))
				(setq st (car st2))
				(setq st (cadr st2))
			)
			(if (> (distance st (car st1))(distance st (cadr st1)))
				(setq ed (car st1))
				(setq ed (cadr st1))
			)
			(setq temp (pline_start ent1 (+ (length code101)(length code102) -2)))
			(setq temp (append temp (list (cons 10 st))))
			(setq temp (append temp (list (cons 40 0.0)(cons 41 0.0)(cons 42 0.0))))
			(setq temp (append temp (list (cons 10 ed))))
			(setq temp (append temp (list (cons 40 0.0)(cons 41 0.0)(cons 42 0.0))))
			(setq temp (append temp (pline_end ent1)))
			(entmod temp)
			(entdel entname2)
			(redraw entname1)
			;(princ "\n結合しました")
			(setq ret T)
		)
	)
	(if (and (not ret) ans)
		(progn
			(setq temp (pline_start ent1 (+ (length code101)(length code102) -2)))
			(if (= (car ans) 0)
				(setq code101 (reverse code101) code421 (reverse_code42 code421))
			)
			(setq temp (append_1042 temp code101 code421 0))
			(if (= (cadr ans) 1)
				(setq code102 (reverse code102) code422 (reverse_code42 code422))
			)
			(setq temp (append_1042 temp code102 code422 1))
			(setq temp (append temp (pline_end ent1)))
			(entmod temp)
			(entdel entname2)
			(redraw entname1)
			;(princ "\n結合しました")
			(setq ret T)
		)
	)
	
	;(if (not ret)(princ "\n結合できません"))
	;(princ)
	entname1;
) 

;*************************************************
;線分結合
;*************************************************
(defun line_joint0 (selobj / selobj i j ename1 ename2 edata1 edata2 )
  (if (/= selobj nil)
	(progn
	  ;(princ (sslength selobj))
	  (setq i 0)
	  (while (< i (1- (sslength selobj))) ;while1
		(setq j (1+ i))
		(while (< j (sslength selobj)) ;while2
		  ;(princ (ssname selobj i)) ;disp Entity name:
		  (setq ename1 (ssname selobj i))
		  (setq ename2 (ssname selobj j))
			
		  (setq edata1 (entget ename1))
		  (setq edata2 (entget ename2))
		  (if (= (cdr(assoc 0 edata1)) "LINE") ;if1
			(if (= (cdr(assoc 0 edata2)) "LINE") ;if2
			  (progn
				;結合処理
				(ketugou ename1 ename2)
			  );progn end	
			);if2 end
		  );if1 end
		  (setq j (1+ j))
		);while2 end
		(setq i (1+ i))
	  );while1 end
	);progn end
  );if end
)
;*************************************************
;同一直線上の判断
;*************************************************
(defun is_douitu( p1 p2 p3 p4 / res kaku p1 p2 p3 p4 lg1 lg2)
  (setq kaku (angle p1 p2))
  (setq lg1 (* (distance p1 p3) (sin (- kaku (angle p1 p3)))))
  (if (equal 0 lg1 0.001)
	(progn
	  (setq lg2 (* (distance p1 p4) (sin (- kaku (angle p1 p4)))))
	  (setq res (equal 0 lg2 0.001))
	)
  )
  res;
)
;*************************************************
;同一直線上の判断、結合処理
;*************************************************
(defun ketugou ( entdt1 entnm2 
  / entnm1 entnm2 entdt1 entdt2 ang1 ang2 pos1a pos1b pos2a pos2b max_pos)
  (setq entdt2 (entget entnm2))
  (setq pos1a (cdr(assoc 10 entdt1)))
  (setq pos1b (cdr(assoc 11 entdt1)))
  (setq pos2a (cdr(assoc 10 entdt2)))
  (setq pos2b (cdr(assoc 11 entdt2)))
  (if (is_douitu pos1a pos1b pos2a pos2b)  ;同一直線上であれば
	  (progn
		(setq max_pos (get_maxpos pos1a pos1b pos2a pos2b))	 ;４点から端点を取得
		(entdel entnm2)
		(setq entdt1 (subst (cons 10 (car max_pos)) (assoc 10 entdt1) entdt1))
		(setq entdt1 (subst (cons 11 (cadr max_pos)) (assoc 11 entdt1) entdt1))
		(entmod entdt1)
	 )
  )
  entdt1;
)
;*************************************************
;２直線の両端同士の距離からその最大値を求める
;*************************************************
(defun get_maxpos( posa posb posc posd 
  / posa posb posc posd distab distac distad distbc distbd distcd pos1 pos2 )
  ;初期値設定
  (setq pos1 posa)
  (setq pos2 posc)
  ;各距離を求める
  (setq distac (distance posa posc))
  (setq distad (distance posa posd))
  (setq distbc (distance posb posc))
  (setq distbd (distance posb posd))
  (setq distab (distance posa posb))
  (setq distcd (distance posc posd))
  ;最大値を求める
  (if (and (< distac distcd )(< distad distcd )(< distbc distbd )
		   (< distbd distcd )(< distab distcd )) 
	(progn
	  (setq pos1 posc)
	  (setq pos2 posd)
	)
	(progn
	  (if (and (< distcd distac )(< distad distac )(< distbc distac )
			   (< distbd distac )(< distab distac )) 
		(progn
		  (setq pos1 posa)
		  (setq pos2 posc)
		)
		(if (and (< distac distad )(< distcd distad )(< distbc distad )
				 (< distbd distad )(< distab distad )) 
		  (progn
			(setq pos1 posa)
			(setq pos2 posd)
		  )
		  (if (and (< distac distbc )(< distcd distbc )(< distad distbc )
				   (< distbd distbc )(< distab distbc )) 
			(progn
			  (setq pos1 posb)
			  (setq pos2 posc)
			)
			(if (and (< distac distbd )(< distcd distbd )(< distad distbd )
					 (< distbc distbd )(< distab distbd )) 
			  (progn
				(setq pos1 posb)
				(setq pos2 posd)
			  )
			  (if (and (< distac distab )(< distcd distab )(< distad distab )
					   (< distbc distab )(< distbd distab )) 
				(progn
				  (setq pos1 posa)
				  (setq pos2 posb)
				)
			  )
			)
		  )
		)
	  )
	)
  );if end
  (list pos1 pos2)
) 

;*************************************************
;選択セットの線分同士を結合
;*************************************************
(defun line_joint (selobj / selobj i j ename1 ename2 edata1 edata2 )
  (if (/= selobj nil)
	(progn
	  (setq i 0)
	  (while (< i (1- (sslength selobj))) ;while1
		(setq ename1 (ssname selobj i))
		(setq edata1 (entget ename1))
		(setq j (1+ i))
		(while (< j (sslength selobj)) ;while2
		  (setq ename2 (ssname selobj j))
		  (setq edata2 (entget ename2))
		  (if (= (cdr(assoc 0 edata1)) "LINE") ;if1
			(if (= (cdr(assoc 0 edata2)) "LINE") ;if2
			  (progn
				;結合処理
				(setq edata1 (ketugou edata1 ename2))
			  );progn end	
			);if2 end
		  );if1 end
		  (setq j (1+ j))
		);while2 end
		(setq i (1+ i))
	  );while1 end
	);progn end
  );if end
)
;*****************************************************
;線分と円の交点を得る関数
;;要 is_tyokusen is_ang_ang
;*****************************************************
(defun get_lc_inters(
	posa	;線分端点１
	posb	;線分端点２
	pos0	;円、円弧中心
	rad		;円、円弧半径
	stang	;円弧開始角度(nil=円)
	edang	;円弧終了角度(nil=円)
	/ res temp ang ang0 ang1 posx dist len posx1 posx2 )
  ;線分の角度を取得
  (setq ang0 (angle posa posb))
  ;線分の角度を180度以内に補正
  (while (< pi ang0)
	(setq temp posa posa posb posb temp)
	(setq ang0 (- ang0 pi))
  )
  (setq ang1 (+ ang0 pi)) 
  ;円の中心から線分と90度で交わる点を得る
  (setq ang (+ ang0 (/ pi 2)))
  (setq pos1 (polar pos0 ang rad))
  (setq posx (inters posa posb pos0 pos1 nil ))
  (if (/= posx nil)
	(progn
	  ;その交点と中心との距離を求める
	  (setq dist (distance pos0 posx))
	  ;その距離が半径より小さいければ、交点が存在する可能性あり
	  (if (>= rad dist )
		(progn
		  ;残りの１辺の長さを求める
		  (setq len (sqrt (- (expt rad 2.0) (expt dist 2.0))))
		  ;;(setq len (- len 0.001)) ;誤差を含む
		  (if (>= (distance posx posb) len )
			(progn
			  ;その距離にある点を求める
			  (setq posx1 (polar posx ang0 len ))
			  (setq flag (is_tyokusen posa posx1 posb 0.001))
			  (if (and (/= stang nil)(/= edang nil)) ;ARC
				(progn
				  (setq flag nil)
				  (if (/= (is_ang_ang (angle pos0 posx1) stang edang) nil)(setq flag T))
				)
			  ) 
			  (if (/= flag nil)(setq res (cons posx1 res)))
			)
		  )
			
		  (if (>= (distance posx posa) len )
			(progn
			  ;反対側の点を求める
			  (setq posx2 (polar posx ang1 len ))
			  ;線分の中に点が存在するかを長さで判断
			  (setq flag (is_tyokusen posa posx2 posb 0.001))
			  ;円弧であれば
			  (if (and (/= stang nil)(/= edang nil)) ;ARC
				(progn
				  (setq flag nil)
				  ;角度範囲に入っているかを判断
				  (if (/= (is_ang_ang (angle pos0 posx2) stang edang) nil )(setq flag T))
				)
			  ) 
			  ;リストに追加
			  (if (/= flag nil)(setq res (cons posx2 res)))
			)
		  )
		)
	  )
	)
  )
  res  ;戻り値
)
;***************************************
;角度範囲内かの判断
;***************************************
(defun is_ang_ang(ang st ed / st1 ed1 st2 ed2 res )
  (if (> st ed)
      (progn (setq st1 st ed1 (* pi 2) st2 0.0 ed2 ed)
           (if (or (and (>= ang st1 )(<= ang ed1))(and (>= ang st2 )(<= ang ed2)))(setq res T))
      )
      (if (and (>= ang st )(<= ang ed))(setq res T))
  )
  res
) 
;;**********************************************************
;;直線上の点かどうかを長さで判断
;;**********************************************************
(defun is_tyokusen(p1 p0 p2 gosa)
    (equal (distance p1 p2)(+ (distance p1 p0)(distance p0 p2)) gosa)
)
;(princ)

;;==================================================================================================
;;54.簡易銘板表を作成     NP-TXT.LSP     2020/OCT IJCAD対応       T.TERADA
;;==================================================================================================
(defun C:NP-TXT(/ PC P1 P2 P3 P4 P11 P12 PW DN IN1 OUT NO TX PX PO1 PO2 n PLs PLe)
	(setq P1 nil P2 nil PLs nil PSe nil n nil H nil P11 nil P12 nil)
    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
    (setvar "OSMODE" 16384)
    (princ "簡易銘板表を作成します")
    (setq DN (getint "\n何段にしますか？(3段以上)："))
    (setq PC (getpoint "\n左上端を指示してください．:"))
    (setq DNT DN)
    (setq WH (* DN 50))
    (setq DN (+ DN 1))
    (setq PW 1000)
        (setq P1 (list (car PC)(- (cadr PC) 10)))
        (setq P2 (list (+ (car P1) PW) (- (cadr P1) WH)));外枠を描く
        (setq P3 (list (+ (car P1) 200)(cadr P1)))
        (setq P4 (list (+ (car P1) 200)(- (cadr P1) WH)))
        (command "COLOR" 4)
        (command "rectang" "F" 0 P1 P2)(princ "\nEndRect")
        (command "PLINE" P3 P4 "")(princ "\nEnd P3 to P4")
		(setq n 0)
		(setq H 50)
	(while (<= n DN)
		   (setq DN (- DN 1))
           (setq PLs (list (car P1)(- (cadr P1) H)))
           (setq PLe (list (car P2)(- (cadr P1) H)))
           (command "PLINE" PLs PLe "")(princ "\nPline")
			  (setq n (+ n 1))
			  (setq H (+ H 50))
	);end while

   (setq IN 1)
   (setq OUT DNT)
   (command "COLOR" 2)
   (while (<= IN OUT)         
         (setq NO1 (getstring "\n銘板の番号を入力して下さい。:"))
         (setq NO (strcat "NP-" NO1))
         (setq TX (getstring "\n名称を入力して下さい。:"))
         (setq PX (* 50 IN))(princ "\nPX=")(princ PX)
         (setq P11 (list (+ (car PC) 50) (- (cadr PC) PX)))
         (setq P12 (list (+ (car PC) 250) (- (cadr PC) PX)))
         (command "TEXT" P11 "25" "0" NO)(princ "\nP11=")(princ P11)
         (command "TEXT" P12 "25" "0" TX)(princ "\nP12=")(princ P12)
         (setq IN (1+ IN))
   )
   (command "COLOR" 2)
   (setvar "CMDECHO" 1)
   (setvar "BLIPMODE" 1)
   (setvar "OSMODE" 33)
)
;;=========================================================================================
;; 55.電線サイズ記入プログラム          By T.Terada   C++ CSEN_SIZE
;;=========================================================================================
  (defun C:IV_SEN ( / SSZ P1)
     (C:ST1)
     (setq dcl_id (load_dialog "X:/LSP-i/2KD.DCL"))
     (if (not (new_dialog "IV_SEN" dcl_id) )(exit))
     (D_IV)
     (if (= SSZ nil)
         (progn
            (alert "  容量が、選択されていません！")
            (setq dcl_id (load_dialog "X:/LSP-i/2KD.DCL"))
            (if (not (new_dialog "D_IV" dcl_id) )(exit))
            (D_IV)
         )
     )
     (setq P1 (getpoint "\n挿入点を指示（線の上）："))
     (setq PT (list (+ (car P1) 125) (cadr P1)))
     (command "INSERT" "YA1" P1 "1" "1" "0" SSZ)
     (action_tile "cancel" "(done_dialog 0)(exit)")
     (C:END1)
  )
;;----------------------------------------------------------
  (defun D_IV (/ LB FSU) ;電線サイズ
        (action_tile "cancel" "(done_dialog)(exit)")
	(action_tile "1"  "(setq SSZ \"  2 Sqe\")")
	(action_tile "2"  "(setq SSZ \"3.5 Sqe\")")
	(action_tile "3"  "(setq SSZ \"5.5 Sqe\")")
	(action_tile "4"  "(setq SSZ \"  8 Sqe\")")
	(action_tile "5"  "(setq SSZ \" 14 Sqe\")")
	(action_tile "6"  "(setq SSZ \" 22 Sqe\")")
	(action_tile "7"  "(setq SSZ \" 38 Sqe\")")
	(action_tile "8"  "(setq SSZ \" 60 Sqe\")")
	(action_tile "9"  "(setq SSZ \"100 Sqe\")")
	(action_tile "10" "(setq SSZ \"150 Sqe\")")
	(action_tile "11" "(setq SSZ \"200 Sqe\")")
	(action_tile "12" "(setq SSZ \"250 Sqe\")")
	(action_tile "13" "(setq SSZ \"150Sqe X2\")")
	(action_tile "14" "(setq SSZ \"200Sqe X2\")")
    (action_tile "15" "(setq SSZ \"325 Sqe\")")
	(action_tile "16" "(setq SSZ \"250Sqe X2\")")
	(action_tile "17" "(setq SSZ \"325Sqe X2\")")
        (start_dialog)(unload_dialog dcl_id)(princ)
  )
;;=========================================================================================
;; 56.ＮＴ－ＳＷ  外形入力プログラム      不要
;;=========================================================================================

;;=========================================================================================
;; 57.現在層と表示画層をＫＤ標準画層に戻します。               2004/Feb/25    By T.Terada
;;=========================================================================================
(defun C:STD-LA (/)
  (setq *error* *err1*)
  (setvar "CMDECHO" 0)
   
  (command "LAYER" "S" "S1" "")
  (command "LAYER" "ON" "BOX" "")
  (command "LAYER" "ON" "SHEET" "")
  (command "LAYER" "ON" "ZOKU1" "")
  (command "LAYER" "OFF" "BANKIN" "")
  (command "LAYER" "OFF" "ZOKU2" "")
  (setvar "CMDECHO" 1)
  (princ "\n現在層と表示画層をＫＤ標準画層に戻しました。")
)
;;=========================================================================================
;; 58.フリ－アングル描画プログラム
;;=========================================================================================
(defun C:FREEANG ( / pc a01 p01 p02 p03 p04 p05 p06 w01 w02 w03 w04 ang01)
 (setq *error* *err1*)
 (setvar "cmdecho" 0) 
 (setq PC 1)
  (while (< PC 10)(princ "\n\n")(setq PC (+ PC 1)))
 (TEXTPAGE)
 (princ "\n アングルを描く方向によって歯の向きが異なります。")
 (princ "\n")
 (princ "\n       描く方向        歯の向き")
 (princ "\n      ===============  ========")
 (princ "\n      始点 ==> 右方向    上")
 (princ "\n      始点 ==> 左方向    下")
 (princ "\n      始点 ==> 下方向    右")
 (princ "\n      始点 ==> 上方向    左")
 (setq a01 (getreal "\n  アングルの幅を入力:"))
 (GRAPHSCR)
 (cond ((/= a01 nil)
  
  (setq p01 (getpoint "\n  始点を指示..."))
   
  (cond ((/= p01 nil) (setq p02 (getpoint "\n  終点を指示..."))
   (cond ((/= p02 nil) (setq ang01 (angle p01 p02))
	  (cond ((or (> ang01 5.49779)(<= ang01 0.785398))(setq ang01 0.0)
		 (setq p02 (list (car p02) (cadr p01))))
	        ((and (> ang01 0.785398)(<= ang01 2.35619))(setq ang01 1.5708)
		 (setq p02 (list (car p01) (cadr p02))))
	        ((and (> ang01 2.35619) (<= ang01 3.92699))(setq ang01 3.14159)
		 (setq p02 (list (car p02) (cadr p01))))
	        ((and (> ang01 3.92699) (<= ang01 5.49779))(setq ang01 4.71239)
		 (setq p02 (list (car p01) (cadr p02))))
	  )
	  (setq p03 (polar p01 (+ ang01 1.5708) (- a01 3.2)))
	  (setq p04 (polar p02 (+ ang01 1.5708) (- a01 3.2)))
	  (command "layer" "s" "S1" "")
	  (command "linetype" "s" "hidden" "") (setvar "blipmode" 0)
	  (command "line" p03 p04 "")
	  (setq p03 (polar p01 (+ ang01 1.5708) a01))
	  (setq p04 (polar p02 (+ ang01 1.5708) a01))
	  (command "linetype" "s" "bylayer" "")
	  (command "line" p02 p01 p03 p04 "c")
	  (setq p03 (polar (polar p01 (+ ang01 1.5708) 18.5) ang01 10.5))
	  (setq p04 (polar (polar p01 (+ ang01 1.5708) 18.5) ang01 39.5))
	  (cond ((= ang01 0.0)
			(setq p05 (list (car p03) (+ (cadr p03) 5.5)))
			(setq p06 (list (car p04) (- (cadr p04) 5.5))))
		((= ang01 1.5708)
			(setq p05 (list (- (car p03) 5.5) (cadr p03)))
			(setq p06 (list (+ (car p04) 5.5) (cadr p04))))
		((= ang01 3.14159)
			(setq p05 (list (car p03) (- (cadr p03) 5.5)))
			(setq p06 (list (car p04) (+ (cadr p04) 5.5))))
		((= ang01 4.71239)
			(setq p05 (list (+ (car p03) 5.5) (cadr p03)))
			(setq p06 (list (- (car p04) 5.5) (cadr p04))))
	  )
	  (command "arc" "c" p03 p05 "a" "180") (setq w01 (ssget "l"))
	  (command "arc" "c" p04 p06 "a" "180") (setq w02 (ssget "l"))
	  (setq p03 (polar (polar p01 (+ ang01 1.5708) 13.0) ang01 10.5))
	  (setq p04 (polar (polar p01 (+ ang01 1.5708) 13.0) ang01 39.5))
	  (command "line" p03 p04 "") (setq w03 (ssget "l"))
	  (setq p03 (polar (polar p01 (+ ang01 1.5708) 24.0) ang01 10.5))
	  (setq p04 (polar (polar p01 (+ ang01 1.5708) 24.0) ang01 39.5))
	  (command "line" p03 p04 "") (setq w04 (ssget "l"))
	  (setq p03 (polar p01 ang01 50.0))
	  (setq p04 (polar p01 ang01 100.0))
	  (command "copy" w01 w02 w03 w04 "" "m" p01 p03 p04 "")
	  (setq p03 (polar p01 ang01 (/ (distance p01 p02) 2)))
	  (setq p04 (polar p03 (+ ang01 1.5708) (/ (distance p01 p02) 2)))
	  (command "mirror" w01 "" p03 p04 "n") (setq w01 (ssget "l"))
	  (command "mirror" w02 "" p03 p04 "n") (setq w02 (ssget "l"))
	  (command "mirror" w03 "" p03 p04 "n") (setq w03 (ssget "l"))
	  (command "mirror" w04 "" p03 p04 "n") (setq w04 (ssget "l"))
	  (setq p03 (polar p01 (+ ang01 3.14159) 50.0))
	  (setq p04 (polar p01 (+ ang01 3.14159) 100.0))
	  (command "copy" w01 w02 w03 w04 "" "m" p01 p03 p04 "")
         )
   )
        )
  )
       )
 )
 (setvar "blipmode" 1) (setvar "cmdecho" 1)
)

;;==================================================================================================
;;60.トランスの外形入力用ダイアログボックス   D_TR.LSP        1998/Jun/14   By T.Terada
;; ＊事前に、(setq TYP "1F")等を X:\C-MENU\KD-CAD.XLS/T-MENU/ﾌﾟﾛｸﾞﾗﾑ覧で実行しておく事。
;;==================================================================================================
  (defun C:D_TR ( / SYM1 P1)
;     (setq *error* *err1*)
;     (C:ST1)
     (setq dcl_id (load_dialog "X:/DCL/DTRSCSR.DCL"))
     (if (not (new_dialog "D_TR" dcl_id) )(exit))
     (D_TR)
     (if (= SYM1 nil)
         (progn               
            (alert "  容量が、選択されていません！")
            (setq dcl_id (load_dialog "X:/DCL/DTRSCSR.DCL"))
            (if (not (new_dialog "D_TR" dcl_id) )(exit))
            (D_TR)
         )
     )
      ;(princ "\nSYM1=")(princ SYM1)
     (princ "\n挿入点を指示")(command "INSERT" SYM1 pause "" "" "")
     (action_tile "cancel" "(done_dialog 0)(exit)")
     (setq TYP nil OM nil PHI nil)
     (C:END1)
  )
;;-------------------------------------------------------------------------------------------------
  (defun  D_TR(/ LB FSU OMT SOU)
        (action_tile "cancel" "(done_dialog)(exit)")
        (action_tile "10K"  "(setq SYM1 (strcat \"DTR\" OM PHI \"010\" TYP \"M\"))")
	(action_tile "20K"  "(setq SYM1 (strcat \"DTR\" OM PHI \"020\" TYP \"M\"))")
	(action_tile "30K"  "(setq SYM1 (strcat \"DTR\" OM PHI \"030\" TYP \"M\"))")
	(action_tile "50K"  "(setq SYM1 (strcat \"DTR\" OM PHI \"050\" TYP \"M\"))")
	(action_tile "75K"  "(setq SYM1 (strcat \"DTR\" OM PHI \"075\" TYP \"M\"))")
	(action_tile "100K" "(setq SYM1 (strcat \"DTR\" OM PHI \"100\" TYP \"M\"))")
	(action_tile "150K" "(setq SYM1 (strcat \"DTR\" OM PHI \"150\" TYP \"M\"))")
	(action_tile "200K" "(setq SYM1 (strcat \"DTR\" OM PHI \"200\" TYP \"M\"))")
	(action_tile "300K" "(setq SYM1 (strcat \"DTR\" OM PHI \"300\" TYP \"M\"))")
	(action_tile "500K" "(setq SYM1 (strcat \"DTR\" OM PHI \"500\" TYP \"M\"))")
	(action_tile "750K" "(setq SYM1 (strcat \"DTR\" OM PHI \"750\" TYP \"M\"))")
	(action_tile "X00K" "(setq SYM1 (strcat \"DTR\" OM PHI \"X00\" TYP \"M\"))")
	   (if (= PHI "1P")(setq SOU "1φ"))
	   (if (= PHI "3P")(setq SOU "3φ"))
	   (if (= TYP "F")(setq FSU "正面図"))
	   (if (= TYP "S")(setq FSU "側面図"))
	   (if (= TYP "U")(setq FSU "上面図"))
	   (if (= OM "O")(setq OMT "油冷"))
	   (if (= OM "M")(setq OMT "ﾓｰﾙﾄﾞ"))
	   (setq LB (strcat "TR：" SOU " ・ " OMT "型 ・ " FSU))
	   (set_tile "LB" LB)
	   (if (and (= PHI "3P")(= OM "O"))(mode_tile "10K" 1))
	   (if (and (= PHI "1P")(= OM "O"))(progn (mode_tile "750K" 1)(mode_tile "X00K" 1)))
	   (if (and (= PHI "3P")(= OM "M"))(progn (mode_tile "10K" 1)
	                                          (mode_tile "750K" 1)
	                                          (mode_tile "X00K" 1)))
	   (if (and (= PHI "1P")(= OM "M"))(progn (mode_tile "10K" 1)
	                                          (mode_tile "500K" 1)
	                                          (mode_tile "750K" 1)
	                                          (mode_tile "X00K" 1)))
        (start_dialog)(unload_dialog dcl_id)(princ)
  )
;;==================================================================================================
;;61.コンデンサ－外形図入力用ダイアログボックス   D_SC.LSP    1998/Jun/14   By T.Terada
;;==================================================================================================
  (defun C:D_SC ( / SYM1 P1)
     (C:ST1)
     (setq dcl_id (load_dialog "X:/DCL/DTRSCSR.DCL"))
     (if (not (new_dialog "D_SC" dcl_id) )(exit))
     (D_SC)
     (if (= SYM1 nil)
         (progn
               
            (alert "  容量が、選択されていません！")
            (setq dcl_id (load_dialog "X:/DCL/DTRSCSR.DCL"))
            (if (not (new_dialog "D_SC" dcl_id) )(exit))
            (D_SC)
         )
     )
      (princ "\n挿入点を指示")(command "INSERT" SYM1 pause "" "" "")
     (action_tile "cancel" "(done_dialog 0)(exit)")
     (setq TYP nil OM nil PS nil SYM1 nil)
     (C:END1)
  )
;;----------------------------------------------------------
  (defun D_SC (/ LB FSU) ;ＳＣ 正面図
        (action_tile "cancel" "(done_dialog)(exit)")
	(action_tile "3P10"  "(setq SYM1 (strcat \"DSC\" PS \"010\" TYP))")
	(action_tile "3P15"  "(setq SYM1 (strcat \"DSC\" PS \"015\" TYP))")
	(action_tile "3P20"  "(setq SYM1 (strcat \"DSC\" PS \"020\" TYP))")
	(action_tile "3P25"  "(setq SYM1 (strcat \"DSC\" PS \"025\" TYP))")
	(action_tile "3P30"  "(setq SYM1 (strcat \"DSC\" PS \"030\" TYP))")
	(action_tile "3P50"  "(setq SYM1 (strcat \"DSC\" PS \"050\" TYP))")
	(action_tile "3P75"  "(setq SYM1 (strcat \"DSC\" PS \"075\" TYP))")
	(action_tile "3P100" "(setq SYM1 (strcat \"DSC\" PS \"100\" TYP))")
	(action_tile "3P150" "(setq SYM1 (strcat \"DSC\" PS \"150\" TYP))")
	(action_tile "3P200" "(setq SYM1 (strcat \"DSC\" PS \"200\" TYP))")
	(action_tile "3P250" "(setq SYM1 (strcat \"DSC\" PS \"250\" TYP))")
	(action_tile "3P300" "(setq SYM1 (strcat \"DSC\" PS \"300\" TYP))")
	   (if (= TYP "F")(setq FSU "正面図"))
	   (if (= TYP "S")(setq FSU "側面図"))
	   (if (= TYP "U")(setq FSU "上面図"))
	   (setq LB (strcat "SC：" PS " ％ ・ " FSU))
	(set_tile "LB" LB)
        (start_dialog)(unload_dialog dcl_id)(princ)
  )
;;=======================================================================================================
;;62.リアクトル 正面図入力用ダイアログボックス    D_SR.LSP    1998/Jun/14   By T.Terada
;;＊事前に、(setq TYP "F")(setq OM "1F")(setq PS "6")等を X:\C-MENU\KD-CAD.XLS/T-MENU/ﾌﾟﾛｸﾞﾗﾑ覧で実行
;;=======================================================================================================
  (defun C:D_SR (/ SYM1 P1)
     (C:ST1)
     (setq TYP "S")(setq OM "O")(setq PS "6")
     (setq dcl_id (load_dialog "X:/DCL/DTRSCSR.DCL"))
     (if (not (new_dialog "D_SR" dcl_id) )(exit))
     (D_SR)
     (if (= SYM1 nil)
         (progn
            (alert "  容量が、選択されていません！")
            (setq dcl_id (load_dialog "X:/DCL/DTRSCSR.DCL"))
            (if (not (new_dialog "D_SR" dcl_id) )(exit))
            (D_SR)
         )
     )
;    (princ "\nTYP=")(princ TYP)(princ "\nOM=")(princ OM)(princ "\nPS=")(princ PS)(princ "\nSYM1=")(princ SYM1)
      (princ "\n挿入点を指示")(command "INSERT" SYM1 pause "" "" "")
     (action_tile "cancel" "(done_dialog 0)(exit)")
     (setq TYP nil OM nil PS nil)
     (C:END1)
  )
;;---------------------------------------------------------------
  (defun D_SR (/ LB FSU OMT) ;ＳＲ 
        (action_tile "cancel" "(done_dialog)(exit)")
	(action_tile "3P50"  "(setq SYM1 (strcat \"DSR\" OM PS \"050\" TYP))")
	(action_tile "3P75"  "(setq SYM1 (strcat \"DSR\" OM PS \"075\" TYP))")
	(action_tile "3P100" "(setq SYM1 (strcat \"DSR\" OM PS \"100\" TYP))")
	(action_tile "3P150" "(setq SYM1 (strcat \"DSR\" OM PS \"150\" TYP))")
	(action_tile "3P200" "(setq SYM1 (strcat \"DSR\" OM PS \"200\" TYP))")
	(action_tile "3P250" "(setq SYM1 (strcat \"DSR\" OM PS \"250\" TYP))")
	(action_tile "3P300" "(setq SYM1 (strcat \"DSR\" OM PS \"300\" TYP))")
	(action_tile "3P400" "(setq SYM1 (strcat \"DSR\" OM PS \"400\" TYP))")
	(action_tile "3P500" "(setq SYM1 (strcat \"DSR\" OM PS \"500\" TYP))")
	   (if (= TYP "F")(setq FSU "正面図"))
	   (if (= TYP "S")(setq FSU "側面図"))
	   (if (= TYP "U")(setq FSU "上面図"))
	   (if (= OM "O")(setq OMT "油冷"))
	   (if (= OM "M")(setq OMT "ﾓｰﾙﾄﾞ"))
	   (setq LB (strcat "SR：" PS " ％ ・ " OMT "型 ・ " FSU))
	   (set_tile "LB" LB)
	   (if (= OM "M")(progn (mode_tile "3P400" 1)(mode_tile "3P500" 1)))
        (start_dialog)(unload_dialog dcl_id)(princ)
  )
;;=================================================================================================
;;63.ブスバーサポートの入力。     Cuz.DCL   CUZ.LSP    2005/3/19  By T.terada
;;=================================================================================================
(defun C:CUZ ( / SYM1 SYM2 SYM3 PC AF SH T11 T12 T13 PNA PNO)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (DCL_CUZ)
      
      (cond ((or (= AF nil)(= SH nil))(alert "項目が入力されていません！")(DCL_CUZ)))
      (cond ((and (= KOU "YES")(= SH "82"))(alert "「高さ82」の「協約型高遮断用」はありません！")(DCL_CUZ)))
      (setq PNA "CuZ"  MN "36"  SUU "1"  Z1 "BBM1"  SYM1 "MCUZ182"  SYM2 "MCUZ286"  SYM3 "MCUZ4106")
      
      ;50AF
      (cond ((and (= KOU "NO")(= AF "50")(= SH "82")) (setq T11 "BS-20")))
      (cond ((and (= KOU "NO")(= AF "50")(= SH "106"))(setq T11 "BS-20")(setq T12 "PEB-24")))
      (cond ((and (= KOU "NO")(= AF "50")(= SH "146"))(setq T11 "BS-20")(setq T12 "PEB-64")))
      ;50AF 協約型高遮断用
      (cond ((and (= KOU "YES")(= AF "50")(= SH "106"))(setq T11 "BS-25")(setq T12 "BS-3")(setq T13 "BS-2")))
      (cond ((and (= KOU "YES")(= AF "50")(= SH "146"))(setq T11 "BS-20")(setq T12 "PEB-54")(setq T13 "BS-2")))
      
      ;100AF
      (cond ((and (= KOU "NO")(= AF "100")(= SH "82")) (setq T11 "BS-20")))
      (cond ((and (= KOU "NO")(= AF "100")(= SH "106"))(setq T11 "BS-20")(setq T12 "PEB-24")))
      (cond ((and (= KOU "NO")(= AF "100")(= SH "146"))(setq T11 "BS-20")(setq T12 "PEB-64")))
      ;100AF 協約型高遮断用
      (cond ((and (= KOU "YES")(= AF "100")(= SH "106"))(setq T11 "BS-25")(setq T12 "BS-3")(setq T13 "BS-2")))
      (cond ((and (= KOU "YES")(= AF "100")(= SH "146"))(setq T11 "BS-20")(setq T12 "PEB-54")(setq T13 "BS-2")))
            
      ;225AF
      (cond ((and (= KOU "NO")(= AF "225")(= SH "106"))(setq T11 "BS-25")(setq T12 "PEB-17")))
      (cond ((and (= KOU "NO")(= AF "225")(= SH "146"))(setq T11 "BS-25")(setq T12 "PEB-54")(setq T13 "BS-2")))
      ;225AF 協約型高遮断用
      (cond ((and (= KOU "YES")(= AF "225")(= SH "106"))(setq T11 "BS-25")(setq T12 "BS-3")))
      (cond ((and (= KOU "YES")(= AF "225")(= SH "146"))(setq T11 "BS-20")(setq T12 "PEB-54")))
      
      ;400AF
      (cond ((and (= KOU "NO")(= AF "400")(= SH "106"))(setq T11 "BS-20")(setq T12 "PEB-17")(setq T13 "BS-2")))
      (cond ((and (= KOU "NO")(= AF "400")(= SH "146"))(setq T11 "BS-25")(setq T12 "PEB-54")))
      ;400AF 協約型高遮断用
      (cond ((and (= KOU "YES")(= AF "400")(= SH "106"))(setq T11 "BS-25")(setq T12 "BS-3")))
      (cond ((and (= KOU "YES")(= AF "400")(= SH "146"))(setq T11 "BS-25")(setq T12 "KPEB-46")))
      
      ;数量・メーカーの変更
      (cond ((and (= KOU "YES")(= AF "50") (= SH "106"))(setq SYM2 "MCUZ286B")(setq SUU "3")))
      (cond ((and (= KOU "YES")(= AF "100")(= SH "106"))(setq SYM2 "MCUZ286B")(setq SUU "3")))
      (cond ((and (= KOU "YES")(= AF "225")(= SH "106"))(setq SYM2 "MCUZ286B")(setq SUU "3")))
      (cond ((and (= KOU "YES")(= AF "400")(= SH "106"))(setq SYM2 "MCUZ286B")(setq SUU "2")))
      (cond ((and (= KOU "YES")(= AF "400")(= SH "146"))(setq MN "100")))
      
       (setq PNO (getstring "\n機器番号を指示:(例：-1)"))
      (cond ((= PNO "") (setq PNO "-1")))(setq PNO (strcat PNA PNO))
       (princ "\n挿入点を指示...")
      
      ;(princ "\nKOU:") (princ KOU)
      ;(princ "\nAF:")  (princ AF)
      ;(princ "\nSH:")  (princ SH)
      ;(princ "\nSYM1:")(princ SYM1)
      ;(princ "\nPNA:") (princ PNA)
      ;(princ "\nPNO:") (princ PNO)
      ;(princ "\nT11:") (princ T11)
      ;(princ "\nMN:")  (princ MN)
      ;(princ "\nSUU:") (princ SUU)
      ;(princ "\nZ1:")  (princ Z1)
      
      (command "INSERT" SYM1 pause "1" "1" "0" PNA PNO T11 "" "" "" "" "" "" "" "" "36" "1" Z1 "")
      
      (setq PC  (getvar "LASTPOINT"))
      (cond ((/= T12 nil)
            (command "INSERT" SYM2 PC "1" "1" "0" PNA PNO T12 "" "" "" "" "" "" "" "" MN SUU "" ""))
      )
      
      (cond ((= SYM2 "MCUZ286B")(setq PC (list (+ (car PC) 10)(cadr PC)))));MCUZ286Bは数量も表示するので幅が変わる
      (cond ((/= T13 nil)
            (command "INSERT" SYM3 PC "1" "1" "0" PNA PNO T13 "" "" "" "" "" "" "" "" "36" "1" "" ""))
      )
      
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
      (redraw)
)
;---------------------------------------------------------------------------------------------------
(defun DCL_CUZ(/)
      (setq KOU "NO")
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "CUZ" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "50AF"  "(setq AF \"50\")   (mode_tile \"82\" 0)")
          (action_tile "100AF" "(setq AF \"100\")  (mode_tile \"82\" 0)")
          (action_tile "225AF" "(setq AF \"225\")  (mode_tile \"82\" 1)")
          (action_tile "400AF" "(setq AF \"400\")  (mode_tile \"82\" 1)")
          (action_tile "82"    "(setq SH \"82\")")
          (action_tile "106"   "(setq SH \"106\")")
          (action_tile "146"   "(setq SH \"146\")")
          (action_tile "NO"    "(setq KOU \"NO\")")
          (action_tile "YES"   "(setq KOU \"YES\") (mode_tile \"82\" 1)")
          
          ;DCLではif文が使えない。高遮断用の高さ82はダイアログに表記と実行時にエラー表示で対応
          ;(action_tile "50AF"  "(setq AF \"50\")   (cond ((= KOU "NO")(mode_tile \"82\" 0)))")
      (start_dialog) (unload_dialog DCL_ID) (princ)
)
;;==================================================================================================
;;64.バ－サイズ記入プログラム    BAR-SIZE.LSP
;;==================================================================================================
(defun C:BARSZ(/)
   (setq *error* *err1*)(setvar "CMDECHO" 0)(command "COLOR" 2)
   (DCL_BARSZ)
    (setq P01 (getpoint "\n挿入点を指示..."))
   (command "INSERT" "BU-3P" P01 "1" "1" "0")
      (setq PT (list (+ (car P01) 125) (cadr P01)))
      (command "TEXT" "S" "M0" PT "20" "0" Cu)
      (redraw)(command "COLOR" 4)(setvar "CMDECHO" 1)
)
;---------------------------------------------------------------------------------------------------
(defun DCL_BARSZ(/)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "BARSZ" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq Cu \"Cu4×18\")") 
          (action_tile "2" "(setq Cu \"Cu6×20\")")
          (action_tile "3" "(setq Cu \"Cu8×28\")")
          (action_tile "8" "(setq Cu \"Cu4×50\")")
          (action_tile "9" "(setq Cu \"Cu5×50\")")
          (action_tile "4" "(setq Cu \"Cu8×50\")")
          (action_tile "10" "(setq Cu \"Cu12×50\")")
          (action_tile "11" "(setq Cu \"Cu8×100\")")
          (action_tile "12" "(setq Cu \"Cu10×100\")")
          (action_tile "5" "(setq Cu \"Cu3×15\")")
          (action_tile "6" "(setq Cu \"Cu6×20\")")
          (action_tile "7" "(setq Cu \"Cu8×28\")")
      (start_dialog) (unload_dialog DCL_ID) (princ)
)
;;=================================================================================================
;;65.突き合せＢＯＸの上面図、描画           BOX_U.DCL   BOX_U.LSP    2005/3/24  By T.terada
;;=================================================================================================
(defun C:BOX_U ( / LR UD DN PC PC1 PC2 PC3 W0 D0 DW DR1 PM1 PM2 PM3)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
       (DCL_UBOX)
       (cond ((or (= LR nil)(= UD nil)(= DN nil))(alert "項目が入力されていません！")(DCL_UBOX)))
        (setq W0 (getreal  "\n横幅を指示してください．:"))
        (setq H0 (getreal  "\n奥行を指示してください．:"))
        (setq DW (getreal  "\n扉の厚みを指示してください．:"))
;      (setq W0 600 H0 300 DW 30)
        (setq PC (getpoint "\n箱体の起点を指示："))
       (princ "\nLR=")(princ LR)(princ "\nUD=")(princ UD)(princ "\nDN=")(princ DN)
    ;;入力項目
       (command "LINETYPE" "S" "CONTINUOUS" "")
       (setq H1 (- H0 (+ DW 5)))                  ;BOX本体の奥行き
       (setq W1 (- W0 10))                        ;扉幅1
       (setq W2 (/ (- W0 20) 2))                  ;２枚扉幅
       (cond ((= LR "R")(setq PM1 +)(setq PM2 +)));左起点
       (cond ((= LR "L")(setq PM1 -)(setq PM2 +)));右起点
       (cond ((and (= LR "R")(= UD "U"))(setq AP +30)(setq APL +30)(setq APR -30)))
       (cond ((and (= LR "R")(= UD "D"))(setq AP -30)(setq APL -30)(setq APR +30)))
       (cond ((and (= LR "L")(= UD "U"))(setq AP -30)(setq APL -30)(setq APR +30)))
       (cond ((and (= LR "L")(= UD "D"))(setq AP +30)(setq APL +30)(setq APR -30)))
       (setq DR1 (list (PM1 (car PC) W0) (- (cadr PC) H1)))
       (command "rectangle" PC DR1);BOX本体
       
       ;扉上面-1
       (cond ((and (= UD "U")(= DN "D1"));１枚扉
              (setq PC1 (list (PM1 (car PC) 5)  (PM2 (cadr PC)  5 )))
              (setq DRU (list (PM1 (car PC1) W1)(PM2 (cadr PC1) DW)))
              (command "LINETYPE" "S" "DASHED" "")(command "rectangle" PC1 DRU)
              (command "rotate" "L" "" PC1 AP)  (command "arc" "C" PC1 DRU "A" AP)
              (command "LINETYPE" "S" "CONTINUOUS" "")
              (command "rectangle" PC1 DRU))       ;Door1U
       )
       (cond ((and (= UD "U")(= DN "D2"));２枚扉
              (setq PC1 (list (PM1 (car PC) 5)  (PM2 (cadr PC)  5)))
              (setq DRU (list (PM1 (car PC1) W1)(PM2 (cadr PC1) DW)))
              (setq DRR (list (PM1 (car PC1) W1)(cadr PC1)))
              (setq CL  (list (PM1 (car PC1) W2)(cadr DRU)))
              (setq CR  (list (PM1 (car PC1) (+ W2 10))(cadr PC1)))
              (command "LINETYPE" "S" "DASHED" "")
              (command "rectangle" PC1 CL);左扉
              (command "rotate" "L" "" PC1 APL) (command "arc" "C" PC1 CL "A" APL)
              (command "rectangle" CR DRU);右扉
              (command "rotate" "L" "" DRR APR) (command "arc" "C" DRR CR "A" APR)
              (command "LINETYPE" "S" "CONTINUOUS" "")
              (command "rectangle" PC1 CL) ;Door2LU
              (command "rectangle" CR DRU));Door2RU
       )
       ;扉下面-2
       (cond ((and (= UD "D")(= DN "D1"));１枚扉
              (setq PM3 -)
              (setq PC2 (list (PM1 (car PC) 5)  (PM3 (cadr PC)(+ H1 5))))
              (setq DRD (list (PM1 (car PC2) W1)(PM3(cadr PC2) DW)))
              (command "LINETYPE" "S" "DASHED" "")(command "rectangle" PC2 DRD)
              (command "rotate" "L" "" PC2 AP) (command "arc" "C" PC2 DRD "A" AP)
              (command "LINETYPE" "S" "CONTINUOUS" "")
              (command "rectangle" PC2 DRD))       ;Door1D
       )
       (cond ((and (= UD "D")(= DN "D2"));２枚扉
              (setq PM3 -)
              (setq PC2 (list (PM1 (car PC) 5)  (PM3 (cadr PC)(+ H1 5))))
              (setq DRD (list (PM1 (car PC2) W1)(PM3 (cadr PC2) DW)))
              (setq DRR (list (PM1 (car PC2) W1)(cadr PC2)))
              (setq CL  (list (PM1 (car PC2) W2)(cadr DRD)))
              (setq CR  (list (PM1 (car PC2)(+ W2 10))(cadr PC2)))
              (command "LINETYPE" "S" "DASHED" "")
              (command "rectangle" PC2 CL);左扉
              (command "rotate" "L" "" PC2 APL) (command "arc" "C" PC2 CL "A" APL)
              (command "rectangle" CR DRD);右扉
              (command "rotate" "L" "" DRR APR) (command "arc" "C" DRR CR "A" APR)
              (command "LINETYPE" "S" "CONTINUOUS" "")
              (command "rectangle" PC2 CL)
              (command "rectangle" CR DRD))        ;Door2RD
       )
    (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(redraw)
)
;---------------------------------------------------------------------------------------------------
(defun DCL_UBOX(/)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "UBOX" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "L"  "(setq LR  \"L\")") (action_tile "R"  "(setq LR  \"R\")")
          (action_tile "U"  "(setq UD  \"U\")") (action_tile "D"  "(setq UD  \"D\")")
          (action_tile "D1" "(setq DN  \"D1\")")(action_tile "D2" "(setq DN  \"D2\")")
      (start_dialog) (unload_dialog DCL_ID) (princ)
)

;;=================================================================================================
;;67.外部接点記号の入力（操作回路）              OUT-C.LSP    2005/4/10  By T.terada
;;=================================================================================================
(defun C:OUT-C ( / P1 P2 PX1 PX2 SS SYM T1 T2 PC P1 P2 P12 PY1 PY2 ENT)
    (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
     (setq MSG "外部接点の挿入点を指示：")(setq PC (getpoint MSG))
             (setq PX1 (list (- (car PC) 10) (- (cadr PC) 5)))
             (setq PX2 (list (+ (car PC) 10) (+ (cadr PC) 5)))
    (setq SS (ssget "C" PX1 PX2))
    (if (/= SS nil) (progn (setq ENT (ssname ss 0))))
    (setq KIKI "\nＡ接点")(setq SYM "OUT-C") 
    (setq T1 (getstring "\n１:外部記号の名称を指示:(例：外部接点)"))(cond ((= T1 "") (setq T1 "外部接点")))
     
    (setq T2 (getstring "\n２:上端子の番号を指示:(例：1A)"))(cond ((= T2 "") (setq T2 "1A")))
     
    (setq T3 (getstring "\n３:上端子の番号を指示:(例：1B)"))(cond ((= T3 "") (setq T3 "1B")))
    (GC)
    (if (/= ENT nil)
         (progn
            (setq PY1 (list (car PC) (+ (cadr PC) 10)))
            (setq P12 (list (car PC) (- (cadr PC) 100)))
            (command "BREAK" PY1 "F" PC P12)
         )
         (princ "\n線のカットはしません");else  ;縦線が存しないなら 
     )
     (command "COLOR" 2)
     (command "INSERT" SYM PC "1" "1" "0" T1 T2 T3 "" "" "")
;;後始末
     (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(setq P1 nil P2 nil PC nil PY1 nil P12 nil)
     (princ "\n入力完了")
     (redraw)
)
;;=================================================================================================
;;68.開口図 正面 水平方向              開口図.LSP    2005/4/13  By T.terada
;;=================================================================================================
(defun C:KAI_FH ( / PC PC1 PC3 WW DD W2 PLU PLD PRU PRD PCL WWT DDI STRR)
;       (setq *error* *err1*)
       (setvar "CMDECHO" 1)(setvar "BLIPMODE" 0)(command "COLOR" 4)
        (setq PC (getpoint "\n 開口部の中心点を指示...."))
        (setq WW (getint  "\n 開口部の横幅を指示:"))
        (setq DD (getstring  "\n 開口部の奥行を指示:"))
       (setq W2 (/ WW 2))
    ;正面の開口位置
       (setq PLU (list (- (car PC) W2)(+ (cadr PC) 15)))
       (setq PLD (list (- (car PC) W2)(- (cadr PC) 15)))
       (setq PRU (list (+ (car PC) W2)(+ (cadr PC) 15)))
       (setq PRD (list (+ (car PC) W2)(- (cadr PC) 15)))
      ;(setq PC1 (list (car PC)(+ (cadr PC) 60)))
      ;(setq PC2 (list (car PC)(+ (cadr PC) 30)))
      ;(setq WWT (itoa WW))
      ;(setq TX1 (strcat "開口 (W" WWT "× D" DD ")"))
      ;(setvar "TEXTSIZE" 20)
       (command "PLINE" PLU PRD PRU PLD "C")
    ;側面の開口位置 
       (setq DDI (atoi DD))
       (setq PRU (list (+ (car PRU) 750)(cadr PRU)))
       (setq PRD (list (car PRU)(- (cadr PRU) 30)))
       (setq PLD (list (- (car PRD) DDI)(cadr PRD)))
      ;(setq PLD (list (- (car PRD) 100)(cadr PRD)))
       (setq PLU (list (car PLD)(+ (cadr PLD) 30)))
      ;(command "PLINE" PLU PRD PRU PLD "C")
       (princ)(redraw)
       (setq WWT (itoa WW))
       (setq TX1 (strcat "開口 (W" WWT "× D" DD ")"))
       (setvar "TEXTSIZE" 20)
       (command "PLINE" PLU PRD PRU PLD "C")
      ;(imemode T);IME ON
      ;(command "COLOR" 2)(command "TEXT" "C" PC1 "" "0" TX1)
        
;;       (CKAI_CMT)
       (princ "\nSTR=")(princ STR)
       (setq STRR (substr STR 1 2))
       (princ "\nSTRR=")(princ STRR)
    ;箱体開口のコメント入力で、先頭に"上"と入力された場合の文字の挿入位置 
       (if (= STRR "上")
           (progn
            (setq PC1 (list (car PC)(+ (cadr PC) 60)))
            (setq PC2 (list (car PC)(+ (cadr PC) 30)))
           )
       )
    ;箱体開口のコメント入力で、先頭に"下"と入力された場合の文字の挿入位置
       (if (= STRR "下")
           (progn 
            (setq PC1 (list (car PC)(- (cadr PC) 80)))
            (setq PC2 (list (car PC)(- (cadr PC) 50)))
           )
           (progn
            (setq PC1 (list (car PC)(+ (cadr PC) 60)))
            (setq PC2 (list (car PC)(+ (cadr PC) 30)))
           )
       )
　　 ;それ以外の文字が入力された場合は、先頭に"上"と入力された場合と同じ挿入位置
       ;(princ "\nPC1=")(princ PC1)
       ;(princ "\nPC2=")(princ PC2)
       ;(imemode nil)
       (command "COLOR" 2)(command "TEXT" "C" PC1 "" "0" TX1)
                          (command "TEXT" "C" PC2 "" "0" STR)
       (command "COLOR" 4)(setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
)
;;=================================================================================================
;;69.開口図 正面 垂直方向              KAI_FV.LSP    2005/4/13  By T.terada
;;=================================================================================================
(defun C:KAI_FV ( / PC WW W2 PLU PLD PRU PRD)
   (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 1)(command "COLOR" 4)
        (setq PC (getpoint "\n 開口部の中心点を指示...."))
        (setq WW (getreal  "\n 開口部の高さを指示:"))
       (setq W2 (/ WW 2))
       (setq PLU (list (- (car PC) 15)(+ (cadr PC) W2)))
       (setq PLD (list (- (car PC) 15)(- (cadr PC) W2)))
       (setq PRU (list (+ (car PC) 15)(+ (cadr PC) W2)))
       (setq PRD (list (+ (car PC) 15)(- (cadr PC) W2)))
       (command "PLINE" PLU PRD PLD PRU "C")
   (REDRAW)(setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
)
;;==================================================================================================
;;70.図枠の日付更新         DATEUP.LSP For I-CAD     2005/MAY   By T.Terada
;;==================================================================================================
(defun C:DATEUP( / n n2 AA BB CC DD ENT LMAX SZ SZW FP1 FP2 FP3 FP4 HIZUKE HI JI)
  (setq *error* *err1*)(setvar "CMDECHO" 0)

  (command "ZOOM" "E") ;図枠右下の日付部分が画面に表示されていないと変更できないのでズームする

  (setq SZ nil)
  (setq LMAX (getvar "limmax"))                  ;現図面の図面範囲

        (if (= (car LMAX) 2100)(setq SZ "A5"));現図面の図面範囲

        (if (= (car LMAX) 2970)(setq SZ "A4"))
        (if (= (car LMAX) 4200)(setq SZ "A3"))
        (if (= (car LMAX) 5940)(setq SZ "A2"))
        (if (= (car LMAX) 8400)(setq SZ "A1"))
        (if (= (car LMAX) 11880)(setq SZ "A0"))
  (setq SZW (strcat SZ "W"))                     ;変更元の図枠記入覧
        (setq FP1 (list 0 8400))    ;フェンス左上
        (setq FP2 (list 0 0))       ;フェンス左下
        (setq FP3 (list 11880 0))   ;フェンス右下
        (setq FP4 (list 11880 8400));フェンス右上

  (setq HIZUKE (rtos (getvar "CDATE") 2 8));;13/05/07 4から8に変更
  (setq HI (strcat (substr HIZUKE 3 2) "/" (substr HIZUKE 5 2) "/" (substr HIZUKE 7 2)))
  (setq JI (strcat (substr HIZUKE 10 2) ":" (substr HIZUKE 12 2)))
  (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
; (princ "\nAA=")(princ AA)
;;シンボルの属性を読み込んで書き換える   ATT-RW.LSP
  (if (/= AA nil)
      (progn
         (setq n 0)
         (setq AA (ssget "WP" (list FP1 FP2 FP3 FP4) (list '(66 . 1))))
           (while (setq BB (ssname AA n))
              (setq ENT (entget BB))
              (if (= (cdr (assoc 2 ENT)) SZW);;属性の書込対象となるシンボル
                 (progn
                    (setq N2 1)
                    (setq CC (entnext BB)  DD (entget CC))
                    (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                       (if (= (cdr (assoc 0 DD)) "ATTRIB")
                           (progn
                              (if (= (cdr (assoc 2 DD)) "ZW9") (entmod (subst (cons 1 HI)(assoc 1 DD) DD)))
                              (if (= (cdr (assoc 2 DD)) "ZW12")(entmod (subst (cons 1 JI)(assoc 1 DD) DD)))
                           )
                       )
                       (setq N2 (1+ N2))
                       (setq CC (entnext CC) DD (entget CC))
                    )
                 );progn
              );if
              (setq n (1+ n)) 
          );while
      );progn
  )
 (command "regen")
)

;;==================================================================================================
;;71.キュービクル用チャンネルバース         QB-CHANB.LSP -> QBC    2005/MAY   By T.Terada   不動作
;;==================================================================================================
(defun C:QBC(/ MSG1 PC BW PDY STL PDYL PDYR DSTR DBASE SEL);※※ストレッチを使わずにPLINEで描く事！！！！
   (setvar "CMDECHO" 1);;;;;(setq *error* *err1*)
   (command "COLOR" 4)(setvar "PLINEWID" 0)  ;線幅を0
   (command "-LINETYPE" "S" "CONTINUOUS" "");(command "-LAYER" "S" "BOX" "" "");画層 BOXに変更
;;---------------------------------------------------------------------------------------------------
	(setq SEL(getstring "チャンネルベースは 左端 / 1  中 / 2   右端 / 3 :"))
	(princ SEL)
	  (if (= SEL "1")(setq SYM "*3041"))
	  (if (= SEL "2")(setq SYM "*3042"))
	  (if (= SEL "3")(setq SYM "*3043"))
	(princ SYM)
    (if (or (= SYM "*3041")(= SYM "*3042"))(setq MSG1 "（左上端）："));BLOCKの幅=1500
    (if (= SYM "*3043")(setq MSG1 "（右上端）："))
     (setq PC (getpoint (strcat "\n挿入点を指示 " MSG1)))
     (setq BW (getint  "\n横幅を指示してください．:"))
     (setq PDY  (list (car PC)(- (cadr PC) 10000)))                 ;ﾁｬﾝﾈﾙの仮挿入点
    (if (or (= SYM "*3041")(= SYM "*3042"))
        (progn
           (setq STL  (+ 1500 (- BW 1500)))                              ;ｽﾄﾚｯﾁ距離 
           (setq PDYL (list (+ (car PDY) 1600)(+ (cadr PDY) 150)))       ;ｽﾄﾚｯﾁ左ﾌｪﾝｽ点
           (setq PDYR (list (+ (car PDY) 6000)(- (cadr PDY) 150)))       ;ｽﾄﾚｯﾁ右ﾌｪﾝｽ点
           (setq DSTR (list (+ (car PDY) STL) (cadr PDY)))               ;ｽﾄﾚｯﾁ目標点
           (setq DBASE(list (+ (car PDY) 1500)(cadr PDY)))               ;ｽﾄﾚｯﾁ基点
           (command "INSERT" SYM PDY "1" "0")
           (command "STRETCH" "C" PDYL PDYR "" DBASE DSTR)
           (command "MOVE" "W" (list (- (car PDY) 200)(- (cadr PDY) 200)) PDYL "" PDY PC)
           (princ "PDY = ")(princ PDY)
        )
    )
    (if (= SYM "*3043")
        (progn
           (setq STL  (+ 1500 (- BW 1500)))                              ;ｽﾄﾚｯﾁ距離 
           (setq PDYL (list (- (car PDY) 1600)(+ (cadr PDY) 150)))       ;ｽﾄﾚｯﾁ左ﾌｪﾝｽ点
           (setq PDYR (list (- (car PDY) 6000)(- (cadr PDY) 150)))       ;ｽﾄﾚｯﾁ右ﾌｪﾝｽ点
           (setq DSTR (list (- (car PDY) STL) (cadr PDY)))               ;ｽﾄﾚｯﾁ目標点
           (setq DBASE(list (- (car PDY) 1500)(cadr PDY)))               ;ｽﾄﾚｯﾁ基点
           (command "INSERT" SYM PDY "1" "0")
           (command "STRETCH" "C" PDYL PDYR "" DBASE DSTR)
           (command "MOVE" "W" (list (+ (car PDY) 200)(- (cadr PDY) 200)) PDYl "" PDY PC)
        )
    )
           (princ "   PDTL = ")(princ PDYL)
           (princ "   PDTR = ")(princ PDYR)
  (setvar "CMDECHO" 1)(redraw)
)

;;==================================================================================================
;;72-1.制御回路用ｼﾝﾎﾞﾙの属性値（N1:機器番号）をまとめて変更する。 N1.LSP  2003/SEP   By T.Terada
;;==================================================================================================
(defun C:N1(/ AA BB CC DD n NewAtt)
  (setq *error* *err1*)
  (setq n 0)
  (setvar "CMDECHO" 0)
   
  (princ "\n接点の機器番号を変更します。： ")
  (setq NEWATT (getstring "機器番号を入力(例:AX)："))
  ;;(princ "\n変更するオブジェクトを指示:<ﾘﾀｰﾝ=全て>")
  ;;(if (null (setq AA (ssget (list '(66 . 1)))))
  ;;          (setq AA (ssget "x" (list '(66 . 1))))
  ;;)
  (princ "\n変更するオブジェクトを指示:")
  (if (null (setq AA (ssget (list '(66 . 1)))))
            (exit)
  )

  (while (setq BB (ssname AA n))
     (setq CC (entnext BB)  DD (entget CC))
          (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
             (if (= (cdr (assoc 0 DD)) "ATTRIB")
                 (progn
                    (if (= (cdr (assoc 2 DD)) "N1")
                        (entmod (subst (cons 1 NewAtt)(assoc 1 DD) DD))
                    )
                 )
             )
;            (princ "\nDD=")(princ DD)
             (setq CC (entnext CC) DD (entget CC))
          );while
       (setq n (1+ n))
   );while
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (setvar "CMDECHO" 1)
)
;;==================================================================================================
;;72-2.制御回路用ｼﾝﾎﾞﾙの属性値（N2:回路番号）をまとめて変更する。 N2.LSP  2003/SEP   By T.Terada
;;==================================================================================================
(defun C:N2(/ AA BB CC DD n NewAtt)
  (setq *error* *err1*)(setvar "CMDECHO" 0) 
  (setq n 0)
  (if (= (getvar "USERI5") 0)
    (progn
      (princ "\n接点の回路番号を変更します。　：")
      (setq NewAtt (getstring "回路番号を入力(例: -12)："))
      ;;(princ "\n変更するオブジェクトを指示:<ﾘﾀｰﾝ=全て>")
      ;;(if (null (setq AA (ssget (list '(66 . 1)))))
      ;;    (setq AA (ssget "x" (list '(66 . 1))))
      ;;)
      (princ "\n変更するオブジェクトを指示:")
      (if (null (setq AA (ssget (list '(66 . 1)))))
          (exit)
      )
    )
  )
  (if (= (getvar "USERI5") 1)
    (progn
      (setq NewAtt zoku_cnt)        ;回路番号入力（文字で設定 例："12"）
      (setq PC1(list PCX PCY))      ;選択範囲始点
      (setq PC2(list PCX2 PCY2))    ;   〃   終点
      ;(setq NewAtt "20")           ;回路番号入力（文字で設定 例："12"）
      ;(setq PC1(list 500 2500))    ;選択範囲始点
      ;(setq PC2(list 1340 1510))   ;   〃   終点
      (setq AA (ssget "W" PC1 PC2)) ;'(66 . 1)[INSERTされた属性付きのみをフィルタする]はいらない
    )
  )
  
   (while (setq BB (ssname AA n))
       (setq CC (entnext BB)  DD (entget CC))
          (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
             (if (= (cdr (assoc 0 DD)) "ATTRIB")
                 (progn
                    (if (= (cdr (assoc 2 DD)) "N2")
                        (entmod (subst (cons 1 NewAtt)(assoc 1 DD) DD))
                    )
                 )
             )
;            (princ "\nDD=")(princ DD)
             (setq CC (entnext CC) DD (entget CC))
          );while
       (setq n (1+ n))
   );while
   (command "REGEN");再作図をしないと画面上に変更が反映されない
   (setvar "CMDECHO" 1)
   (setvar "USERI5" 0)
)
;;==================================================================================================
;;72-3.画層変更
;;==================================================================================================
(defun C:CH-LA ( / W1 EN1 IND1 A1);;;CHLAでは不動作の為CH-LAに変更
   (setq *error* *err1*)(setvar "cmdecho" 0)
   (setq W1 nil) (setq EN1 nil) (setq IND1 0)
    
   (princ "\n画層を変更") (setq W1 (ssget))
   (cond ((/= W1 nil)
         (setq A1 (getstring "\n変更後の画層を指示："))
         (command "change" W1 "" "p" "LA" A1 "")
   ))
 (setvar "cmdecho" 1)
)
;;==================================================================================================
;;72-4.線種変更
;;==================================================================================================
(defun C:CHLT ( / EN1 IND1 A1 A2)
   (setq *error* *err1*)(setvar "cmdecho" 0)
   (setq W1 nil) (setq EN1 nil) (setq IND1 0)
    
   (princ "\n線種を変更") (setq W1 (ssget))
   (initget "1:実線 2:点線 3:一点鎖線")
   (setq A1 (getkword "\n線種: 実線/1  ,点線/2  ,一点鎖線/3, "))
   (cond ((= A1 "1:実線")    (setq A2 "continuous"))
         ((= A1 "2:点線")    (setq A2 "dashed"))
         ((= A1 "3:一点鎖線")(setq A2 "center"))
   )
   (command "change" W1 "" "p" "lt" A2 "")
   (setvar "cmdecho" 1)
)
;;====================================================================
;;72-5.色変更
;;====================================================================
(defun C:CHCO ( / W1 A1)
   (setq *error* *err1*)(setvar "cmdecho" 0)
    (princ "\n色を変更") (setq W1 (ssget))
   (cond ((/= W1 nil)
      (princ "1.赤 2.黄 3.緑 4.水 5.青 6.シアン 7.白")
      (setq A1 (getint "\n色を指定:"))
      (command "change" W1 "" "p" "c" A1 "")
   ))
  (setvar "cmdecho" 1)
)
;;====================================================================
;;72-6.既、記入文字サイズの変更   2020/7/20   既存文字サイズ取得を追加
;;====================================================================
(defun C:CHTS( / H1 EN1 IND1 A1 A2 LEN1 ENT1 COM1 T_SIZE)
    (setq *error* *err1*)
    (setvar "cmdecho" 1)
    (setq W1 nil) (setq EN1 nil) (setq IND1 0) (setq H1 nil)
;-----------------------------------------------------------------------      
    (setq H1 (ssget))
             (setq EN1 (ssname H1 0)) ;図形名を取得
             (setq EN2 (entget EN1))  ;図形の定義リストを取得
             (setq EN3 (assoc 40 EN2));リスト40のデーターを取得（文字高さ）
             (setq EN4 (cdr EN3))     ;リスト40の値を取得（文字高さ）
    (princ "\n文字サイズ変更！ 現在のサイズは：")(PRINC EN4)
;-----------------------------------------------------------------------       
    (cond ((/= H1 nil)
           (setq A1 (getint "\n新しい文字サイズを入力:"))
              (cond ((/= A1 nil)
                 (setq LEN1 (sslength H1))
                 (repeat LEN1
                     (setq EN1 (ssname H1 IND1))
                         (cond ((/= EN1 nil)
                               (setq ENT1 (entget EN1))
                               (setq COM1  (cdr (assoc 0 ENT1)))
                                  (cond ((or (= COM1 "TEXT") (= COM1 "ATTDEF"))
                                     (setq ENT1 (subst (cons 40 A1) (assoc 40 ENT1) ENT1))
                                     (entmod ENT1)
                         ))
                 ))
                 (setq EN1 nil) (setq IND1 (+ IND1 1))
             )))
    setvar "cmdecho" 1)
    )
)
;;====================================================================
;;72-7.既、記入寸法値の文字サイズを変更
;;====================================================================
(defun C:CHDIMTXT ()
 (SETVAR "CMDECHO" 0)
 (SETVAR "DIMTXSTY" "M0")
 (PROMPT "\n寸法値の文字サイズを変更します：") (SETQ W01 (SSGET))
    (COND ((/= W01 nil)
          (SETQ A01 (GETREAL "\n新しい文字サイズを入力："))
              (COND ((/= A01 nil)
                    (SETQ A01 (/ A01 80))
;(princ "\nA01=")(PRINC A01)(PROMPT "\n")
                    (COMMAND "DIMTXT" A01)
                    (COMMAND "DIM1" "UPDATE" W01 "")
              ))
    ))
 (setvar "cmdecho" 1)
)
;;==================================================================================================
;;72-8.一括文字サイズ変更     a) 既に書かれた文字列全体を、新しい文字列に置換する。
;;                            b) 文字の高さ・比率・角度・字体・起点は変わらない。
;;==================================================================================================
(defun  C:CHTXT ( / OSM E1 EN Do CN NSL EE1)
  (setq *error* *err1*)(setvar "CMDECHO" 0)
  (setq OSM (getvar "osmode"))
  (setvar "osmode" 0)
   (princ "\n選択した文字列すべてを、新しい文字列に置き換える")
   (princ "\n変更したい文字列を選択")
  (setq E1 (ssget)
        EN (sslength e1)
        Do -1
        CN  0 )
   (setq NSL (getstring T "\n変更後の文字列:"))
  (repeat EN
     (setq Do  (+ Do 1) EE1 (entget (ssname E1 Do)))
       (if (= (cdr (assoc 0 EE1)) "TEXT")
           (progn
             (setq EE1 (subst (cons 1 NSL) (assoc 1 EE1) EE1))
             (entmod ee1)
             (setq CN (+ CN 1))
           );progn
       );if
  );repeat
  (terpri);改行
  (princ CN)
  (princ "箇所変更    ")
  (setvar "osmode" osm)
  (setvar "CMDECHO" 1)
);defun
;;==================================================================================================
;;72-9.新規記入文字サイズの変更           CHTSZ.LSP   2003/Apr/19 
;;==================================================================================================
(defun C:CHTSZ ( / DMC MSZ)
    (setq *error* *err1*)(setvar "CMDECHO" 0)
    (setq DCL_ID (load_dialog "X:/DCL/CHTSZ.DCL"))
    (if (not (new_dialog "TXSZ" DCL_ID) )(EXIT)) 
    (setq DMC (getvar "dimscale"))
        (action_tile "cancel" "(done_dialog)(exit)")
                                   ;記入文字               ;寸法文字
        (action_tile "0" "(setvar \"TEXTSIZE\" 15)(setvar \"DIMTXT\" (/ 15.0 DMC))(setq MSZ \"15\")")
        (action_tile "1" "(setvar \"TEXTSIZE\" 20)(setvar \"DIMTXT\" (/ 20.0 DMC))(setq MSZ \"20\")")
        (action_tile "2" "(setvar \"TEXTSIZE\" 30)(setvar \"DIMTXT\" (/ 30.0 DMC))(setq MSZ \"30\")")
        (action_tile "3" "(setvar \"TEXTSIZE\" 40)(setvar \"DIMTXT\" (/ 40.0 DMC))(setq MSZ \"40\")")
        (action_tile "4" "(setvar \"TEXTSIZE\" 50)(setvar \"DIMTXT\" (/ 50.0 DMC))(setq MSZ \"50\")")
        (action_tile "5" "(setvar \"TEXTSIZE\" 60)(setvar \"DIMTXT\" (/ 60.0 DMC))(setq MSZ \"60\")")
        (action_tile "6" "(setvar \"TEXTSIZE\" 80)(setvar \"DIMTXT\" (/ 80.0 DMC))(setq MSZ \"80\")")
        (action_tile "7" "(setvar \"TEXTSIZE\" 100)(setvar \"DIMTXT\" (/ 100.0 DMC))(setq MSZ \"100\")")
        (action_tile "8" "(setvar \"TEXTSIZE\" 150)(setvar \"DIMTXT\" (/ 150.0 DMC))(setq MSZ \"150\")")
        (action_tile "9" "(setvar \"TEXTSIZE\" 200)(setvar \"DIMTXT\" (/ 200.0 DMC))(setq MSZ \"200\")")
        (action_tile "10" "(setvar \"TEXTSIZE\" 250)(setvar \"DIMTXT\" (/ 250.0 DMC))(setq MSZ \"250\")")
        (action_tile "11" "(setvar \"TEXTSIZE\" 300)(setvar \"DIMTXT\" (/ 300.0 DMC))(setq MSZ \"300\")")
        (start_dialog)(unload_dialog DCL_ID)(princ)
    (princ (strcat "\n記入文字寸法を <" MSZ "> に変更しました。"))
    (setvar "CMDECHO" 1)
)
;;==================================================================================================
;;72-X.スナップ値の変更           SNAP.LSP   2005/Jan/17 
;;==================================================================================================
(defun C:CHSNAP ( /); SS)
    (setq *error* *err1*)(setvar "CMDECHO" 0)
    (setq DCL_ID (load_dialog "X:/DCL/SNAP.DCL"))
    (if (not (new_dialog "SNAP" DCL_ID) )(exit)) 
        (action_tile "cancel" "(done_dialog)(exit)")
        (action_tile "S1"  "(setq SS 1)")
        (action_tile "S5"  "(setq SS 5)")
        (action_tile "S10" "(setq SS 10)")
        (action_tile "S25" "(setq SS 25)")
        (action_tile "S50" "(setq SS 50)")
        (action_tile "S100" "(setq SS 100)")
        (start_dialog)(unload_dialog DCL_ID)(princ)
    (command "SNAP" SS)(princ "\n")
    (princ (strcat "スナップを<" (itoa SS) ">に変更しました。"))
    (setvar "CMDECHO" 1)
)
;;==================================================================================================
;;73.図面を終了せずにEND-I           SAVE-i.LSP   2005/Jun/1 
;;==================================================================================================
(defun C:SAVE-I(/ DAT AA BB CC DD n Z1D Z2D F NAME01 NAME02 NAME03 NAME04 LN NEN DWG_NAME FNAME NDWG)
   (setq *error* *err1*)
   (setvar "cmdecho" 0)
   
   ;;先に全図面の保存をしておく
   (command "SAVEALL")
   
   ;;部品表属性ﾌｧｲﾙ出力 ----保存するファイル名の取得と設定----------------------------------------------
   (setq NDWG (length (mdi_get_all)))
   (while (>= NDWG 1)
      (setq NAME01 (getvar "dwgname"))
      (princ "\nDWGNAME =")(princ NAME01)
      
      (if (/= "Drawing" (substr NAME01 1 7))
         (progn
            (setq NEN (substr NAME01 1 1))
            (setq LN (strlen NAME01))
            (setq NAME01 (substr NAME01 3 (- LN 6)))
            (setq NAME02 (strcat "Z:" "\\BUHIN-I\\" NEN "\\" NAME01 ".TXT"))
            (setq NAME03 (strcat "Z:" "\\BUHIN-I\\" NEN "\\" NAME01 ".IPL"));IPL=Icad Parts List  部品表ファイル
            (setq NAME04 (strcat "X:/DDAT/" NEN "/" NAME01 ".DAT"))         ;外形情報ファイル
            ;;(princ "\nNAME01 =")(princ NAME01)
            ;;(princ "\nNAME02 =")(princ NAME02)
            ;;(princ "\nNAME03 =")(princ NAME03)

            (setq AA (ssget "x" (list '(66 . 1))))      ;属性の付いた図形だけ。"INSERT"ではダメ。
            (if (/= AA nil)
               (progn
                  ;;部品表ファイルの保管                                    ********.IPL IPL=Icad Parts List
                  (command "ATTEXT" "c" "X:\\LSP-i\\ATT-OUT.TXT" NAME02)
                  (princ "\n  部品表  書出完了...")
               )
               (progn
                  (setq F (open NAME02 "w"))
                  (close F)
               )
            )
            (dos_copy NAME02 NAME03)                                   ; DOS Lib
            (dos_delete NAME02)

            ;;回路図面から機器外形情報をファイルに書き出す。 D_OUT.LSP------------------------------------------
            ;;(setq DAT nil)(setq n 0)       ;;オブジェクトをDATに収納
            ;;(if (/= AA nil)
            ;;   (progn
            ;;      (while (setq BB (ssname AA n))
            ;;         (setq CC (entnext BB)  DD (entget CC))
            ;;         (setq Z1D nil  Z2D nil)
            ;;         (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
            ;;            (if (and (= (cdr (assoc 0 DD)) "ATTRIB")(= (cdr (assoc 2 DD)) "Z1"))
            ;;               (if (and (/= (cdr (assoc 2 DD)) "")(/= (cdr (assoc 1 DD)) ""))
            ;;                  (setq Z1D (cdr (assoc 1 DD)))
            ;;               )
            ;;            )
            ;;            (if (and (= (cdr (assoc 0 DD)) "ATTRIB")(= (cdr (assoc 2 DD)) "PNO"))
            ;;               (if (and (/= (cdr (assoc 2 DD)) "")(/= (cdr (assoc 1 DD)) ""))
            ;;                  (setq Z2D (cdr (assoc 1 DD)))
            ;;               )
            ;;            )
            ;;            (setq CC (entnext CC) DD (entget CC))
            ;;         );while
            ;;
            ;;         (if (or (= Z1D nil)(= Z2D nil))
            ;;            (princ)
            ;;            (setq DAT (append DAT (list (list Z1D Z2D))))
            ;;         )
            ;;         (setq n (1+ n))
            ;;      );while
            ;;
            ;;      ;;外形情報ファイルの書き込み
            ;;      (setq  F (open NAME04 "w")) (print DAT F) (close F)
            ;;      (command "REDRAW")
            ;;      (princ "\n  外形情報書出完了...")
            ;;   )
            ;;   (progn
            ;;      (setq F (open NAME04 "w"))
            ;;      (close F)
            ;;   )
            ;;)

            ;;図面保管 -----------------------------------------------------------------------------------------
            (if (= (getvar "USERI5") 0)                  ;Access自動作図のフラグ（初期値はOFF）
               (progn (setvar "CMDECHO" 1)(command "QSAVE"))
            )
            (if (= (getvar "USERI5") 1)                  ;Access自動作図のフラグ（初期値はOFF）
               (progn (setvar "CMDECHO" 1)(setvar "USERI5" 0)(command "QSAVE")) ;;QSAVEは最後にすること
            )
         )
      )
      
      (mdi_next)
      (setq NDWG (1- NDWG))
   );End While
   (setvar "cmdecho" 1)
)
;;==================================================================================================
;;74.回路図用コンセントの入力     CONS.LSP     2005/6/17 By T.Terada
;;==================================================================================================
(defun C:CONS ( / PC PC1 PNO)
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (DCL_CON1)
      (if (= DCL "CON2")(DCL_CON2))
      (if (= DCL "CON3")(DCL_CON3))
      (if (= DCL "CON4")(DCL_CON4))
      (if (= DCL "CON5")(DCL_CON5))
      (if (= DCL "CON6")(DCL_CON6))
       (setq PNO (getstring "\nコンセント番号を指示:(例：-1)"))
      (cond ((= PNO "") (setq PNO "-1")))(setq PNO (strcat "CON" PNO))
       (princ "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" "CON" PNO T1 "" T3 T4 "" "" "" "" "" "15" "1" Z1 "")
      (setq PC (getvar "LASTPOINT"))
      (if (or (= DCL "CON3")(= DCL "CON5"))
          (progn
             (setq PC1 (list (+ (car PC) 25)(- (cadr PC) 150)))
             (setq PC2 (list (+ (car PC) 25)(- (cadr PC) 175)))
             (setq PC11 (list (+ (car PC) 55)(- (cadr PC) 25)))
             (setq PC12 (list (+ (car PC) 55)(- (cadr PC) 50)))
             (command "COLOR" 2)
             (if (or (= T1 "WF1031E")(= T1 "WF3003E"))
               (progn
                  (command "INSERT" "ZOKU" PC11 "1" "1" "0" "CON" PNO T11 "" "" "" "" "" "" "" "" "15" "1" "" "")
                  (command "TEXT" "S" "M0" PC12  "20" "0" "新金プレート")
               )
               (progn ;else
                  (command "INSERT" "ZOKU" PC1 "1" "1" "0" "CON" PNO T11 "" "" "" "" "" "" "" "" "15" "1" "" "")
                  (command "TEXT" "S" "M0" PC2  "20" "0" "新金プレート")
               )
             )
          )
      )
      (command "COLOR" 4)
      (setq SYM nil Z1 nil T1 nil T11 nil T3 nil DCL nil)
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(REDRAW)(GC)
)
(defun DCL_CON1 (/);主メニュー
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON1" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq DCL \"CON2\")")
          (action_tile "2" "(setq DCL \"CON3\")")
          (action_tile "3" "(setq DCL \"CON4\")")
          (action_tile "4" "(setq DCL \"CON5\")")
          (action_tile "5" "(setq DCL \"CON6\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON2 ( / );露出コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON2" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "11" "(setq SYM \"SCON1\"  Z1 \"DCON2F015\"   T1 \"WK1012W\" T3 \"15A 125V\" T4 \"\")")
          (action_tile "12" "(setq SYM \"SCON1E\" Z1 \"DCON2FE015\"  T1 \"WK3001W\" T3 \"15A 125V\" T4 \"E付\")")
          (action_tile "13" "(setq SYM \"SCON2\"  Z1 \"DCON22F015\"  T1 \"WK1021W\" T3 \"15A 125V\" T4 \"\")")
          (action_tile "14" "(setq SYM \"SCON2E\" Z1 \"DCON23EF015\" T1 \"WK3064\"  T3 \"15A 125V\" T4 \"E付\")")
          (action_tile "15" "(setq SYM \"SCON2E\" Z1 \"DCON22EF015\" T1 \"WK3004W\" T3 \"15A 125V\" T4 \"E付\")")
          (action_tile "16" "(setq SYM \"SCON2E\" Z1 \"DCON22EF015\" T1 \"WK3005\"  T3 \"15A 125V\" T4 \"E付\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON3 ( / );埋込コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON3" DCL_ID) )(EXIT)) 
        (action_tile "cancel" "(done_dialog)(exit)")
        (action_tile "21" "(setq SYM \"SCON1\"  Z1 \"DCON11F015\" T1 \"WF1025EK\" T11 \"WN9513\" T3 \"15A 125V\" T4 \"\")")
        (action_tile "22" "(setq SYM \"SCON1E\" Z1 \"DCON11F015\" T1 \"WF3002EK\" T11 \"WN9513\" T3 \"15A 125V\" T4 \"E付\")")
        (action_tile "23" "(setq SYM \"SCON2\"  Z1 \"DCON12F015\" T1 \"WF1031E\"  T11 \"WF9531\" T3 \"15A 125V\" T4 \"\")")
        (action_tile "24" "(setq SYM \"SCON2E\" Z1 \"DCON12F015\" T1 \"WF3003E\"  T11 \"WF9531\" T3 \"15A 125V\" T4 \"E付\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON4 ( / );引掛・露出コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON4" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "31" "(setq SYM \"SCON1\"  Z1 \"DCONH21F015\" T1 \"WK2215\"  T3 \"15A 125V\" T4 \"\")")
          (action_tile "32" "(setq SYM \"SCON1\"  Z1 \"DCONH22F015\" T1 \"WK2220K\" T3 \"20A 250V\" T4 \"\")")
          (action_tile "33" "(setq SYM \"SCON3\"  Z1 \"DCONH31F015\" T1 \"WK2320K\" T3 \"20A 250V\" T4 \"\")")
          (action_tile "34" "(setq SYM \"SCON3\"  Z1 \"DCONH32F015\" T1 \"WK2331B\" T3 \"30A 250V\" T4 \"\")")
          (action_tile "35" "(setq SYM \"SCON1E\" Z1 \"DCONH33F015\" T1 \"WK2315K\" T3 \"15A 125V\" T4 \"E付\")")
          (action_tile "36" "(setq SYM \"SCON1E\" Z1 \"DCONH31F015\" T1 \"WK2520B\" T3 \"20A 250V\" T4 \"E付\")")
          (action_tile "37" "(setq SYM \"SCON1E\" Z1 \"DCONH32F015\" T1 \"WK2330\"  T3 \"30A 250V\" T4 \"E付\")")
          (action_tile "38" "(setq SYM \"SCON3E\" Z1 \"DCONH41F015\" T1 \"WK2420K\" T3 \"20A 250V\" T4 \"E付\")")
          (action_tile "39" "(setq SYM \"SCON3E\" Z1 \"DCONH42F015\" T1 \"WK2430\"  T3 \"30A 250V\" T4 \"E付\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON5 ( / );引掛・埋込コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON5" DCL_ID) )(EXIT)) 
        (action_tile "cancel" "(done_dialog)(exit)")
        (action_tile "41" "(setq SYM \"SCON1\"  Z1 \"DCON11F015\"  T1 \"WF2215BK\" T11 \"WN9513\" T3 \"15A 125V\" T4 \"\")")
        (action_tile "42" "(setq SYM \"SCON1\"  Z1 \"DCON11FE015\" T1 \"WF2220BK\" T11 \"WN9513\" T3 \"20A 250V\" T4 \"\")")
        (action_tile "43" "(setq SYM \"SCON1\"  Z1 \"DCON11F015\"  T1 \"WF2320BK\" T11 \"WN9513\" T3 \"20A 250V\" T4 \"\")")
        (action_tile "44" "(setq SYM \"SCON3\"  Z1 \"DCON11EF015\" T1 \"WF2331B\"  T11 \"WN9514\" T3 \"30A 250V\" T4 \"\")")
        (action_tile "45" "(setq SYM \"SCON3\"  Z1 \"DCON13EF015\" T1 \"WF2360B\"  T11 \"WN9516\" T3 \"60A 250V\" T4 \"\")")
        (action_tile "46" "(setq SYM \"SCON1E\" Z1 \"DCON11EF015\" T1 \"WF2315BK\" T11 \"WN9513\" T3 \"15A 125V\" T4 \"E付\")")
        (action_tile "47" "(setq SYM \"SCON1E\" Z1 \"DCON11EF015\" T1 \"WF2520B\"  T11 \"WN9513\" T3 \"20A 250V\" T4 \"E付\")")
        (action_tile "48" "(setq SYM \"SCON1E\" Z1 \"DCON11EF015\" T1 \"WF2330B\"  T11 \"WN9514\" T3 \"30A 250V\" T4 \"E付\")")
        (action_tile "49" "(setq SYM \"SCON3E\" Z1 \"DCON11EF015\" T1 \"WF2420BK\" T11 \"WN9514\" T3 \"20A 250V\" T4 \"E付\")")
        (action_tile "4X" "(setq SYM \"SCON3E\" Z1 \"DCON11EF015\" T1 \"WF2430B\"  T11 \"WN9514\" T3 \"30A 250V\" T4 \"E付\")")
        (action_tile "4Y" "(setq SYM \"SCON3E\" Z1 \"DCON13EF015\" T1 \"WF2460B\"  T11 \"WN9516\" T3 \"60A 250V\" T4 \"E付\")")
     (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON6 ( / );引掛・防雨コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON6" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "51" "(setq SYM \"SCON3\"  Z1 \"DCONH5F015\" T1 \"WK6320\" T3 \"20A 250V\" T4 \"\")")
          (action_tile "52" "(setq SYM \"SCON3\"  Z1 \"DCONH5F015\" T1 \"WK6331\" T3 \"30A 250V\" T4 \"\")")
          (action_tile "53" "(setq SYM \"SCON1E\" Z1 \"DCONH5F015\" T1 \"WK6315\" T3 \"15A 125V\" T4 \"E付\")")
          (action_tile "54" "(setq SYM \"SCON1E\" Z1 \"DCONH5F015\" T1 \"WK6520\" T3 \"20A 250V\" T4 \"E付\")")
          (action_tile "55" "(setq SYM \"SCON1E\" Z1 \"DCONH5F015\" T1 \"WK6330\" T3 \"30A 250V\" T4 \"E付\")")
          (action_tile "56" "(setq SYM \"SCON3E\" Z1 \"DCONH5F015\" T1 \"WK6420\" T3 \"20A 250V\" T4 \"E付\")")
          (action_tile "57" "(setq SYM \"SCON3E\" Z1 \"DCONH5F015\" T1 \"WK6430\" T3 \"30A 250V\" T4 \"E付\")")
       (start_dialog)(unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;;75.外形図用コンセントの入力     COND.LSP     2005/6/17 By T.Terada
;;==================================================================================================
(defun C:COND ( / )
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (DCL_CON1D)
      (if (= DCL "CON2")(DCL_CON2D))
      (if (= DCL "CON3")(DCL_CON3D))
      (if (= DCL "CON4")(DCL_CON4D))
      (if (= DCL "CON5")(DCL_CON5D))
      (if (= DCL "CON6")(DCL_CON6D))
       (princ "\n挿入点を指示")
      (command "INSERT" Z1 pause "1" "1" "0" "" "" "" "" "" "")
      (setq Z1 nil)
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(REDRAW)(GC)
)
(defun DCL_CON1D (/);主メニュー
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON1" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq DCL \"CON2\")")
          (action_tile "2" "(setq DCL \"CON3\")")
          (action_tile "3" "(setq DCL \"CON4\")")
          (action_tile "4" "(setq DCL \"CON5\")")
          (action_tile "5" "(setq DCL \"CON6\")")
       (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON2D ( / );露出コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON2" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "11" "(setq Z1 \"DCON2F015\")  ")
          (action_tile "12" "(setq Z1 \"DCON2EF015\") ")
          (action_tile "13" "(setq Z1 \"DCON22F015\") ")
          (action_tile "14" "(setq Z1 \"DCON23EF015\")")
          (action_tile "15" "(setq Z1 \"DCON22EF015\")")
          (action_tile "16" "(setq Z1 \"DCON22EF015\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON3D ( / );埋込コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON3" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "21" "(setq Z1 \"DCON11F015\")")
          (action_tile "22" "(setq Z1 \"DCON11F015\")")
          (action_tile "23" "(setq Z1 \"DCON12F015\")")
          (action_tile "24" "(setq Z1 \"DCON12F015\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON4D ( / );引掛・露出コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON4" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "31" "(setq Z1 \"DCONH21F015\")")
          (action_tile "32" "(setq Z1 \"DCONH22F015\")")
          (action_tile "33" "(setq Z1 \"DCONH31F015\")")
          (action_tile "34" "(setq Z1 \"DCONH32F015\")")
          (action_tile "35" "(setq Z1 \"DCONH33F015\")")
          (action_tile "36" "(setq Z1 \"DCONH31F015\")")
          (action_tile "37" "(setq Z1 \"DCONH32F015\")")
          (action_tile "38" "(setq Z1 \"DCONH41F015\")")
          (action_tile "39" "(setq Z1 \"DCONH42F015\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON5D ( / );引掛・埋込コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON5" DCL_ID) )(EXIT)) 
        (action_tile "cancel" "(done_dialog)(exit)")
        (action_tile "41" "(setq Z1 \"DCON11F015\" )")
        (action_tile "42" "(setq Z1 \"DCON11F015\")")
        (action_tile "43" "(setq Z1 \"DCON11F015\" )")
        (action_tile "44" "(setq Z1 \"DCON11F015\")")
        (action_tile "45" "(setq Z1 \"DCON13F015\")")
        (action_tile "46" "(setq Z1 \"DCON11F015\")")
        (action_tile "47" "(setq Z1 \"DCON11F015\")")
        (action_tile "48" "(setq Z1 \"DCON11F015\")")
        (action_tile "49" "(setq Z1 \"DCON11F015\")")
        (action_tile "4X" "(setq Z1 \"DCON11F015\")")
        (action_tile "4Y" "(setq Z1 \"DCON13F015\")")
     (start_dialog)(unload_dialog DCL_ID)(princ)
)
(defun DCL_CON6D ( / );引掛・防雨コンセント
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "CON6" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "51" "(setq Z1 \"DCONH5F015\")")
          (action_tile "52" "(setq Z1 \"DCONH5F015\")")
          (action_tile "53" "(setq Z1 \"DCONH5F015\")")
          (action_tile "54" "(setq Z1 \"DCONH5F015\")")
          (action_tile "55" "(setq Z1 \"DCONH5F015\")")
          (action_tile "56" "(setq Z1 \"DCONH5F015\")")
          (action_tile "57" "(setq Z1 \"DCONH5F015\")")
       (start_dialog)(unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;;76.架台の上面図を作図    架台.LSP        2005/JUN/25     BY T.Terada
;;                                 2009/7/21          北野
;;==================================================================================================
(defun C:KADAI( / L G LL PC PC2 BW BD BH LU1 RU1 LD1 RD1 LU2 RD2 LU3 RD3 LUN RUN LDN RDN VDIM HDIM 
                  SUN1 SUN2 SUN3 SUN4 SUN5 SUN6 SUN7 SUN8 SUN9 TYU1 TYU2 PLL WARI WARI1 WARI2 CD 
                  WARI3 WARI4 WARI5 WARI6 WARI7 WARI8 MOJIS TETUMLT TETUMLY TETUMRY TETUML
                  TETUMR WARI9 BISUY BISUM TETUMO TOMEM BISUT WARI10 HIKI HIKI1 TETUMCC TETUMCL1
                  TETUMCL2 TETUMCR1 TETUMCR2 TYUL1 TYUL2 TYUR1 TYUR2 TETU ITAL1 ITAL2 ITAL3 
                  ITAL4 ITAR1 ITAR2 ITAR3 ITAR4  ITAS1 ITAS2 ITAS3 ITAS4 BISUSLU BISUSLD 
                  BISUSRU BISUSRD BISUMLU BISUMLD BISUMRU BISUMRD BISUMLU1 BISUMLD1 BISUMRU1 
                  BISUMRD1 BISUMRC BISUMLC LWARI TETUCWARI PLTETU TETUBCLU TETUBCLD TETUBCRU
                  TETUBCRD)

;;変数の初期化
(setq L "")(setq G "")(setq PC "")(setq PC2 "")(setq BW "")(setq BD "")(setq BH "")(setq LU1 "")
(setq LL "")(setq RU1 "")(setq RD1 "")(setq LD1 "")(setq RD1 "")(setq LU2 "")(setq RD2 "")
(setq LU3 "")(setq RD3 "")(setq LUN "")(setq RUN "")(setq RUN "")(setq LDN "")(setq RDN "")(setq VDIM "")
(setq HDIM "")(setq SUN1 "")(setq SUN2 "")(setq SUN3 "")(setq SUN4 "")(setq SUN5 "")(setq SUN6 "")
(setq SUN7 "")(setq SUN8 "")(setq SUN9 "")(setq PLL "")(setq WARI "")(setq WARI1 "")(setq WARI2 "")
(setq WARI3 "")(setq WARI4 "")(setq WARI4 "")(setq WARI5 "")(setq WARI6 "")(setq WARI7 "")(setq CD "")
(setq WARI8 "")(setq MOJIS "")(setq TETUMLT "")(setq TETUMLY "")(setq TETUMRY "")(setq TETUML "")
(setq TETUMR "")(setq WARI9 "")(setq BISUY "")(setq BISUM "")(setq TETUMO "")(setq TOMEM "")
(setq BISUT "")(setq WARI10 "")(setq HIKI "")(setq HIKI1 "")(setq TETUMCC "")
(setq TYU1 "")(setq TYU2 "")(setq TYUL1 "")(setq TYUL2 "")(setq TYUR1 "")(setq TYUR2 "")(setq TETU "")
(setq ITAL1 "")(setq ITAL2 "")(setq ITAL3 "")(setq ITAL4 "")(setq ITAR1 "")(setq ITAR2 "")
(setq ITAR3 "")(setq ITAR4 "")(setq ITAS1 "")(setq ITAS2 "")(setq ITAS3 "")(setq ITAS4 "")
(setq BISUSLU "")(setq BISUSLD "")(setq BISUSRU "")(setq BISUSRD "")(setq BISMLU "")(setq BISUMLD "")
(setq BISUMRU "")(setq BISUMRU "")(setq BISUMLU1 "")(setq BISUMLD1 "")(setq BISUMLD1 "")(setq BISUMRU1 "")
(setq BISUMRD1 "")(setq BISUMRC "")(setq BISUMLC "")(setq LWARI "")(setq TETUMCL1 "")(setq TETUMCL2 "")
(setq TETUMCR1 "")(setq TETUMCR2 "")(setq TETUCWARI "")(setq PLTETU "")(setq TETUBCLU "")(setq TETUBCLD "")
(setq TETUBCRU "")(setq TETUBCRD "")

;初期設定                                          
;   (setq *error* *err1*)
   (setvar "CMDECHO" 1) 
;;;   (command "COLOR" 4)(setvar "PLINEWID" 0)  ;線幅を0
;;;   (command "LINETYPE" "S" "CONTINUOUS" "");;;;;(command "LAYER" "S" "BOX" "");画層 BOXに変更
;;---------------------------------------------------------------------------------------------------
   (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "KADAI" DCL_ID) )(exit))
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "L45" "(setq L 50)(setq G 30)(setq LL \"L4 X 50\")")
          (action_tile "L54" "(setq L 40)(setq G 25)(setq LL \"L5 X 40\")")
          (action_tile "L65" "(setq L 50)(setq G 30)(setq LL \"L6 X 50\")")
   (start_dialog) (unload_dialog DCL_ID) (princ)
   
   (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "KADAIT" DCL_ID) )(exit))
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "Y" "(setq TETU \"Y\")")
          (action_tile "N" "(setq TETU \"N\")")
   (start_dialog) (unload_dialog DCL_ID) (princ)

    (setq PC (getpoint "\n挿入点を指示して下さい．(上端の中心点):"))
    (setq BW (getreal  "\n盤幅を指示してください．:"))
    (setq BH (getreal  "\n高さを指示してください．:"))
    (setq BD (getreal  "\n奥行を指示してください．:"))
;;------------------------------------------------------------------------------------------------------------
;架台の正面図
    (setq LU1 (list (- (car PC) (/ BW 2)) (cadr PC)))        ;左上端
    (setq RU1 (list (+ (car PC) (/ BW 2)) (cadr PC)))        ;右上端
    (setq LD1 (list (- (car PC) (/ BW 2)) (- (cadr PC) BH))) ;左下端
    (setq RD1 (list (+ (car PC) (/ BW 2)) (- (cadr PC) BH))) ;右下端
    (setq LU2 (list (+ (car LU1) 5) (- (cadr LU1) 5)))       ;右下端 アングルの点線
    (setq RD2 (list (- (car RD1) 5) (+ (cadr RD1) 5)))       ;右下端 アングルの点線
    (setq LU3 (list (+ (car LU1) L) (- (cadr LU1) L)))       ;右下端 アングルの幅
    (setq RD3 (list (- (car RD1) L) (+ (cadr RD1) L)))       ;右下端 アングルの幅
    (setq LUN (list (+ (car LU1) 100) (cadr LU1)))           ;左上端 ネジ位置
    (setq RUN (list (- (car RU1) 100) (cadr RU1)))           ;右上端 ネジ位置
    (setq LDN (list (+ (car LD1) 100) (cadr LD1)))           ;左下端 ネジ位置
    (setq RDN (list (- (car RD1) 100) (cadr RD1)))           ;右下端 ネジ位置
    (setq PC2 (list (car PC) (- (cadr PC) L)))               ;OFFSET 内側
    (setq CD (list (car PC)(cadr LD1)))
    (setq VDIM (list (- (car LU1) 100)(cadr LU1)))           
    (setq HDIM (list (car LD1)(- (cadr LD1) 100)))
    (setq SUN1 (list (+ (car LUN) 100)(+ (cadr LUN) 75)))    
    (setq SUN2 (list (+ (car LUN) 120)(+ (cadr LUN) 80)))
    (setq SUN3 (list (+ (car SUN1) 125)(cadr SUN1)))
    ;(setq SUN4 (list (+ (car LDN) 100)(+ (cadr LDN) 75)))
    (setq SUN4 (list (+ (car LDN) 100)(- (cadr LDN) 75)))
    ;(setq SUN5 (list (+ (car LDN) 120)(+ (cadr LDN) 70)))
    (setq SUN5 (list (+ (car LDN) 120)(- (cadr LDN) 70)))
    (setq SUN6 (list (+ (car SUN4) 250)(cadr SUN4)))
    (setq SUN7 (list (+ (car RDN) 105)(- (cadr RDN) 75)))
    (setq SUN8 (list (+ (car RDN) 300)(cadr SUN7)))
    (setq SUN9 (list (+ (car RDN) 125)(- (cadr RDN) 70)))
    (setq LWARI (/ L 2))
    (setq TYU1 (list (car PC)(cadr LU3)))
    (setq TYU2 (list (car PC)(cadr RD3)))
    (setq TYUL1 (list (- (car PC) LWARI)(cadr LU3)))
    (setq TYUL2 (list (- (car PC) LWARI)(cadr RD3)))
    (setq TYUR1 (list (+ (car PC) LWARI)(cadr LU3)))
    (setq TYUR2 (list (+ (car PC) LWARI)(cadr RD3)))
    (setq MOJIS 30)
    (setq WARI (- (cadr RD3)(cadr RD2)))
    (princ "\nWARI=")(princ WARI)
    (setq WARI1 (- WARI 30))
    ;(princ "\nSUN1=")(princ SUN1)
    ;(princ "\nSUN2=")(princ SUN2)
    ;(princ "\nSUN3=")(princ SUN3)
    ;(princ "\nSUN4=")(princ SUN4)
    ;(princ "\nSUN5=")(princ SUN5)
    ;(princ "\nSUN6=")(princ SUN6)
    ;(princ "\nSUN7=")(princ SUN7)
    ;(princ "\nSUN8=")(princ SUN8)
    ;(princ "\nSUN9=")(princ SUN9)

    (if (< WARI1 10)
          (progn
             (setq MOJIS 25)
             (setq WARI1 (- WARI 25))
          );progn
    );if
    (setq WARI2 (/ WARI1 2))
    ;(princ "\nWARI1=")(princ WARI1)
    ;(princ "\nWARI2=")(princ WARI2)
    ;(setq PLL  (list (car PC) (- (cadr PC) 125)))
    (setq PLL  (list (car PC) (+ (cadr RD2 ) WARI2)))
    ;(command "RECTANGLE" LU1 RD1)(command "LINETYPE" "S" "DASHED" "")
    ;(princ "\nLU1=")(princ LU1)
    ;(princ "\nRD1=")(princ RD1)
    ;(princ "\nLU2=")(princ LU2)
    ;(princ "\nRD2=")(princ RD2)
    ;(princ "\nLU3=")(princ LU3)
    ;(princ "\nRD3=")(princ RD3)
    ;(princ "\nPC=")(princ PC)
    ;(princ "\nVDIM=")(princ VDIM)
    ;(princ "\nLDN=")(princ LDN)
    ;(princ "\nRDN=")(princ RDN)
    ;(princ "\nRD1=")(princ RD1)
    ;(princ "\n=HDIM")(princ HDIM )

    (command "RECTANGLE" LU1 RD1)
    (command "LINETYPE" "S" "DASHED" "")
    (command "RECTANGLE" LU2 RD2)
    (command "LINETYPE" "S" "CONTINUOUS" "")
    ;(command "RECTANGLE" LU3 RD3)
    (command "DIM" "VER" LU1 LD1 VDIM ""
                   "HOR" LDN LD1 HDIM ""
                   "HOR" RDN RD1 HDIM "" "EXIT")
   
    (if (>= BW 1000)
        (progn
           (command "DIM" "HOR" LDN CD HDIM ""
                          "HOR" CD RDN HDIM "" "EXIT")
        );progn
        (progn
           (command "DIM" "HOR" LDN RDN HDIM "" "EXIT")
        );progn
    );if

    ;(command "INSERT" "REN" LUN "1" "1" "0")
    (command "INSERT" "REN" LDN "1" "1" "0")
    ;(command "INSERT" "REN" RUN "1" "1" "0")
    (command "INSERT" "REN" RDN "1" "1" "0")
    ;(command "PLINE" LUN SUN1 SUN3 "")
    ;(command "PLINE" LDN SUN4 SUN6 "")
    (command "PLINE" RDN SUN7 SUN8 "")
    ;(princ "\nTETU=")(princ TETU)

    (setq TETUCWARI (/ 5 2))
    ;(princ "\nTETUCWARI=")(princ TETUCWARI)
    (setq TETUMCL1 (list (- (car PC) 2.5)(cadr LU2)))
    (setq TETUMCL2 (list (- (car PC) 2.5)(cadr RD2)))   
    (setq TETUMCR1 (list (+ (car PC) 2.5)(cadr LU2)))
    (setq TETUMCR2 (list (+ (car PC) 2.5)(cadr RD2)))
    ;(setq PLLL (list (car TETUML)(cadr PLL)))
    ;(princ "\nTETUML=")(princ TETUML)
    ;(princ "\nPLL=")(princ PLL)
    ;(princ "\nPLLL=")(princ PLLL)


    (if (>= BW 1000)
          (progn
             (command "INSERT" "REN" CD "1" "1" "0")
          );progn
    );if    

    (if (and (= TETU "N")(< BW 1000))
          (progn
             (command "LINETYPE" "S" "DASHED" "")
             (command "RECTANGLE" LU2 RD2)
             ;(command "LINE" TYU1 TYU2 "")
             (command "LINETYPE" "S" "CONTINUOUS" "")
             (command "RECTANGLE" LU3 RD3)
             ;(command "RECTANGLE" TYUR1 RD3)
             ;(command "LINE" TYUL1 TYUL2 "")
             ;(command "LINE" TYUR1 TYUR2 "")
          );progn
    );if
    
   (if (and (= TETU "N")(>= BW 1000))
          (progn
             (command "LINETYPE" "S" "DASHED" "")
             (command "RECTANGLE" LU2 RD2)
             (command "LINETYPE" "S" "CONTINUOUS" "")
             (command "RECTANGLE" LU3 TYUL2)
             (command "RECTANGLE" TYUR1 RD3)
             ;(princ "\nTYUL2=")(princ TYUL2)
             ;(princ "\nTYUR1=")(princ TYUR1)
          );progn
   );if

    

    (if (and (= TETU "Y")(< BW 1000))
          (progn
             ;(command "LINE" TYUL1 TYUL2 "")     
             ;(command "LINE" TYUR1 TYUR2 "")
             (command "RECTANGLE" LU2 RD2)
             (command "LINETYPE" "S" "DASHED" "")
             (command "RECTANGLE" LU3 RD3)
             ;(command "RECTANGLE" TYUR1 RD3)
             (command "LINETYPE" "S" "CONTINUOUS" "")
          );progn
    );if

    (if (and (= TETU "Y")(>= BW 1000))
          (progn
             ;(command "LINE" TYUL1 TYUL2 "")     
             ;(command "LINE" TYUR1 TYUR2 "")
             ;(command "RECTANGLE" LU2 RD2)
             (command "RECTANGLE" LU2 TETUMCL2)
             (command "RECTANGLE" TETUMCR1 RD2)
             (command "LINETYPE" "S" "DASHED" "")
             (command "RECTANGLE" LU3 TYUL2)
             (command "RECTANGLE" TYUR1 RD3)
             ;(command "RECTANGLE" TYUR1 RD3)
             (command "LINETYPE" "S" "CONTINUOUS" "")
          );progn
    );if

    
    (setq WARI3 (- (cadr LU3)(cadr RD3)))
    (setq WARI4 (/ WARI3 2))
    (setq WARI5 (- (car TYUL1)(car LU3)))
    (setq WARI6 (/ WARI5 2))
    (setq WARI7 (- (car RD3)(car TYUR1)))
    (setq WARI8 (/ WARI7 2)) 
    ;(princ "\nWARI8=")(princ WARI8)    
    ;(princ "\nWARI4=")(princ WARI4)

    (setq TETUMLT (+ (cadr RD3) WARI4))
    ;(princ "\nTETUMLT=")(princ TETUMLT)
    ;(princ "\nWARI7=")(princ WARI7) 
    (setq TETUMLY (+ (car LU3) WARI6))
    (setq TETUMRY (+ (car TYUR1) WARI8))
    ;(princ "\nTETUMLY=")(princ TETUMLY)
    ;(princ "\nTETUMRY=")(princ TETUMRY)
    (setq TETUML (list TETUMLY TETUMLT))
    ;(princ "\nTETUML=")(princ TETUML)
    (setq TETUMR (list TETUMRY TETUMLT))
    ;(princ "\nTETUML=")(princ TETUML)
    ;(princ "\nTETUMR=")(princ TETUMR)
    (setq TETUMCC (list (car PC) TETUMLT))
    (setq ITAL1 (list (car LU1)(- (cadr LU1) 5)))
    (setq ITAL2 (list (- (car LU1) 5)(cadr ITAL1)))
    (setq ITAL3 (list (car ITAL2)(+ (cadr RD1) 5)))
    (setq ITAL4 (list (car ITAL1)(cadr ITAL3)))
    ;(princ "\nITAL1=")(princ ITAL1)
    ;(princ "\nITAL2=")(princ ITAL2)
    ;(princ "\nITAL3=")(princ ITAL3)
    ;(princ "\nITAL4=")(princ ITAL4)
    (setq ITAR1 (list (car RD1)(- (cadr LU1) 5)))
    (setq ITAR2 (list (+ (car RD1) 5)(cadr ITAR1)))
    (setq ITAR3 (list (car ITAR2)(+ (cadr RD1) 5)))
    (setq ITAR4 (list (car ITAR1)(cadr ITAR3)))

    (setq BISUMLU (list (+ (car LU1) 15)(- (cadr LU1) 15)))
    (setq BISUMLD (list (car BISUMLU)(+ (cadr RD1) 15)))
    (setq BISUMRU (list (- (car RD1) 15)(cadr BISUMLU)))
    (setq BISUMRD (list (car BISUMRU)(cadr BISUMLD)))
    (setq BISUMLU1 (list (car TETUML)(cadr BISUMLU)))
    (setq BISUMLD1 (list (car BISUMLU1)(cadr BISUMLD)))
    (setq BISUMRU1 (list (car TETUMR)(cadr BISUMRU)))
    (setq BISUMRD1 (list (car BISUMRU1)(cadr BISUMRD)))
    (setq HIKI (- (cadr BISUMLU)(cadr BISUMLD)))
    (setq HIKI1 (/ HIKI 2))
    (setq BISUMLC (list (car BISUMLU)(+ (cadr BISUMLD) HIKI1)))
    (setq BISUMRC (list (car BISUMRU)(cadr BISUMLC)))
    ;(princ "\nBISUMLC=")(princ BISUMLC)
    (setq PLTETU (list (car TETUML)(cadr PLL)))   

    (if (= TETU "Y")
         (progn
            (command "COLOR" 2)
            ;(command "TEXT" "C" TETUML "20" "0" "鉄板ビス止め")
            ;(command "TEXT" "C" TETUMR "20" "0" "鉄板ビス止め")
            (command "INSERT" "KOUTEN" BISUMLU "1" "1" "0")
            (command "INSERT" "KOUTEN" BISUMLD "1" "1" "0")
            (command "INSERT" "KOUTEN" BISUMRU "1" "1" "0")
            (command "INSERT" "KOUTEN" BISUMRD "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMRD "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMRD1 "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMLU1 "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMLD1 "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMRU1 "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMRD1 "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMLC "1" "1" "0")
            (command "COLOR" 4)
            ;(command "LINE" ITAL1 ITAL2 ITAL3 ITAL4 "")
            ;(command "LINE" ITAR1 ITAR2 ITAR3 ITAR4 "")
         );progn
    );if 
    
    (if (and (= TETU "Y")(>= BW 1000))
         (progn
            (command "COLOR" 2)
            (command "TEXT" "C" TETUML "20" "0" "鉄板ビス止め")
            (command "TEXT" "C" TETUMR "20" "0" "鉄板ビス止め")
            (command "COLOR" 4)
         );progn
    );if
    
    (if (and (= TETU "Y")(< BW 1000))
         (progn
            (command "COLOR" 2)
            (command "TEXT" "C" TETUMCC "20" "0" "鉄板ビス止め")
            (command "COLOR" 4)
         );progn
    );if
            

    ;(if (and (= TETU "Y")(>= BH 500))
         ;(progn
            ;(command "INSERT" "KOUTEN" BISUMLC "1" "1" "0")
            ;(command "INSERT" "KOUTEN" BISUMRC "1" "1" "0")
         ;);progn
    ;);if

    ;(princ "\nLUN=")(princ LUN)
    ;(princ "\nSUN1=")(princ SUN1)
    ;(princ "\nSUN2=")(princ SUN2)
    ;(princ "\nSUN3=")(princ SUN3)
    ;(princ "\nSUN4=")(princ SUN4)
    ;(princ "\nSUN5=")(princ SUN5)
    ;(princ "\nSUN6=")(princ SUN6)

    (setq TETUBCLU (list (- (car TETUMCL1) 10)(cadr BISUMLU)))
    (setq TETUBCLD (list (- (car TETUMCL2) 10)(cadr BISUMLD)))
    (setq TETUBCRU (list (+ (car TETUMCR1) 10)(cadr BISUMRU)))
    (setq TETUBCRD (list (+ (car TETUMCR2) 10)(cadr BISUMRD)))
    ;(princ "\nTETUMCL=")(princ TETUMCL)
    ;(princ "\nBISUMLU=")(princ BISUMLU)
   
    (command "COLOR" 2)
    ;(command "TEXT" "S" "M0" SUN2 "20" "0" "4-14φ")
    ;(command "TEXT" "S" "M0" SUN5 "20" "0" "ｱﾝｶｰ孔4-φ17")
    (if (and (= TETU "N")(>= BW 1000))
         (progn
            (command "TEXT" "S" "M0" SUN9 "20" "0" "ｱﾝｶｰ孔6-φ17")
            (command "TEXT" "C" PLL MOJIS "0" LL)(command "COLOR" 4)
            ;(command "TEXTSIZE" "20")
         );progn
    );if

    (if (< BW 1000)
         (progn
            (command "TEXT" "S" "M0" SUN9 "20" "0" "ｱﾝｶｰ孔4-φ17")
            (command "TEXT" "C" PLL MOJIS "0" LL)(command "COLOR" 4)
            ;(command "TEXTSIZE" "20")
         );progn
    );if
    
    (if (and (= TETU "Y")(>= BW 1000))
         (progn
            ;(command "TEXT" "C" PLL "30" "0" (strcat "ｱﾝｸﾞﾙ：" LL))(command "COLOR" 4)
            (command "TEXT" "C" PLTETU MOJIS "0" LL)
            (command "COLOR" 2)
            (command "TEXT" "S" "M0" SUN9 "20" "0" "ｱﾝｶｰ孔6-φ17")
            (command "COLOR" 4)
            (command "INSERT" "KOUTEN" TETUBCLU "1" "1" "0")
            (command "INSERT" "KOUTEN" TETUBCLD "1" "1" "0")
            (command "INSERT" "KOUTEN" TETUBCRU "1" "1" "0")
            (command "INSERT" "KOUTEN" TETUBCRD "1" "1" "0")
            ;(command "TEXTSIZE" "20")
         );progn
    );if
    
    (command "TEXTSIZE" "20")

;;--------------------------------------------------------------------------------------------------
;架台の側面図
    (setq PC   (list (+ (car PC) 1000)(cadr PC)))
    (setq LU1 (list (- (car PC) (/ BD 2)) (cadr PC)))        ;左上端
    (setq RU1 (list (+ (car PC) (/ BD 2)) (cadr PC)))        ;右上端
    (setq LD1 (list (- (car PC) (/ BD 2)) (- (cadr PC) BH))) ;左下端
    (setq RD1 (list (+ (car PC) (/ BD 2)) (- (cadr PC) BH))) ;右下端
    (setq LU2 (list (+ (car LU1) 5) (- (cadr LU1) 5)))       ;右下端 アングルの点線
    (setq RD2 (list (- (car RD1) 5) (+ (cadr RD1) 5)))       ;右下端 アングルの点線
    (setq LU3 (list (+ (car LU1) L) (- (cadr LU1) L)))       ;右下端 アングルの幅
    (setq RD3 (list (- (car RD1) L) (+ (cadr RD1) L)))       ;右下端 アングルの幅
    (setq LUN (list (+ (car LU1) G) (cadr LU1)))             ;左上端 ネジ位置
    (setq RUN (list (- (car RU1) G) (cadr RU1)))             ;右上端 ネジ位置
    (setq LDN (list (+ (car LD1) G) (cadr LD1)))             ;左下端 ネジ位置
    (setq RDN (list (- (car RD1) G) (cadr RD1)))             ;右下端 ネジ位置
    (setq PC2 (list (car PC) (- (cadr PC) L)))               ;OFFSET 内側
    (setq VDIM (list (- (car LU1) 100)(cadr LU1)))
    (setq HDIM (list (car LD1)(- (cadr LD1) 100)))
    ;(princ "\nRD3=")(princ RD3)
    ;(princ "\nLD3=")(princ LD3)
    (setq WARI9 (- (cadr LU3)(cadr RD3)))
    (setq WARI10 (/ WARI9 2))
    ;(princ "\nWARI9=")(princ WARI10)
    (setq BISUT (+ (cadr RD3) WARI10))
    ;(princ "\nBISUT=")(princ BISUT)
    (setq BISUM (list (car PC) BISUT))
    (setq TETUMO (list (car PC)(+ BISUT 30)))
    (setq TOMEM (list (car PC)(- BISUT 30)))
    ;(princ "\nBISUM=")(princ BISUM)
    ;(princ "\nTETUMO=")(princ TETUMO)
    ;(princ "\nTOMEM=")(princ TOMEM)
    
    (setq ITAS1 (list (car LU1)(- (cadr LU1) 5)))
    (setq ITAS2 (list (- (car LU1) 5)(cadr ITAS1)))
    (setq ITAS3 (list (car ITAS2)(+ (cadr RD1) 5)))
    (setq ITAS4 (list (car ITAS1)(cadr ITAS3)))

    (setq BISUSLU (list (+ (car LU1) 15)(- (cadr LU1) 15)))
    (setq BISUSLD (list (car BISUSLU)(+ (cadr RD1) 15)))
    (setq BISUSRU (list (- (car RD1) 15)(cadr BISUSLU)))
    (setq BISUSRD (list (car BISUSRU)(cadr BISUSLD)))

    (command "RECTANGLE" LU1 RD1)

    (if (= TETU "Y")
         (progn
            (command "RECTANGLE" LU2 RD2)
            (command "LINETYPE" "S" "DASHED" "")
            (command "RECTANGLE" LU3 RD3)
            (command "LINETYPE" "S" "CONTINUOUS" "")
         );progn
         (progn
            (command "LINETYPE" "S" "DASHED" "")
            (command "RECTANGLE" LU2 RD2)
            (command "LINETYPE" "S" "CONTINUOUS" "")
            (command "RECTANGLE" LU3 RD3)
         );progn
    );if 
    
    ;(command "DIM" "VER" LU1 LD1 VDIM ""
    ;               "HOR" LDN LD1 HDIM ""
    ;               "HOR" LDN RDN HDIM ""
    ;               "HOR" RDN RD1 HDIM "" "EXIT")
    (command "DIM" "HOR" LDN LD1 HDIM ""
                   "HOR" LDN RDN HDIM ""
                   "HOR" RDN RD1 HDIM "" "EXIT")
    (princ "\nTETU=")(princ TETU)
    (if (= TETU "Y")
         (progn
             (command "COLOR" 2)
             (command "TEXT" "C" TETUMO "20" "0" "鉄板")
             ;(princ "\nTETU@=")(princ TETU)
             (command "TEXT" "C" BISUM "20" "0" "ビス")
             (command "TEXT" "C" TOMEM "20" "0" "止め")
             (command "INSERT" "KOUTEN" BISUSLU "1" "1" "0")
             (command "INSERT" "KOUTEN" BISUSLD "1" "1" "0")
             (command "INSERT" "KOUTEN" BISUSRU "1" "1" "0")
             (command "INSERT" "KOUTEN" BISUSRD "1" "1" "0")
             (command "COLOR" 4)
             ;(command "LINE" ITAS1 ITAS2 ITAS3 ITAS4 "")
         );progn
    );if

    ;(command "INSERT" "REN" LUN "1" "1" "0")
    (command "INSERT" "REN" LDN "1" "1" "0")
    ;(command "INSERT" "REN" RUN "1" "1" "0")
    (command "INSERT" "REN" RDN "1" "1" "0")
;;--------------------------------------------------------------------------------------------------
;;終了処理
   (command "LAYER" "S" "S1" "")(setvar "PLINEWID" 0)(command "COLOR" 4)(setvar "USERS5" "M")
   (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(princ "\n箱体外形図、作画終了！")(redraw)
)
;;==================================================================================================
;;77.文字整列                    TXRAY.LSP
;;==================================================================================================
(defun c:txray( / cmd sw1 sw2 pit pit_flag ss n ename elsit yiti xiti res i alist tt n_iti
                 code10 code11 code72 code73 xsa ysa new_xiti new_yiti new_x new_y
                 new10 new11 code10flag loop temp str)
  (setq pit_flag T)
  (princ "\n整列方向を入力して下さい: ")
  (initget 0 "Vertical Horizontal")
  (setq res (getkword "\nV=垂直整列(Y軸方向) /H=水平整列(X軸方向) <Enter=垂直>: "))
  (if (= res nil)(setq res "Tate"))
  (if res
    (progn (princ "\n対象文字を選んで下さい: ")(setq ss (ssget '((0 . "TEXT")))));変更する文字の選択
  )
  (if ss
    (progn
      (if (= res "Horizontal")
        (setq cmd "\n水平整列(X軸方向)")
        (setq cmd "\n垂直整列(Y軸方向)")
      )
      (setq str (getstring (strcat cmd "の間隔を入力して下さい <R=参照 Enter=現在位置>: ")))
      (if (/= str "")
        (if (or (= str "R")(= str "r"))
          (setq pit 0)
          (setq pit (atof str))
        )
        (setq pit_flag nil)
      )
     ;(setq pit (getreal (strcat cmd "の間隔を入力して下さい <0= 2点指示 Enter=現在位置>: ")))
      (if (and pit_flag (= pit 0))
        (setq pit (getdist "\n参照長さを2点で指示して下さい: ")) 
      )
    )
  )
  (if ss
    (progn
      ;基準文字位置取得-------------------------
      (setq loop T)
      (while loop
        (princ "\n基準文字を選んで下さい: ")
        (setq ename (car (entsel)))
        (if (/= ename nil)
          (progn
            (setq elist (entget ename))
            (if (=(cdr (assoc 0 elist)) "TEXT")
              (progn
                (setq loop nil)
                ;基準点x,y座標値を取得
                (setq flag T)
                (setq code10 (cdr(assoc 10 elist)) code11 (cdr(assoc 11 elist))
                      code72 (cdr(assoc 72 elist)) code73 (cdr(assoc 73 elist)))
                (if (and (or (= code72 0)(= code72 3)(= code72 5))(= code73 0))
                  (setq yiti (cadr code10) xiti (car  code10))
                  (setq yiti (cadr code11) xiti (car  code11))
                )
              )
            )
          )
        ) 
      )
    )  
  )
  (if flag
    (progn
        ;基準文字を選択セットの中に含める
        (ssadd ename ss)
        (setq n (sslength ss))
        ;ソートした別のLISTを用意
        (if (= res "Horizontal")
          (setq sw1 0 sw2 0);ｘ順、昇順
          (setq sw1 1 sw2 0);ｙ順、昇順
        )
        (setq srt_lst (sort_xxx_list (set_xxx_list ss) sw1 sw2))
        ;ソートリスト内の基準文字の位置を得る
        (setq iti (get_lst_nth srt_lst ename))
        ;カウンター初期化----------------------
        (setq i  0)
        ;変更処理------------------------------
        (repeat n
          (setq ename (ssname ss i)
                alist (entget ename)
                tt    (cdr (assoc 0 alist)))
          (setq n_iti (get_lst_nth srt_lst ename))
          (if (= tt "TEXT")
             (progn 
               (setq xsa 0 ysa 0)
               (setq code10 (cdr(assoc 10 alist )) code11 (cdr(assoc 11 alist )) 
                     code72 (cdr(assoc 72 alist )) code73 (cdr(assoc 73 alist )))
               ;code10とcode11の差を記憶
               ;基点左以外の時
               (if (not (and (= code72 0)(= code73 0)))
                 (progn
                   (setq xsa (- (car  code11)(car  code10))) 
                   (setq ysa (- (cadr code11)(cadr code10))) 
                 );progn end
               )
               ;左、フィット、両端の時の時
               (if (and (or (= code72 0)(= code72 3)(= code72 5))(= code73 0))
                 (setq m_iti code10)
                 (setq m_iti code11)
               )
               ;新しい位置の計算
               (if (= res "Horizontal")
                 (progn
                   (if pit_flag
                     (setq new_xiti (+ xiti (* (- n_iti iti) pit)))
                     (setq new_xiti (car m_iti))
                   )
                   (setq new_yiti yiti)
                 )
                 (progn
                   (setq new_xiti xiti)
                   (if pit_flag
                     (setq new_yiti (+ yiti (* (- n_iti iti) pit)))
                     (setq new_yiti (cadr m_iti))
                   )
                 )
               )
               (setq newx (+ xsa new_xiti) newy (+ ysa new_yiti))
               (setq new10 (list newx newy 0.0))
               (setq new11 (list new_xiti new_yiti (caddr code11)))
               ;フィット、両端の時
               (if (and (or (= code72 3)(= code72 5))(= code73 0))
                 (setq temp new10 new10 new11 new11 temp))
               ;左以外の時は、CODE 11書換え
               (if (not (and (= code72 0) (= code73 0)))
                 (setq alist (subst (cons  11 new11) (assoc 11 alist) alist))
               )
               ;;左、フィット、両端の時は、CODE 10書換え
               (if (and (or (= code72 0)(= code72 3)(= code72 5))(= code73 0))
                 (setq  alist (subst (cons 10 new10) (assoc 10 alist) alist))
               )
               (entmod alist)
             )
           )
          (setq i (+ 1 i))
        );--------------------------------------------------------
    )
  )
  (redraw)   
  (princ)
)
;*************************************************
;ソート用のLISTに変換する（汎用性無し）
;*************************************************
(defun set_xxx_list (ss / lst i cnt ename)
  (setq cnt (sslength ss))
  (setq i 0)
  (repeat cnt
    (setq ename (ssname ss i))
    (setq ent (entget ename)) 
    (setq lst (cons (list ename (cdr (assoc 10 ent))) lst))
    (setq i (+ 1 i))
  )
  lst;
)
;*************************************************
;LISTをソートする（汎用性無し）
;*************************************************
(defun sort_xxx_list(lst n sw  / n lst sw i j cnt )
;;(princ "\nIN:") ;(princ lst) ;n=0:x順 1:y順 ;sw=0:昇順 1:降順 ;sw=0 ｘ順 1:ｙ順
  (setq cnt (length lst))
  (setq st (car lst))
  (setq i 0)
  (repeat (- cnt 1)
    (setq j (+ i 1))
    (repeat (- (- cnt i) 1)
      (setq st (nth n (car (cdr (nth i lst)))))
      (setq ed (nth n (car (cdr (nth j lst)))))
      (if (= sw 1)
        (if (< st ed)
          (setq lst (lst_swap lst i j))
        )
        (if (> st ed)
          (setq lst (lst_swap lst i j))
        )
      )
      (setq j (+ 1 j))
    )
    (setq i (+ 1 i))
  )
  lst;; (princ "\nOUT:") ;(princ lst)
)
;*************************************************
;LIST内の位置を返す（汎用性無し）
;*************************************************
(defun get_lst_nth( lst item / lst item iti ans i)
  (setq iti -1 ans T i 0)
  (while ans
    (if (equal item (car (nth i lst)))
      (progn (setq iti i)(setq ans nil))
    )
    (setq i (+ i 1))
  )
  iti;
)
(defun get_lst_nth2( lst item / lst item iti ans i)
  (setq iti -1 ans T i 0)
  (while ans
    (if (equal item (cadr (nth i lst)))
      (progn (setq iti i)(setq ans nil))
    )
    (setq i (+ i 1))
  )
  iti;
)
;*************************************************
;LIST内の要素ｎ番目とｍ番目を入れ替える（汎用関数）
;*************************************************
(defun lst_swap (lst n m / lst n m len i new_lst n_item m_item )
  ;(setq new_lst nil)
  (setq len (length lst))
  (setq n_item (nth n lst))
  (setq m_item (nth m lst))
  (setq i 0)
  (repeat len
    (if (= i n)
      (setq new_lst (cons m_item new_lst)) 
      (if (= i m)
        (setq new_lst (cons n_item new_lst)) 
        (setq new_lst (cons (nth i lst) new_lst)) 
      )
    )
    (setq i (+ i 1)) 
  )
  (reverse new_lst)
)
;;==================================================================================================
;;78. セパレーターの作図           SEPA.LSP    2005/JUL/15 ==> 2020/Sep/9   By T.Terada
;;==================================================================================================
(defun C:SEPA( / GYO PC DJT DHV DHH DCC JT HV HH CC SYM WHAT_NEXT DJT_BAK DHV_BAK DHH_BAK DCC_BAK RETU SEL)
   (setq *error* *err1*)(setvar "CMDECHO" 0)
   (DCL_SEPA)
   (if (not GYO)(progn  (alert "長さが選択されていません")(DCL_SEPA)))
       (cond ((= WHAT_NEXT 0)(setq DJT DJT_BAK)(princ))((= WHAT_NEXT 1) DJT))
       (cond ((= WHAT_NEXT 0)(setq DHV DHV_BAK)(princ))((= WHAT_NEXT 1) DHV))
       (cond ((= WHAT_NEXT 0)(setq DHH DHH_BAK)(princ))((= WHAT_NEXT 1) DHH))
       (cond ((= WHAT_NEXT 0)(setq DCC DCC_BAK)(princ))((= WHAT_NEXT 1) DCC))
       (cond ((= DJT "1")(setq JT "J"))
             ((= DJT "J")(setq JT "J"))
             ((= DJT "T")(setq JT "T"))
       )
       (cond ((= DHV "1")(setq HV "Y"))
             ((= DHV "H")(setq HV "Y"))
             ((= DHV "V")(setq HV "T"))
       )
       (cond ((= DHH "1") (setq HH "75"))
             ((= DHH "H1")(setq HH "75"))
             ((= DHH "H2")(setq HH "145"))
       )
       (cond ((= DCC "1") (setq CC "C1"))
             ((= DCC "C1")(setq CC "C1"))
             ((= DCC "C2")(setq CC "C2"))
       )
   (setq SYM (strcat "SEP" LL "0" JT HV))            ;;シンボル名の合成
   (if (and (= HH "75") (= CC "C1"))(setq RETU "C"))
   (if (and (= HH "75") (= CC "C2"))(setq RETU "D"))
   (if (and (= HH "145")(= CC "C1"))(setq RETU "E"))
   (if (and (= HH "145")(= CC "C2"))(setq RETU "F"))
   (setq FILE "X:/XLSX/SEPA-SEL.xlsx")        ;;型式ﾃﾞｰﾀｰの読込
   (setq CELL (strcat RETU GYO))
   (REX)
   (setq T1 Value)
   (princ "\n挿入点を指示して下さい。")
   (command "INSERT" SYM pause "1" "1" "0")(setq PC (getvar "LASTPOINT"))
   (if (= HV "T")(setq PC (list (+ (car PC) 35)(cadr PC))))
   (command "INSERT" "ZOKU" PC "1" "1" "0" "KANA" "" T1 "" "" "" "" "" "" "" "" "39" "1" "" "")
   (redraw)
)
;;---------------------------------------------------------------------------------------------------
(defun DCL_SEPA ()
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (setq DJT_BAK DJT DHV_BAK DHV DHH_BAK DHH DCC_BAK DCC )
      (if (not DJT)(setq DJT "1"))
      (if (not DHV)(setq DHV "1"))
      (if (not DHH)(setq DHH "1"))
      (if (not DCC)(setq DCC "1")) 
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "SEPA" DCL_ID) )(EXIT))
         (action_tile "cancel" "(done_dialog)(exit)")
         (action_tile "J"  "(setq DJT $key)")
         (action_tile "T"  "(setq DJT $key)")
         (action_tile "H"  "(setq DHV $key)")
         (action_tile "V"  "(setq DHV $key)")
         (action_tile "H1" "(setq DHH $key)")
         (action_tile "H2" "(setq DHH $key)")
         (action_tile "C1" "(setq DCC $key)")
         (action_tile "C2" "(setq DCC $key)")
         (action_tile "L1" "(setq GYO \"11\" LL \"25\")")
         (action_tile "L2" "(setq GYO \"12\" LL \"30\")")
         (action_tile "L3" "(setq GYO \"13\" LL \"35\")")
         (action_tile "L4" "(setq GYO \"14\" LL \"40\")")
         (action_tile "L5" "(setq GYO \"15\" LL \"45\")")
         (action_tile "L6" "(setq GYO \"16\" LL \"50\")")
         (action_tile "L7" "(setq GYO \"17\" LL \"60\")")
         (action_tile "L8" "(setq GYO \"18\" LL \"70\")")
         (action_tile "L9" "(setq GYO \"19\" LL \"80\")")
         (action_tile "LX" "(setq GYO \"\")")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;;79.現在層と表示画層をＫＤ標準画層に戻します。       画層標準.LSP    2004/Feb/25    By T.Terada
;;==================================================================================================
(defun C:STD-LA (/)
  (setvar "CMDECHO" 0)
   
  (command "LAYER" "S" "S1" "")
  (command "LAYER" "ON" "BOX" "")
  (command "LAYER" "ON" "SHEET" "")
  (command "LAYER" "ON" "ZOKU1" "")
  (command "LAYER" "OFF" "BANKIN" "")
  (command "LAYER" "OFF" "ZOKU2" "")
  (setvar "CMDECHO" 1)
  (princ "\n現在層と表示画層をＫＤ標準画層に戻しました。")
)
;;==================================================================================================
;;80.Ｔ／Ｕ関係機器の回路作図                      TU.DCL   TU.LSP
;;==================================================================================================
(defun C:TU ( / PNO );SYM PNA 
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (DCL_TU)
      (if (not SYM)(progn  (alert "機器が選択されていません")(DCL_TU)))
      (setq PNO (getstring "\n機器番号を指示:(例：-1)"))
      (cond ((= PNO "") (setq PNO "-1")))(setq PNO (strcat PNA PNO))
      (princ "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" PNA PNO T1 T2 "" "" "" "" "" "" "" "15" "1" "" "")
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(REDRAW)(GC)
)
(defun DCL_TU (/)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "STU" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq SYM \"STUTYOL\" T1 \"WRT4345\"  PNA \"TU\" T2 \"\")")
          (action_tile "2" "(setq SYM \"STU6AKL\" T1 \"WR3416\"   PNA \"TU\" T2 \"\")")
          (action_tile "3" "(setq SYM \"STU6ARL\" T1 \"WR3417\"   PNA \"TU\" T2 \"\")")
          (action_tile "4" "(setq SYM \"STUSNL\"  T1 \"WRT3224K\" PNA \"TU\" T2 \"\")")
          (action_tile "5" "(setq SYM \"SDNSOL\"  T1 \"WRT2050\"  PNA \"DN\" T2 \"AC100-200V\")")
          (action_tile "6" "(setq SYM \"SZOHL\"   T1 \"WR3912\"   PNA \"TU\" T2 \"\")")
          
          (action_tile "7" "(setq SYM \"STUTYOR\" T1 \"WRT4345\"  PNA \"TU\" T2 \"\")")
          (action_tile "8" "(setq SYM \"STU6AKR\" T1 \"WR3416\"   PNA \"TU\" T2 \"\")")
          (action_tile "9" "(setq SYM \"STU6ARR\" T1 \"WR3417\"   PNA \"TU\" T2 \"\")")
          (action_tile "X" "(setq SYM \"STUSNR\"  T1 \"WRT3224K\" PNA \"TU\" T2 \"\")")
          (action_tile "Y" "(setq SYM \"SDNSOR\"  T1 \"WRT2050\"  PNA \"DN\" T2 \"AC100-200V\")")
          (action_tile "Z" "(setq SYM \"SZOHR\"   T1 \"WR3912\"   PNA \"TU\" T2 \"\")")
       (start_dialog)(unload_dialog DCL_ID)(princ)
)
;;==================================================================================================
;;81.主幹用ランプ回路の作図             MPL.DCL   MPL.LSP
;;==================================================================================================
(defun C:MPL ( / SYM PNO )
      (setq *error* *err1*)
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (DCL_MPL)
      (if (not SYM)(progn  (alert "回路が選択されていません")(DCL_MPL)))
      (princ "\n主幹用、表示灯を描きます")
      (princ "\n挿入点を指示")
      (command "INSERT" SYM pause "1" "0")
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(REDRAW)(GC)
)
(defun DCL_MPL (/)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
      (if (not (new_dialog "MPL" DCL_ID) )(EXIT)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "1" "(setq SYM \"*WL2P100VL\")")
          (action_tile "2" "(setq SYM \"*WL2P200VM\")")
          (action_tile "3" "(setq SYM \"*WL2P200VL\")")
          (action_tile "4" "(setq SYM \"*WL3P100VL\")")
          (action_tile "5" "(setq SYM \"*WL3P200VL\")")
          (action_tile "6" "(setq SYM \"*WL3P200VM\")")
          (action_tile "7" "(setq SYM \"*WL3P440V\")")
      (start_dialog)(unload_dialog DCL_ID)(princ)
)

;;==================================================================================================
;;83.AddNum2:数字混じりの文字列(TEXT)の数字部分を加減算する。URL http://plaza29.mbn.or.jp/~f_izawa/
;;==================================================================================================
;;AddNum2:数字混じりの文字列(TEXT)の数字部分を加減算する。  ;; 2001.3.9
;;AddNCopy:文字列の数字部分を＋１しながら指定回数コピーする ;; 2001.3.11
;;TCopyEd:文字列をコピーしながら編集、または数字部を＋１する;; 2001.3.12
;;--------------------------------------------------------------------------------------------------
(defun ifs_error(msg)
  (setq *error* old_error)
  (princ (strcat "\n" msg))
  (if (getvar "ICADVER")(icad_esc))
  (princ)
)
(defun c:AddNum2( / kw_res kw iti rflag addnum i ss ent lstcnt cnt j rflag res item txt anstxt addtxt)
        (princ "\nAddNum2 :数字混じりの文字の数字部分を加減算します．")
	(setq i 0 iti 1)
	(setq kw_res (get_kw_001))
	(setq iti (car kw_res) rflag (cadr kw_res))
	(if (/= iti 0)
		(setq addnum (getint "\n加算する数値（整数）を入力: "))
		(princ "\n位置番号が異常です．")
	)
	(if (and (/= addnum nil)(/= addnum 0))
		(progn
			(princ "\n対象文字(TEXT)を選んで下さい : ")
			(if (= (setq ss (ssget (list (cons 0 "TEXT")))) nil)
				(princ "\n何も選択されませんでした。")
				(repeat (sslength ss)
					(setq ent (entget (ssname ss i)))
					(setq txt (cdr (assoc 1 ent)))
					;;文字列を数字と文字に分離
					(setq lst (ifs_txt_bunri txt))
					;;後ろから数える時は、リストを反転
					(if rflag (setq lst (reverse lst)))
					(setq j 0 cnt 0 ansiti 0)
					;;文字列リスト内の位置を取得
					(foreach item lst
						(if (ifs_is_suuji item)
							(progn
								(setq cnt (1+ cnt))
								(if (= cnt iti)(setq ansiti (1+ j) anstxt item))
							)
						)
						(setq j (1+ j))
					)
					(if (> ansiti 0)
						(progn
							;;０を付加し最初の桁数に戻す
							(setq addtxt (itoa (+ (atoi anstxt) addnum)))
							(if (> (strlen anstxt)(strlen addtxt))
								(setq addtxt (ifs_add_zero addtxt (strlen anstxt)))
							)
							;;対象部分の文字列リストを入れ替える
							(setq lst (ifs_txtlst_conv lst ansiti addtxt))
							;;後ろから数える時は、リストを反転し、元に戻す
							(if rflag (setq lst (reverse lst)))
							;;文字列リストを１つに結合
							(setq restxt "")
							(foreach item lst (setq restxt (strcat restxt item)))
							;;結果を表示
							(princ (strcat "\n" txt " -> " restxt))
							;;文字列を書き換え
							(setq ent (subst (cons 1 restxt )(assoc 1 ent) ent))
							(entmod ent)
						)
					)
					(setq i (1+ i))
				)
			)
		)
		(princ "\n加算する数値が異常か０です．")
	)
	(princ)	
)
;;**********************************************************
;;AddNcopy:文字列の数字部分を＋１しながら指定回数コピーする
;;**********************************************************
(defun c:AddNCopy( / kw_res kw i sel ent txt pos10 pos11 iti kai posa posb sax say cnt rflag ansiti
                     anstxt txtlst restxt maetxt atotxt new10 new11 txtcnt cnt kai keta item lstans )
        (princ "\nAddNCopy:文字列の数字部分を＋１しながら指定回数コピーします．")
	(setq old_error *error*)
	(setq *error* ifs_error)
	(setq iti 1 i 0)
	(if (setq sel (entsel "\n基準文字を選択: "))
		(progn
			(setq ent (entget (car sel)))
			(if (= (cdr (assoc 0 ent)) "TEXT")
				(progn
					(setq txt (cdr (assoc 1 ent)) pos10 (cdr (assoc 10 ent)) pos11 (cdr (assoc 11 ent)))
					(setq txtlst (ifs_txt_bunri txt) txtcnt (length txtlst))
					(if (> (get_suuji_cnt txtlst) 1)
						(progn
							(setq kw_res (get_kw_001))
							(setq iti (car kw_res) rflag (cadr kw_res))
						)
					)
					(setq kai (getint "\nコピー回数を入力; "))
				)
				(princ "\n文字列(TEXT)ではありません．")
			)
		)
	)
	(if kai
		(if (setq posa (getpoint "\n基準点を指示 :"))
			(progn
				;;文字のイメージを見せるためにCOPYコマンドを使う
				(princ "\nコピー先を指示: ")
				(setvar "CMDECHO" 0)
				(command "COPY" "single" (car sel) posa pause)
				(if (not (equal posa (setq posb (getvar "LASTPOINT"))))
					(setq sax (- (car posb)(car posa)) say (- (cadr posb)(cadr posa)))
					(progn
						(princ "\n指示点エラー")
					)
				)
				;;最後を削除、念のために文字列で比較
				(setq ent (entget (entlast)))
				(if (= txt (cdr (assoc 1 ent))) 
					(command "ERASE" "single" "last")
				)
				(setvar "CMDECHO" 1)
			)
			(princ "\n指示点エラー")
		)
		(princ "\n文字列ではないか、入力数値のエラーです．")
	)
	(if sax
		(progn
			(setq lstans (get_txtlst_itians txtlst iti rflag))
			(setq ansiti (car lstans) anstxt (cadr lstans))
			(setq keta (strlen anstxt))
			(if rflag (setq ansiti (1+ (- txtcnt ansiti))))
			(setq maeato (get_maeato_txt txtlst ansiti))
			(setq maetxt (car maeato) atotxt (cadr maeato))
			(setq i 1)
			(repeat kai
				(setq restxt (itoa (+ (atoi anstxt) i)))
				;;最初の桁数に満たない時は文字列の前に０を付ける。
				(if (> keta (strlen restxt))(setq restxt (ifs_add_zero restxt keta)))
				;;新しく文字を作成する
				(setq ent (subst (cons 1 (strcat maetxt restxt atotxt))(assoc 1 ent) ent))
				(setq new10 (list (+ (car pos10)(* sax i))(+ (cadr pos10)(* say i)) 0.0)
					  ent  (subst (cons 10 new10)(assoc 10 ent) ent))
				(setq new11 (list (+ (car pos11)(* sax i))(+ (cadr pos11)(* say i)) 0.0)
					  ent  (subst (cons 11 new11)(assoc 11 ent) ent))
				(entmake ent)
				(setq i (1+ i))
			)
		)
	)
	(setq *error* old_error)
	(princ)	
)
;;**********************************************************
;;TCopyEd:文字列をコピーしながら編集、または数字部を＋１する
;;**********************************************************
(defun c:TCopyEd( / kw2 iti lstans ansiti anstxt keta maeato maetxt atotxt sel ent txt 
					code72 code73 tg loop basepos txtlst rflag)
	(setq old_error *error*)
	(setq *error* ifs_error)
        (princ "\nTCopyEd :文字列をコピーしながら編集、または数字部分を＋１します．")
	(setq iti 1)
	(princ "\nコピー後の処理を選択: ")
	(initget 0 "Edit Inc")
	(setq kw2 (getkword "\nE=編集する /I=＋１する <Enter=＋１する>: "))
	(if (= kw2 nil)(setq kw2 "Inc"))
	(if (setq sel (entsel "\n基準文字を選択: "))
		(progn
			(setq ent (entget (car sel)))
			(if (= (cdr (assoc 0 ent)) "TEXT")
				(progn
					(setq txt (cdr (assoc 1 ent)) code72 (cdr (assoc 72 ent)) code73 (cdr (assoc 73 ent)))
					(if (/= kw2 "Edit")
						(progn
							(setq txtlst (ifs_txt_bunri txt) txtcnt (length txtlst))
							(if (> (get_suuji_cnt txtlst) 1)
								(progn
									(setq kw_res (get_kw_001))
									(setq iti (car kw_res) rflag (cadr kw_res))
								)
							)
							(setq lstans (get_txtlst_itians txtlst iti rflag))
							(setq ansiti (car lstans) anstxt (cadr lstans))
							(setq keta (strlen anstxt))
							(if rflag (setq ansiti (1+ (- txtcnt ansiti))))
							(setq maeato (get_maeato_txt txtlst ansiti))
							(setq maetxt (car maeato) atotxt (cadr maeato))
						)
					)
					;;基点を得る
					(if (and (= code72 0) (= code73 0))
						(setq basepos (cdr (assoc 10 ent)))
						(setq basepos (cdr (assoc 11 ent)))
					)
					(setq tg (car sel))
					(setvar "CMDECHO" 0)
					(setq loop T i 1)
					(while loop
						(princ "\n複写先を指示 <ESC=終了> : ")
						(command "COPY" "single" tg basepos pause)
						(if(setq loop (not (equal (getvar "LASTPOINT") basepos)))
							(progn
								;;DDEDITで編集する
								(if (= kw2 "Edit")
									(progn
										(princ "\n文字列編集: ")
										(command "DDEDIT" "last" "")
									)
								)
								;;次のループのために最後の文字列を得る
								(setq tg (entlast) ent (entget tg))
								(setq txt (cdr (assoc 1 ent)) code72 (cdr (assoc 72 ent)) code73 (cdr (assoc 73 ent)))
								(if (and (= code72 0) (= code73 0))
									(setq basepos (cdr (assoc 10 ent)))
									(setq basepos (cdr (assoc 11 ent)))
								)
								;;数字部分を＋１する
								(if (/= kw2 "Edit")
									(progn
										(setq restxt (itoa (+ (atoi anstxt) i)))
										;;最初の桁数に満たない時は文字列の前に０を付ける。
										(if (> keta (strlen restxt))(setq restxt (ifs_add_zero restxt keta)))
										;;新しく文字を作成する
										(setq txt (strcat maetxt restxt atotxt))
										(setq ent (subst (cons 1 txt)(assoc 1 ent) ent))
										(entmod ent)
										(setq i (1+ i)) 
									)
								)
							)
						)
					)	
					(setvar "CMDECHO" 1)
				
				)
				(princ "\n文字列(TEXT)ではありません．")
			)
		)
		(princ "\n何も選択されませんでした．")
	)
	(setq *error* old_error)
	(princ)
)
;;文字列リストの指定位置より前後の文字列を得る
(defun get_maeato_txt(txtlst iti / i maetxt atotxt item)
	(setq maetxt "" atotxt "" i 1)
	(foreach item txtlst
		(if (< i iti)(setq maetxt (strcat maetxt item)))
		(if (> i iti)(setq atotxt (strcat atotxt item)))
		(setq i (1+ i))
	)
	(list maetxt atotxt)
)

;;数字群の位置番号から文字列リスト全体の位置とその文字列を得る
(defun get_txtlst_itians( txtlst iti rflag / cnt i item ansiti anstxt)
	(if rflag (setq txtlst (reverse txtlst)))
	;;文字列リスト内の位置を取得
	(setq cnt 0 i 0)
	(foreach item txtlst
		(if (ifs_is_suuji item)
			(progn
				(setq cnt (1+ cnt))
				(if (= cnt iti)(setq ansiti (1+ i) anstxt item))
			)
		)
		(setq i (1+ i))
	)
	(list ansiti anstxt)
)
;;数字群の番号とフラグを返す
(defun get_kw_001( / kw rflag)
	(setq kw (getint "\n加算する数字群の位置を入力（Enter=1,マイナス=後ろからの位置）: "))
	(if kw (if (< kw 0) (setq kw (abs kw) rflag T)) (setq kw 1))
	(list kw rflag)
)

;;数字の前に０を付加する
(defun ifs_add_zero( str keta / res)
	(setq res "")
	(repeat (- keta (strlen str))(setq res (strcat res "0")))
	(strcat res str)
)
;;文字列リストのn番目をstrに入れ替える
(defun ifs_txtlst_conv(txtlst iti str / i res)
	(setq i 0 iti (1- iti))
	(repeat (length txtlst)
		(if (/= i iti)
			(setq res (append res (list (nth i txtlst))))
			(setq res (append res (list str)))
		)
		(setq i (1+ i))
	)
	res;
)
;;文字列を数字とそれ以外に分離しLISTにする
(defun ifs_txt_bunri(str / len st ed i flag1 flag2 lst)
	(setq i 1 st 1)
	(setq len (strlen str))
	(repeat (1- len)
		(setq flag1 (ifs_is_suuji (substr str i 1)))
		(setq flag2 (ifs_is_suuji (substr str (1+ i) 1)))
		(if (or (and flag1 (not flag2 ))(and (not flag1) flag2))
			(setq ed (+ i 1)
				  lst (append lst (list (substr str st (- ed st))))
				  st ed
			)
		)
		(setq i (1+ i))
	)
	(append lst (list (substr str st (+ (- len st)1))))
)
;;数字（1234567890）か？
(defun ifs_is_suuji( str / asc res)
	(setq asc (ascii (substr str 1 1)))
	(and (>= asc 48)(<= asc 57))
)
;;文字列リスト内の数字群の個数を得る
(defun get_suuji_cnt (txtlist / item cnt)
	(setq cnt 0)
	(foreach item txtlist
		(if (ifs_is_suuji item)(setq cnt (1+ cnt)))
	)
	cnt;
)
;***************************************************
; Icadでの残像対策（暫定）
;***************************************************
(defun icad_esc()
  (setvar "CMDECHO" 0)
  (command "LINE" "0,0" "0,0" "")
  (command "ERASE" "SINGLE" "LAST")
  (setvar "CMDECHO" 1)
  (redraw)
)

;;==================================================================================================
;;84.PickW:配線条数を線分(LWPOLYLINEorLINE)上に挿入します。  PickWire.lsp
;;==================================================================================================
;;2001.10.23                                          makeup by f.izawa
;;2001.10.24 角度判断でマイナスの値の処理を追加              by f.izawa
;;行62,63,64を改造してKDの図枠サイズ(1/10)に対応しただけ     By T.Terada
;;--------------------------------------------------------------------------------------------------
(defun c:pickw( / jousu sensu eflag str loop esel pick entname ent code1042 blkname
	pick_item item_no ang xc yc deg ret dimsc)
	(setq
		senkan		0.75 ;;斜線間の距離
		nagasa		0.75 ;;斜線の長さの1/2
		katamuki	60.0 ;;斜線の傾き
	)
	(setq dimsc (getvar "DIMSCALE"))
	(setvar "CMDECHO" 0)
	(setq jousu (getstring "\n条数(MAX 9)を入力  X:終了 <Enter=2> :"))
	(if (= jousu "")(setq jousu "2"))
	(setq jousu (strcase jousu))
	(if (/= str "X")
		(progn
			(setq str (substr jousu 1 1) sensu (atoi str) loop T)
			;;アース付き判断
			(setq eflag (and (= (strlen jousu) 2)(= (substr jousu 2 1) "E")))
			;;ブロック名を作成
			(setq blkname (strcat "I_WIRE_" str ))
			(if eflag (setq blkname (strcat blkname "E"))(setq blkname (strcat blkname "C")))
			;;ブロック名が無ければ、ブロックを作成する
			(if (not (tblsearch "BLOCK" blkname))
				(ifs_make_wireblock blkname sensu senkan nagasa katamuki eflag))
		)
	)
	(while loop
		(setq ret nil)
		(if (setq esel (entsel "\n条数を挿入する線分を指示 <Enter=終了> : "))
			(progn
				(setq pick (car (cdr esel)))
				(setq entname (car esel))
				(setq ent (entget entname))
				(if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
					(progn
						;;LWPLINEのEntityからcode10,code42を抜き出す
						(setq code1042 (get_lwp_code1042 ent))
						;;ピックした点に一番近い線分への垂線点と線分の位置を得る
						(setq pick_item (get_1042pos code1042 pick))
						(setq item_no (cadr pick_item) pick (car pick_item))
						;;ピックした線分の角度を得る
						(setq ang (angle (car (nth item_no code1042))(car (nth (1+ item_no) code1042))) ret T)
					)
				)
				(if (= (cdr (assoc 0 ent)) "LINE")
					(progn
						(setq pick (get_suisen_pos pick (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
						(setq ang (angle (cdr (assoc 10 ent))(cdr (assoc 11 ent))) ret T)
					)
				)
				(if ret
					(progn
						;;180度以内にする
						(if ang (setq ang (ang_to_ang ang pi)))
						(if (> ang (/ pi 2.0))(setq ang (- ang pi)))
						
						(setq xc (/ dimsc 5)); ORG = (setq xc dimsc)
;						(if (>= dimsc 200.0)(setq xc 76.666))
;						(if (and (<= dimsc	50.0)(> dimsc 1.0))(setq xc 133.333))
						(setq yc xc deg (/ (* ang 180.0) pi))
						;;2001.10.24 -範囲を追加
						(if (and (> deg 60.0)(< deg 120.0))
							(setq yc (* yc 1.0) xc (* xc -1.0))
							(if (and (< deg -60.0)(> deg -120.0))
								(setq yc (* yc -1.0) xc (* xc 1.0))
							)
						)
						;;(princ deg)
						(command "INSERT" blkname pick xc yc deg)
						(command "REDRAW")
					)
					(princ "\n線分ではありません")
				)
			)
			(setq loop nil)
		)
	)
	(princ)
)
;;ピックした点に近いPLINE要素を得るテスト
(defun c:pickt()
	(setvar "CMDECHO" 0)
	(initget "T C B")
	(if (not (setq kw (getkword "\n文字基点 B:下/C:中央/T:上 <Enter=B:下> : ")))
		(setq kw "B")
	)
	(setq txtH (getreal "\n文字高さ : "))
	(setq txtS (getstring "\n文字列 : "))
	(setq loop T)
	(while loop
		(if (setq esel (entsel "\nPick PLINEorLINE : "))
			(progn
				(setq pick (car (cdr esel)))
				(setq entname (car esel))
				(setq ent (entget entname))
				(if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
					(progn
						;;LWPLINEのEntityからcode10,code42を抜き出す
						(setq code1042 (get_lwp_code1042 ent))
						;;ピックした点に一番近い線分への垂線点と線分の位置を得る
						(setq pick_item (get_1042pos code1042 pick))
						(setq item_no (cadr pick_item) pick (car pick_item))
						;;ピックした線分の角度を得る
						(setq ang (angle (car (nth item_no code1042))(car (nth (1+ item_no) code1042))))
					)
				)
				(if (= (cdr (assoc 0 ent)) "LINE")
					(progn
						(setq pick (get_suisen_pos pick (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
						(setq ang (angle (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
					)
				)
				;;180度以内にする
				(if ang (setq ang (ang_to_ang ang pi)))
				(if (> ang (/ pi 2.0))(setq ang (- ang pi)))
				(command "TEXT" "J" (strcat kw "C") pick txtH (/ (* ang 180.0) pi) txtS)
				;;(command "TRIM" "SINGLE" "LAST" pick "")
			)
			(setq loop nil)
		)
	)
	(princ)
)
;;LWPOLYLINEのｺｰﾄﾞ10と42のリストを返す
;;ResEx:(((1.250 4.250 0.00) 0.00)....)
;;閉鎖されている時は、先頭を最後に追加する
(defun get_lwp_code1042( ent / item item1 item2 code10 res cflag)
	(foreach item ent
		(setq item1 (car item) item2 (cdr item))
		(cond
			((= item1 70)(if (or (= item2 1)(= item2 -127))(setq cflag T)))
			((= item1 10)(setq code10 item2))
			((= item1 42)(setq res (append res (list (list code10 item2)))))
		)
	)
	(if cflag (setq res (append res (list (car res)))))
	res;
)
;;点P0から線分P1-P2上の垂線の点を返す
(defun get_suisen_pos(p0 p1 p2 / kouten)
	(if (setq kouten (inters p0 (polar p0 (+ (angle p1 p2) (/ pi 2.0)) 100.0) p1 p2 nil))
		(if (not (equal (+ (distance kouten p1)(distance kouten p2))(distance p1 p2) 0.001))
			(setq kouten nil);
		)
	)
	kouten;
)
;;code10-42のリストから点posに一番近い要素への垂線の交点と、その要素の位置を返す
;;要：get_suisen_pos
;;ResEx:((1.23 4.56 0.00) 2))
(defun get_1042pos(lst1042 pos / loop i distmin dist res item1 item2 kouten)
	(setq i 0 cnt (1- (length lst1042)) loop T)
	(while loop
		(setq item1 (car (nth i lst1042)) item2 (car (nth (1+ i) lst1042)))
		(if (setq kouten (get_suisen_pos pos item1 item2))
			(progn
				(setq dist (distance pos kouten))
				(if (or (not distmin)(and distmin (> distmin dist)))
					(setq res (list kouten i) distmin dist)
				)
			)
		)
		(if (< i (1- cnt))(setq i (1+ i))(setq loop nil))
	)
	res;
)
;;angをang0以内にする
(defun ang_to_ang(ang maxang)
	(while (> ang maxang)
		(setq ang (- ang maxang))
	)
	ang;
)
(defun ifs_make_wireblock(
	blkname		;ブロック名
	cnt			;心線数
	pit			;斜線ピッチ
	hdis		;斜線長さの1/2
	deg			;斜線の傾き
	earth		;T=アース付き
	/ stx p0 p10 p11 ang)
	;;62:線色0=ByBlock 256=ByLayer;;8:画層
	(setq stx (* (/ (1- cnt) -2.0) pit) ang (/ (* deg pi) 180.0) i 0 count cnt)
	;;5C以上の時
	(if (> cnt 4) (setq stx (- stx (* pit 0.25))))
	(if earth (setq stx (- stx (* pit 0.75)) count (1+ cnt)))
	(entmake (list '(0 . "BLOCK")(cons 2 blkname)'(10 0.0 0.0 0.0)'(70 . 0)))
	(repeat count
		(setq p0 (list stx 0.0 0.0))
		(setq p10 (polar p0 ang hdis) p11 (polar p0 ang (* hdis -1.0)))
		(entmake (list '(0 . "LINE")'(8 . "0")(cons 10 p10)(cons 11 p11)'(62 . 0)))
		(setq stx (+ stx pit) i(1+ i))
		(if (and (> cnt 4)(= i 3))(setq stx (+ stx (* pit 0.25))))
		(if (and earth (= i cnt))(setq stx (+ stx (* pit 0.25)) ang (- pi ang)))
	)
	(entmake '((0 . "ENDBLK")))
)
(princ)

;;==================================================================================================
;;86.４角形を描く        4KAU2.LSP
;;==================================================================================================
  (defun C:4KAKU2 (/ SEN); DEF DEF_BAK WHAT_NEXT)
      (setq *error* *err1*)(setvar "CMDECHO" 0)
      (setq DEF nil)(setq DEF_BAK DEF)
      (if (not DEF)(setq DEF "J"))
      (setq DCL_ID (load_dialog "X:\/LSP-I/2KD.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
          (if (null (new_dialog "JT" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "J" "(setq DEF $key)")
          (action_tile "T" "(setq DEF $key)")
          (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)
      (cond ((= WHAT_NEXT 0)(setq DEF DEF_BAK)(princ))
            ((= WHAT_NEXT 1) DEF)
      )
      (cond ((= DEF "J")(setq SEN "J"))
            ((= DEF "T")(setq SEN "T"))
      )
      (if (= SEN "J")(command "LINETYPE" "S" "CONTINUOUS" ""))
      (if (= SEN "T")(command "LINETYPE" "S" "DASHED" ""))
      (command "RECTANGLE" pause pause)
      (redraw)(setvar "CMDECHO" 1)      
  )
;;==================================================================================================
;;87.Ｓ＿ＭＥＮＵの起動  S_MENU.LSP  1998/Mar/28 ==> 2005/8/5 ==> 2021/3/4               By T.Terada
;;==================================================================================================
;;このプログラムを起動する前に (setq S_MENU "S_MENU01")を実行して表示させるメニュ－を設定
;;EXCELからデーターを読み込めるように変更
;;メニューの画面サイズを変更

(defun C:S_MENU ( / ADD LB1 LB2 DCL_ID X_MAX Y_MAX X_BOX Y_BOX X_P GYO TYP SYM1 PNA T1 T2 T3 T4 MKN)
;;初期設定
    (vl-load-com)
    (setq *error* *err1*)
    (setvar "CMDECHO" 0)
    (setq ADD 20)
    (setq S_MENU "S_MENU01")
    (if (= S_MENU "S_MENU01")(progn (setq ADD  20) (setq LB1 "   回 路 図 用")))
    (if (= S_MENU "S_MENU02")(progn (setq ADD 170) (setq LB1 "   機器外形図")))
    (if (= S_MENU "S_MENU03")(progn (setq ADD 320) (setq LB1 "   箱体用部品")))
    (if (= S_MENU "S_MENU04")(progn (setq ADD 470) (setq LB1 "")))
    (if (= S_MENU "S_MENU05")(progn (setq ADD 620) (setq LB1 "")))
    (if (= S_MENU "S_MENU06")(progn (setq ADD 770) (setq LB1 "")))
    (if (= S_MENU "S_MENU07")(progn (setq ADD 920) (setq LB1 "")))
    (if (= S_MENU "S_MENU08")(progn (setq ADD 1070)(setq LB1 "")))
    (if (= S_MENU "S_MENU09")(progn (setq ADD 1220)(setq LB1 "")))
    (if (= S_MENU "S_MENU0A")(progn (setq ADD 1370)(setq LB1 "")))
    (if (= S_MENU "S_MENU0B")(progn (setq ADD 1520)(setq LB1 "   ｷｭ-ﾋﾞｸﾙ箱体 屋内")))
    (if (= S_MENU "S_MENU0C")(progn (setq ADD 1670)(setq LB1 "   ｷｭ-ﾋﾞｸﾙ箱体 屋内")))
    (if (= S_MENU "S_MENU0D")(progn (setq ADD 1820)(setq LB1 "")))
    (if (= S_MENU "S_MENU0E")(progn (setq ADD 1970)(setq LB1 "")))
    (if (= S_MENU "S_MENU0F")(progn (setq ADD 2120)(setq LB1 "")))

    (C:MKR)
    (if (= MAKER "0")(setq LB2 "現在の基本メ－カ－：三 菱"))
    (if (= MAKER "1")(setq LB2 "現在の基本メ－カ－：富 士"))
    (if (= MAKER "2")(setq LB2 "現在の基本メ－カ－：寺崎＋三菱"))
    (if (= MAKER "3")(setq LB2 "現在の基本メ－カ－：寺崎＋富士"))
    (setq DCL_ID (load_dialog "X:/DCL/S_MENU-B.DCL"))
    (if (not (new_dialog "SMB" DCL_ID) )(exit))

;ｽﾗｲﾄﾞﾌｧｲﾙの起動
          (setq X_MAX (DIMX_TILE "ACCEPT")
                Y_MAX (DIMY_TILE "ACCEPT")
          )
          (setq X_MAX (+ X_MAX 1))(setq Y_MAX (+ Y_MAX 1))
;          (princ "\nX_MAX=")(princ X_MAX)(princ "\nY_MAX=")(princ Y_MAX)
          (start_image "ACCEPT");(princ "\nS_MENU=")(princ S_MENU)
          (slide_image 0 0 X_MAX Y_MAX S_MENU)
          (end_image)
          (set_tile "LB1" LB1)(set_tile "LB2" LB2)

;;ラジオボタンのアクション
         ;;アイコンで選択した項目を、ラジオボタンに割り当て
              (if (= S_MENU "S_MENU01")(set_tile "1" "1"))(if (= S_MENU "S_MENU02")(set_tile "2" "1"))
              (if (= S_MENU "S_MENU03")(set_tile "3" "1"))(if (= S_MENU "S_MENU04")(set_tile "4" "1"))
              (if (= S_MENU "S_MENU05")(set_tile "5" "1"))(if (= S_MENU "S_MENU06")(set_tile "6" "1"))
              (if (= S_MENU "S_MENU07")(set_tile "7" "1"))(if (= S_MENU "S_MENU08")(set_tile "8" "1"))
              (if (= S_MENU "S_MENU09")(set_tile "9" "1"))(if (= S_MENU "S_MENU0A")(set_tile "A" "1"))
              (if (= S_MENU "S_MENU0B")(set_tile "B" "1"))(if (= S_MENU "S_MENU0C")(set_tile "C" "1"))
              (if (= S_MENU "S_MENU0D")(set_tile "D" "1"))(if (= S_MENU "S_MENU0E")(set_tile "E" "1"))
              (if (= S_MENU "S_MENU0F")(set_tile "F" "1"))
              (action_tile "1" "(setq S_MENU \"S_MENU01\")(SLD_ON)")
              (action_tile "2" "(setq S_MENU \"S_MENU02\")(SLD_ON)")
              (action_tile "3" "(setq S_MENU \"S_MENU03\")(SLD_ON)")
              (action_tile "4" "(setq S_MENU \"S_MENU04\")(SLD_ON)")
              (action_tile "5" "(setq S_MENU \"S_MENU05\")(SLD_ON)")
              (action_tile "6" "(setq S_MENU \"S_MENU06\")(SLD_ON)")
              (action_tile "7" "(setq S_MENU \"S_MENU07\")(SLD_ON)")
              (action_tile "8" "(setq S_MENU \"S_MENU08\")(SLD_ON)")
              (action_tile "9" "(setq S_MENU \"S_MENU09\")(SLD_ON)")
              (action_tile "A" "(setq S_MENU \"S_MENU0A\")(SLD_ON)")
              (action_tile "B" "(setq S_MENU \"S_MENU0B\")(SLD_ON)")
              (action_tile "C" "(setq S_MENU \"S_MENU0C\")(SLD_ON)")
              (action_tile "D" "(setq S_MENU \"S_MENU0D\")(SLD_ON)")
              (action_tile "E" "(setq S_MENU \"S_MENU0E\")(SLD_ON)")
              (action_tile "F" "(setq S_MENU \"S_MENU0F\")(SLD_ON)")
          (action_tile "ACCEPT" "(setq X_P $X Y_P $Y)(DONE_DIALOG 1)")

;キャンセルボタンの処理
      (action_tile "cancel" "(done_dialog 0)(exit)")
      (start_dialog)(unload_dialog DCL_ID)(princ)

;;１升の行・列 区画を取得                       最大座標 (X_MAX) / 列数 (14) = 60
;;                                              最大座標 (Y_MAX) / 行数 (10) = 60
      (setq X_BOX (+ (abs (/ X_P 60)) 1))
      (setq Y_BOX (+ (abs (/ Y_P 60)) 1))       ;列(Y)の値を英文字に変換 (64 + 1)
      (setq GYO (+ (+ (- (* Y_BOX 14) 14) X_BOX) ADD))
      (princ "\nX_BOX = ")(princ X_BOX)(princ "\nY_BOX = ")(princ Y_BOX)
      (princ "\nGYO = ")  (princ GYO)  (princ "\nADD = ")  (princ ADD)
      (setq GYO (itoa GYO))
      (setq FILE "X:/XLSX/S_MENU.xlsx")        ;;型式ﾃﾞｰﾀｰの読込
      (setq TN 68);アルファベット"D"の10進数
      (setq TNn 68)
      (setq CELL nil)
      (XLSR)
	  (princ "\nTYP = ")  (princ TYP)
;;S_MENUの記述方法は、X:\XLS\S_MENU.XLSの「記述方法」を参照して下さい。
;; 空白のTYP
    (if (or (= TYP "")(= TYP "nil"))(progn (alert "入力タイプが、登録されていません！")(exit)))

;; 協約外形の入力
    (if (= TYP "A")(progn (setq NM PNA T11 T1 T12 T2 T13 T3 T14 T4 PN MKN)(eval (read SYM1))))

;;  関数を起動(SYM1)
    (if (= TYP "C")(eval (read SYM1)))

;;  関数を起動(T1 + SYM)
    (if (= TYP "D")(progn (eval (read T1))(eval (read SYM1))))
    
;;  関数を起動(T1 + T2 + T3 + SYM1)トランスの外形入力等
    (if (= TYP "E")(progn (eval (read T1))(eval (read T2))(eval (read T3))(eval (read SYM1))))

;;  関数を起動(T1 + T2 + SYM)ＳＣの外形入力等
    (if (= TYP "F")(progn (eval (read T1))(eval (read T2))(eval (read SYM1))))

;;  外型属性有りのシンボルをインサ－ト
    (if (= TYP "G")
        (progn  (princ "\n挿入点を指示")
                (command "INSERT" SYM1 pause "1" "1" "0" PNA T1 T2 T3 T4 "")
        )
    )
;;  部品表属性有りのシンボルをインサ－ト（型式をLSP1、メーカー番号をLSP2）
    (if (= TYP "I")
        (progn  (setq MKN (itoa (fix MKN)))  ;MKNが実数になるのを回避
                (if (= PNA nil)(setq PNA ""));値がnilでエラーとなるのを回避
                (if (= T1 nil)(setq T1 ""))
                (if (= T2 nil)(setq T2 ""))
                (if (= T3 nil)(setq T3 ""))
                (if (= T4 nil)(setq T4 ""))
                (princ "\n挿入点を指示")
                (command "INSERT" SYM1 pause "1" "1" "0" PNA "" T1 T2 T3 T4 "" "" "" "" "" MKN "1" "" "")
        )
    )
;;  ＬＩＳＰファイルをロ－ド
    (if (= TYP "L")(read SYM1))

;;  属性無しのシンボルをインサ－ト
    (if (= TYP "S")
        (progn  (princ "\n挿入点を指示")(command "INSERT" SYM1 pause "1" "1" "0"))
    )

;;  属性無しのシンボルを*インサ－ト（分解挿入）
    (if (= TYP "T")
        (progn  (princ "\n挿入点を指示")
                (setq SYM (strcat "*" SYM1))
                (command "INSERT" SYM pause "1" "0")
        )
    )
    (command "REDRAW")(setvar "CMDECHO" 0)(princ "\nS_MENU終了")
);S_MENU終り
;;--------------------------------------------------------------------------------------------------      
;;EXCELファイルの読込み
(defun XLSR (/ sheet)
	(setq excelApplication (vlax-get-or-create-object "Excel.Application"))
	(vlax-put-property excelApplication 'Visible :vlax-false);excelを表示しない

;既存のブックを開きます。
	(setq workbooks (vlax-get-property excelApplication 'Workbooks));アクティブなブックを取得
	(setq workbook (vlax-invoke-method workbooks 'Open FILE))
	(setq workbook (vlax-get-property excelApplication 'ActiveWorkbook))
	(vlax-invoke-method workbook 'Activate)                         ;ブックをアクティブにする
;	(setq window (vlax-get-property (vlax-get-property workbook 'Windows) 'Item 1))

;ワークシートの読込み。
	(setq sheets (vlax-get-property workbook 'Worksheets))
	(vlax-invoke-method (vlax-get-property sheets 'Item "Sheet1") 'Activate)
	(setq sheet (vlax-get-property excelApplication 'ActiveSheet))  ；アクティブなシートを得る

;セルの読込み。
    (setq n 1)
    (while (<= n 8) ;D～Kまでのループ
        (setq PFX (chr TN))             ;(princ "\nPFX=")(princ PFX)(princ "\nGYO=")(princ GYO)
	    (setq CELL (strcat PFX GYO))    ;(princ "\nCELL=")(princ CELL)
	        (setq rangeObj (vlax-get-property sheet 'Range CELL))
	        (setq value2 (vlax-get-property rangeObj 'Value))
	        (setq value  (vlax-variant-value value2));(princ "\nvalue=")(princ value)
			    (if (= n 1)(setq TYP value))
	    		(if (= n 2)(setq SYM1 value))
			    (if (= n 3)(setq PNA value))
	    		(if (= n 4)(setq T1 value))
			    (if (= n 5)(setq T2 value))
	    		(if (= n 6)(setq T3 value))
			    (if (= n 7)(setq T4 value))
	    		(if (= n 8)(setq MKN value))
	    (setq TN (+ 1 TN))
	    (setq n (+ 1 n))
    )
;EXCEL終了処理	
    
    (vlax-invoke-method workbook 'Close)
    (vlax-invoke-method workbooks 'Close)
	(vlax-invoke-method excelApplication 'Quit);EXCEL終了
	(vlax-release-object excelApplication)     ;VLAの開放
	(vlax-object-released-p excelApplication)
	(princ "\nEXCEL終了")
)
;;--------------------------------------------------------------------------------------------------
(defun SLD_ON() (princ "\nSLD-ON")
    (if (= S_MENU "S_MENU01")(progn (setq ADD  20) (setq LB1 "回路図用"))); +150
    (if (= S_MENU "S_MENU02")(progn (setq ADD 170) (setq LB1 "機器外形")))
    (if (= S_MENU "S_MENU03")(progn (setq ADD 320) (setq LB1 "板金部品")))
    (if (= S_MENU "S_MENU04")(progn (setq ADD 470) (setq LB1 "協約外形")))
    (if (= S_MENU "S_MENU05")(progn (setq ADD 620) (setq LB1 "")))
    (if (= S_MENU "S_MENU06")(progn (setq ADD 770) (setq LB1 "")))
    (if (= S_MENU "S_MENU07")(progn (setq ADD 920) (setq LB1 "")))
    (if (= S_MENU "S_MENU08")(progn (setq ADD 1070)(setq LB1 "")))
    (if (= S_MENU "S_MENU09")(progn (setq ADD 1220)(setq LB1 "")))
    (if (= S_MENU "S_MENU0A")(progn (setq ADD 1370)(setq LB1 "")))
    (if (= S_MENU "S_MENU0B")(progn (setq ADD 1520)(setq LB1 "ｷｭ-ﾋﾞｸﾙ箱体 屋内")))
    (if (= S_MENU "S_MENU0C")(progn (setq ADD 1670)(setq LB1 "ｷｭ-ﾋﾞｸﾙ箱体 屋内")))
    (if (= S_MENU "S_MENU0D")(progn (setq ADD 1820)(setq LB1 "")))
    (if (= S_MENU "S_MENU0E")(progn (setq ADD 1970)(setq LB1 "")))
    (if (= S_MENU "S_MENU0F")(progn (setq ADD 2120)(setq LB1 "")))
    (set_tile "LB1" LB1)(set_tile "LB2" LB2)
    (start_image "ACCEPT")
    (fill_image 0 0 X_MAX Y_MAX 0)
    (slide_image 0 0 X_MAX Y_MAX S_MENU)
    (end_image)
)
;;==================================================================================================
;;88.１Ｐ端子台   S_MENUから呼出し、S_MENU専用  SYMはS_MENUで指定          単品回路入力.LSP         
;;==================================================================================================
  (defun C:STB ( / PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 MN)
      (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (setq SYM "STB")
      (setq MN "76");東洋技研
      (setq T2 "")(setq T3 "")(setq T4 "")(setq T5 "")(setq T6 "")(setq T7 "")(setq T8 "")(setq T9 "")
       (setq PNo (getstring "\n端子番号を指示:(例：C1)"))
                (cond ((= PNo "") (setq PNo "C1")))
       (setq T1  (getstring "\n端子サイズを指示:(例：PT-20)"))
                (cond ((= T1 "") (setq T1 "PT-20")))
      (princ "\n端子台の挿入点を指示")
      (command "INSERT" SYM pause "1" "1" "0" "TB" PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 "76" "1" "" "")
      (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
    (GC)
  )
;;==================================================================================================
;;89.ダイオードの入力  S_MENUから呼出し、S_MENU専用   SYMはS_MENUで指定      単品回路入力.LSP
;;==================================================================================================
  (defun C:SDI ( / PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 MK DT KIKI ENT)
      (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (setq PNA "DI")
      (setq MK "4");;;;;;;;;;;;仮　富士
      (setq T1 "")(setq T2 "")(setq T5 "")(setq T6 "")(setq T7 "")(setq T8 "")(setq T9 "")
       (setq PNo (getstring "\nダイオード番号を指示:(例：-1)"))
                (cond ((= PNo "") (setq PNo "-1")))
       (setq T3 (getstring "\n定格電圧を指示:(例：400V)"))
                (cond ((= T3 "") (setq T3 "400V")))
       (setq T4 (getstring "\n定格電流を指示:(例：1A)"))
                (cond ((= T4 "") (setq T4 "1A")))
    (setq PNo (strcat PNA PNo))
     (princ "\nダイオードの挿入点を指示")
    (command "INSERT" SYM pause "1" "1" "0" PNA PNo T1 T2 T3 T4 T5 T6 T7 T8 T9 "4" "1" "" "")
    (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
    (GC)
  )

;;==================================================================================================
;;90.協約型の外形入力 DBr.LSP / DBr2.DCL  2002/Sep/07=>2020/Aug/25 VBA対応に更新     By T.Terada
;;==================================================================================================
(defun C:DBr ( / PC P1 PX LPR LPL LPC PCL PCR BRL BRR LR PCCL PCCR BRH SPNL SPNR INn SPH)
    (setq *error* *err1*)
    (setvar "CMDECHO" 0)

;DCL -----------------------------------------------------------------------------------------------
    (setq DEF_BAK DEF)
    (if (not DEF)(setq DEF "NG"))
    (setq DCL_ID (load_dialog "X:\/LSP-I/2KD.DCL"))
    (setq WHAT_NEXT 2)
    (while (>= WHAT_NEXT 2)
        (if (or (= PNA "MCB")(= PNA "SB")(= PNA "TM"))
            (progn (setq DLG "DBr2"))
            (progn (setq DLG "DBr"))
        )
        (if (null (new_dialog DLG DCL_ID))(exit))
        (act_radioset '("NG" "KD") DEF)
        (action_tile "cancel" "(done_dialog)(exit)")
        
        ;内外・ＫＤピッチ
        (action_tile "NG" "(setq DEF $KEY)")  ;内外 190
        (action_tile "KD" "(setq DEF $KEY)")  ;ＫＤ 220
        ;実線・点線
        (action_tile "J" "(setq JT \"J\")")
        (action_tile "T" "(setq JT \"T\")")
        ;CHの種類
        (setq CH "NOR")
        (action_tile "NOR" "(setq CH \"NOR\")")
        (action_tile "CHO" "(setq CH \"CHO\")")
        (action_tile "KAN" "(setq CH \"KAN\")")
        (setq WHAT_NEXT (start_dialog))
    );end while    
    (unload_dialog DCL_ID)(princ)
    (cond ((= WHAT_NEXT 0)(setq DEF DEF_BAK)(princ))
          ((= WHAT_NEXT 1) DEF)
    )
    (cond ((= DEF "NG") (setq PT "NG"))
          ((= DEF "KD") (setq PT "KD"))
    )
    (princ "\nPT=")(princ PT)
    (princ "\nJT=")(princ JT)
    (princ "\nCH=")(princ CH)
;---------------------------------------------------------------------------------------------------
    (setq PC (getpoint "\n挿入点を指示..."))
    (setq PX (getint   "\n何個、入力しますか？:"))
    (cond ((= PX 1) (setq LR (getint "\n左右どちらに挿入しますか？ 左 = 1, 右 = 2 ："))))
    (setq LPR (FIX (/ PX 2)))
    (setq LPL (FIX (+ 0.5 (/ PX 2.0))))  (setq LPC (FIX (+ 0.5 (/ PX 2.0))))
    (setq PCL (cadr PC))                 (setq PCR (cadr PC))
    (if (= PN "1P")
        (progn
            (setq PCCL (- (cadr PC) 2)) (setq PCCR (- (cadr PC) 2));1P CHの位置
            (setq BRH 25)                                          ;ﾌﾞﾚｰｶｰの高さ
        )
    )
    (if (= PN "2P")
        (progn
            (setq PCCL (- (cadr PC) 12)) (setq PCCR (- (cadr PC) 12));2P CHの位置
            (setq BRH 50)                                            ;ﾌﾞﾚｰｶｰの高さ
        )
    )
    (if (= PN "3P")
        (progn
            (setq PCCL (- (cadr PC) 24)) (setq PCCR (- (cadr PC) 24));3P CHの位置
            (setq BRH 75)                                            ;ﾌﾞﾚｰｶｰの高さ
        )
    )
    (if (= PN "4P")
        (progn
            (setq PCCL (- (cadr PC) 2)) (setq PCCR (- (cadr PC) 2))  ;SB CHの位置
            (setq BRH 100)                                           ;ﾌﾞﾚｰｶｰの高さ
        )
    )
    (if (= PN "SB")
        (progn
            (setq PCCL (- (cadr PC) 2)) (setq PCCR (- (cadr PC) 2))  ;SB CHの位置
            (setq BRH 33)                                            ;ﾌﾞﾚｰｶｰの高さ
        )
    )
    (if (= PN "5P");伝送
        (progn
            (setq PCCL (- (cadr PC) 2)) (setq PCCR (- (cadr PC) 2))  ;伝送 CHの位置
            (setq BRH 125)                                           ;ﾌﾞﾚｰｶｰの高さ
        )
    )
    (if (= PN "7P");調光T/U 1500W
        (progn
            (setq PCCL (- (cadr PC) 2)) (setq PCCR (- (cadr PC) 2))  ;伝送 CHの位置
            (setq BRH 175)                                           ;ﾌﾞﾚｰｶｰの高さ
        )
    )

    ;サブルーチンに分岐 １個入力の場合
    (if (= PT "NG")(setq PL PC))(if (= PT "NG")(setq PR PC))
    (if (= PT "KD")(setq PL (list(- (car PC) 15)(cadr PC))))
    (if (= PT "KD")(setq PR (list(+ (car PC) 15)(cadr PC))))
    ;実線・点線
    (if (= JT "J")(setq BRL T11 BRR T12));(princ "\nJT=")(princ JT)
    (if (= JT "T")(setq BRL T13 BRR T14));(princ "\nBRL=")(princ BRL)(princ "\nBRR=")(princ BRR)
;-------------------------------------------------------------------------------------------------------------------------
    (if (= CH "NOR")(progn (setq CHLSYM "CH-L2") (setq CHRSYM "CH-R2") (setq KATA "CS-5")(setq MEIKA "30")))
    (if (= CH "CHO")(progn (setq CHLSYM "OCH-L2")(setq CHRSYM "OCH-R2")(setq KATA "CS-2")(setq MEIKA "30")))
    (if (= CH "KAN")(progn (setq CHLSYM "OCH-L2")(setq CHRSYM "OCH-R2")(setq KATA "CH-EG3号 （難燃性）")(setq MEIKA "22")))

    ;１個入力の場合
    (if (= LR 1)
        (progn (command "INSERT" BRL PL "1" "1" "0" NM "" "" "" "" "")
            (if (or (= PNA "MCB")(= PNA "SB")(= PNA "TM"))
                (command "INSERT" CHLSYM (list (CAR PC) PCCL) "" "" "" "CH" "" KATA "" "" "" "" "" "" "" "" MEIKA "1" "" "")
            )
        )
    )
    (if (= LR 2)
        (progn (command "INSERT" BRR PR "1" "1" "0" NM "" "" "" "" "")
            (if (or (= PNA "MCB")(= PNA "SB")(= PNA "TM"))
                (command "INSERT" CHRSYM (list (CAR PC) PCCR) "" "" "" "CH" "" KATA "" "" "" "" "" "" "" "" MEIKA "1" "" "")
            )
        )
    )
    
    ;バーサポートの設定                           バーサポートを入れる箇所の個数
    (if (= PN "1P")(setq INn 26));1P
    (if (= PN "2P")(setq INn 13));2P 
    (if (= PN "3P")(setq INn 8)) ;3P 
    (if (= PN "SB")(setq INn 20));SB 
    (if (= INn nil)(setq INn 1000));その他
    (setq SPNL1 (- LPL INn)) (setq SPNL2 (- LPL (* INn 2))) (setq SPNL3 (- LPL (* INn 3)))
    (setq SPNR1 (- LPR INn)) (setq SPNR2 (- LPR (* INn 2))) (setq SPNR3 (- LPR (* INn 3)))
    (if (or (= PN "1P")(= PN "2P")(= PN "3P"))(setq SPH 25))
    (if (= PN "SB")(setq SPH 33))
    
;左側複数入力
    (while (and (>= LPL 1)(/= PX 1));  (princ "\nLPL=")(princ LPL)
            ;;(princ "\nSPNL1=")(princ SPNL1)
            ;;(princ "\nSPNL2=")(princ SPNL2)
            ;;(princ "\nSPNL3=")(princ SPNL3)
            ;;(princ "\nLPL=")(princ LPL)
        (if (or (= LPL SPNL1)(= LPL SPNL2)(= LPL SPNL3))
            (progn
                (command "INSERT" "DBARSP" (list (CAR PL) PCL) "1" "1" "0")
                (setq PCL (- PCL SPH)) (setq PCCL (- PCCL SPH))
                (if (= LPL SPNL1)(setq SPNL1 0))
                (if (= LPL SPNL2)(setq SPNL2 0))
                (if (= LPL SPNL3)(setq SPNL3 0))
            )
            (progn ;else
                (command "INSERT" BRL (list (CAR PL) PCL) "1" "1" "0" NM "" "" "" "" "")
                (if (or (= PNA "MCB")(= PNA "SB")(= PNA "TM"))
                    (command "INSERT" CHLSYM (list (CAR PC) PCCL) "" "" "" "CH" "" KATA "" "" "" "" "" "" "" "" MEIKA "1" "" "")
                )
                (setq PCL (- PCL BRH))(setq PCCL (- PCCL BRH))
                (setq LPL (1- LPL))   (setq LPC (1- LPC))
            )
        );endif
    );while

;右側複数入力
    (while (and (>= LPR 1)(/= PX 1));  (princ "\nLPL=")(princ LPL)
        (if (or (= LPR SPNR1)(= LPR SPNR2)(= LPR SPNR3))
            (progn
                (setq PCR (- PCR SPH)) (setq PCCR (- PCCR SPH))
                ;;(setq LPR (1- LPR))    (setq LPC (1- LPC))
                (if (= LPR SPNR1)(setq SPNR1 0));
                (if (= LPR SPNR2)(setq SPNR2 0));
                (if (= LPR SPNR3)(setq SPNR3 0));
            )
            (progn ;else
                (command "INSERT" BRR (LIST (CAR PR) PCR) "1" "1" "0" NM "" "" "" "" "")
                (if (or (= PNA "MCB")(= PNA "SB")(= PNA "TM"))
                    (command "INSERT" CHRSYM (list (CAR PC) PCCR) "" "" "" "CH" "" KATA "" "" "" "" "" "" "" "" MEIKA "1" "" "")
                )
                (setq PCR (- PCR BRH))(setq PCCR (- PCCR BRH))
                (setq LPR (1- LPR))   (setq LPC (1- LPC))
            )
        )
     );while
    (command "REDRAW")
)

;;===============================================================================================
;;91.盤一覧表作成リスプ     BanList.LSP
;;===============================================================================================
;;盤一覧表１枚目
(defun C:Banlist1(/ DWG Ln DWG1 DWG2 FN01 F01 A01 IND01 L01 P01 P02 P03 BAN01 MEN01 BIK01)
   (setvar "cmdecho" 0) (setvar "blipmode" 0)
   (SETQ DWG (GETVAR "DWGNAME"))

   ;;(SETQ Ln (STRLEN DWG))
   ;;(SETQ DWG1 (SUBSTR DWG 1 (- Ln 4)))
   (SETQ DWG2 (SUBSTR DWG 1 5))
   (STARTAPP "VBeep" "")
   ;;(COMMAND "TEXT" "S" "M0" "3400,45" "40" "" DWG1)

   ;;テキストファイルの読み込みと一覧表に記入
   (SETQ FN01 (STRCAT "Z:\\LIST-TXT\\" DWG2 ".TXT"))
   (SETQ F01 (OPEN  FN01 "r"))
   (COND ((/= F01 nil)
      (SETQ A01 0)
      (SETQ IND01 1)
      (SETQ L01 (READ-LINE F01))
      (SETQ P01 (LIST 2250 2425))
      (SETQ P02 (LIST 3425 2425))
      (SETQ P03 (LIST 2125 2425))
      (WHILE (AND (/= L01 nil) (<= IND01 45))
         (SETQ P01 (LIST (CAR P01) (+ (CADR P01) A01)))
         (SETQ P02 (LIST (CAR P02) (+ (CADR P02) A01)))
         (SETQ P03 (LIST (CAR P03) (+ (CADR P03) A01)))
         (SETQ BAN01 (SUBSTR L01 31 30))
         (SETQ MEN01 (SUBSTR L01 63 3))
         (SETQ BIK01 (SUBSTR L01 61 2))
         (COMMAND "TEXT" "S" "M0" P01 "30" "" BAN01)
         (COMMAND "TEXT" "S" "M0" P02 "30" "" MEN01)
         (COMMAND "TEXT" "S" "M0" P03 "30" "" BIK01)
         (SETQ A01 -50)
         (SETQ IND01 (+ IND01 1))
         (SETQ L01 (READ-LINE F01))
      )
      (COND ((AND (= IND01 46) (/= L01 nil))
         (TEXTPAGE)
         (princ "\n-----------------------------------------")
         (princ "\n   データが残っています。                ")
         (princ "\n   製作盤一覧表をもう一枚作成して下さい  ")
         (princ "\n-----------------------------------------")
      ))
      (CLOSE F01)
   ))
   (setvar "blipmode" 1) (setvar "cmdecho" 1)
)
;;===============================================================================================
;;盤一覧表２枚目
(defun C:Banlist2(/ DWG Ln DWG1 DWG2 FN01 F01 A01 IND01 IND02 L01 P01 P02 P03 BAN01 MEN01 BIK01)
	(setvar "cmdecho" 0) (setvar "blipmode" 0)
	(SETQ DWG (GETVAR "DWGNAME"))

	 ;;(SETQ Ln (STRLEN DWG))
	 ;;(SETQ DWG1 (SUBSTR DWG 1 (- Ln 4)))
	 (SETQ DWG2 (SUBSTR DWG 1 5))
	 (STARTAPP "VBeep" "")
	 ;;(COMMAND "TEXT" "S" "M0" "3400,45" "40" "" DWG1)

	 ;;テキストファイルの読み込みと一覧表に記入
	 (SETQ FN01 (STRCAT "Z:\\LIST-TXT\\" DWG2 ".TXT"))
	 (SETQ F01 (OPEN  FN01 "r"))
	 (COND ((/= F01 nil)
	  (SETQ A01 0)
	  (SETQ IND01 46)
	  (SETQ IND02 1)
	  
	  (WHILE (>= IND01 IND02)
	   (SETQ L01 (READ-LINE F01))
	   (SETQ IND02 (+ IND02 1))
	  )
	  
	  (SETQ P01 (LIST 2250 2425))
	  (SETQ P02 (LIST 3425 2425))
	  (SETQ P03 (LIST 2125 2425))
	  (WHILE (AND (/= L01 nil) (<= IND01 90))
	   (SETQ P01 (LIST (CAR P01) (+ (CADR P01) A01)))
	   (SETQ P02 (LIST (CAR P02) (+ (CADR P02) A01)))
	   (SETQ P03 (LIST (CAR P03) (+ (CADR P03) A01)))
	   (SETQ BAN01 (SUBSTR L01 31 30))
	   (SETQ MEN01 (SUBSTR L01 63 3))
	   (SETQ BIK01 (SUBSTR L01 61 2))
	   (COMMAND "TEXT" "S" "M0" P01 "30" "" BAN01)
	   (COMMAND "TEXT" "S" "M0" P02 "30" "" MEN01)
	   (COMMAND "TEXT" "S" "M0" P03 "30" "" BIK01)
	   (SETQ A01 -50)
	   (SETQ IND01 (+ IND01 1))
	   (SETQ L01 (READ-LINE F01))
	  )
	 (COND ((AND (= IND01 91) (/= L01 nil))
	 (TEXTPAGE)
	  (princ "\n----------------------------------------------")
	  (princ "\n    データが残っています。                    ")
	  (princ "\n    製作盤一覧表をもう一枚作成して下さい。    ")
	  (princ "\n----------------------------------------------")
	 ))
	 (CLOSE F01)
	))
	(setvar "blipmode" 1) (setvar "cmdecho" 1)
)
;;===============================================================================================
;;盤一覧表３枚目
(defun C:Banlist3(/ DWG Ln DWG1 DWG2 FN01 F01 A01 IND01 IND02 L01 P01 P02 P03 BAN01 MEN01 BIK01)
	(setvar "cmdecho" 0) (setvar "blipmode" 0)
	(SETQ DWG (GETVAR "DWGNAME"))

	 ;;(SETQ Ln (STRLEN DWG))
	 ;;(SETQ DWG1 (SUBSTR DWG 1 (- Ln 4)))
	 (SETQ DWG2 (SUBSTR DWG 1 5))
	 (STARTAPP "VBeep" "")
	 ;;(COMMAND "TEXT" "S" "M0" "3400,45" "40" "" DWG1)

	 ;;テキストファイルの読み込みと一覧表に記入
	 (SETQ FN01 (STRCAT "Z:\\LIST-TXT\\" DWG2 ".TXT"))
	 (SETQ F01 (OPEN  FN01 "r"))
	 (COND ((/= F01 nil)
	  (SETQ A01 0)
	  (SETQ IND01 91)
	  (SETQ IND02 1)
	  
	  (WHILE (>= IND01 IND02)
	   (SETQ L01 (READ-LINE F01))
	   (SETQ IND02 (+ IND02 1))
	  )
	  
	  (SETQ P01 (LIST 2250 2425))
	  (SETQ P02 (LIST 3425 2425))
	  (SETQ P03 (LIST 2125 2425))
	  (WHILE (AND (/= L01 nil) (<= IND01 135))
	   (SETQ P01 (LIST (CAR P01) (+ (CADR P01) A01)))
	   (SETQ P02 (LIST (CAR P02) (+ (CADR P02) A01)))
	   (SETQ P03 (LIST (CAR P03) (+ (CADR P03) A01)))
	   (SETQ BAN01 (SUBSTR L01 31 30))
	   (SETQ MEN01 (SUBSTR L01 63 3))
	   (SETQ BIK01 (SUBSTR L01 61 2))
	   (COMMAND "TEXT" "S" "M0" P01 "30" "" BAN01)
	   (COMMAND "TEXT" "S" "M0" P02 "30" "" MEN01)
	   (COMMAND "TEXT" "S" "M0" P03 "30" "" BIK01)
	   (SETQ A01 -50)
	   (SETQ IND01 (+ IND01 1))
	   (SETQ L01 (READ-LINE F01))
	  )
	 (COND ((AND (= IND01 136) (/= L01 nil))
	 (TEXTPAGE)
	  (princ "\n----------------------------------------------")
	  (princ "\n    データが残っています。                    ")
	  (princ "\n    製作盤一覧表をもう一枚作成して下さい。    ")
	  (princ "\n----------------------------------------------")
	 ))
	 (CLOSE F01)
	))
	(setvar "blipmode" 1) (setvar "cmdecho" 1)
)
;;===============================================================================================
;;盤一覧表４枚目
(defun C:Banlist4(/ DWG Ln DWG1 DWG2 FN01 F01 A01 IND01 IND02 L01 P01 P02 P03 BAN01 MEN01 BIK01)
	(setvar "cmdecho" 0) (setvar "blipmode" 0)
	(SETQ DWG (GETVAR "DWGNAME"))

	 ;;(SETQ Ln (STRLEN DWG))
	 ;;(SETQ DWG1 (SUBSTR DWG 1 (- Ln 4)))
	 (SETQ DWG2 (SUBSTR DWG 1 5))
	 (STARTAPP "VBeep" "")
	 ;;(COMMAND "TEXT" "S" "M0" "3400,45" "40" "" DWG1)

	 ;;テキストファイルの読み込みと一覧表に記入
	 (SETQ FN01 (STRCAT "Z:\\LIST-TXT\\" DWG2 ".TXT"))
	 (SETQ F01 (OPEN  FN01 "r"))
	 (COND ((/= F01 nil)
	  (SETQ A01 0)
	  (SETQ IND01 136)
	  (SETQ IND02 1)
	  
	  (WHILE (>= IND01 IND02)
	   (SETQ L01 (READ-LINE F01))
	   (SETQ IND02 (+ IND02 1))
	  )
	  
	  (SETQ P01 (LIST 2250 2425))
	  (SETQ P02 (LIST 3425 2425))
	  (SETQ P03 (LIST 2125 2425))
	  (WHILE (AND (/= L01 nil) (<= IND01 180))
	   (SETQ P01 (LIST (CAR P01) (+ (CADR P01) A01)))
	   (SETQ P02 (LIST (CAR P02) (+ (CADR P02) A01)))
	   (SETQ P03 (LIST (CAR P03) (+ (CADR P03) A01)))
	   (SETQ BAN01 (SUBSTR L01 31 30))
	   (SETQ MEN01 (SUBSTR L01 63 3))
	   (SETQ BIK01 (SUBSTR L01 61 2))
	   (COMMAND "TEXT" "S" "M0" P01 "30" "" BAN01)
	   (COMMAND "TEXT" "S" "M0" P02 "30" "" MEN01)
	   (COMMAND "TEXT" "S" "M0" P03 "30" "" BIK01)
	   (SETQ A01 -50)
	   (SETQ IND01 (+ IND01 1))
	   (SETQ L01 (READ-LINE F01))
	  )
	 (COND ((AND (= IND01 181) (/= L01 nil))
	 (TEXTPAGE)
	  (princ "\n----------------------------------------------")
	  (princ "\n    データが残っています。                    ")
	  (princ "\n    製作盤一覧表をもう一枚作成して下さい。    ")
	  (princ "\n----------------------------------------------")
	 ))
	 (CLOSE F01)
	))
	(setvar "blipmode" 1) (setvar "cmdecho" 1)
)
;;===============================================================================================
;;盤一覧表５枚目
(defun C:Banlist5(/ DWG Ln DWG1 DWG2 FN01 F01 A01 IND01 IND02 L01 P01 P02 P03 BAN01 MEN01 BIK01)
	(setvar "cmdecho" 0) (setvar "blipmode" 0)
	(SETQ DWG (GETVAR "DWGNAME"))

	 ;;(SETQ Ln (STRLEN DWG))
	 ;;(SETQ DWG1 (SUBSTR DWG 1 (- Ln 4)))
	 (SETQ DWG2 (SUBSTR DWG 1 5))
	 (STARTAPP "VBeep" "")
	 ;;(COMMAND "TEXT" "S" "M0" "3400,45" "40" "" DWG1)

	 ;;テキストファイルの読み込みと一覧表に記入
	 (SETQ FN01 (STRCAT "Z:\\LIST-TXT\\" DWG2 ".TXT"))
	 (SETQ F01 (OPEN  FN01 "r"))
	 (COND ((/= F01 nil)
	  (SETQ A01 0)
	  (SETQ IND01 181)
	  (SETQ IND02 1)
	  
	  (WHILE (>= IND01 IND02)
	   (SETQ L01 (READ-LINE F01))
	   (SETQ IND02 (+ IND02 1))
	  )
	  
	  (SETQ P01 (LIST 2250 2425))
	  (SETQ P02 (LIST 3425 2425))
	  (SETQ P03 (LIST 2125 2425))
	  (WHILE (AND (/= L01 nil) (<= IND01 225))
	   (SETQ P01 (LIST (CAR P01) (+ (CADR P01) A01)))
	   (SETQ P02 (LIST (CAR P02) (+ (CADR P02) A01)))
	   (SETQ P03 (LIST (CAR P03) (+ (CADR P03) A01)))
	   (SETQ BAN01 (SUBSTR L01 31 30))
	   (SETQ MEN01 (SUBSTR L01 63 3))
	   (SETQ BIK01 (SUBSTR L01 61 2))
	   (COMMAND "TEXT" "S" "M0" P01 "30" "" BAN01)
	   (COMMAND "TEXT" "S" "M0" P02 "30" "" MEN01)
	   (COMMAND "TEXT" "S" "M0" P03 "30" "" BIK01)
	   (SETQ A01 -50)
	   (SETQ IND01 (+ IND01 1))
	   (SETQ L01 (READ-LINE F01))
	  )
	 (COND ((AND (= IND01 226) (/= L01 nil))
	 (TEXTPAGE)
	  (princ "\n----------------------------------------------")
	  (princ "\n    データが残っています。                    ")
	  (princ "\n    製作盤一覧表をもう一枚作成して下さい。    ")
	  (princ "\n----------------------------------------------")
	 ))
	 (CLOSE F01)
	))
	(setvar "blipmode" 1) (setvar "cmdecho" 1)
)
;;===============================================================================================
;;盤一覧表６枚目
(defun C:Banlist6(/ DWG Ln DWG1 DWG2 FN01 F01 A01 IND01 IND02 L01 P01 P02 P03 BAN01 MEN01 BIK01)
	(setvar "cmdecho" 0) (setvar "blipmode" 0)
	(SETQ DWG (GETVAR "DWGNAME"))

	 ;;(SETQ Ln (STRLEN DWG))
	 ;;(SETQ DWG1 (SUBSTR DWG 1 (- Ln 4)))
	 (SETQ DWG2 (SUBSTR DWG 1 5))
	 (STARTAPP "VBeep" "")
	 ;;(COMMAND "TEXT" "S" "M0" "3400,45" "40" "" DWG1)

	 ;;テキストファイルの読み込みと一覧表に記入
	 (SETQ FN01 (STRCAT "Z:\\LIST-TXT\\" DWG2 ".TXT"))
	 (SETQ F01 (OPEN  FN01 "r"))
	 (COND ((/= F01 nil)
	  (SETQ A01 0)
	  (SETQ IND01 226)
	  (SETQ IND02 1)
	  
	  (WHILE (>= IND01 IND02)
	   (SETQ L01 (READ-LINE F01))
	   (SETQ IND02 (+ IND02 1))
	  )
	  
	  (SETQ P01 (LIST 2250 2425))
	  (SETQ P02 (LIST 3425 2425))
	  (SETQ P03 (LIST 2125 2425))
	  (WHILE (AND (/= L01 nil) (<= IND01 270))
	   (SETQ P01 (LIST (CAR P01) (+ (CADR P01) A01)))
	   (SETQ P02 (LIST (CAR P02) (+ (CADR P02) A01)))
	   (SETQ P03 (LIST (CAR P03) (+ (CADR P03) A01)))
	   (SETQ BAN01 (SUBSTR L01 31 30))
	   (SETQ MEN01 (SUBSTR L01 63 3))
	   (SETQ BIK01 (SUBSTR L01 61 2))
	   (COMMAND "TEXT" "S" "M0" P01 "30" "" BAN01)
	   (COMMAND "TEXT" "S" "M0" P02 "30" "" MEN01)
	   (COMMAND "TEXT" "S" "M0" P03 "30" "" BIK01)
	   (SETQ A01 -50)
	   (SETQ IND01 (+ IND01 1))
	   (SETQ L01 (READ-LINE F01))
	  )
	 (COND ((AND (= IND01 271) (/= L01 nil))
	 (TEXTPAGE)
	  (princ "\n----------------------------------------------")
	  (princ "\n    データが残っています。                    ")
	  (princ "\n    製作盤一覧表をもう一枚作成して下さい。    ")
	  (princ "\n----------------------------------------------")
	 ))
	 (CLOSE F01)
	))
	(setvar "blipmode" 1) (setvar "cmdecho" 1)
      )
;;===============================================================================================
;;ＡＳＡＶＥで書出したファイルから銘板用文字デ－タ－を銘板表に読み込む
;;                                                 1998/May/24     By T.Terada
;; フォルダ－ "NP"の存在するドライブ名
;; DRV-ME = D: <========== ACLT.LSPで定義されてます。
;;===============================================================================================
;;銘板データ１枚目
(defun C:NpAuto1(/ DRV-ME DWG01 DWG02 fp01 ind01 l01 p01)
	(SETQ DRV-ME "X:")
	  (setvar "cmdecho" 0) (setvar "blipmode" 0)
	  (SETQ DWG01 (GETVAR "DWGNAME"))
	  (SETQ DWG02 (STRCAT DRV-ME "/NP/" (SUBSTR DWG01 1 8) ".NP1"))
	  (setq fp01 (open DWG02 "r"))
	        (princ "\n銘板一覧のファイル名：")(PRIN1 DWG01)
	        (princ "\n読み込みファイル名　：")(PRIN1 DWG02)
	  (setq ind01 1)
	  (cond ((= fp01 nil)
	         (princ "\nﾌｧｲﾙが見つかりません！ 図面の名称が間違っていませんか？"))
	         ((/= fp01 nil)
	          (setq l01 (read-line fp01))
	             (while (AND (/= l01 nil)(<= IND01 32))
	                    (cond 
	                           ;;左列 １３行
	                           ((= ind01 1) (setq p01 (list 725 1450))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 2) (<= ind01 13))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ;;中央列　１５行
	                          ((= ind01 14) (setq p01 (list 1875 1650))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 15) (<= ind01 28))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ;;右列　４行
	                          ((= ind01 29) (setq p01 (list 3025 550))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 30) (<= ind01 32))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                            )
	                      ); 2nd cond
	                     (setq l01 (read-line fp01))
	                     (setq ind01 (+ ind01 1))
	             ); while
	  ));   1st cond
	;;
	;;
	 (COND ((AND (= IND01 33) (/= L01 nil))
	 (TEXTPAGE)(STARTAPP "VBeep" "")
	    (princ "\n----------------------------------------")
	    (princ "\n  データが残っています。                ")
	    (princ "\n    銘板シ－トをもう一枚作成して下さい。")
	    (princ "\n----------------------------------------")
	    (princ "\n")
	  ))
	 (close fp01)
	 (setvar "cmdecho" 1) (setvar "blipmode" 1)
)
;;===============================================================================================
;;銘板データ２枚目
(defun C:NpAuto2(/ DRV-ME DWG01 DWG02 fp01 ind01 l01 p01)
	(SETQ DRV-ME "X:")
	  (setvar "cmdecho" 0) (setvar "blipmode" 0)
	  (SETQ DWG01 (GETVAR "DWGNAME"))
	  (SETQ DWG02 (STRCAT DRV-ME "/NP/" (SUBSTR DWG01 1 8) ".NP1"))
	  (setq fp01 (open DWG02 "r"))
	        (princ "\n銘板表のファイル名：")(PRIN1 DWG01)
	        (princ "\n読み込みファイル名：")(PRIN1 DWG02)
	  (cond ((= fp01 nil)
	         (princ "\nﾌｧｲﾙが見つかりません！ 図面の名称が間違っていませんか？"))
	         ((/= fp01 nil)
	         (setq ind01 33)
	         (setq ind02 1)
	            (WHILE (>= IND01 IND02)
	                   (setq l01 (read-line fp01))
	                   (SETQ IND02 (+ IND02 1))
	            )
	             (while (AND (/= l01 nil)(<= IND01 64))
	                    (cond 
	                           ;;左列 １３行
	                           ((= ind01 33) (setq p01 (list 725 1450))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 34) (<= ind01 45))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ;;中央列　１５行
	                          ((= ind01 46) (setq p01 (list 1875 1650))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 47) (<= ind01 60))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ;;右列　４行
	                          ((= ind01 61) (setq p01 (list 3025 550))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 62) (<= ind01 64))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                            )
	                      ); 2nd cond
	                     (setq l01 (read-line fp01))
	                     (setq ind01 (+ ind01 1))
	             ); while
	  ));   1st cond
	;;
	;;
	 (COND ((AND (= IND01 65) (/= L01 nil))
	 (TEXTPAGE)
	    (STARTAPP "VBeep" "")
	    (princ "\n----------------------------------------")
	    (princ "\n  データが残っています。                ")
	    (princ "\n    銘板シ－トをもう一枚作成して下さい。")
	    (princ "\n----------------------------------------")
	  ))
	 (close fp01)
	 (setvar "cmdecho" 1) (setvar "blipmode" 1)
)
;;===============================================================================================
;;銘板データ３枚目
(defun C:NpAuto3(/ DRV-ME DWG01 DWG02 fp01 ind01 l01 p01)
	(SETQ DRV-ME "X:")
	  (setvar "cmdecho" 0) (setvar "blipmode" 0)
	  (SETQ DWG01 (GETVAR "DWGNAME"))
	  (SETQ DWG02 (STRCAT DRV-ME "/NP/" (SUBSTR DWG01 1 8) ".NP1"))
	  (setq fp01 (open DWG02 "r"))
	        (princ "\n銘板一覧のファイル名：")(PRIN1 DWG01)
	        (princ "\n読み込みファイル名　：")(PRIN1 DWG02)
	  (cond ((= fp01 nil)
	        (princ "\nﾌｧｲﾙが見つかりません！ 図面の名称が間違っていませんか？"))
	        ((/= fp01 nil)
	        (setq ind01 65)
	        (setq ind02 1)
	            (WHILE (>= IND01 IND02)
	                   (setq l01 (read-line fp01))
	                   (SETQ IND02 (+ IND02 1))
	            )
	             (while (AND (/= l01 nil)(<= IND01 97))
	                    (cond 
	                           ;;左列 １３行
	                           ((= ind01 65) (setq p01 (list 725 1450))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 66) (<= ind01 77))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ;;中央列　１５行
	                          ((= ind01 78) (setq p01 (list 1875 1650))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 79) (<= ind01 92))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ;;右列　４行
	                          ((= ind01 93) (setq p01 (list 3025 550))
	                           (command "text" "s" "M0" p01 "30" "" l01)
	                           )
	                           ((and (>= ind01 94) (<= ind01 96))
	                                 (setq p01 (list (car p01) (- (cadr p01) 100)))
	                                 (command "text" "s" "M0" p01 "30" "" l01)
	                            )
	                      ); 2nd cond
	                     (setq l01 (read-line fp01))
	                     (setq ind01 (+ ind01 1))
	             ); while
	  ));   1st cond
	;;
	;;
	 (COND ((AND (= IND01 98) (/= L01 nil))
	 (TEXTPAGE)
	    (STARTAPP "VBeep" "")
	    (princ "\n----------------------------------------")
	    (princ "\n  データが残っています。                ")
	    (princ "\n  でもシ－トがもうありません。残念でした！")
	    (princ "\n----------------------------------------")
	  ))
	 (close fp01)
	 (setvar "cmdecho" 1) (setvar "blipmode" 1)
)
;;==================================================================================================
;;92.PickWire:配線条数を線分(LWPOLYLINEorLINE)上に挿入します。
;;==================================================================================================
;;2001.10.23                                          makeup by f.izawa
;;2001.10.24 角度判断でマイナスの値の処理を追加              by f.izawa
;;行62,63,64を改造してKDの図枠サイズ(1/10)に対応しただけ     By T.Terada

(defun c:pickw( / jousu sensu eflag str loop esel pick entname ent code1042 blkname
	pick_item item_no ang xc yc deg ret dimsc)
	(setq
		senkan		0.75 ;;斜線間の距離
		nagasa		0.75 ;;斜線の長さの1/2
		katamuki	60.0 ;;斜線の傾き
	)
	(setq dimsc (getvar "DIMSCALE"))
	(setvar "CMDECHO" 0)
	(setq jousu (getstring "\n条数(MAX 9)を入力  X:終了 <Enter=2> :"))
	(if (= jousu "")(setq jousu "2"))
	(setq jousu (strcase jousu))
	(if (/= str "X")
		(progn
			(setq str (substr jousu 1 1) sensu (atoi str) loop T)
			;;アース付き判断
			(setq eflag (and (= (strlen jousu) 2)(= (substr jousu 2 1) "E")))
			;;ブロック名を作成
			(setq blkname (strcat "I_WIRE_" str ))
			(if eflag (setq blkname (strcat blkname "E"))(setq blkname (strcat blkname "C")))
			;;ブロック名が無ければ、ブロックを作成する
			(if (not (tblsearch "BLOCK" blkname))
				(ifs_make_wireblock blkname sensu senkan nagasa katamuki eflag))
		)
	)
	(while loop
		(setq ret nil)
		(if (setq esel (entsel "\n条数を挿入する線分を指示 <Enter=終了> : "))
			(progn
				(setq pick (car (cdr esel)))
				(setq entname (car esel))
				(setq ent (entget entname))
				(if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
					(progn
						;;LWPLINEのEntityからcode10,code42を抜き出す
						(setq code1042 (get_lwp_code1042 ent))
						;;ピックした点に一番近い線分への垂線点と線分の位置を得る
						(setq pick_item (get_1042pos code1042 pick))
						(setq item_no (cadr pick_item) pick (car pick_item))
						;;ピックした線分の角度を得る
						(setq ang (angle (car (nth item_no code1042))(car (nth (1+ item_no) code1042))) ret T)
					)
				)
				(if (= (cdr (assoc 0 ent)) "LINE")
					(progn
						(setq pick (get_suisen_pos pick (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
						(setq ang (angle (cdr (assoc 10 ent))(cdr (assoc 11 ent))) ret T)
					)
				)
				(if ret
					(progn
						;;180度以内にする
						(if ang (setq ang (ang_to_ang ang pi)))
						(if (> ang (/ pi 2.0))(setq ang (- ang pi)))
						
						(setq xc (/ dimsc 5)); ORG = (setq xc dimsc)
;						(if (>= dimsc 200.0)(setq xc 76.666))
;						(if (and (<= dimsc	50.0)(> dimsc 1.0))(setq xc 133.333))
						(setq yc xc deg (/ (* ang 180.0) pi))
						;;2001.10.24 -範囲を追加
						(if (and (> deg 60.0)(< deg 120.0))
							(setq yc (* yc 1.0) xc (* xc -1.0))
							(if (and (< deg -60.0)(> deg -120.0))
								(setq yc (* yc -1.0) xc (* xc 1.0))
							)
						)
						;;(princ deg)
						(command "INSERT" blkname pick xc yc deg)
						(command "REDRAW")
					)
					(princ "\n線分ではありません")
				)
			)
			(setq loop nil)
		)
	)
	(princ)
)
;;ピックした点に近いPLINE要素を得るテスト
(defun c:pickt()
	(setvar "CMDECHO" 0)
	(initget "T C B")
	(if (not (setq kw (getkword "\n文字基点 B:下/C:中央/T:上 <Enter=B:下> : ")))
		(setq kw "B")
	)
	(setq txtH (getreal "\n文字高さ : "))
	(setq txtS (getstring "\n文字列 : "))
	(setq loop T)
	(while loop
		(if (setq esel (entsel "\nPick PLINEorLINE : "))
			(progn
				(setq pick (car (cdr esel)))
				(setq entname (car esel))
				(setq ent (entget entname))
				(if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
					(progn
						;;LWPLINEのEntityからcode10,code42を抜き出す
						(setq code1042 (get_lwp_code1042 ent))
						;;ピックした点に一番近い線分への垂線点と線分の位置を得る
						(setq pick_item (get_1042pos code1042 pick))
						(setq item_no (cadr pick_item) pick (car pick_item))
						;;ピックした線分の角度を得る
						(setq ang (angle (car (nth item_no code1042))(car (nth (1+ item_no) code1042))))
					)
				)
				(if (= (cdr (assoc 0 ent)) "LINE")
					(progn
						(setq pick (get_suisen_pos pick (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
						(setq ang (angle (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
					)
				)
				;;180度以内にする
				(if ang (setq ang (ang_to_ang ang pi)))
				(if (> ang (/ pi 2.0))(setq ang (- ang pi)))
				(command "TEXT" "J" (strcat kw "C") pick txtH (/ (* ang 180.0) pi) txtS)
				;;(command "TRIM" "SINGLE" "LAST" pick "")
			)
			(setq loop nil)
		)
	)
	(princ)
)
;;LWPOLYLINEのｺｰﾄﾞ10と42のリストを返す
;;ResEx:(((1.250 4.250 0.00) 0.00)....)
;;閉鎖されている時は、先頭を最後に追加する
(defun get_lwp_code1042( ent / item item1 item2 code10 res cflag)
	(foreach item ent
		(setq item1 (car item) item2 (cdr item))
		(cond
			((= item1 70)(if (or (= item2 1)(= item2 -127))(setq cflag T)))
			((= item1 10)(setq code10 item2))
			((= item1 42)(setq res (append res (list (list code10 item2)))))
		)
	)
	(if cflag (setq res (append res (list (car res)))))
	res;
)
;;点P0から線分P1-P2上の垂線の点を返す
(defun get_suisen_pos(p0 p1 p2 / kouten)
	(if (setq kouten (inters p0 (polar p0 (+ (angle p1 p2) (/ pi 2.0)) 100.0) p1 p2 nil))
		(if (not (equal (+ (distance kouten p1)(distance kouten p2))(distance p1 p2) 0.001))
			(setq kouten nil);
		)
	)
	kouten;
)
;;code10-42のリストから点posに一番近い要素への垂線の交点と、その要素の位置を返す
;;要：get_suisen_pos
;;ResEx:((1.23 4.56 0.00) 2))
(defun get_1042pos(lst1042 pos / loop i distmin dist res item1 item2 kouten)
	(setq i 0 cnt (1- (length lst1042)) loop T)
	(while loop
		(setq item1 (car (nth i lst1042)) item2 (car (nth (1+ i) lst1042)))
		(if (setq kouten (get_suisen_pos pos item1 item2))
			(progn
				(setq dist (distance pos kouten))
				(if (or (not distmin)(and distmin (> distmin dist)))
					(setq res (list kouten i) distmin dist)
				)
			)
		)
		(if (< i (1- cnt))(setq i (1+ i))(setq loop nil))
	)
	res;
)
;;angをang0以内にする
(defun ang_to_ang(ang maxang)
	(while (> ang maxang)
		(setq ang (- ang maxang))
	)
	ang;
)
(defun ifs_make_wireblock(
	blkname		;ブロック名
	cnt			;心線数
	pit			;斜線ピッチ
	hdis		;斜線長さの1/2
	deg			;斜線の傾き
	earth		;T=アース付き
	/ stx p0 p10 p11 ang)
	;;62:線色0=ByBlock 256=ByLayer
	;;8:画層
	(setq stx (* (/ (1- cnt) -2.0) pit) ang (/ (* deg pi) 180.0) i 0 count cnt)
	;;5C以上の時
	(if (> cnt 4) (setq stx (- stx (* pit 0.25))))
	(if earth (setq stx (- stx (* pit 0.75)) count (1+ cnt)))
	(entmake (list '(0 . "BLOCK")(cons 2 blkname)'(10 0.0 0.0 0.0)'(70 . 0)))
	(repeat count
		(setq p0 (list stx 0.0 0.0))
		(setq p10 (polar p0 ang hdis) p11 (polar p0 ang (* hdis -1.0)))
		(entmake (list '(0 . "LINE")'(8 . "0")(cons 10 p10)(cons 11 p11)'(62 . 0)))
		(setq stx (+ stx pit) i(1+ i))
		(if (and (> cnt 4)(= i 3))(setq stx (+ stx (* pit 0.25))))
		(if (and earth (= i cnt))(setq stx (+ stx (* pit 0.25)) ang (- pi ang)))
	)
	(entmake '((0 . "ENDBLK")))
)
;(princ "\nPickW:配線条数を線分(LWPOLYLINEorLINE)上に挿入します。")
(princ)

;;==================================================================================================
;;93. ３Ｐ端子台の回路入力   STB_INN.LSP DTB.DCL   2004/Aug/31 By T.Terada
;;==================================================================================================
(defun C:STB_M ( / T1 PNo LB1 MK QT)
;     (setq *error* *err1*)
     (setvar "CMDECHO" 1)
     ;*(setq DCL_ID (load_dialog "C:/USR/2KD.DCL"))    ;*テスト用
     (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))      ;*本番用
    ;(if (not (new_dialog "DTB" DCL_ID) )(EXIT))
     (if (not (new_dialog "DTB3PTB" DCL_ID) )(EXIT))
      (setq LB1 "３Ｐ端子の回路入力")(set_tile "LB1" LB1)
        (setq MK "")(setq T1 "")(setq PNo "")(setq LB1 "")
;(princ "\nここまでＯＫ")
;;  図枠の選択用ダイアログボックス
        (action_tile "cancel" "(done_dialog)(exit)")
        (action_tile "1"  "(setq MK \"76\")(setq T1 \"PT-20\")(setq QT \"3\")") ;PT-20を選択
        (action_tile "2"  "(setq MK \"76\")(setq T1 \"PT-30\")(setq QT \"3\")") ;PT-30を選択
        (action_tile "3"  "(setq MK \"76\")(setq T1 \"PT-40\")(setq QT \"3\")") ;PT-40を選択
        (action_tile "4"  "(setq MK \"76\")(setq T1 \"PT-80\")(setq QT \"3\")") ;PT-80を選択
        (action_tile "5"  "(setq MK \"76\")(setq T1 \"PT-90\")(setq QT \"3\")") ;PT-90を選択
        (action_tile "6"  "(setq MK \"76\")(setq T1 \"PT-150\")(setq QT \"3\")");PT-150を選択
        (action_tile "7"  "(setq MK \"76\")(setq T1 \"PT-200\")(setq QT \"3\")");PT-200を選択
        (action_tile "8"  "(setq MK \"76\")(setq T1 \"PT-300\")(setq QT \"3\")");PT-300を選択
        (action_tile "9"  "(setq MK \"76\")(setq T1 \"PT-400\")(setq QT \"3\")");PT-400を選択
        (action_tile "X"  "(setq MK \"76\")(setq T1 \"PT-600\")(setq QT \"3\")");PT-600を選択
        
        (action_tile "11"  "(setq MK \"42\")(setq T1 \"TS-802RK2\")(setq QT \"3\")") ;TS-802RK2を選択
        (action_tile "12"  "(setq MK \"42\")(setq T1 \"TS-803RK2\")(setq QT \"3\")") ;TS-803RK2を選択
        (action_tile "13"  "(setq MK \"42\")(setq T1 \"TS-804RK2\")(setq QT \"3\")") ;TS-804RK2を選択
        (action_tile "14"  "(setq MK \"42\")(setq T1 \"TS-805RK2\")(setq QT \"3\")") ;TS-805RK2を選択
        (action_tile "15"  "(setq MK \"42\")(setq T1 \"TS-806RK2\")(setq QT \"3\")") ;TS-806RK2を選択
        (action_tile "16"  "(setq MK \"42\")(setq T1 \"TS-807RK2\")(setq QT \"3\")") ;TS-807RK2を選択
        (action_tile "17"  "(setq MK \"42\")(setq T1 \"TS-808RK2\")(setq QT \"3\")") ;TS-808RK2を選択
        (action_tile "18"  "(setq MK \"42\")(setq T1 \"TS-809RK2\")(setq QT \"3\")") ;TS-809RK2を選択
        (action_tile "19"  "(setq MK \"42\")(setq T1 \"TS-810RK2\")(setq QT \"3\")") ;TS-810RK2を選択
        (action_tile "1X"  "(setq MK \"42\")(setq T1 \"TS-811RK2\")(setq QT \"3\")") ;TS-811RK2を選択

        (action_tile "40"  "(setq MK \"108\")(setq T1 \"TM-150-3PS\")(setq QT \"1\")") ;TM-150-Sを選択
        (action_tile "41"  "(setq MK \"108\")(setq T1 \"TM-200-3PS\")(setq QT \"1\")") ;TM-200-Sを選択
        (action_tile "42"  "(setq MK \"108\")(setq T1 \"TM-300-3PS\")(setq QT \"1\")") ;TM-300Sを選択
        (action_tile "43"  "(setq MK \"108\")(setq T1 \"TM-400-3PS\")(setq QT \"1\")") ;TM-400-Sを選択
        (action_tile "44"  "(setq MK \"108\")(setq T1 \"TMD-500-3PS\")(setq QT \"1\")") ;TMD-500-Sを選択
        (action_tile "45"  "(setq MK \"108\")(setq T1 \"TMD-600-3PS\")(setq QT \"1\")") ;TMD-600-Sを選択
    (start_dialog)(unload_dialog DCL_ID)(princ)
    (setq PNo (getstring "\n端子番号を指示:(例：-1)"))
              (cond ((= PNo "") (setq PNo "-1")))(setq PNo (strcat "TB" PNo))
    (princ "\n挿入点を指示")
    (command "INSERT" "M-TB" pause "1" "1" "0" "TB" PNo T1 "" "" "" "" "" "" "" "" MK QT "" "")  ;*端子台の挿入する
    (setvar "CMDECHO" 0)(redraw)
)
;;==================================================================================================
;; .窓ガラスを書く    MADO.LSP     2005/Oct   2020/OCT  IJCAD対応                 by T.Terada
;;==================================================================================================
(defun C:MADO(/ PC WW HH H2 W2 PLU PRD)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(command "COLOR" 4)
   (setvar "OSMODE" 0)
       (setq WW (getint   "\n 窓の横幅を指示:"))       
       (setq HH (getint   "\n 窓の高さを指示:"))
       (setq W2 (/ WW 2))
       (setq H2 (/ HH 2))
       (setq PC (getpoint "\n 窓の中心を指示...."))
       (setq PLU (list (- (car PC) W2)(+ (cadr PC) H2)))
       (setq PRD (list (+ (car PC) W2)(- (cadr PC) H2)))
       (command "RECTANG" "F" 50 "W" 15 PLU PRD)
       (command "TEXT" "S" "M0" "C" PC "40" "0" "ガラス")(command "COLOR" 4)
       (command "-BHATCH" "P" "ANSI31" "20" "0" PC "")(princ)
   (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
   (redraw)(princ "\n描画完了。")(princ)
)


;;=================================================================================================
;;97. マグネット主回路の選定          S_MC.LSP  S_MC.DCL    2005/Feb/06 By T.Terada                
;;=================================================================================================
;; ＭＳのデーターはX:\XLS\M2NFLS.xlsx, M4NFLS.xlsx, F2NFLS.xlsx, F4NFLS.xlsxを使ってます。
;; ＭＣのデーターはX:\XLS\MC-SEL.xlsxを使ってます。
;; (act_radioset 引数1 引数2) はラジオボタン用の set_tile 関数。KD.LSPに収納。
;;
(defun C:S_MC ( / DAT DAT1 DAT2 DEF DEF1 DEF_BAK DEF_BAK1 DCL_ID WHAT_NEXT DLG OPV SYM PC D1 D2 D3 D5 D6
                  A01 TP1 TP2 TP3 TP4 X SEL1 SEL GYO RETU 52T 52A 49A D52 Rasc PNA PNO T1 T2 T3 T4 T5 T6
                  T7 T8 T9 MN QT Z1 Z2 FILE PX1 49PNA PC PC1
              )
      (setq *error* *err1*)(setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (setq DAT1 nil DAT2 nil DAT nil)
;;--------------------------------------------------------------------------------------------------
;;ﾒｰｶｰの設定
      (C:MKR) ;基本メーカーを調べる
      (if (or (= MAKER "0")(= MAKER "2"))(progn (setq PX1 "M")(setq MN "1"))) ;三菱
      (if (or (= MAKER "1")(= MAKER "3"))(progn (setq PX1 "F")(setq MN "4"))) ;富士
      (princ "\n読込みに時間が掛かります。約１５秒、お待ち下さい。")
;;選定方法でダイアログを分岐
      (setq DEF_BAK  DEF)  (if (not DEF) (setq DEF "1"))
      (setq DEF_BAK1 DEF1) (if (not DEF1)(setq DEF1 "1"))
      (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
      (setq WHAT_NEXT 2)
      (while (>= WHAT_NEXT 2)
         (if (null (new_dialog "S_MC" DCL_ID) )(exit))
         (act_radioset '("V2" "V4") DEF)
             (action_tile "cancel" "(done_dialog)(exit)")
             (action_tile "V2"  "(setq DEF $key)")
             (action_tile "V4"  "(setq DEF $key)")
             (if (= DEF  "1")(setq DEF  "V2"))
         (act_radioset '("OP2" "OP1") DEF1)
             (action_tile "OP2" "(setq DEF1 $key)")
             (action_tile "OP1" "(setq DEF1 $key)")
             (if (= DEF1 "1")(setq DEF1 "OP2"))
         (action_tile "KW" "(setq DLG \"KW\")")
         (action_tile "AA" "(setq DLG \"AA\")")
         (action_tile "BU" "(setq DLG \"BU\")")
         (setq WHAT_NEXT (start_dialog))
      )
      (unload_dialog DCL_ID)
      (cond ((= WHAT_NEXT 0)(setq DEF  DEF_BAK) (princ)) ((= WHAT_NEXT 1) DEF))
      (cond ((= WHAT_NEXT 0)(setq DEF1 DEF_BAK1)(princ)) ((= WHAT_NEXT 1) DEF1))
      (cond ((= DEF "V2") (setq VV "200V"))((= DEF "V4") (setq VV "400V")))      ;回路電圧
      (cond ((= DEF1 "OP2") (setq OPV "AC200V"))((= DEF1 "OP1") (setq OPV "AC100V")));操作電圧
;;==================================================================================================
    ;;ＫＷで選定
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (if (= DLG "KW")
         (progn
            (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
            (if (not (new_dialog "MC_KW" DCL_ID) )(exit))
               (action_tile "cancel" "(done_dialog)(exit)")
               (action_tile "0.2K"   "(setq A01 \"0.2\")")
               (action_tile "0.4K"   "(setq A01 \"0.4\")")
               (action_tile "0.75K"  "(setq A01 \"0.75\")")
               (action_tile "1.5K"   "(setq A01 \"1.5\")")
               (action_tile "2K"     "(setq A01 \"2.2\")")
               (action_tile "3K"     "(setq A01 \"3.7\")")
               (action_tile "5K"     "(setq A01 \"5.5\")")
               (action_tile "7K"     "(setq A01 \"7.5\")")
               (action_tile "11K"    "(setq A01 \"11.0\")")
               (action_tile "15K"    "(setq A01 \"15.0\")")
               (action_tile "18K"    "(setq A01 \"18.5\")")
               (action_tile "22K"    "(setq A01 \"22.0\")")
               (action_tile "30K"    "(setq A01 \"30.0\")")
               (action_tile "37K"    "(setq A01 \"37.0\")")
               (action_tile "45K"    "(setq A01 \"45.0\")")
               (action_tile "55K"    "(setq A01 \"55.0\")")
            (start_dialog)
            (unload_dialog DCL_ID)(princ)
          ;;ﾃﾞｰﾀｰの読込
            (if (= VV "200V")(setq TP3 "2"))
            (if (= VV "400V")(setq TP3 "4"))
            (setq GYO 5)(setq TP1 "LS")(setq TP4 "NF")
            (cond                ;  C                          D                          E
               ((= A01 "0.2") (setq X 67))((= A01 "0.4") (setq X 68))((= A01 "0.75")(setq X 69))
               ((= A01 "1.5") (setq X 70))((= A01 "2.2") (setq X 71))((= A01 "3.7") (setq X 72))
               ((= A01 "5.5") (setq X 73))((= A01 "7.5") (setq X 74))((= A01 "11.0")(setq X 75))
               ((= A01 "15.0")(setq X 76))((= A01 "18.5")(setq X 77))((= A01 "22.0")(setq X 78))
               ((= A01 "30.0")(setq X 79))((= A01 "37.0")(setq X 80))((= A01 "45.0")(setq X 81))
               ((= A01 "55.0")(setq X 82));82=R
            )
;---------------------------------------------------------------------------------------------------
            (setq FILE (strcat "X:/XLSX/" PX1 TP3 TP4 TP1 ".xlsx"))            
            (princ "\nFILE=")(princ FILE)(princ "\nA01=")(princ A01)(princ "\nX=")(princ X)
              (repeat 35                                ;読込む行の数 ;回路ﾃﾞｰﾀｰ DAT1                  
                  (setq CELL (strcat (chr X) (itoa GYO)))                  
                  (REX)
;                  (princ "\nCELL=")(princ CELL)
                  (setq DAT1 (append DAT1 (list value)))
                  (setq GYO (+ GYO 1))
               )
               (setq GYO 51)
               (repeat 9                                 ;読込む行の数 ;外形ﾃﾞｰﾀｰ DAT2
                  (setq CELL (strcat (chr X) (itoa GYO)))
                  (REX)
;                  (princ "\nCELL=")(princ CELL)
                  (setq DAT2 (append DAT2 (list value)))
                  (setq GYO (+ GYO 1))
               )
;              (princ "\nDAT1=")(princ DAT1)(prompt "\n")  (princ "\nDAT2=")(princ DAT2)(prompt "\n")
;---------------------------------------------------------------------------------------------------           
           (setq 52T (nth 13 DAT1)) ; 52,42型式
           (setq 52A (nth 14 DAT1)) ; 52,42定格電流
           (setq 49A (nth 18 DAT1)) ; 49定格電流
           (setq D52 (nth 4 DAT2))  ; 52の外形ｼﾝﾎﾞﾙ名
           (setq D2 (getstring "\nMC番号を指示:(例：-1)"))
           (cond ((= D2 "") (setq D2 "-1"))) (setq D3 (strcat "52" D2))
           (setq D1 "MS")
           (setq 49PNA (strcat "49" D2))
           (princ "\n挿入点を指示")
           (command "INSERT" "MMS" pause "1" "1" "0" D1 D3 52T OPV 49A "" "" "" "" "" "" MN "1" D52 "")
           (setq PC (getvar "LASTPOINT"))
           (setq PC1 (list (+ (car PC) 100)(- (cadr PC) 125)))
           (command "COLOR" 2)
           (command "TEXT" "S" "M0" PC1 "20" "0" 49PNA)
           (command "COLOR" 4)
         );end_progn
      )
;;==================================================================================================
;;電流で選定
    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
    (if (and (= PX1 "M")(= VV "200V"))(setq DCL_NAM "MC_AM2"))
    (if (and (= PX1 "M")(= VV "400V"))(setq DCL_NAM "MC_AM4"))
    (if (and (= PX1 "F")(= VV "200V"))(setq DCL_NAM "MC_AF2"))
    (if (and (= PX1 "F")(= VV "400V"))(setq DCL_NAM "MC_AF4"))
    (cond ((or (= DLG "AA")(= DLG "BU"))
      (if (= DLG "AA")
         (progn
            (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
            (if (not (new_dialog DCL_NAM DCL_ID) )(exit))
               (action_tile "cancel" "(done_dialog)(exit)")
               (action_tile "11" "(setq GYO \"11\")")
               (action_tile "12" "(setq GYO \"12\")")
               (action_tile "13" "(setq GYO \"13\")")
               (action_tile "14" "(setq GYO \"14\")")
               (action_tile "15" "(setq GYO \"15\")")
               (action_tile "16" "(setq GYO \"16\")")
               (action_tile "17" "(setq GYO \"17\")")
               (action_tile "18" "(setq GYO \"18\")")
               (action_tile "19" "(setq GYO \"19\")")
               (action_tile "20" "(setq GYO \"20\")")
               (action_tile "21" "(setq GYO \"21\")")
               (action_tile "22" "(setq GYO \"22\")")
               (action_tile "23" "(setq GYO \"23\")")
               (action_tile "24" "(setq GYO \"24\")")
               (action_tile "25" "(setq GYO \"25\")")
               (action_tile "26" "(setq GYO \"26\")")
               (action_tile "27" "(setq GYO \"27\")")
               (action_tile "28" "(setq GYO \"28\")")
               (action_tile "29" "(setq GYO \"29\")")
               (action_tile "30" "(setq GYO \"30\")")
               (action_tile "31" "(setq GYO \"31\")")
            (start_dialog)
            (unload_dialog DCL_ID)(princ)

            ;;EXCELの読込み開始"列"を決定
            (if (= MN "1") (progn (setq RETU "E") (setq Rasc (ascii "E")))) ;三菱の場合
            (if (= MN "4") (progn (setq RETU "AE")(setq Rasc (ascii "AE"))));富士の場合            
;           (princ "\nAA列=")(princ RETU) (princ "\n行=")(princ GYO)
         );end_progn
      )
;;==================================================================================================
;;分電盤協約で選定
      (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
      (if (= DLG "BU")
         (progn
            (setq DCL_ID (load_dialog "X:/LSP-I/KD.DCL"))
            (if (not (new_dialog "MC_B" DCL_ID) )(exit))
               (action_tile "cancel" "(done_dialog)(exit)")
               (action_tile "2L20" "(setq T3 \"20A\")(setq SYM \"MMC2PHL\")(setq GYO \"41\")")
               (action_tile "2R20" "(setq T3 \"20A\")(setq SYM \"MMC2PHR\")(setq GYO \"42\")")
               (action_tile "3L20" "(setq T3 \"20A\")(setq SYM \"MMC3PHL\")(setq GYO \"43\")")
               (action_tile "3R20" "(setq T3 \"20A\")(setq SYM \"MMC3PHR\")(setq GYO \"44\")")
               (action_tile "2L35" "(setq T3 \"35A\")(setq SYM \"MMC2PHL\")(setq GYO \"46\")")
               (action_tile "2R35" "(setq T3 \"35A\")(setq SYM \"MMC2PHR\")(setq GYO \"47\")")
               (action_tile "3L35" "(setq T3 \"35A\")(setq SYM \"MMC3PHL\")(setq GYO \"48\")")
               (action_tile "3R35" "(setq T3 \"35A\")(setq SYM \"MMC3PHR\")(setq GYO \"49\")")
               (action_tile "2L50" "(setq T3 \"50A\")(setq SYM \"MMC2PHL\")(setq GYO \"51\")")
               (action_tile "2R50" "(setq T3 \"50A\")(setq SYM \"MMC2PHR\")(setq GYO \"52\")")
               (action_tile "3L50" "(setq T3 \"50A\")(setq SYM \"MMC3PHL\")(setq GYO \"53\")")
               (action_tile "3R50" "(setq T3 \"50A\")(setq SYM \"MMC3PHR\")(setq GYO \"54\")")
            (start_dialog)
            (unload_dialog DCL_ID)(princ)

          ;;EXCELの読込み開始"列"を決定
            (if (= MN "1") (progn (setq RETU "E") (setq Rasc (ascii "E")))) ;三菱の場合
            (if (= MN "4") (progn (setq RETU "E") (setq Rasc (ascii "E")))) ;富士は無し、三菱を選定
;           (princ "\nBU列=")(princ RETU) (princ "\n行=")(princ GYO)
         );end_progn
      );end_if
;;==================================================================================================
;;電流選定・分電盤協約 EXCEL読込み
         (setq DAT nil)(setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
            (repeat 15 
               (setq FILE "X:/XLSX/MC-SEL.xlsx")
               (setq CELL (strcat RETU GYO))(princ "\nCELL =")(princ CELL)
               (REX)
               (cond ((= value nil)(setq value "")))
               (setq DAT (append DAT (list value)))
               (cond ((= (strlen RETU) 2)
                         (setq RETU (substr RETU 2 1))
                         (setq RETU (strcat "A" (chr (+ (ascii RETU) 1)))))
               )
               (if (= (strlen RETU) 1)(setq RETU (chr (+ (ascii RETU) 1))))
            );Rep
            (princ "\nDAT =")(princ DAT)
            (setq PNA (nth 0 DAT) SYM (nth 1 DAT)  T1 (nth 2 DAT)  T2 (nth 3 DAT)  T3 (nth 4 DAT)
                  T4  (nth 5 DAT) T5  (nth 6 DAT)  T6 (nth 7 DAT)  T7 (nth 8 DAT)  T8 (nth 9 DAT)
                  T9 (nth 10 DAT) MN  (nth 11 DAT) QT (nth 12 DAT) Z1 (nth 13 DAT) Z2 (nth 14 DAT)
            )
            (setq PNO (getstring "\nMC番号を指示:(例：-1)"))
            (cond ((= PNO "") (setq PNO "-1"))) (setq PNO (strcat "52" PNO))
            	(princ "\n挿入点を指示")
           		(command "INSERT" SYM pause "1" "1" "0" PNA PNO T1 OPV T3 T4 T5 "" T7 T8 T9 MN QT Z1 Z2)
            )
    );end_cond
    (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)
    (redraw)
);end_defun

;;==================================================================================================
;;98-1.寸法線を手動入力した場合に線の色を変更する   C:DIMX        DIMX.LSP
;;==================================================================================================
(defun C:DIMX(/ W1 SM CL)
    (setq *error* *err3*)
    (setvar "cmdecho" 0)
    (setq W1 nil)(setq SM nil)(setq CL nil)

    (princ "\n寸法図形を選択：")(setq W1 (ssget))

    (cond ((/= W1 nil)
        (setq SM (getstring "\n新しい寸法文字 <>："))
        (if (= SM "")(setq SM "<>"))

        ;;(cond ((/= SM "<>")
        ;;    (command "change" W1 "" "p" "c" "6" "")
        ;;    (command "dimclrt" 6)
        ;;    (command "dimstyle" "a" W1 "")
        ;;    (command "dimclrt" 2)
        ;;))
        ;;(cond ((= SM "<>")
        ;;    (command "change" W1 "" "p" "c" "2" "")
        ;;    (command "dimclrt" 2)
        ;;    (command "dimstyle" "a" W1 "")
        ;;))

        (cond ((/= SM "<>")
            (setq CL "6") ;;ピンク色
        ))
        (cond ((= SM "<>")
            (setq CL "2") ;;黄色
        ))

        (command "change" W1 "" "p" "c" CL "")
        (command "DIMOVER" "DIMCLRT" CL "" W1 "")

        ;;ICADのバグでDIMEDITを実行すると、そこでプログラムが終了してしまうので先にCMDECHOを実行しておく
        (setvar "cmdecho" 1)
        (command "dimedit" "e" SM W1 "")
        (princ "\n完了")
    ))
    (setvar "cmdecho" 1)
)
;;==================================================================================================
;;98-2.寸法線の尺度を一括で変更する　　　　　　　   C:DIMST       DIMST.LSP
;;==================================================================================================
(defun C:DIMST(/ W1 LFAC LFAC2)
    (setq *error* *err3*)
    (setvar "cmdecho" 0)
    (setq W1 nil)(setq LFAC nil)(setq LFAC2 nil)

    (princ "\n尺度を変更する寸法線を選択：")(setq W1 (ssget))
    ;;(princ "\nW1=")(princ W1)
 
    (if (/= W1 nil)
        (progn
            (setq LFAC (getreal "\n尺度を入力 <Enter=1>："))
            (if (= LFAC nil)(setq LFAC 1))
            ;;(princ "\nLFAC=")(princ LFAC) 

            ;;(setq LFAC2 (getvar "DIMLFAC"))
            ;;(princ "\nLFAC2=")(princ LFAC2)
            ;;(setvar "DIMLFAC" LFAC)
            ;;(command "DIMSTYLE" "A" W1 "")
            ;;(setvar "DIMLFAC" LFAC2)

            (command "DIMOVER" "DIMLFAC" LFAC "" W1 "")
        )
    )

    ;;(command "DIMLFAC" 1);;尺度が戻らない時があるらしいので必ず１に戻るように設定
    (setvar "cmdecho" 1)
)
;;==================================================================================================
;;99. 分岐Ｃｕバーの回路入力              BrCu.DCL   BrCuS.LSP       DCLは回路・外形、共用
;;==================================================================================================
(defun C:BrCuS ( / LB1 GYO GYO1 GYO2 RETU SELT1 SELT2 T1 T2)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0) 
   (setq LB1 "回路入力")
   (DCL_BrCu)
   (if (or (= GYO1 nil)(= GYO2 nil)(= RETU nil))
       (progn (alert "選択されていない項目があります!")(DCL_BrCu))
   )
   (setq GYO (+ GYO1 GYO2))
   (setq SELT1 (strcat RETU (itoa GYO)))
   (setq SELT2 (strcat "D"  (itoa GYO)))
   (xls_open "X:/XLS/動力盤用分岐バー" "r")
      (setq T1 (xls_getval SELT1))
      (setq T2 (xls_getval SELT2))
      (if (or (= T1 "")(= T2 ""))(progn (alert "この形式は、選択出来ません！！ 中断します。")(exit)))
   (xls_close)
   (princ "\nX:/XLS/動力盤用分岐バー.XLSを参照しています。")
    (princ "\n分岐バーの挿入点を指示")
   (command "INSERT" "BU-3PC" pause "1" "1" "0" "Cu" "" T1 T2 "" "" "" "" "" "" "" "100" "1" "" "")
   (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(GC)(redraw)
)
;;==================================================================================================
;;100. 分岐Ｃｕバーの外形入力              BrCu.DCL   BrCuD.LSP       DCLは回路・外形、共用
;;==================================================================================================
(defun C:BrCuD ( / LB1 GYO GYO1 GYO2 RETU SEL SYM)
   (setq *error* *err1*)
   (setvar "CMDECHO" 0) 
   (setq LB1 "外形入力")
   (DCL_BrCu)
   (if (or (= GYO1 nil)(= GYO2 nil)(= RETU nil))
       (progn (alert "選択されていない項目があります!")(DCL_BrCu))
   )
   (setq GYO (+ GYO1 GYO2 30))
   (setq SEL (strcat RETU (itoa GYO)))
   (xls_open "X:/XLS/動力盤用分岐バー" "r")
      (setq SYM (xls_getval SEL))
      (setq SYM (strcat "*" SYM))
      (if (= SYM "")(progn (alert "この形式は、選択出来ません！！ 中断します。")(exit)))
    (xls_close)
    (princ "\nX:/XLS/動力盤用分岐バー.XLSを参照しています。")
     (princ "\n図面の挿入点を指示")
    (command "INSERT" SYM pause "1" "0")
    (setvar "CMDECHO" 1)(setvar "BLIPMODE" 1)(GC)(redraw)
)
;---------------------------------------------------------------------------------------------------
  (defun DCL_BrCu(/)
      (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
          (if (not (new_dialog "BrCu" DCL_ID) )(exit)) 
          (action_tile "cancel" "(done_dialog)(exit)")
          (action_tile "A1" "(setq GYO1 10)")
          (action_tile "A2" "(setq GYO1 15)")
          (action_tile "A3" "(setq GYO1 20)")
          (action_tile "A4" "(setq GYO1 25)")
             (action_tile "B1" "(setq GYO2 1)")
             (action_tile "B2" "(setq GYO2 2)")
             (action_tile "B3" "(setq GYO2 3)")
             (action_tile "B4" "(setq GYO2 4)")
          (action_tile "C1" "(setq RETU  \"E\")")
          (action_tile "C2" "(setq RETU  \"F\")")
          (action_tile "C3" "(setq RETU  \"G\")")
          (action_tile "C4" "(setq RETU  \"H\")")
          (action_tile "C5" "(setq RETU  \"I\")")
          (set_tile "LB1" LB1)
          (start_dialog) (unload_dialog DCL_ID) (princ)
  )

;;==================================================================================================
;;102.操作変圧器用ブレーカー選定                                北野 09/08/27
;;==================================================================================================
(defun C:SOUSATORA ( / DENATU GYOU TYOU MK MN RETU1 RETU2 TORARETU TORAGYOU TORASEL TTORASEL TKRETU 
                    BP TKSEL TTKSEL BKSEL BBKSEL BK2SEL BBK2SEL TBK TT3 TT4 TMN BBK TT4SEL KKVA PC
                    FP1 FP2 FP3 FP4 n AA BB ENT D-MCB D-TOR DATT N2 CC DD ZOKU AAAA RITI SITI ACIT
                    RMOJI SMOJI ACMOJI SEISU)
(setq *error* *err1*)
(setvar "CMDECHO" 1)(setvar "BLIPMODE" 0)

;変数の初期化
     (setq DENATU "")(setq GYOU "")(setq TYOU "")(setq MK "")(setq MN "")(setq RETU1 "")(setq RETU2 "")
     (setq TORARETU "")(setq TORAGYOU "")(setq TORASEL "")(setq TTORASEL "")(setq TMN "")(setq TKRETU "")
     (setq TTKSEL "")(setq BKSEL "")(setq BBKSEL "")(setq BK2SEL "")(setq BBK2SEL "")(setq TBK "")(setq TKSEL "")
     (setq TT3 "")(setq TT4 "")(setq BBK "")(setq TT4SEL "")(setq KKVA "")(setq PC "")(setq FP1 "")
     (setq FP2 "")(setq FP3 "")(setq FP4 "")(setq n "")(setq AA "")(setq BB "")(setq ENT "")(setq D-MCB "")
     (setq D-TOR "")(setq DATT "")(setq N2 "")(setq CC "")(setq DD "")(setq ZOKU "")(setq AAAA "")(setq BP "")
     (setq RITI "")(setq SITI "")(setq ACIT "")(setq RMOJI "")(setq SMOJI "")(setq ACMOJI "")(setq SEISU "")
    

     ;(setq DCL_ID (load_dialog "Z:/TMP/KITANO/2KD.DCL"))
     (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))
     (if (not (new_dialog "SOUSAT" DCL_ID) )(exit))
     (action_tile "cancel" "(done_dialog)(exit)")
     (action_tile "1" "(setq DENATU \"100V\")(setq GYOU \"61\")(setq TT3 \"200/100V\") ")
     (action_tile "2" "(setq DENATU \"200V\")(setq GYOU \"80\")(setq TT3 \"440/200V\") ")
     (action_tile "3" "(setq DENATU \"100V\")(setq GYOU \"98\")(setq TT3 \"440/100V\") ")
     (action_tile "4" "(setq TYOU \"50\")")     ;50VA
     (action_tile "5" "(setq TYOU \"100\")")    ;100VA
     (action_tile "6" "(setq TYOU \"150\")")    ;150VA
     (action_tile "7" "(setq TYOU \"200\")")    ;200VA
     (action_tile "8" "(setq TYOU \"300\")")    ;300VA
     (action_tile "9" "(setq TYOU \"500\")")    ;500VA
     (action_tile "10" "(setq TYOU \"750\")")   ;750VA
     (action_tile "11" "(setq TYOU \"1000\")")  ;1000VA
     (action_tile "12" "(setq TYOU \"1500\")")  ;1500VA
     (action_tile "13" "(setq TYOU \"2000\")")  ;2000VA
     (action_tile "14" "(setq TYOU \"3000\")")  ;3000VA
     (action_tile "15" "(setq TYOU \"4000\")")  ;4000VA
     (action_tile "16" "(setq TYOU \"5000\")")  ;5000VA
     (start_dialog)(unload_dialog DCL_ID)(princ)
(C:MKR)
(if (= MAKER "0")(progn (setq MK "M")(setq MN "1")))                 ;三菱の場合     
(if (= MAKER "1")(progn (setq MK "F")(setq MN "4")))                 ;富士の場合           
(if (or (= MAKER "2")(= MAKER "3"))(progn (setq MK "T")(setq MN "7"))) ;寺崎の場合

(if (= MK "M")(progn (setq RETU1 "G")(setq RETU2 "H")))            ;三菱ブレーカーの読み取る列
(if (= MK "F")(progn (setq RETU1 "I")(setq RETU2 "J")))            ;富士ブレーカーの読み取る列
(if (= MK "T")(progn (setq RETU1 "K")(setq RETU2 "L")))            ;寺崎ブレーカーの読み取る列


(setq TORARETU "A")    ;トランス容量を読み取る列                                                
(setq TORAGYOU GYOU)   ;トランス容量を読み取る行
(setq TKRETU "B")      ;トランス型番を読み取る列
(setq BP "2P")         ;ブレーカーの極数
(setq TMN "20")        ;トランスメーカー番号
(setq TBK "TrL")       ;トランス部品記号
(setq BBK "MCB")       ;ブレーカー部品記号

(if (= DENATU "100V")
    (progn 
       (setq RMOJI "R100")(setq SMOJI "S100")(setq ACMOJI "(AC100V)")
    );progn
);if

(if (= DENATU "200V")
    (progn
       (setq RMOJI "R200")(setq SMOJI "S200")(setq ACMOJI "(AC200V)")
    );progn
);if

;(setq TORAGYOU (itoa TORAGYOU))
(setq TORASEL (strcat TORARETU TORAGYOU))   ;トランス容量を読み取るセルを指定

(xls_open "X:/XLS/操作変圧器用ブレーカー選定表" "r")   ;ファイルを指定して、読み込みモードで開く
(setq TTORASEL (xls_getval TORASEL))                   ;トランス容量のセルのデータを読み込む

(while (/= TTORASEL TYOU)     ;セルの中と型式が一致するまで
   (setq TORAGYOU (atoi TORAGYOU))    ;数値化                            
   (setq TORAGYOU (+ TORAGYOU 1))     ;行移動
   (setq TORAGYOU (itoa TORAGYOU))    ;文字列化
   (setq TORASEL (strcat TORARETU TORAGYOU))   ;トランス容量を読み取るセルを指定
   (setq TTORASEL (xls_getval TORASEL))        ;トランス容量のセルのデータを読み込む
   (setq TT4SEL (atoi TTORASEL))
   ;(princ "\nTTORASEL=")(princ TTORASEL)       
   ;(princ "\nTORASEL=")(princ TORASEL)
);while

(setq TKSEL (strcat TKRETU TORAGYOU))          ;トランス型番を読み取るセルを指定
(setq TTKSEL (xls_getval TKSEL))               ;トランス型番のセルのデータを読み込む
(setq BKSEL (strcat RETU1 TORAGYOU))           ;ブレーカーの型番を読み取るセルを指定
(setq BBKSEL (xls_getval BKSEL))               ;ブレーカーの型番のセルのデータを読み込む
;(princ "\nBBKSEL=")(princ BBKSEL)
(setq BK2SEL (strcat RETU2 TORAGYOU))          ;ブレーカーのトリップ値を読み取るセルを指定
(setq BBK2SEL (xls_getval BK2SEL))             ;ブレーカーのトリップ値のセルのデータを読み込む
(setq TT4 (strcat TTORASEL "VA"))              ;トランス容量 属性用

(if (and (= BBKSEL "")(= BBK2SEL ""))
    (progn
       (alert "選定表にブレーカーの型式とトリップ値が記入されていません。")
       (exit)
    );progn
);if

(if (= BBKSEL "")
    (progn
       (alert "選定表にブレーカーの型式が記入されていません。")
       (exit)
    );progn
);if

(if (= BBK2SEL "")
    (progn
       (alert "選定表にブレーカーのトリップ値が記入されていません。")
       (exit)
    );progn
);if


(if (>= TT4SEL 1000)
    (progn
        (setq KKVA (/ (float TT4SEL) 1000))    ;トランス容量が1000VA以上の場合       
        ;(setq KKVA (/ TT4SEL 1000))
        ;(setq KKVA (/ 1 1000))                 
        ;(princ "\nKKVA=")(princ KKVA)
        ;(setq KKVA (ftoa KKVA))                
        (setq TT4 (rtos KKVA 2 1))             ;文字列化
        (setq SEISU (substr TT4 3 1))          ;整数か実数を識別
        ;(princ "\nSEISU=")(princ SEISU)
           (if (= SEISU "0")                   ;KVA表記時に整数の場合
               (progn
                  (setq KKVA (/ TT4SEL 1000))
                  ;(princ "\nKKVA@=")(princ KKVA)   
                  (setq TT4 (itoa KKVA))               
               );progn
           );if
        (setq TT4 (strcat TT4 "kVA"))          ;トランス容量ｋVA用
        ;(setq TT4 "1.5KVA")
    );progn
);if

;(princ "\nTTKSEL=")(princ TTKSEL)
;(princ "\nBBKSEL=")(princ BBKSEL)
;(princ "\nTT4=")(princ TT4)

;(command "INSERT" "ZOKU" "1000,1000" "1" "1" "0" TBK "1" TTKSEL "" TT3 TT4 "" "" "" "" "" TMN "1" "" "")
;(command "INSERT" "ZOKU" "1500,1500" "1" "1" "0" BBK "1" BBKSEL BP BBK2SEL "" "" "" "" "" "" MN "1" "" "")
(xls_close)

;(setq PC (getpoint "\n挿入点を指示... "))
 (princ "\n挿入点を指示")
(command "INSERT" "*MCBTR" pause "1" "0")
(setq PC (getvar "LASTPOINT"))

(setq FP1 (list (- (car PC) 90)(cadr PC)))           ;属性を変更する範囲
(setq FP2 (list (- (car PC) 90)(- (cadr PC) 920)))
(setq FP3 (list (+ (car PC) 350)(- (cadr PC) 920)))
(setq FP4 (list (+ (car PC) 350)(cadr PC)))

;(setq LUX (- (car PC) 90))
;(setq LUY (cadr PC))
;(setq LUXY (list LUX LUY))
;(setq RSX (+ (car PC) 350))
;(setq RSY (- (cadr PC) 920))
;(setq RSXY (list RSX RSY))

(setq RITI (list (- (car PC) 50)(- (cadr PC) 775)))  ;R100のテキスト位置
(setq SITI (list (+ (car PC) 40)(- (cadr PC) 775)))  ;S100のテキスト位置
(setq ACIT (list (+ (car PC) 25)(- (cadr PC) 850)))  ;(AC100V)のテキスト位置

;(princ "\nFP1=")(princ FP1)
;(princ "\nFP2=")(princ FP2)
;(princ "\nFP3=")(princ FP3)
;(princ "\nFP4=")(princ FP4)
;(princ "\nLUXY=")(princ LUXY)

(if (= TTKSEL  "WTP-5000A" )
    (setq TT3 "220/110V")
);if

;(setq D-MCB (list BBK "MCB-1" BBKSEL BP BBK2SEL "" "" "" "" "" "" "3" "99" "MMCB2P" ""))
(setq D-MCB (list BBK "MCB-1" BBKSEL BP BBK2SEL "" "" "" "" "" "" MN "1" "MMCB2P" ""))    ;ブレーカー用に格納されているデータ
(setq D-TOR (list TBK "TRL-1" TTKSEL "" TT3 TT4 "" "" "" "" "" TMN "1" "TROP-V" ""))      ;トランス用に格納されているデータ
;(princ "\nD-MCB=")(princ D-MCB)
;(princ "\nD-TOR=")(princ D-TOR)

(setq n 0)
(setq AA (ssget "WP" (list FP1 FP2 FP3 FP4)))                                             ;図形を選択する範囲
;(setq AA (ssget "WP" (list FP1 FP2 FP3 FP4)(list '(66 . 1))))   
;(setq AA (ssget "W" LUXY RSXY))
;(princ "\nAA=")(princ AA)

(while (setq BB (ssname AA n)) ;選択した範囲内の図形がなくなるまで
   (setq ENT (entget BB))      ;図形のデータを獲得する
   ;(princ "\nBB=")(princ BB)
   ;(princ "\nENT=")(princ ENT)
   ;(princ "\nn=")(princ n)


   ;;属性書き込み対象シンボル
   (if (or (= (cdr (assoc 2 ENT)) "MMCB2P")
           (= (cdr (assoc 2 ENT)) "TROP-V")
       );or
       (progn
          (if (= (cdr (assoc 2 ENT)) "MMCB2P")(setq DATT D-MCB))
          (if (= (cdr (assoc 2 ENT)) "TROP-V")(setq DATT D-TOR))
          ;(princ "\nDATT=")(princ DATT)
          (setq N2 1)
          ;(princ "\nN2=")(princ N2)
          (setq CC (entnext BB) DD (entget CC))  ;図面内の次の図形を選択する
              ;(princ "\nCC=")(princ CC)
              ;(princ "\nDD=")(princ DD)
              (while (/= (cdr (assoc 0 DD)) "SEQEND");SEQENDまで繰り返し
                   (if (= (cdr (assoc 0 DD)) "ATTRIB")
                       (progn
                          (if (= N2 1)(setq ZOKU "PNA"))
                          (if (= N2 2)(setq ZOKU "PNO"))
                          (if (= N2 3)(setq ZOKU "T1")) 
                          (if (= N2 4)(setq ZOKU "T2"))
                          (if (= N2 5)(setq ZOKU "T3")) 
                          (if (= N2 6)(setq ZOKU "T4"))
                          (if (= N2 7)(setq ZOKU "T5")) 
                          (if (= N2 8)(setq ZOKU "T6"))
                          (if (= N2 9)(setq ZOKU "T7")) 
                          (if (= N2 10)(setq ZOKU "T8"))
                          (if (= N2 11)(setq ZOKU "T9")) 
                          (if (= N2 12)(setq ZOKU "MN"))
                          (if (= N2 13)(setq ZOKU "QT")) 
                          (if (= N2 14)(setq ZOKU "Z1"))
                          (if (= N2 15)(setq ZOKU "Z2"))
                          ;(princ "\nZOKU@=")(princ ZOKU)
                          (if (= (cdr (assoc 2 DD)) ZOKU) ;DDにZOKUが入っている場合
                              (progn
                              ;(entmod (subst (cons 1 (nth (- 1 N2) DATT))(assoc 1 DD) DD))
                              ;(entmod (subst (cons 1 (nth (- 1 N2) DATT))(assoc 1 DD) DD))
                              (entmod (subst (cons 1 (nth (- N2 1) DATT))(assoc 1 DD) DD))  ;図形データを変更する
                              ;(setq AAAA (cons 1 (nth (- N2 1) DATT)))
                              ;(princ "\nAAAA=")(princ AAAA)
                              );progn
                          );if
                          ;(princ "\nZOKU@@=")(princ ZOKU)
                          ;(princ "\nDD@=")(princ DD)
                       );progn
                   );if 

                   (setq N2 (+ 1 N2))
                   ;(princ "\nN2@=")(princ N2)
                   (setq CC (entnext CC) DD (entget CC))  ;次の図形に移動しデータを獲得する
                   ;(princ "\nCC@=")(princ CC)
                   ;(princ "\nDD@@=")(princ DD)
              );while   
       );progn
   );if

   (if (= (cdr (assoc 1 ENT)) "R100")(entdel BB))         ;テキストを消去
   (if (= (cdr (assoc 1 ENT)) "N100")(entdel BB))
   (if (= (cdr (assoc 1 ENT)) "(AC100V))")(entdel BB))
   (setq n (+ 1 n))
   ;(princ "\n@@=")(princ n)

);while


(command "COLOR" "2")
(command "TEXT" "S" "M0" "J" "BL" RITI "20" "0" RMOJI)    ;テキスト挿入  
(command "TEXT" "S" "M0" "J" "BL" SITI "20" "0" SMOJI)
;(command "TEXT" RITI "20" "0" RMOJI)    ;テキスト挿入  
;(command "TEXT" SITI "20" "0" SMOJI)

(command "TEXT" "S" "M0" "C" ACIT "20" "0" ACMOJI)
;(command "TEXT" "C" ACIT "20" "0" ACMOJI)

(princ "\nRMOJI=")(princ RMOJI)
(princ "\nSMOJI=")(princ SMOJI)
(princ "\nACMOJI=")(princ ACMOJI)
(command "COLOR" "4")
(command "REGEN")
(GC)(setvar "BLIPMODE" 1)(setvar "CMDECHO" 1)

)


;;==================================================================================================
;;108. 動力分電盤銅バー外形図            C:CuBrGAI    北野
;;==================================================================================================
(defun C:CuBrGAI ( / SYU SEN DOU DOUBAR)

(setq SYU "")(setq SEN "")(setq DOU "")(setq DOU "")(setq DOUBAR "") ;変数の初期化
(setq *error* *err1*)
(M-BAR)
(if (= SYU "")(exit)) ;*キャンセルボタンを押すと終了する

;仮変数の設定　画面が完成するまでの仮変数
;(setq SYU "DM")  ;*主幹の有無で、無しを選択した場合
;(setq SEN "FT")  ;*線種の選択で、実践を選択した場合
;(setq DOU "850") ;*銅バーの選択で、CU8×50(600A) 400A無を選択した場合。

(princ "\nSYU=")(princ SYU)
(princ "\nSEN=")(princ SEN)
(princ "\nDOU=")(princ DOU)

(setq DOUBAR (strcat "*" SYU DOU SEN)) ;*インサートする図形を決める為の文字連結
(princ "\nDOUBAR=")(princ DOUBAR)
 
(princ "\n挿入点を指示")               ;*「挿入点を指示」とプロンプトに表示させる。
(command "INSERT" DOUBAR pause "1" "0") ;*銅バー外形図を挿入する。
(command "REGEN")                       ;*再作図を行う。
);defun

;;==================================================================================================
;;109. 可逆型マグネットの入力           C:KAGYAKU      北野
;;==================================================================================================
(defun C:KAGYAKU ( / KMGS PC)

(setq *error* *err1*)

(setq KMGS "")(setq PC "") ;*変数の初期化

(setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL")) ;*本番用
;*(setq DCL_ID (load_dialog "C:/USR/2KD.DCL"))   ;*テスト用
(if (not (new_dialog "KAGYAKU" DCL_ID))(exit))
 

(action_tile "cancel" "(done_dialog)(exit)")  ;*キャンセルボタンを押したら終了する。

;*三菱の場合
(action_tile "1" "(setq KMGS \"DM-MSON10×2\")")    ;*型式:MSO-2×N10
(action_tile "2" "(setq KMGS \"DM-MSON10×2\")")    ;*型式:MSO-2×N11 
(action_tile "3" "(setq KMGS \"DM-MSON18×2\")")    ;*型式:MSO-2×N18
(action_tile "4" "(setq KMGS \"DM-MSON20×2\")")    ;*型式:MSO-2×N20
(action_tile "5" "(setq KMGS \"DM-MSON20×2\")")    ;*型式:MSO-2×N21
(action_tile "6" "(setq KMGS \"DM-MSON25×2\")")    ;*型式:MSO-2×N25
(action_tile "7" "(setq KMGS \"DM-MSON25×2\")")    ;*型式:MSO-2×N35
(action_tile "8" "(setq KMGS \"DM-MSON50×2\")")    ;*型式:MSO-2×N50
(action_tile "9" "(setq KMGS \"DM-MSON50×2\")")    ;*型式:MSO-2×N65
(action_tile "10" "(setq KMGS \"DM-MSON80×2\")")   ;*型式:MSO-2×N80
(action_tile "11" "(setq KMGS \"DM-MSON80×2\")")   ;*型式:MSO-2×N95
(action_tile "12" "(setq KMGS \"DM-MSON125×2\")")  ;*型式:MSO-2×N125
(action_tile "13" "(setq KMGS \"DM-MSON150×2\")")  ;*型式:MSO-2×N150
(action_tile "14" "(setq KMGS \"DM-MSON180×2\")")  ;*型式:MSO-2×N180
(action_tile "15" "(setq KMGS \"DM-MSON180×2\")")  ;*型式:MSO-2×N220
(action_tile "16" "(setq KMGS \"DM-MSON300×2\")")  ;*型式:MSO-2×N300
(action_tile "17" "(setq KMGS \"DM-MSON300×2\")")  ;*型式:MSO-2×N400

;*富士の場合
(action_tile "20" "(setq KMGS \"DF-SW-03RM×2\")")  ;*型式:SW-0RM
(action_tile "21" "(setq KMGS \"DF-SW-03RM×2\")")  ;*型式:SW-03RM
(action_tile "22" "(setq KMGS \"DF-SW-05RM×2\")")  ;*型式:SW-05RM
(action_tile "23" "(setq KMGS \"DF-SW-4-0RM×2\")") ;*型式:SW-4-0RM
(action_tile "24" "(setq KMGS \"DF-SW-4-0RM×2\")") ;*型式:SW-4-1RM
(action_tile "25" "(setq KMGS \"DF-SW-5-1RM×2\")") ;*型式:SW-5-1RM
(action_tile "26" "(setq KMGS \"DF-SW-N1RM×2\")")  ;*型式:SW-N1RM
(action_tile "27" "(setq KMGS \"DF-SW-N1RM×2\")")  ;*型式:SW-N2RM
(action_tile "28" "(setq KMGS \"DF-SW-N2SRM×2\")") ;*型式:SW-N2SRM
(action_tile "29" "(setq KMGS \"DF-SW-N2SRM×2\")") ;*型式:SW-N3RM
(action_tile "30" "(setq KMGS \"DF-SW-N4RM×2\")")  ;*型式:SW-N4RM
(action_tile "31" "(setq KMGS \"DF-SW-N4RM×2\")")  ;*型式:SW-N5RM
(action_tile "32" "(setq KMGS \"DF-SW-N6RM×2\")")  ;*型式:SW-N6RM
(action_tile "33" "(setq KMGS \"DF-SW-N7RM×2\")")  ;*型式:SW-N7RM
(action_tile "34" "(setq KMGS \"DF-SW-N8RM×2\")")  ;*型式:SW-N8RM
(action_tile "35" "(setq KMGS \"DF-SW-N8RM×2\")")  ;*型式:SW-N10RM
(action_tile "36" "(setq KMGS \"DF-SW-N11RM×2\")") ;*型式:SW-N11RM
(action_tile "37" "(setq KMGS \"DF-SW-N11RM×2\")") ;*型式:SW-N12RM
(action_tile "38" "(setq KMGS \"DF-SW-N14RM×2\")") ;*型式:SW-N14RM

(start_dialog)(unload_dialog DCL_ID)(princ)

(princ "\nシンボル名=")(princ KMGS)

(command "INSERT" KMGS pause "1" "1" "0" ) 

);*defun

;;==================================================================================================
;;110.ＰＶ ＳＰＤ（回路図・外形図兼用）
;;==================================================================================================
(defun C:SPD ( / SYM)
    (setq *error* *err1*)
    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
    ;;(setq DCL_ID (load_dialog "C:/Wkspace3/1BBB/2KD.DCL")) ;;テスト用
    (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))

    ;;ダイアログボックス
     
    (if (not (new_dialog "SPD" DCL_ID) )(EXIT))
    
    (if (= SPDFLG "M")
        (progn ;;回路図
            (setq LB1 "ＳＰＤ回路図")(set_tile "LB1" LB1)
        )
        (progn ;;外形図
            (setq LB1 "ＳＰＤ外形図")(set_tile "LB1" LB1)
        )      
    )
      
    (setq SYM "")
    (action_tile "cancel" "(done_dialog)(exit)")

    (action_tile "1" "(setq SYM \"LA-LDD62\")")
    (action_tile "2" "(setq SYM \"LA-TED62\")")
    (action_tile "3" "(setq SYM \"LA-SGZ\")")
    (action_tile "4" "(setq SYM \"LA-SLGV\")")
    (action_tile "5" "(setq SYM \"LA-SRGV\")")
            
    (action_tile "11" "(setq SYM \"LA-MATP\")")
    (action_tile "12" "(setq SYM \"LA-MAT2\")")
    (action_tile "13" "(setq SYM \"LA-MDP24\")")
    (action_tile "14" "(setq SYM \"LA-MDPRB\")")
    
    (action_tile "21" "(setq SYM \"LA-CN5\")")
    (action_tile "22" "(setq SYM \"LA-CN2340\")")
    (action_tile "23" "(setq SYM \"LA-CN226\")")
    (action_tile "24" "(setq SYM \"LA-CN227\")")
    
    (start_dialog)(unload_dialog DCL_ID)(princ)

    ;;シンボル名
    (if (= SPDFLG "M")
        (progn ;;回路図
            (setq SYM (strcat "*" SYM)); 
        )
        (progn ;;外形図
            (setq SYM (strcat "*" "D" SYM)); 
        )
    )

     
    (princ "\n挿入点を指示")
    (command "INSERT" SYM pause "" "")

    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(redraw)
)
;;==================================================================================================
;;111.ＰＶ コメント一覧
;;==================================================================================================
(defun C:PVCOMME ( / SYM)
    (setq *error* *err1*)
    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)
    ;;(setq DCL_ID (load_dialog "C:/Wkspace3/1BBB/2KD.DCL")) ;;テスト用
    (setq DCL_ID (load_dialog "X:/LSP-I/2KD.DCL"))

    ;;ダイアログボックス
     
    (if (not (new_dialog "PVCOMME" DCL_ID) )(EXIT))
          
    (setq SYM "")
    (action_tile "cancel" "(done_dialog)(exit)")

    (action_tile  "1" "(setq SYM \"*ACOM-MA1\")")
    (action_tile  "2" "(setq SYM \"*ACOM-RR1\")")
    (action_tile  "3" "(setq SYM \"*ACOM-RR2\")")
    (action_tile  "4" "(setq SYM \"*ACOM-RR3\")")
    (action_tile  "5" "(setq SYM \"*ACOM-A1\")")
    (action_tile  "6" "(setq SYM \"*ACOM-A2\")")
    (action_tile  "7" "(setq SYM \"*ACOM-A3\")")
    (action_tile  "8" "(setq SYM \"*ACOM-T1\")")
    (action_tile  "9" "(setq SYM \"*ACOM-T2\")")
    (action_tile "10" "(setq SYM \"*ACOM-T3\")")
    
    (start_dialog)(unload_dialog DCL_ID)(princ)
    
     
    (princ "\n挿入点を指示")
    (command "INSERT" SYM pause "" "")

    (setvar "CMDECHO" 0)(setvar "BLIPMODE" 0)(redraw)
)

;;==================================================================================================
;;114. 河村キュービクル図面の自動変換
;;==================================================================================================
(defun C:KWQB(/)
  (setvar "CMDECHO" 0)
  (command "ZOOM" "O")
  (command "ZOOM" "E")
  (command "ZOOM" "O")
)
;;==================================================================================================
;(command "ZOOM" "E")(gc)
(setvar "CMDECHO" 0)
(setvar "FILEDIA" 0)
(princ "\n2KD.LSP   LOAD....OK")(princ "\n")

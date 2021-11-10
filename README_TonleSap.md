# TonleSap湖計算用ブランチ

Stung Torengで河川流量を境界条件に、それより下流を対象としてCaMa-Floodを走らせる
下川さん修論用ブランチ。

## map/tonle_06min

### src_region/
glb_06minのマップをメコン流域を対象に領域化(90-110E, 5-35N) 

### src_param/
Stung Torengから上流のマスクを作る (s00-upbasin_ST.shでst_upst.binができる)
s01-*.shのcalc_outclm.F90が改変してある。nextxy_noedge.binとst_upst.binから、STより下流の河道網nextxy_tonle.binができる

## src/
河川流量を入力データとして使うために改変。
日流量テキストファイル CINFLOW をgoshで指定してnamelistで読む。

MAIN_cmf.F90のループで毎日の流量を読む
cmf_calc_stonxt.F90でSTの直下流のグリッドにrunoffと一緒に足し込む

## inflow/

入力日流量データはここに保存する

## gosh/
tonle_test.shがサンプルスクリプト。
日流量を入力に使うときはLINFLOWをTRUEにして、CINFLOWで流量ファイルを指定。
年単位の日流量データにしか対応していない。


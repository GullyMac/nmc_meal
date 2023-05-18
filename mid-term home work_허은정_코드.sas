/*********************

 반복측정자료분석
 중간시험 대체 과제물
 허은정

**********************/


/*** set directory ***/
libname hw "/home/u63426796";


/*** Q0. Basic ***/

* summary;
proc means data=hw.depression24 maxdec=2;
by group;
var t0 t1 t2 t3;
run;

* wide to long format;
data hw.depression24_long1;
set hw.depression24;
score=t1; time=1; output;
score=t2; time=2; output;
score=t3; time=3; output;
keep id group time t0 score;
run;
data hw.depression24_long2;
set hw.depression24;
score=t0; time=0; output;
score=t1; time=1; output;
score=t2; time=2; output;
score=t3; time=3; output;
keep id group time score;
run;

* plot;
proc sgplot data=hw.depression24_long;
vline time / response=score group=group stat=mean limitstat=clm;
yaxis label='score 평균(신뢰구간)';
run;

* profile plot;
proc sgpanel data=hw.depression24_long;
panelby group;
scatter x=time y=score / group=id markerattrs=(symbol=circlefilled size=10);
series x=time y=score / group=id lineattrs=(pattern=solid thickness=2);
colaxis values=(0 to 3 by 1);
run;


/*** Q1. RMANOVA ***/

* model : t1 t2 t3 = t0 Group으로 설정하는 경우;
proc glm data=hw.depression24;
class group;
model t1 t2 t3 = t0 group / nouni;
repeated time 3 / printe;
lsmeans group;
run;

* mean response profile plot;
proc sgplot data=hw.depression24_long1;
vline time / response=score group=group stat=mean limitstat=stddev;
yaxis label='mean +/- std';
run;

* model : t0 t1 t2 t3 = Group으로 설정하는 경우;
proc glm data=hw.depression24;
class group;
model t0 t1 t2 t3 = group / nouni;
repeated time 4 / printe;
lsmeans group;
run;

* mean response profile plot;
proc sgplot data=hw.depression24_long2;
vline time / response=score group=group stat=mean limitstat=stddev;
yaxis label='mean +/- std';
run;


/*** Q2. Mixed Model ***/

/* mean profile structure */

* generate dummy variables;
data hw.depression24_dummy(drop=group time);
set hw.depression24_long1;
if group=1 and time=1 then do; c11=1; c12=0; c13=0; t21=0; t22=0; t23=0; end;
if group=1 and time=2 then do; c11=0; c12=1; c13=0; t21=0; t22=0; t23=0; end;
if group=1 and time=3 then do; c11=0; c12=0; c13=1; t21=0; t22=0; t23=0; end;
if group=2 and time=1 then do; c11=0; c12=0; c13=0; t21=1; t22=0; t23=0; end;
if group=2 and time=2 then do; c11=0; c12=0; c13=0; t21=0; t22=1; t23=0; end;
if group=2 and time=3 then do; c11=0; c12=0; c13=0; t21=0; t22=0; t23=1; end;
run;

* ML method - UN;
proc mixed data=hw.depression24_dummy method=ml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=un subject=id;
run;

* ML method - TOEP(1);
proc mixed data=hw.depression24_dummy method=ml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=toep(1) subject=id;
run;

* ML method - AR(1);
proc mixed data=hw.depression24_dummy method=ml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=ar(1) subject=id;
run;

* ML method - CS;
proc mixed data=hw.depression24_dummy method=ml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=cs subject=id;
run;

* ML method - UN(1);
proc mixed data=hw.depression24_dummy method=ml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=un(1) subject=id;
run;

* ML method - simple;
proc mixed data=hw.depression24_dummy method=ml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=vc subject=id;
run;

* REML method - UN;
proc mixed data=hw.depression24_dummy method=reml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=un subject=id;
run;

* REML method - TOEP(1);
proc mixed data=hw.depression24_dummy method=reml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=toep(1) subject=id;
run;

* REML method - AR(1);
proc mixed data=hw.depression24_dummy method=reml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=ar(1) subject=id;
run;

* REML method - CS;
proc mixed data=hw.depression24_dummy method=reml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=cs subject=id;
run;

* REML method - UN(1);
proc mixed data=hw.depression24_dummy method=reml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=un(1) subject=id;
run;

* REML method - simple;
proc mixed data=hw.depression24_dummy method=reml;
class id;
model score = c11 c12 c13 t21 t22 t23 / noint solution;
random t0 / solution;
repeated / type=vc subject=id;
run;

/* factor structure */

* data preprocessing;
data hw.depression24_long1;
set hw.depression24_long1;
if group=1 then group1="2_ctr";
if group=2 then group1='1_trt';
run;

* ML method - UN;
proc mixed data=hw.depression24_long1 method=ml;
class id time group1;
model score = group1 time group1*time / solution;
random t0 / solution;
repeated / type=un subject=id;
run;

* ML method - TOEP(1);
proc mixed data=hw.depression24_long1 method=ml;
class id time group1;
model score = group1 time group1*time / solution;
random t0 / solution;
repeated / type=toep(1) subject=id;
run;

* ML method - AR(1);
proc mixed data=hw.depression24_long1 method=ml;
class id time group1;
model score = group1 time group1*time / solution;
random t0 / solution;
repeated / type=ar(1) subject=id;
run;

* ML method - CS;
proc mixed data=hw.depression24_long1 method=ml;
class id time group1;
model score = group1 time group1*time / solution;
random t0 / solution;
repeated / type=cs subject=id;
run;

* ML method - UN(1);
proc mixed data=hw.depression24_long1 method=ml;
class id time group1;
model score = group1 time group1*time / solution;
random t0 / solution;
repeated / type=un(1) subject=id;
run;

* ML method - simple;
proc mixed data=hw.depression24_long1 method=ml;
class id time group1;
model score = group1 time group1*time / solution;
random t0 / solution;
repeated / type=vc subject=id;
run;

* plot;
data temp;
input group$ time$ intercept group_effect time_effect interaction_effect t0_effect;
cards;
'Trt' '1' 197.92 -90.75 +59.25 -71.50 +00.00
'Ctr' '1' 197.92 +00.00 +59.25 +00.00 +00.00
'Trt' '2' 197.92 -90.75 +18.33 -24.83 +00.00
'Ctr' '2' 197.92 +00.00 +18.33 +00.00 +00.00
'Trt' '3' 197.92 -90.75 +00.00 +00.00 +00.00
'Ctr' '3' 197.92 +00.00 +00.00 +00.00 +00.00
;
run;
data temp;
set temp;
score = intercept + group_effect + time_effect + interaction_effect + t0_effect;
run;
proc sgplot;
scatter x=time y=score / group=group markerattrs=(symbol=circlefilled size=10);
series x=time y=score / group=group lineattrs=(pattern=solid thickness=2);
run;
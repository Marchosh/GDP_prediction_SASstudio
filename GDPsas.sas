/*school comp*/
proc import datafile='M:\My_Private_Files\STAT450\gdp.csv'
DBMS=csv
replace
out=gdp;
getnames=yes;
run;

/*persoanl comp*/
proc import datafile='D:\MNSU Assignment\STAT 450\gdp.csv'
DBMS=csv
replace
out=gdp;
getnames=yes;
run;

/*persoanl comp clean version*/
proc import datafile='D:\MNSU Assignment\STAT 450\gdpclean.csv'
DBMS=csv
replace
out=gdp;
getnames=yes;
run;

proc print data=gdp;
run;

/*scatter plot see python better*/

/*data correlation*/
PROC CORR DATA=gdp plots=matrix(histogram);
    VAR GDP Population Area Density Coastline Net_migration Infant_mortality Phones Climate Birthrate Deathrate Agriculture Industry Service Literacy Arable Crops Other;
RUN;

PROC SGSCATTER DATA=gdp; 
     MATRIX GDP Population Area Density Coastline Net_migration Infant_mortality Phones Climate Birthrate Deathrate Agriculture Industry Service Literacy Arable Crops Other;
RUN; 

/*full model*/
/*backward model selection*/
proc reg data=gdp;
	model GDP=Population Area Density Coastline Net_migration Infant_mortality Phones Climate Birthrate Deathrate Agriculture Industry Service Literacy Arable Crops Other/vif selection=backward;
run;

/*CP model selection*/
proc reg data=gdp;
	model GDP=Population Area Density Coastline Net_migration Infant_mortality Phones Climate Birthrate Deathrate Agriculture Industry Service Literacy Arable Crops Other/vif selection=CP;
run;

/*stepwise model selection*/
proc reg data=gdp;
	model GDP=Population Area Density Coastline Net_migration Infant_mortality Phones Climate Birthrate Deathrate Agriculture Industry Service Literacy Arable Crops Other/vif selection=stepwise;
run;

/*-------------------------------------Analysis------------------------------------------*/
/*backward model*/
proc reg data=gdp;
	model GDP=Net_migration Infant_mortality Phones Deathrate Literacy Other/influence;
run;
	/*outlier*/
	proc reg data=gdp;
	model GDP=Net_migration Infant_mortality Phones Deathrate Literacy Other/r;
	run;


/*Cp model*/
proc reg data=gdp;
	model GDP=Net_migration Infant_mortality Phones Deathrate Literacy Other industry/r;
run;
	
/*stepwise*/
proc reg data=gdp;
	model GDP=Net_migration Phones Climate Industry Literacy Other/r;
run;

	proc glmmod data=gdp outdesign=gdpdesign outparm=gdpparam;
		class Climate;
		model GDP=Net_migration Phones Climate Industry Literacy Other;
	run;
	
	/*rename*/
	data gdpdesign;
	set gdpdesign;
	Net_migration = col2;
	Phones = col3;
	Industry = col8;
	Literacy = col9;
	Other = col10;
	run;
	
	/*Climate ref=1 (col4)*/
	proc reg data=gdpdesign;
		model GDP=Net_migration Phones Industry Literacy Other col5 col6 col7/r;
	run;
	
	/*with interaction*/
	data gdpdesign;
	set gdpdesign;
	/*col 5 interaction*/
	Net_migrationcol5 = col2*col5;
	Phonescol5 = col3*col5;
	Industrycol5 = col8*col5;
	Literacycol5 = col9*col5;
	Othercol5 = col10*col5;
	/*col 6 intercation*/
	Net_migrationcol6 = col2*col6;
	Phonescol6 = col3*col6;
	Industrycol6 = col8*col6;
	Literacycol6 = col9*col6;
	Othercol6 = col10*col6;
	/*col 7 interaction*/
	Net_migrationcol7 = col2*col7;
	Phonescol7 = col3*col7;
	Industrycol7 = col8*col7;
	Literacycol7 = col9*col7;
	Othercol7 = col10*col7;
	run;
	
	/*Climate ref=1 (col4) with interaction*/
	proc reg data=gdpdesign;
		model GDP=Net_migration Phones Industry Literacy Other col5 col6 col7 
		Net_migrationcol5 Phonescol5 Industrycol5 Literacycol5 Othercol5 
		Net_migrationcol6 Phonescol6 Industrycol6 Literacycol6 Othercol6
		Net_migrationcol7 Phonescol7 Industrycol7 Literacycol7 Othercol7/r;
	run;
	
/*-----------------------------------------Transformation--------------------------------------------------------*/

/*backward boxcox Method lambda = 0.25*/
proc transreg data=gdp test;
	model BoxCox(GDP)= identity(Net_migration Infant_mortality Phones Deathrate Literacy Other);
run;

	data gdpmodel_1_2;
	set gdp;
	gdp3 = gdp**0.25;
	run;
	
	/*after transform lmabda=0.25*/
	proc reg data=gdpmodel_1_2;
		model gdp3=Net_migration Infant_mortality Phones Deathrate Literacy Other/influence;
	run;

/*CP model box cox method*/
proc transreg data=gdp test;
	model BoxCox(GDP)= identity(Net_migration Infant_mortality Phones Deathrate Literacy Other industry);
run;

	data gdpmodel_2_2;
	set gdp;
	gdp3 = gdp**0.25;
	run;
	
	/*after transform lmabda=0.25*/
	proc reg data=gdpmodel_2_2;
		model gdp3=Net_migration Infant_mortality Phones Deathrate Literacy Other industry/influence;
	run;
	
	/*checking outlier with scatter plot*/
	PROC SGSCATTER DATA=gdpmodel_2_2; 
     MATRIX gdp3 Net_migration Infant_mortality Phones Deathrate Literacy Other industry;
	RUN; 
	
	/*before transformed*/
	PROC SGSCATTER DATA=gdp; 
     MATRIX gdp Net_migration Infant_mortality Phones Deathrate Literacy Other industry;
	RUN; 
	
	/*x^2 transformation*/
	data gdpmodel_2_2;
	set gdpmodel_2_2;
	infant_sqrt = sqrt(Infant_mortality);
	literacy_2 = literacy*literacy;
	phones_2 = phones*phones;
	migration_2 = Net_migration*Net_migration;
	run;
	
	proc reg data=gdpmodel_2_2;
		model gdp3=Net_migration Infant_mortality Phones Deathrate Literacy Other industry infant_2 literacy_2 phones_2 migration_2/r;
	run;
	
	/*clean without outlier*/
	proc import datafile='D:\MNSU Assignment\STAT 450\gdpcleanoutlier.csv'
	DBMS=csv
	replace
	out=gdpclean;
	getnames=yes;
	run;
	
	data gdpmodel_2_2_clean;
	set gdpclean;
	gdp3 = gdp**0.25;
	run;
	
	/*after transform lmabda=0.25*/
	proc reg data=gdpmodel_2_2_clean;
		model gdp3=Net_migration Infant_mortality Phones Deathrate Literacy Other industry/influence;
	run;
	
		
/*stepwise*/
proc transreg data=gdp test;
	model BoxCox(GDP)= identity(Net_migration Phones Climate Industry Literacy Other);
run;
	/*transform*/
	data gdpmodel_3_2;
	set gdp;
	gdp3 = gdp**0.25;
	run;
	/*categorical*/
	proc glmmod data=gdpmodel_3_2 outdesign=gdp3_2design outparm=gdp3_2param;
		class Climate;
		model gdp3=Net_migration Phones Climate Industry Literacy Other;
	run;
	
	/*rename*/
	data gdpmodel_3_2;
	set gdp3_2design;
	Net_migration = col2;
	Phones = col3;
	Industry = col8;
	Literacy = col9;
	Other = col10;
	run;
	
	/*Climate ref=1 (col4)*/
	proc reg data=gdpmodel_3_2;
		model gdp3=Net_migration Phones Industry Literacy Other col5 col6 col7/r;
	run;
	
	/*with interaction*/
	/*with interaction*/
	data gdpmodel_3_2_interaction;
	set gdpmodel_3_2;
	/*col 5 interaction*/
	Net_migrationcol5 = col2*col5;
	Phonescol5 = col3*col5;
	Industrycol5 = col8*col5;
	Literacycol5 = col9*col5;
	Othercol5 = col10*col5;
	/*col 6 intercation*/
	Net_migrationcol6 = col2*col6;
	Phonescol6 = col3*col6;
	Industrycol6 = col8*col6;
	Literacycol6 = col9*col6;
	Othercol6 = col10*col6;
	/*col 7 interaction*/
	Net_migrationcol7 = col2*col7;
	Phonescol7 = col3*col7;
	Industrycol7 = col8*col7;
	Literacycol7 = col9*col7;
	Othercol7 = col10*col7;
	run;
	
	/*Climate ref=1 (col4) with interaction*/
	proc reg data=gdpmodel_3_2_interaction;
		model gdp3=Net_migration Phones Industry Literacy Other col5 col6 col7 
		Net_migrationcol5 Phonescol5 Industrycol5 Literacycol5 Othercol5 
		Net_migrationcol6 Phonescol6 Industrycol6 Literacycol6 Othercol6
		Net_migrationcol7 Phonescol7 Industrycol7 Literacycol7 Othercol7/r;
	run;



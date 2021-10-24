** PROJECT: SAVINGS - GENDER COLOMBIA / ANOVA - SLR SAGE RESEARCH CASE
** PROGRAM: savings_gender_colombia_anova_slr_rtaborda20210313.do
** PROGRAM TASK: EXTRACT DATA AND ESTIMATE ANOVA AND SINGLE LINEAR REGRESSION TO EXAMINE SAVINGS AND GENDER IN COLOMBIA
** AUTHOR: RODRIGO TABORDA / LUZ ELENA OROZCO
** DATE CREATEC: 2021/03/13
** DATE REVISION 1:
** DATE REVISION #:

********************************************************************;
** #0
********************************************************************;

** PROGRAM SETUP

    pause on
    #delimit ;

**** #10 ** DATA;

**** #10.1 ** IMPORT DATA;

    import delimited "http://www.rodrigotaborda.com/ad/data/iefic/2016/iefic_2016_s13.csv", clear;

**** #10.2 ** KEEP AND TIDY DATA FOR ANALYSIS;

    keep p35 ingreso p2478_*;

    gen gender = (p35 == "Mujer");
        label var gender "Gender";
        label define gender 0 "Male" 1 "Female";
        label values gender gender;

    /*INCOME*/;
    rename ingreso income;
        label var income "Income";

    gen income_mill = income / 1000000;
        label var income_mill "Income (1,000,000)";

    gen income_mill_ln = ln(income_mill);
        label var income_mill_ln "Income (1,000,000) Ln";

    /*EXPENDITURE*/;
    egen expenditure = rowtotal(p2478_*);
        label var expenditure "Expenditure";

    gen expenditure_mill = expenditure / 1000000;
        label var expenditure_mill "Expenditure (1,000,000)";

    gen expenditure_mill_ln = ln(expenditure_mill);
        label var expenditure_mill_ln "Expenditure (1,000,000) Ln";

    /*SAVINGS*/;
    gen savings = income - expenditure;
        label var savings "Savings";

    gen savings_mill = savings / 1000000;
        label var savings_mill "Savings (1,000,000)";

    gen savings_mill_ln = ln(savings_mill);
        label var savings_mill_ln "Savings (1,000,000) Ln";

    gen savings_income = (savings / income) * 100;
        label var savings_income "Savings / Income %";

    gen savings_income_ln = ln(savings_income);
        label var savings_income_ln "Savings / Income % Ln";

    keep gender expenditure* savings* income*;
    aorder;
    order gender;

**** #20 ** FULL SAMPLE ANALYSIS;

**** #20.1 ** DESCRIPTIVE STATISTICS;

    estpost tabstat *mill savings_income
        ,
        by(gender)
        statistics(count mean sd min max p50)
        columns(statistics)
        ;

    esttab using stats.xls
        ,
        cells("count mean(fmt(3)) p50(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))")
        noobs
        label
        tab
        replace
        ;

**** #20.2 ** GRAPHICAL ANALYSIS;

    /*VARIABLES IN LEVELS*/;
    foreach var in income_mill expenditure_mill savings_mill savings_income {;

        local title : variable label `var';

        graph box `var'
            ,
            over(gender)
            title(`title')
            scheme(s1mono)
            name(`var')
            ;

        };

        graph combine income_mill expenditure_mill savings_mill savings_income
            ,
            title(Variables in levels)
            cols(2)
            scheme(s1mono)
            ;

            graph export levels.jpg
                ,
                replace
                ;

    /*VARIABLES IN NATURAL LOGARITHM*/;
    foreach var in income_mill_ln expenditure_mill_ln savings_mill_ln savings_income_ln {;

        local title : variable label `var';

        graph box `var'
            ,
            over(gender)
            title(`title')
            scheme(s1mono)
            name(`var')
            ;

        };

        graph combine income_mill_ln expenditure_mill_ln savings_mill_ln savings_income_ln
            ,
            title(Variables in natural log)
            cols(2)
            scheme(s1mono)
            ;

            graph export ln.jpg
                ,
                replace
                ;

**** #20.3 ** ANOVA;

    anova savings_mill i.gender;
        margins i.gender;

    anova savings_income i.gender;
        margins i.gender;

**** #20.4 ** SLR;

    reg savings_mill i.gender;
        margins i.gender;

    reg savings_income i.gender;
        margins i.gender;

**** #30 ** TRIMMED SAMPLE ANALYSIS;

    foreach var in income_mill savings_mill savings_income {;

        sum `var', detail;
            local `var'_5 = r(p5);
            local `var'_95 = r(p95);

        keep if `var' > ``var'_5' & `var' < ``var'_95' ;

    };

**** #30.1 ** DESCRIPTIVE STATISTICS;

    estpost tabstat *mill savings_income
        ,
        by(gender)
        statistics(count mean sd min max p50)
        columns(statistics)
        ;

    esttab using stats_5_95.xls
        ,
        cells("count mean(fmt(3)) p50(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))")
        noobs
        label
        replace
        tab
        ;

**** #30.2 ** GRAPHICAL ANALYSIS;

    /*VARIABLES IN LEVELS*/;
    foreach var in income_mill expenditure_mill savings_mill savings_income {;

        local title : variable label `var';

        graph box `var'
            ,
            over(gender)
            title(`title')
            scheme(s1mono)
            name(`var'_5_95)
            ;

        };

        graph combine income_mill_5_95 expenditure_mill_5_95 savings_mill_5_95 savings_income_5_95
            ,
            title(Variables in levels)
            subtitle(Trimmed sample)
            cols(2)
            scheme(s1mono)
            ;

            graph export levels_5_95.jpg
                ,
                replace
                ;

    /*VARIABLES IN NATURAL LOGARITHM*/;
    foreach var in income_mill_ln expenditure_mill_ln savings_mill_ln savings_income_ln {;

        local title : variable label `var';

        graph box `var'
            ,
            over(gender)
            title(`title')
            subtitle(Trimmed sample)
            scheme(s1mono)
            name(`var'_5_95)
            ;

        };

        graph combine income_mill_ln_5_95 expenditure_mill_ln_5_95 savings_mill_ln_5_95 savings_income_ln_5_95
            ,
            title(Variables in natural log)
            cols(2)
            scheme(s1mono)
            ;

            graph export ln_5_95.jpg
                ,
                replace
                ;

**** #30.3 ** ANOVA;

    anova savings_mill i.gender;
        margins i.gender;

    anova savings_income i.gender;
        margins i.gender;

**** #30.4 ** SLR;

    reg savings_mill i.gender;
        margins i.gender;

    reg savings_income i.gender;
        margins i.gender;

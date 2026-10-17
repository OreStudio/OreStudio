/* Manage the conventions. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * This page owns the conventions interaction of a conventions configuration.
 * The two prototypes that used to repeat it hand the work here:
 *
 *   doc/prototypes/reporting/create-a-report/            step 2 links here
 *   doc/prototypes/reporting/change-what-a-report-runs/  its conventions row
 *                                                        links here
 *
 * The rail is computed from state: the set of every convention kind, then one
 * step per kind that needs a decision, then a final step that reads the set
 * back and offers the way out. A kind needs a decision when no document is
 * bound to it, or when the document picked for it differs from the one bound.
 * A kind that already has a document and needs no change gets no step, because
 * 25 steps is not a wizard.
 *
 * Every convention kind is one instrument family's set of market terms, one
 * ORE conventions.xml element, and one artefact table. The page draws real
 * rows from those tables, and says that the per-kind binding is a fixture
 * because the reporting model has no column for it.
 *
 * Three readings of the per-kind step only, behind ?variant=A|B|C. The set and
 * the rail are identical across the three.
 *
 *   ?step=N                               numeric over the current rail
 *   ?kind=<code>                          that kind's step, whatever its number
 *   ?variant=A|B|C                        which reading of the kind step
 *   ?guide=N                              the field variant C is showing
 *   ?from=create-a-report|change-what-a-report-runs
 *                                         where "Return to the report" goes
 */

(function () {
    'use strict';

    // ------------------------------------------------------------ mock data

    var NOW = new Date(2026, 9, 20, 14, 32, 0);

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };
    var PERSON = { name: 'Anneke Vos', role: 'Market-risk owner' };

    /* The conventions configuration the report binds. It is a reporting row of
       type conventions whose detail lives with refdata. It names no document
       per kind: the reporting model has no such column. */
    var CONFIGURATION = {
        id: 'cfg-4c81e2a7',
        name: 'Group conventions',
        type: 'conventions',
        owningComponent: 'ores.refdata',
        boundBy: 31
    };

    var BOUND_DEFINITIONS = [
        'Group Consolidated Exposure',
        'Board Risk Dashboard',
        'Group DV01 by book',
        'Treasury Sensitivity'
    ];

    /* The candidate documents of each kind, drawn from the curated data-quality
       tables ores_dq_<kind>_conventions_artefact_tbl. Every value below is a
       real row in ores_dev_bright_faraday: the ids, the field names and the
       values. A kind with an empty list has no curated row at all. */
    var DOCS = {"average_ois":[{"id":"GBP-BoEBASE-AVERAGE-OIS","version":1,"fields":[{"name":"spot_lag","value":0},{"name":"fixed_tenor","value":"6M"},{"name":"fixed_day_count_fraction","value":"ACT/365.FIXED"},{"name":"fixed_calendar","value":"UK"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_payment_convention","value":"ModifiedFollowing"},{"name":"index","value":"GBP-BoEBASE"},{"name":"on_tenor","value":"3M"},{"name":"rate_cutoff","value":"0"}]},{"id":"USD-AVERAGE-OIS-CONVENTIONS","version":1,"fields":[{"name":"spot_lag","value":2},{"name":"fixed_tenor","value":"6M"},{"name":"fixed_day_count_fraction","value":"30/360"},{"name":"fixed_calendar","value":"US"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_payment_convention","value":"ModifiedFollowing"},{"name":"index","value":"USD-FedFunds"},{"name":"on_tenor","value":"3M"},{"name":"rate_cutoff","value":"2"}]},{"id":"USD-AVERAGE-SOFR","version":1,"fields":[{"name":"spot_lag","value":2},{"name":"fixed_tenor","value":"6M"},{"name":"fixed_day_count_fraction","value":"30/360"},{"name":"fixed_calendar","value":"US"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_payment_convention","value":"ModifiedFollowing"},{"name":"index","value":"USD-SOFR"},{"name":"on_tenor","value":"3M"},{"name":"rate_cutoff","value":"2"}]}],"bma_basis_swap":[{"id":"USD-LIBOR-SIFMA-3M-BASIS-CONVENTIONS","version":1,"fields":[{"name":"index","value":"USD-LIBOR-3M"},{"name":"bma_index","value":"USD-SIFMA"}]},{"id":"USD-SOFR-SIFMA-3M-BASIS-CONVENTIONS","version":1,"fields":[{"name":"index","value":"USD-SOFR-3M"},{"name":"bma_index","value":"USD-SIFMA"}]},{"id":"USD-SOFR-SIFMA-BASIS-CONVENTIONS","version":1,"fields":[{"name":"index","value":"USD-SOFR"},{"name":"bma_index","value":"USD-SIFMA"},{"name":"bma_payment_lag","value":0},{"name":"index_payment_lag","value":2},{"name":"index_settlement_days","value":0},{"name":"index_payment_period","value":"3M"},{"name":"overnight_lockout_days","value":0}]}],"bond_yield":[{"id":"CMB-USD-MMD","version":1,"fields":[{"name":"compounding","value":"Continuous"},{"name":"frequency","value":"Weekly"},{"name":"price_type","value":"Clean"},{"name":"accuracy","value":1e-08},{"name":"max_evaluations","value":100},{"name":"guess","value":0.05}]},{"id":"CMB-US-TBILL-HD-13W","version":1,"fields":[{"name":"compounding","value":"Continuous"},{"name":"frequency","value":"Weekly"},{"name":"price_type","value":"Clean"},{"name":"accuracy","value":1e-08},{"name":"max_evaluations","value":100},{"name":"guess","value":0.05}]},{"id":"CMB-US-TIPS-10Y","version":1,"fields":[{"name":"compounding","value":"Continuous"},{"name":"frequency","value":"Weekly"},{"name":"price_type","value":"Clean"},{"name":"accuracy","value":1e-08},{"name":"max_evaluations","value":100},{"name":"guess","value":0.05}]}],"cds":[{"id":"CDS-STANDARD-CONVENTIONS","version":1,"fields":[{"name":"settlement_days","value":0},{"name":"calendar","value":"WeekendsOnly"},{"name":"frequency","value":"Quarterly"},{"name":"payment_convention","value":"Following"},{"name":"rule","value":"CDS2015"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"settles_accrual","value":true},{"name":"pays_at_default_time","value":true}]},{"id":"CDS-STANDARD-CONVENTIONS-EUR","version":1,"fields":[{"name":"settlement_days","value":0},{"name":"calendar","value":"TARGET"},{"name":"frequency","value":"Quarterly"},{"name":"payment_convention","value":"Following"},{"name":"rule","value":"CDS2015"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"settles_accrual","value":true},{"name":"pays_at_default_time","value":true}]},{"id":"CDS-STANDARD-CONVENTIONS-USD","version":1,"fields":[{"name":"settlement_days","value":0},{"name":"calendar","value":"USD"},{"name":"frequency","value":"Quarterly"},{"name":"payment_convention","value":"Following"},{"name":"rule","value":"CDS2015"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"settles_accrual","value":true},{"name":"pays_at_default_time","value":true}]}],"cms_spread_option":[{"id":"EUR-CMS-10Y-2Y-CONVENTION","version":1,"fields":[{"name":"forward_start","value":"0M"},{"name":"spot_days","value":"2D"},{"name":"swap_tenor","value":"3M"},{"name":"fixing_days","value":2},{"name":"calendar","value":"TARGET"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"roll_convention","value":"ModifiedFollowing"}]}],"commodity_forward":[{"id":"PM:XAGUSD","version":1,"fields":[{"name":"spot_days","value":2},{"name":"points_factor","value":100},{"name":"advance_calendar","value":"USD"},{"name":"spot_relative","value":true},{"name":"business_day_convention","value":"ModifiedFollowing"},{"name":"outright","value":false}]},{"id":"PM:XAUUSD","version":1,"fields":[{"name":"spot_days","value":2},{"name":"points_factor","value":100},{"name":"advance_calendar","value":"USD"},{"name":"spot_relative","value":true},{"name":"business_day_convention","value":"ModifiedFollowing"},{"name":"outright","value":false}]},{"id":"PM:XPDUSD","version":1,"fields":[{"name":"spot_days","value":2},{"name":"points_factor","value":100},{"name":"advance_calendar","value":"USD"},{"name":"spot_relative","value":true},{"name":"business_day_convention","value":"ModifiedFollowing"},{"name":"outright","value":false}]}],"commodity_future":[{"id":"BVMF:ATR","version":1,"fields":[{"name":"contract_frequency","value":"Monthly"},{"name":"calendar","value":"BRL"},{"name":"business_day_convention","value":"Following"},{"name":"is_averaging","value":false}]},{"id":"BVMF:ETH","version":1,"fields":[{"name":"contract_frequency","value":"Monthly"},{"name":"calendar","value":"BRL"},{"name":"business_day_convention","value":"Following"},{"name":"is_averaging","value":false}]},{"id":"CAISO_NP_15_DA_OPK","version":1,"fields":[{"name":"contract_frequency","value":"Daily"},{"name":"calendar","value":"NullCalendar"},{"name":"off_peak_index","value":"ICE:UNP"},{"name":"peak_index","value":"ICE:DPN"}]}],"cross_currency_basis":[{"id":"AUD-USD-IBOR-XCCY-BASIS","version":1,"fields":[{"name":"settlement_days","value":2},{"name":"settlement_calendar","value":"AU,US,UK"},{"name":"roll_convention","value":"ModifiedFollowing"},{"name":"flat_index","value":"USD-LIBOR-3M"},{"name":"spread_index","value":"AUD-BBSW-3M"},{"name":"is_resettable","value":true},{"name":"flat_index_is_resettable","value":true}]},{"id":"AUD-USD-XCCY-BASIS","version":1,"fields":[{"name":"settlement_days","value":2},{"name":"settlement_calendar","value":"AU,US"},{"name":"roll_convention","value":"ModifiedFollowing"},{"name":"flat_index","value":"USD-SOFR"},{"name":"spread_index","value":"AUD-BBSW-3M"},{"name":"is_resettable","value":true},{"name":"flat_index_is_resettable","value":true},{"name":"flat_tenor","value":"3M"},{"name":"spread_tenor","value":"3M"}]},{"id":"CAD-USD-ON-XCCY-BASIS","version":1,"fields":[{"name":"settlement_days","value":2},{"name":"settlement_calendar","value":"CA,US"},{"name":"roll_convention","value":"ModifiedFollowing"},{"name":"flat_index","value":"USD-SOFR"},{"name":"spread_index","value":"CAD-CORRA"},{"name":"is_resettable","value":true},{"name":"flat_index_is_resettable","value":true},{"name":"flat_tenor","value":"3M"},{"name":"spread_tenor","value":"3M"}]}],"cross_currency_fix_float":[{"id":"EUR-RON-XCCY-FIX-FLOAT-CONVENTIONS","version":1,"fields":[{"name":"settlement_days","value":2},{"name":"settlement_calendar","value":"TARGET,RON"},{"name":"settlement_convention","value":"ModifiedFollowing"},{"name":"fixed_currency","value":"RON"},{"name":"fixed_frequency","value":"Annual"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_day_count_fraction","value":"ACT/360"},{"name":"index","value":"EUR-EURIBOR-3M"}]},{"id":"MXN-MXV-XCCY-FIX-FLOAT-CONVENTIONS","version":1,"fields":[{"name":"settlement_days","value":2},{"name":"settlement_calendar","value":"MXN,MXV"},{"name":"settlement_convention","value":"ModifiedFollowing"},{"name":"fixed_currency","value":"MXV"},{"name":"fixed_frequency","value":"Monthly"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_day_count_fraction","value":"ACT/360"},{"name":"index","value":"MXN-TIIE-28D"}]},{"id":"USD-CLF-XCCY-FIX-FLOAT-CONVENTIONS","version":1,"fields":[{"name":"settlement_days","value":2},{"name":"settlement_calendar","value":"CLF,US,UK"},{"name":"settlement_convention","value":"ModifiedFollowing"},{"name":"fixed_currency","value":"CLF"},{"name":"fixed_frequency","value":"Semiannual"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_day_count_fraction","value":"ACT/360"},{"name":"index","value":"USD-LIBOR-6M"}]}],"currency_pair":[],"deposit":[{"id":"EUR-DEPOSIT","version":1,"fields":[{"name":"index_based","value":true},{"name":"index","value":"EUR-EURIBOR"}]},{"id":"GBP-DEPOSIT","version":1,"fields":[{"name":"index_based","value":true},{"name":"index","value":"GBP-LIBOR"}]},{"id":"USD-DEPOSIT","version":1,"fields":[{"name":"index_based","value":true},{"name":"index","value":"USD-LIBOR"}]}],"fra":[{"id":"AED-3M-FRA","version":1,"fields":[{"name":"index","value":"AED-EIBOR-3M"}]},{"id":"AUD-3M-FRA-CONVENTIONS","version":1,"fields":[{"name":"index","value":"AUD-BBSW-3M"}]},{"id":"AUD-6M-FRA","version":1,"fields":[{"name":"index","value":"AUD-BBSW-6M"}]}],"future":[{"id":"AUD-BBSW-3M-FUTURES-CONVENTIONS","version":1,"fields":[{"name":"index","value":"AUD-BBSW-3M"}]},{"id":"EURIBOR-3M-FUTURES-CONVENTIONS","version":1,"fields":[{"name":"index","value":"EUR-EURIBOR-3M"},{"name":"date_generation_rule","value":"IMM"},{"name":"netting_type","value":"Compounding"}]},{"id":"JPY-LIBOR-3M-FUTURES-CONVENTIONS","version":1,"fields":[{"name":"index","value":"JPY-LIBOR-3M"}]}],"fx_option":[{"id":"AUD-CAD-FXOPTION","version":1,"fields":[{"name":"fx_convention_id","value":"AUD-CAD-FX"},{"name":"atm_type","value":"AtmDeltaNeutral"},{"name":"delta_type","value":"PaSpot"},{"name":"switch_tenor","value":"2Y"},{"name":"long_term_atm_type","value":"AtmDeltaNeutral"},{"name":"long_term_delta_type","value":"PaFwd"},{"name":"risk_reversal_in_favor_of","value":"Call"},{"name":"butterfly_style","value":"Broker"}]},{"id":"AUD-JPY-FXOPTION","version":1,"fields":[{"name":"fx_convention_id","value":"AUD-JPY-FX"},{"name":"atm_type","value":"AtmDeltaNeutral"},{"name":"delta_type","value":"PaSpot"},{"name":"switch_tenor","value":"2Y"},{"name":"long_term_atm_type","value":"AtmDeltaNeutral"},{"name":"long_term_delta_type","value":"PaFwd"},{"name":"risk_reversal_in_favor_of","value":"Call"},{"name":"butterfly_style","value":"Broker"}]},{"id":"AUD-SGD-FXOPTION","version":1,"fields":[{"name":"fx_convention_id","value":"AUD-SGD-FX"},{"name":"atm_type","value":"AtmDeltaNeutral"},{"name":"delta_type","value":"PaFwd"},{"name":"switch_tenor","value":"0D"},{"name":"long_term_atm_type","value":"AtmDeltaNeutral"},{"name":"long_term_delta_type","value":"PaFwd"},{"name":"risk_reversal_in_favor_of","value":"Call"},{"name":"butterfly_style","value":"Broker"}]}],"ibor_index":[{"id":"AED-EIBOR-3M","version":1,"fields":[{"name":"fixing_calendar","value":"AED"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"settlement_days","value":2},{"name":"business_day_convention","value":"ModifiedFollowing"},{"name":"end_of_month","value":false}]},{"id":"USD-BSBY-3M","version":1,"fields":[{"name":"fixing_calendar","value":"US"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"settlement_days","value":0},{"name":"business_day_convention","value":"ModifiedFollowing"},{"name":"end_of_month","value":false}]},{"id":"USD-BSBY-6M","version":1,"fields":[{"name":"fixing_calendar","value":"US"},{"name":"day_count_fraction","value":"ACT/360"},{"name":"settlement_days","value":0},{"name":"business_day_convention","value":"ModifiedFollowing"},{"name":"end_of_month","value":false}]}],"inflation_swap":[{"id":"AUCPI_INFLATIONSWAP","version":1,"fields":[{"name":"fix_calendar","value":"AUD"},{"name":"fix_convention","value":"Following"},{"name":"day_count_fraction","value":"30/360"},{"name":"index","value":"AUCPI"},{"name":"interpolated","value":false},{"name":"observation_lag","value":"3M"},{"name":"adjust_inflation_observation_dates","value":false},{"name":"inflation_calendar","value":"AUD"},{"name":"inflation_convention","value":"Following"},{"name":"publication_roll","value":"AfterPublicationDate"}]},{"id":"BECPIHL_INFLATIONSWAP","version":1,"fields":[{"name":"fix_calendar","value":"TARGET"},{"name":"fix_convention","value":"ModifiedFollowing"},{"name":"day_count_fraction","value":"30/360"},{"name":"index","value":"BECPIHL"},{"name":"interpolated","value":false},{"name":"observation_lag","value":"3M"},{"name":"adjust_inflation_observation_dates","value":false},{"name":"inflation_calendar","value":"TARGET"},{"name":"inflation_convention","value":"ModifiedFollowing"}]},{"id":"BRCPIIPCA_INFLATIONSWAP","version":1,"fields":[{"name":"fix_calendar","value":"BRL"},{"name":"fix_convention","value":"ModifiedFollowing"},{"name":"day_count_fraction","value":"ACT/ACT.ISDA"},{"name":"index","value":"BRCPIIPCA"},{"name":"interpolated","value":false},{"name":"observation_lag","value":"2M"},{"name":"adjust_inflation_observation_dates","value":false},{"name":"inflation_calendar","value":"BRL"},{"name":"inflation_convention","value":"ModifiedFollowing"}]}],"intraday_power_load":[{"id":"LOAD_PROFILE_1_EXPLICT","version":1,"fields":[{"name":"explicit_load_profile","value":"2021-01-01|0;2021-01-04|50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50;2021-01-09|0;2021-01-11|50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50;2021-01-16|0;2021-01-18|50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50;2021-01-23|0;2021-01-25|50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50;2021-01-30|0"}]},{"id":"LOAD_PROFILE_1_RULEBASED","version":1,"fields":[{"name":"business_day_load_rules","value":"2020-01-01|US-NERC||"}]}],"ois":[{"id":"AED-EIBORON-OIS","version":1,"fields":[{"name":"spot_lag","value":1},{"name":"index","value":"AED-EIBORON"},{"name":"fixed_day_count_fraction","value":"ACT/365.FIXED"},{"name":"payment_lag","value":2},{"name":"end_of_month","value":false},{"name":"fixed_frequency","value":"Annual"},{"name":"fixed_convention","value":"Following"},{"name":"fixed_payment_convention","value":"Following"},{"name":"rule","value":"Backward"}]},{"id":"AED-OIS","version":1,"fields":[{"name":"spot_lag","value":1},{"name":"index","value":"AED-EIBORON"},{"name":"fixed_day_count_fraction","value":"ACT/365.FIXED"},{"name":"payment_lag","value":2},{"name":"end_of_month","value":false},{"name":"fixed_frequency","value":"Annual"},{"name":"fixed_convention","value":"Following"},{"name":"fixed_payment_convention","value":"Following"},{"name":"rule","value":"Backward"}]},{"id":"AUD-OIS","version":1,"fields":[{"name":"spot_lag","value":1},{"name":"index","value":"AUD-AONIA"},{"name":"fixed_day_count_fraction","value":"ACT/365.FIXED"},{"name":"payment_lag","value":2},{"name":"end_of_month","value":false},{"name":"fixed_frequency","value":"Annual"},{"name":"fixed_convention","value":"Following"},{"name":"fixed_payment_convention","value":"Following"},{"name":"rule","value":"Backward"}]}],"overnight_index":[{"id":"AED-EIBORON","version":1,"fields":[{"name":"fixing_calendar","value":"AED"},{"name":"day_count_fraction","value":"ACT/365.FIXED"},{"name":"settlement_days","value":0}]},{"id":"BRL-CDI","version":1,"fields":[{"name":"fixing_calendar","value":"BRL"},{"name":"day_count_fraction","value":"BUS/252"},{"name":"settlement_days","value":0}]},{"id":"BRL-SELIC","version":1,"fields":[{"name":"fixing_calendar","value":"BRL"},{"name":"day_count_fraction","value":"BUS/252"},{"name":"settlement_days","value":0}]}],"swap":[{"id":"AED-EIBOR-3M-SWAP","version":1,"fields":[{"name":"fixed_calendar","value":"TARGET"},{"name":"fixed_frequency","value":"Annual"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_day_count_fraction","value":"30/360"},{"name":"index","value":"AED-EIBOR-3M"}]},{"id":"AED-SWAP","version":1,"fields":[{"name":"fixed_calendar","value":"TARGET"},{"name":"fixed_frequency","value":"Annual"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_day_count_fraction","value":"30/360"},{"name":"index","value":"AED-EIBOR-3M"}]},{"id":"AMD-GENERIC-3M-SWAP","version":1,"fields":[{"name":"fixed_calendar","value":"AMD"},{"name":"fixed_frequency","value":"Semiannual"},{"name":"fixed_convention","value":"ModifiedFollowing"},{"name":"fixed_day_count_fraction","value":"ACT/365.FIXED"},{"name":"index","value":"AMD-GENERIC-3M"}]}],"swap_index":[{"id":"AED-CMS-1Y","version":1,"fields":[{"name":"conventions","value":"AED-SWAP"}]},{"id":"AED-CMS-30Y","version":1,"fields":[{"name":"conventions","value":"AED-SWAP"}]},{"id":"AOA-CMS-1Y","version":1,"fields":[{"name":"conventions","value":"AOA-SWAP"}]}],"tenor_basis_swap":[{"id":"AUD-BBSW-1M-3M-BASIS","version":1,"fields":[{"name":"long_index","value":"AUD-BBSW-3M"},{"name":"short_index","value":"AUD-BBSW-1M"}]},{"id":"AUD-BBSW-3M-6M-BASIS","version":1,"fields":[{"name":"pay_index","value":"AUD-BBSW-6M"},{"name":"receive_index","value":"AUD-BBSW-3M"}]},{"id":"CAD-CDOR-1M-3M-BASIS","version":1,"fields":[{"name":"long_index","value":"CAD-CDOR-3M"},{"name":"short_index","value":"CAD-CDOR-1M"}]}],"tenor_basis_two_swap":[{"id":"EUR-EURIBOR-6M-12M-BASIS-CONVENTIONS","version":1,"fields":[{"name":"calendar","value":"TARGET"},{"name":"long_fixed_frequency","value":"Annual"},{"name":"long_fixed_convention","value":"ModifiedFollowing"},{"name":"long_fixed_day_count_fraction","value":"30/360"},{"name":"long_index","value":"EUR-EURIBOR-12M"},{"name":"short_fixed_frequency","value":"Annual"},{"name":"short_fixed_convention","value":"ModifiedFollowing"},{"name":"short_fixed_day_count_fraction","value":"30/360"},{"name":"short_index","value":"EUR-EURIBOR-6M"},{"name":"long_minus_short","value":true}]},{"id":"EURIBOR-3M-6M-BASIS-CONVENTIONS","version":1,"fields":[{"name":"calendar","value":"TARGET"},{"name":"long_fixed_frequency","value":"Annual"},{"name":"long_fixed_convention","value":"ModifiedFollowing"},{"name":"long_fixed_day_count_fraction","value":"30/360"},{"name":"long_index","value":"EUR-EURIBOR-6M"},{"name":"short_fixed_frequency","value":"Annual"},{"name":"short_fixed_convention","value":"ModifiedFollowing"},{"name":"short_fixed_day_count_fraction","value":"30/360"},{"name":"short_index","value":"EUR-EURIBOR-3M"},{"name":"long_minus_short","value":true}]},{"id":"PLN-WIBOR-1M-3M-BASIS","version":1,"fields":[{"name":"calendar","value":"TARGET"},{"name":"long_fixed_frequency","value":"Annual"},{"name":"long_fixed_convention","value":"ModifiedFollowing"},{"name":"long_fixed_day_count_fraction","value":"ACT/ACT.ISDA"},{"name":"long_index","value":"PLN-WIBOR-3M"},{"name":"short_fixed_frequency","value":"Annual"},{"name":"short_fixed_convention","value":"ModifiedFollowing"},{"name":"short_fixed_day_count_fraction","value":"ACT/ACT.ISDA"},{"name":"short_index","value":"PLN-WIBOR-1M"},{"name":"long_minus_short","value":true}]}],"zero":[{"id":"AED-ZERO-TENOR-BASED","version":1,"fields":[{"name":"tenor_based","value":true},{"name":"day_count_fraction","value":"ACT/365.FIXED"},{"name":"compounding","value":"Continuous"},{"name":"compounding_frequency","value":"Daily"},{"name":"tenor_calendar","value":"AED"},{"name":"spot_lag","value":2},{"name":"spot_calendar","value":"AED"},{"name":"roll_convention","value":"Following"},{"name":"end_of_month","value":false}]},{"id":"CAD-ZERO-TENOR-BASED","version":1,"fields":[{"name":"tenor_based","value":true},{"name":"day_count_fraction","value":"ACT/365.FIXED"},{"name":"compounding","value":"Continuous"},{"name":"compounding_frequency","value":"Daily"},{"name":"tenor_calendar","value":"CAD"},{"name":"spot_lag","value":2},{"name":"spot_calendar","value":"CAD"},{"name":"roll_convention","value":"Following"},{"name":"end_of_month","value":false}]},{"id":"CCY-ZERO-CONVENTIONS-TENOR-BASED","version":1,"fields":[{"name":"tenor_based","value":true},{"name":"day_count_fraction","value":"ACT/365.FIXED"},{"name":"compounding","value":"Continuous"},{"name":"compounding_frequency","value":"Daily"},{"name":"tenor_calendar","value":"TARGET"},{"name":"spot_lag","value":2},{"name":"spot_calendar","value":"TARGET"},{"name":"roll_convention","value":"Following"},{"name":"end_of_month","value":false}]}],"zero_inflation_index":[{"id":"BECPIHL","version":1,"fields":[{"name":"region_name","value":"Belgium"},{"name":"region_code","value":"BE"},{"name":"revised","value":false},{"name":"frequency","value":"Monthly"},{"name":"availability_lag","value":"1M"},{"name":"currency","value":"EUR"}]},{"id":"BRCPIIPCA","version":1,"fields":[{"name":"region_name","value":"Brazil"},{"name":"region_code","value":"BR"},{"name":"revised","value":false},{"name":"frequency","value":"Monthly"},{"name":"availability_lag","value":"1M"},{"name":"currency","value":"BRL"}]},{"id":"CLCPI","version":1,"fields":[{"name":"region_name","value":"Chile"},{"name":"region_code","value":"CL"},{"name":"revised","value":false},{"name":"frequency","value":"Monthly"},{"name":"availability_lag","value":"1M"},{"name":"currency","value":"CLP"}]}]};

    var META = [
        {
            code: 'deposit', name: 'Deposit', ore: 'Deposit',
            what: 'A deposit convention fixes the near end of a yield curve: the short money-market ' +
                'instrument a curve starts from. It says how many days after the trade the deposit ' +
                'settles, which calendar its dates roll on, what happens when a date falls on a ' +
                'holiday, whether a month-end date stays at month end, and the day count its interest ' +
                'accrues on.',
            fixes: [
                'How far the near end of the curve starts from the trade date.',
                'The calendar and the business-day rule the settlement date rolls on.',
                'The day count the deposit accrues on.',
                'Whether the terms are inherited from an IBOR index instead: when index_based is ' +
                'true the index names the source and the other fields may be empty.'
            ]
        },
        {
            code: 'fra', name: 'FRA', ore: 'FRA',
            what: 'A FRA convention ties a forward rate agreement to an IBOR index. The model carries ' +
                'one field, the index, because the index convention owns the calendar, the day count ' +
                'and the settlement terms the FRA uses.',
            fixes: [
                'Which IBOR index a FRA reads, and through it the calendar, day count and settlement ' +
                'lag the FRA inherits.',
                'That a FRA carries no terms of its own: the one field is a pointer to the index ' +
                'convention.'
            ]
        },
        {
            code: 'future', name: 'Future', ore: 'Future',
            what: 'A future convention fixes how ORE builds the dates of a rate future and settles ' +
                'its fixings: the index it tracks, the rule that generates its dates, how it nets, ' +
                'the calendar it rolls on, and the overnight tenor it reads when it is an ' +
                'overnight-index future.',
            fixes: [
                'Which index the future settles against.',
                'The date generation rule and the netting type its contract months follow.',
                'The calendar its dates roll on, and the overnight tenor where one applies.'
            ]
        },
        {
            code: 'ois', name: 'OIS', ore: 'OIS',
            what: 'An OIS convention fixes both legs of an overnight-index swap, the instrument used ' +
                'to bootstrap a risk-free overnight discount curve. It names the overnight index, the ' +
                'spot and payment lags, and each leg\u2019s calendar, frequency, business-day rule, ' +
                'payment convention and day count.',
            fixes: [
                'How many days after the trade the swap starts and how long after a period ends it pays.',
                'The fixed leg\u2019s calendar, frequency, business-day rule, payment convention and ' +
                'day count.',
                'The overnight index the floating leg reads, and the rate cutoff that decides when a ' +
                'fixing is taken.'
            ]
        },
        {
            code: 'average_ois', name: 'Average OIS', ore: 'AverageOIS',
            what: 'An average OIS convention fixes how the floating leg of an overnight-index swap is ' +
                'averaged over a tenor, and how the fixed leg pays against it. It names the spot lag, ' +
                'the fixed tenor, the fixed leg\u2019s day count, calendar, business-day rule and ' +
                'payment convention, the index, the overnight tenor and the rate cutoff.',
            fixes: [
                'The tenor the overnight rate is averaged over, and the fixed tenor it pays against.',
                'The fixed leg\u2019s calendar, frequency, business-day rule, payment convention and ' +
                'day count.',
                'The index averaged, and the cutoff at which each fixing is taken.'
            ]
        },
        {
            code: 'swap', name: 'Swap', ore: 'Swap',
            what: 'A swap convention fixes the fixed-leg schedule and the floating-leg index of a ' +
                'standard interest rate swap, the instrument ORE bootstraps par swap rates from. It ' +
                'names the fixed calendar, frequency, business-day rule and day count, the floating ' +
                'index, the floating frequency and the sub-period coupon type.',
            fixes: [
                'The fixed leg\u2019s calendar, frequency, business-day rule and day count.',
                'The index the floating leg reads and how often it pays.',
                'Whether a coupon period that spans several index periods is compounded or paid flat, ' +
                'through sub_periods_coupon_type.'
            ]
        },
        {
            code: 'tenor_basis_swap', name: 'Tenor basis swap', ore: 'TenorBasisSwap',
            what: 'A tenor basis swap convention fixes the two legs of a swap whose legs pay different ' +
                'tenors off different indices. ORE lets the convention be written two ways and the ' +
                'corpus uses both: the paying and receiving form names pay and receive indices, and ' +
                'the long and short form names long and short indices.',
            fixes: [
                'The two indices and the frequency each leg pays at.',
                'Which leg carries the quoted spread, and whether a coupon includes it.',
                'Whether each leg is averaged, and the sub-period coupon type it uses.',
                'The long and short indices and tenors where the convention is written in that form.'
            ]
        },
        {
            code: 'tenor_basis_two_swap', name: 'Tenor basis two swap', ore: 'TenorBasisTwoSwap',
            what: 'A tenor basis two swap convention fixes both legs of a two-tenor basis swap: a long ' +
                'leg and a short leg paying different frequencies off different indices, and which of ' +
                'the two the quoted spread is measured from.',
            fixes: [
                'The long and short indices, and the frequency, business-day rule and day count each ' +
                'fixed leg uses.',
                'The calendar both legs roll on.',
                'Whether the spread is quoted long minus short.'
            ]
        },
        {
            code: 'cross_currency_basis', name: 'Cross-currency basis', ore: 'CrossCurrencyBasis',
            what: 'A cross-currency basis convention fixes both legs of a cross-currency basis swap: ' +
                'the two indices, their tenors and payment lags, the settlement calendar and roll ' +
                'rule, and the fixing and cutoff rules of each leg.',
            fixes: [
                'The flat and spread indices, their tenors and their payment lags.',
                'The settlement calendar, the settlement lag and the roll convention.',
                'Whether each leg includes the spread, and whether it is resettable.',
                'Each leg\u2019s lookback, fixing days, cutoff and whether the rate is averaged.'
            ]
        },
        {
            code: 'cross_currency_fix_float', name: 'Cross-currency fix-float', ore: 'CrossCurrencyFixFloat',
            what: 'A cross-currency fix-float convention fixes a cross-currency swap whose fixed leg ' +
                'pays one currency and whose floating leg pays an index in another. It names the ' +
                'settlement lag, calendar and business-day rule, the fixed currency and its schedule, ' +
                'and the floating index with its own fixing rules.',
            fixes: [
                'The settlement lag, calendar and business-day rule both legs roll on.',
                'The fixed currency, its frequency, business-day rule and day count.',
                'The floating index, and whether its coupon includes the spread.',
                'The floating leg\u2019s lookback, fixing days, cutoff and averaging.'
            ]
        },
        {
            code: 'bond_yield', name: 'Bond yield', ore: 'BondYield',
            what: 'A bond yield convention fixes how a bond\u2019s yield is quoted and solved: the ' +
                'compounding it is quoted on, the frequency it pays at, whether the price is clean or ' +
                'dirty, and the tolerance and iteration cap the solver runs to.',
            fixes: [
                'Whether the yield is compounded continuously, annually or another way.',
                'The coupon frequency the yield is quoted at.',
                'Whether the price read is clean or dirty.',
                'The accuracy and the maximum evaluations the solver may use, and the guess it starts ' +
                'from.'
            ]
        },
        {
            code: 'cds', name: 'CDS', ore: 'CDS',
            what: 'A CDS convention fixes the premium schedule and the accrual rules of a vanilla ' +
                'credit default swap: the settlement lag, the payment calendar and frequency, the ' +
                'payment convention, the roll rule that decides IMM and standard CDS dates, the day ' +
                'count, and whether accrued premium settles on default.',
            fixes: [
                'The settlement lag and the calendar the premium dates roll on.',
                'The payment frequency, the payment convention and the roll rule, such as CDS2015.',
                'The day count, whether accrued premium is settled, and whether payment is at default ' +
                'time.',
                'The upfront settlement lag and the day count of the last period.'
            ]
        },
        {
            code: 'cms_spread_option', name: 'CMS spread option', ore: 'CmsSpreadOption',
            what: 'A CMS spread option convention fixes the schedule of an option on the spread ' +
                'between two constant-maturity swap rates: where its forward starts, how long the ' +
                'underlying swap runs, when it fixes, and the calendar and conventions its schedule ' +
                'rolls on.',
            fixes: [
                'The forward start and the spot days.',
                'The tenor of the underlying swap and the fixing days.',
                'The calendar, the day count and the roll convention of the schedule.'
            ]
        },
        {
            code: 'commodity_forward', name: 'Commodity forward', ore: 'CommodityForward',
            what: 'A commodity forward convention fixes how ORE builds a commodity forward: how many ' +
                'days after the trade it starts, how its points are scaled, the calendar it advances ' +
                'on, whether it is quoted outright or as a spread to spot, and where it delivers.',
            fixes: [
                'The spot days and the calendar the start date advances on.',
                'The points factor that scales the forward points.',
                'Whether the quote is outright or spot-relative, and the delivery location.',
                'The business-day convention the dates roll on.'
            ]
        },
        {
            code: 'commodity_future', name: 'Commodity future', ore: 'CommodityFuture',
            what: 'A commodity future convention fixes when a commodity future expires, which contract ' +
                'months it lists, how its expiry anchors to a day of the month or a weekday, its ' +
                'trading hours and peak and off-peak indices, and the schedule of the options written ' +
                'on it.',
            fixes: [
                'The contract frequency and the contract months the future lists.',
                'The expiry calendar, the expiry month lag, the offset days and the anchor that fixes ' +
                'the expiry date.',
                'The hours per day, the peak and off-peak indices and the delivery location.',
                'The option schedule written against it, and the averaging rules where it averages.'
            ]
        },
        {
            code: 'fx_option', name: 'FX option', ore: 'FxOption',
            what: 'An FX option convention fixes how an FX option is quoted: which strike counts as at ' +
                'the money, how the delta is measured, the tenor at which the long-term quoting takes ' +
                'over, which side a risk reversal favours, and the butterfly style.',
            fixes: [
                'Which strike counts as at the money, through atm_type.',
                'Whether the delta is measured against spot or forward, through delta_type.',
                'The switch tenor where long-term atm and delta types take over.',
                'The side a risk reversal favours, and the butterfly style.'
            ]
        },
        {
            code: 'bma_basis_swap', name: 'BMA basis swap', ore: 'BMABasisSwap',
            what: 'A BMA basis swap convention fixes a basis swap whose two legs reference a BMA ' +
                'municipal index and an IBOR or overnight index, and how each leg\u2019s payments are ' +
                'scheduled.',
            fixes: [
                'The IBOR or overnight index and the BMA index the two legs read.',
                'Each leg\u2019s payment calendar, business-day rule and payment lag.',
                'The settlement days, the payment period and the overnight lockout days.'
            ]
        },
        {
            code: 'zero', name: 'Zero', ore: 'Zero',
            what: 'A zero convention fixes the day count and the compounding ORE uses when it ' +
                'bootstraps a discount or zero-rate curve. It is what the curve\u2019s zero pillar is ' +
                'quoted on.',
            fixes: [
                'The day count fraction the zero rate accrues on.',
                'The compounding and the compounding frequency.',
                'The tenor-based flag, the spot lag and spot calendar, and the roll convention and ' +
                'end-of-month rule where the zero is built from a tenor.'
            ]
        },
        {
            code: 'inflation_swap', name: 'Inflation swap', ore: 'InflationSwap',
            what: 'An inflation swap convention fixes how ORE builds an inflation swap: the calendars ' +
                'and conventions its legs roll on, the day count it accrues on, the zero-coupon ' +
                'inflation index it references, and how the index\u2019s observations are lagged and ' +
                'adjusted.',
            fixes: [
                'The fixed leg\u2019s calendar, business-day rule and day count.',
                'The inflation index, whether it is interpolated, and the observation lag.',
                'Whether inflation observation dates are adjusted, and the inflation calendar, ' +
                'convention and publication roll.',
                'The start delay and the convention it rolls on.'
            ]
        },
        {
            code: 'intraday_power_load', name: 'Intraday power load', ore: 'IntradayPowerLoad',
            what: 'An intraday power load convention fixes how ORE builds a load profile: either a ' +
                'list of dates each carrying its own load factors, or a list of business-day rules ' +
                'that generate them.',
            fixes: [
                'Whether the profile is written explicitly as dated load factors or generated from ' +
                'business-day rules.',
                'The load factors or rules themselves, which is the element\u2019s one data field.'
            ]
        },
        {
            code: 'ibor_index', name: 'IBOR index', ore: 'IborIndex',
            what: 'An IBOR index convention fixes the terms of a term interbank rate such as EURIBOR ' +
                'or USD LIBOR: the fixing calendar, the day count, the settlement lag, the ' +
                'business-day convention and the end-of-month rule.',
            fixes: [
                'The calendar the fixing dates come from.',
                'The day count the index accrues on and the settlement lag after a fixing.',
                'The business-day convention and whether a month-end fixing stays at month end.'
            ]
        },
        {
            code: 'overnight_index', name: 'Overnight index', ore: 'OvernightIndex',
            what: 'An overnight index convention fixes the terms of a risk-free overnight rate: the ' +
                'fixing calendar, the day count and the settlement lag.',
            fixes: [
                'The calendar the fixing dates come from.',
                'The day count the overnight rate accrues on.',
                'The settlement lag after a fixing.'
            ]
        },
        {
            code: 'swap_index', name: 'Named swap index', ore: 'SwapIndex',
            what: 'A named swap index convention points at the swap convention it follows rather than ' +
                'restating that convention\u2019s fields. It is an alias with its own fixing calendar.',
            fixes: [
                'Which swap convention the index follows, through the conventions field.',
                'The calendar its fixing dates come from.'
            ]
        },
        {
            code: 'currency_pair', name: 'Currency pair', ore: 'FX',
            what: 'A currency pair convention fixes the quoting and date terms of one currency pair: ' +
                'the spot days, the advance calendars, and the convention the pair follows. In the ' +
                'conventions document it is the FX message, a pair, its convention and its advance ' +
                'calendars.',
            fixes: [
                'The spot days the pair settles after.',
                'The advance calendars its dates roll on.',
                'The pair\u2019s own convention, which every FX instrument naming the pair reads.'
            ]
        },
        {
            code: 'zero_inflation_index', name: 'Zero inflation index', ore: 'ZeroInflationIndex',
            what: 'A zero-coupon inflation index convention fixes the published index an inflation ' +
                'instrument reads: the region it is published for, how often, how far behind its ' +
                'publication date it becomes available, and the currency it is quoted in.',
            fixes: [
                'The region name and code the index is published for.',
                'Whether the index is revised, and how often it is published.',
                'The availability lag before a published value is usable.',
                'The currency the index is quoted in.'
            ]
        }
    ];

    /* The document bound to each kind today, as the fixture. A null is a kind
       with no document bound. Five kinds are deliberately unbound: four have
       curated candidates, and one has none at all. */
    var BOUND = {
        deposit: 'USD-DEPOSIT',
        fra: 'AED-3M-FRA',
        future: 'EURIBOR-3M-FUTURES-CONVENTIONS',
        ois: 'AED-OIS',
        average_ois: null,
        swap: 'AED-SWAP',
        tenor_basis_swap: 'AUD-BBSW-3M-6M-BASIS',
        tenor_basis_two_swap: 'EURIBOR-3M-6M-BASIS-CONVENTIONS',
        cross_currency_basis: null,
        cross_currency_fix_float: 'EUR-RON-XCCY-FIX-FLOAT-CONVENTIONS',
        bond_yield: null,
        cds: 'CDS-STANDARD-CONVENTIONS',
        cms_spread_option: 'EUR-CMS-10Y-2Y-CONVENTION',
        commodity_forward: 'PM:XAUUSD',
        commodity_future: 'CAISO_NP_15_DA_OPK',
        fx_option: null,
        bma_basis_swap: 'USD-SOFR-SIFMA-3M-BASIS-CONVENTIONS',
        zero: 'CAD-ZERO-TENOR-BASED',
        inflation_swap: 'AUCPI_INFLATIONSWAP',
        intraday_power_load: 'LOAD_PROFILE_1_RULEBASED',
        ibor_index: 'USD-BSBY-3M',
        overnight_index: 'BRL-CDI',
        swap_index: 'AED-CMS-1Y',
        currency_pair: null,
        zero_inflation_index: 'BECPIHL'
    };

    /* The three readings of the per-kind step. Nothing else differs. */
    var VARIANTS = [
        {
            id: 'A',
            name: 'Document first',
            gist: 'The bound document and every field it holds, then the picker of the documents ' +
                'that exist for the kind.'
        },
        {
            id: 'B',
            name: 'Compare',
            gist: 'The picker beside a side-by-side compare of the bound document against the ' +
                'candidate, with the fields that differ marked.'
        },
        {
            id: 'C',
            name: 'Field by field',
            gist: 'One field at a time with its explanation beside it, for the fat families where a ' +
                'whole-document table is too much to read.'
        }
    ];

    var FIELD_HELP = {
        id: 'The document\u2019s natural key: the id ORE writes in conventions.xml.',
        index_based: 'When true, the other fields may be empty and the terms are inherited from the index named.',
        index: 'The reference rate the instrument reads.',
        calendar: 'The holiday calendar the dates roll on.',
        convention: 'What happens when a date falls on a holiday.',
        settlement_calendar: 'The holiday calendar the settlement dates roll on.',
        fixing_calendar: 'The holiday calendar the fixing dates come from.',
        fixed_calendar: 'The fixed leg\u2019s holiday calendar.',
        payment_calendar: 'The holiday calendar the payment dates roll on.',
        advance_calendar: 'The calendar the start date advances on.',
        expiry_calendar: 'The calendar the expiry date rolls on.',
        inflation_calendar: 'The calendar the inflation observations roll on.',
        tenor_calendar: 'The calendar the tenor rolls on.',
        spot_calendar: 'The calendar the spot date rolls on.',
        end_of_month: 'Whether a date at month end stays at month end when it rolls.',
        eom: 'Whether a date at month end stays at month end when it rolls.',
        day_count_fraction: 'How interest accrues between two dates.',
        fixed_day_count_fraction: 'How the fixed leg\u2019s interest accrues between two dates.',
        last_period_day_count_fraction: 'The day count the last coupon period uses.',
        settlement_days: 'How many business days after the trade the instrument settles.',
        spot_lag: 'How many days after the trade the swap starts.',
        spot_days: 'How many days after the trade the spot date falls.',
        payment_lag: 'How many business days after a period ends the payment is made.',
        fixed_frequency: 'How often the fixed leg pays.',
        float_frequency: 'How often the floating leg pays.',
        frequency: 'How often the instrument pays.',
        fixed_tenor: 'The tenor the fixed leg runs for.',
        on_tenor: 'The tenor the overnight rate is averaged over.',
        rate_cutoff: 'How many days before the fixing the rate is cut.',
        atm_type: 'Which strike counts as at the money.',
        delta_type: 'Whether delta is measured against spot or forward.',
        switch_tenor: 'The tenor where the long-term quoting types take over.',
        long_term_atm_type: 'The at-the-money type used beyond the switch tenor.',
        long_term_delta_type: 'The delta type used beyond the switch tenor.',
        risk_reversal_in_favor_of: 'The side a risk reversal favours.',
        butterfly_style: 'The convention the butterfly is quoted in.',
        compounding: 'How the rate is compounded: continuous, annual or another way.',
        compounding_frequency: 'How often the compounded rate is restated.',
        price_type: 'Whether the price read is clean or dirty.',
        accuracy: 'The tolerance the solver runs to.',
        max_evaluations: 'The most iterations the solver may use.',
        guess: 'The value the solver starts from.',
        rule: 'The roll rule that decides IMM and standard dates.',
        payment_convention: 'What happens when a payment date falls on a holiday.',
        settles_accrual: 'Whether accrued premium settles with the default payment.',
        pays_at_default_time: 'Whether payment is made at the default time.',
        upfront_settlement_days: 'How many business days after the trade the upfront amount settles.',
        fixed_convention: 'What happens when the fixed leg\u2019s date falls on a holiday.',
        fixed_payment_convention: 'What happens when a fixed payment date falls on a holiday.',
        roll_convention: 'What happens when a schedule date falls on a holiday.',
        business_day_convention: 'What happens when a date falls on a holiday.',
        flat_index: 'The flat leg\u2019s reference index.',
        spread_index: 'The spread leg\u2019s reference index.',
        flat_tenor: 'The tenor the flat leg runs for.',
        spread_tenor: 'The tenor the spread leg runs for.',
        flat_payment_lag: 'How many business days after a period the flat leg pays.',
        spread_payment_lag: 'How many business days after a period the spread leg pays.',
        spread_include_spread: 'Whether the spread leg\u2019s coupon includes the spread.',
        flat_include_spread: 'Whether the flat leg\u2019s coupon includes the spread.',
        is_resettable: 'Whether a leg\u2019s rate can be reset.',
        flat_index_is_resettable: 'Whether the flat leg\u2019s rate can be reset.',
        include_spread: 'Whether the coupon includes the spread.',
        lookback: 'How far before the fixing the rate is observed.',
        fixing_days: 'How many days before the period the fixing is taken.',
        is_averaged: 'Whether the leg averages its fixings.',
        observation_shift: 'Whether the observation date shifts for holidays.',
        tenor_based: 'Whether the zero is built from a tenor rather than a date.',
        interpolated: 'Whether the index is interpolated.',
        observation_lag: 'How far behind its publication the index is observed.',
        region_name: 'The region the index is published for.',
        region_code: 'The code of the region the index is published for.',
        revised: 'Whether the published index is revised.',
        availability_lag: 'How far behind its publication date the value becomes available.',
        currency: 'The currency the value is quoted in.',
        hours_per_day: 'The trading hours in one day.',
        off_peak_index: 'The index the off-peak hours read.',
        peak_index: 'The index the peak hours read.',
        index_name: 'The index the future settles against.',
        delivery_location: 'Where the commodity delivers.',
        points_factor: 'The factor that scales the forward points.',
        outright: 'Whether the quote is outright or a spread to spot.',
        spot_relative: 'Whether the quote is relative to spot.',
        contract_frequency: 'How often a contract month is listed.',
        expiry_month_lag: 'How many months before delivery the future expires.',
        offset_days: 'How many days the expiry is offset.',
        is_averaging: 'Whether the future averages its settle price.',
        sub_periods_coupon_type: 'How a coupon period spanning several index periods is paid.',
        pay_index: 'The index the paying leg reads.',
        receive_index: 'The index the receiving leg reads.',
        long_index: 'The index the long leg reads.',
        short_index: 'The index the short leg reads.',
        pay_frequency: 'How often the paying leg pays.',
        receive_frequency: 'How often the receiving leg pays.',
        spread_on_rec: 'Whether the quoted spread sits on the receiving leg.',
        spread_on_short: 'Whether the quoted spread sits on the short leg.',
        include_spread: 'Whether a coupon includes the spread.',
        long_minus_short: 'Whether the spread is quoted long minus short.',
        conventions: 'The swap convention the named index follows.',
        explicit_load_profile: 'The dated load factors, where the profile is written explicitly.',
        business_day_load_rules: 'The rules that generate the load factors from business days.',
        payment_period: 'The period the payment schedule uses.',
        explicit_advance_calendars: 'The calendars the pair advances on.'
    };

    var MODEL_ABSENCE = 'The reporting model does not name a document per convention kind. ' +
        'ores_reporting_configurations_tbl carries a name, a configuration_type_code and an ' +
        'owning_component, and no document column. The conventions document refdata owns is read ' +
        'with refdata.v1.conventions_documents.get, which takes a party_id and not a ' +
        'configuration_id, and it carries one vector per kind. So the per-kind binding on this page ' +
        'is a fixture, drawn from each kind\u2019s curated artefact table. Nothing in the model ' +
        'contradicts it, and nothing in the model states it.';

    var NO_SUBJECT_NOTE = 'No operation reads a curated artefact table. The candidate documents ' +
        'below are rows of ores_dq_<kind>_conventions_artefact_tbl, read as a fixture. The bound ' +
        'set would be read with refdata.v1.conventions_documents.get.';

    var SAME_TRADE = 'The same trade read under two convention sets gives two different numbers, ' +
        'and neither of them is wrong: each is the correct price under its own terms.';

    var WC = { set: 'The conventions set', last: 'What the conventions are now' };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function fmt(value) {
        if (value === null || value === undefined || value === '') return '\u2014';
        if (value === true) return 'true';
        if (value === false) return 'false';
        return String(value);
    }

    function isBlank(value) {
        return value === null || value === undefined || value === '';
    }

    function fixtureClock() {
        var days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
        var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        function pad(n) { return (n < 10 ? '0' : '') + n; }
        return days[NOW.getDay()] + ' ' + NOW.getDate() + ' ' + months[NOW.getMonth()] + ' ' +
            NOW.getFullYear() + ' ' + pad(NOW.getHours()) + ':' + pad(NOW.getMinutes()) +
            ' installation clock';
    }

    function fieldHelp(name) {
        return FIELD_HELP[name] || 'One of the terms the convention model declares for this kind.';
    }

    // --------------------------------------------------------------- state

    var S = {
        step: 'set',
        variant: 'A',
        guide: 0,
        kinds: [],
        focus: null,
        from: 'create-a-report',
        activity: 'the fixture, nothing written',
        showState: false,
        log: []
    };

    function seedKinds() {
        return META.map(function (meta) {
            var docs = (DOCS[meta.code] || []).map(function (doc) {
                return { id: doc.id, version: 1, fields: doc.fields };
            });
            var bound = BOUND[meta.code] || null;
            return {
                code: meta.code,
                name: meta.name,
                ore: meta.ore,
                what: meta.what,
                fixes: meta.fixes,
                table: 'ores_dq_' + meta.code + '_conventions_artefact_tbl',
                subject: 'refdata.v1.' + meta.code + '_conventions.list',
                docs: docs,
                bound: bound,
                pick: bound
            };
        });
    }

    function kindOf(code) {
        var found = S.kinds.filter(function (kind) { return kind.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function variantOf(id) {
        var found = VARIANTS.filter(function (entry) { return entry.id === id; });
        return found.length === 1 ? found[0] : VARIANTS[0];
    }

    function docOf(kind, id) {
        if (!id) return null;
        var found = kind.docs.filter(function (doc) { return doc.id === id; });
        return found.length === 1 ? found[0] : null;
    }

    function boundDoc(kind) {
        return docOf(kind, kind.bound);
    }

    function pickedDoc(kind) {
        return docOf(kind, kind.pick);
    }

    function needsStep(kind) {
        return !kind.bound || kind.pick !== kind.bound;
    }

    function pendingKinds() {
        return S.kinds.filter(needsStep);
    }

    function firstAlternative(kind) {
        var found = kind.docs.filter(function (doc) { return doc.id !== kind.bound; });
        return found.length > 0 ? found[0].id : null;
    }

    function stateOf(kind) {
        if (!kind.bound && !kind.pick) return { code: 'none', label: 'no document bound', tag: 'warn' };
        if (!kind.bound && kind.pick) return { code: 'new', label: 'newly bound', tag: 'accent' };
        if (kind.pick !== kind.bound) return { code: 'changed', label: 'changed', tag: 'accent' };
        return { code: 'bound', label: 'bound', tag: 'ok' };
    }

    function counts() {
        var bound = S.kinds.filter(function (k) { return !!k.bound; }).length;
        var unbound = S.kinds.filter(function (k) { return !k.bound; }).length;
        var changed = S.kinds.filter(function (k) { return !!k.bound && k.pick !== k.bound; }).length;
        var added = S.kinds.filter(function (k) { return !k.bound && !!k.pick; }).length;
        return { total: S.kinds.length, bound: bound, unbound: unbound, changed: changed,
            added: added, steps: pendingKinds().length };
    }

    // --------------------------------------------------------------- rail

    function rail() {
        var kindSteps = pendingKinds().map(function (kind) {
            return {
                id: 'kind-' + kind.code,
                title: kind.name,
                code: kind.code,
                kind: kind.code,
                lead: 'What a ' + kind.name + ' convention is, what it fixes about an instrument of ' +
                    'that kind, the document bound to it today, and the documents that exist for the ' +
                    'kind with the fields that distinguish one from another.'
            };
        });
        return [
            {
                id: 'set',
                title: WC.set,
                lead: 'Every convention kind the configuration covers, one row each, with the ' +
                    'document bound to it and its state. This is the index. A kind gets a step of ' +
                    'its own when no document is bound to it, or when the document picked for it ' +
                    'differs from the one bound; a kind that already has a document and needs no ' +
                    'change gets none, because 25 steps is not a wizard.'
            }
        ].concat(kindSteps).concat([
            {
                id: 'read-back',
                title: WC.last,
                lead: 'The set as it now reads, the kinds that changed, and the way back to the ' +
                    'report. It is final, so Back is refused from it.',
                final: true
            }
        ]);
    }

    function stepNumberOf(code) {
        var list = rail();
        for (var i = 0; i < list.length; i += 1) {
            if (list[i].id === 'kind-' + code) return i + 1;
        }
        return 0;
    }

    function railSummary() {
        var c = counts();
        return 'the set, ' + c.steps + ' kind' + (c.steps === 1 ? '' : 's') + ' that need' +
            (c.steps === 1 ? 's' : '') + ' a decision, and what the conventions are now \u00b7 ' +
            (c.steps + 2) + ' step' + (c.steps + 2 === 1 ? '' : 's');
    }

    // ------------------------------------------------------- the set step

    function setRow(kind) {
        var state = stateOf(kind);
        var stepNo = stepNumberOf(kind.code);
        var act;
        if (needsStep(kind)) {
            act = '<button type="button" class="btn small" data-act="go-kind" data-code="' +
                esc(kind.code) + '">Step ' + stepNo + ' \u2192</button>';
        } else if (firstAlternative(kind)) {
            act = '<button type="button" class="btn small" data-act="change" data-code="' +
                esc(kind.code) + '">Change</button>';
        } else {
            act = '<span class="hint">no other document exists for this kind</span>';
        }
        var sum;
        if (!kind.bound && kind.docs.length === 0) {
            sum = 'The curated table for this kind holds no row, so there is nothing to pick.';
        } else {
            sum = kind.docs.length + ' candidate document' + (kind.docs.length === 1 ? '' : 's') +
                ' \u00b7 ' + (boundDoc(kind) ? boundDoc(kind).fields.length : 0) +
                ' field' + ((boundDoc(kind) && boundDoc(kind).fields.length === 1) ? '' : 's') +
                ' in the bound document';
        }
        return '<div class="setrow ' + state.code + '">' +
            '<span class="kname">' + esc(kind.name) +
            '<span class="codename">' + esc(kind.code) + '</span></span>' +
            '<span class="kdoc' + (kind.bound ? '' : ' none') + '">' +
            (kind.bound ? esc(kind.bound) : 'no document bound') + '</span>' +
            '<span class="kstate"><span class="tag ' + state.tag + '">' + esc(state.label) +
            '</span>' + act + '</span>' +
            '<span class="ksum">' + esc(sum) + '</span>' +
            '</div>';
    }

    function setStepBody() {
        var c = counts();
        return '<div class="notice"><b>How a conventions configuration names a document for each ' +
            'kind.</b> ' + esc(MODEL_ABSENCE) + '</div>' +
            '<div class="counts">' +
            '<div class="c"><span class="n">' + c.total + '</span><span class="l">convention ' +
            'kinds</span></div>' +
            '<div class="c ok"><span class="n">' + c.bound + '</span><span class="l">with a ' +
            'document bound</span></div>' +
            '<div class="c bad"><span class="n">' + c.unbound + '</span><span class="l">with no ' +
            'document bound</span></div>' +
            '<div class="c warn"><span class="n">' + c.changed + '</span><span class="l">bound ' +
            'document changed</span></div>' +
            '<div class="c"><span class="n">' + c.steps + '</span><span class="l">steps ' +
            'besides this one</span></div>' +
            '</div>' +
            '<div class="notice warn"><b>The rule for which kinds get a step.</b> A kind gets a step ' +
            'when it has no document bound, or when the document picked for it differs from the one ' +
            'bound. A kind that already has a document and needs no change does not get a step, ' +
            'because 25 steps is not a wizard. <b>' + c.steps + ' of ' + c.total + '</b> kinds need ' +
            'a decision on this reading, so the rail carries ' + (c.steps + 2) + ' steps: this set, ' +
            'those ' + c.steps + ', and the read-back. Changing a bound kind adds its step; putting ' +
            'it back removes it.</div>' +
            '<div class="sectionhead" style="margin-top:14px"><h3>Every convention kind, one row ' +
            'each</h3><span class="meta">' + c.total + ' kinds \u00b7 25 curated artefact tables ' +
            '\u00b7 refdata.v1.conventions_documents.get \u00b7 fixture</span></div>' +
            '<div class="setlist">' + S.kinds.map(setRow).join('') + '</div>' +
            '<div class="reviewbar"><span class="count">The conventions belong to ' +
            '<b>' + esc(CONFIGURATION.name) + '</b>, a reporting configuration of type ' +
            '<b>' + esc(CONFIGURATION.type) + '</b> whose detail lives with ' +
            '<b>' + esc(CONFIGURATION.owningComponent) + '</b>. <b>' + CONFIGURATION.boundBy +
            '</b> definitions bind it, including ' + esc(BOUND_DEFINITIONS.join(', ')) + ', so a ' +
            'change here reaches every one of their next runs. A ' +
            'definition that must read a different set binds a configuration of its own.</span>' +
            '<span class="hint">' + esc(railSummary()) + '</span></div>' +
            '<p class="rowfoot">' + esc(NO_SUBJECT_NOTE) + '</p>';
    }

    // ----------------------------------------------------- one kind step

    function familyBlock(kind) {
        return '<div class="doc"><div class="explain">' +
            '<h3>What a ' + esc(kind.name.toLowerCase()) + ' convention is</h3>' +
            '<p>' + esc(kind.what) + '</p>' +
            '<h3>What it fixes about an instrument of that kind</h3>' +
            '<ul class="fixes">' + kind.fixes.map(function (line) {
                return '<li>' + esc(line) + '</li>';
            }).join('') + '</ul>' +
            '<p class="hint mono">ores.refdata.' + esc(kind.code) + '_convention \u00b7 ORE element ' +
            '&lt;' + esc(kind.ore) + '&gt; \u00b7 ' + esc(kind.table) + '</p>' +
            '</div></div>';
    }

    function documentCard(kind) {
        var doc = boundDoc(kind);
        if (!doc) {
            return '<div class="doc-card none">' +
                '<div class="dhead"><span class="did">no document bound</span>' +
                '<span class="tag warn">unbound</span></div>' +
                '<div class="dmeta">A run that reaches an instrument family this kind covers reads no ' +
                'terms for it until a document is bound.</div></div>';
        }
        return '<div class="doc-card">' +
            '<div class="dhead"><span class="did">' + esc(doc.id) + '</span>' +
            '<span class="tag ok">bound today</span></div>' +
            '<div class="dmeta"><span>' + esc(kind.table) + '</span><span>\u00b7</span>' +
            '<span>version <span class="mono">' + esc(doc.version) + '</span></span><span>\u00b7</span>' +
            '<span><span class="mono">' + esc(doc.fields.length) + '</span> fields</span>' +
            '<span>\u00b7</span><span>ORE element <span class="mono">&lt;' + esc(kind.ore) +
            '&gt;</span></span></div></div>';
    }

    function fieldsTable(doc, withHelp) {
        if (!doc) return '<p class="nosettings">No document is bound, so there are no fields to ' +
            'list for it.</p>';
        return '<div class="fieldtable"><table><thead><tr><th>field</th><th>value</th>' +
            (withHelp ? '<th>what it means</th>' : '') + '</tr></thead><tbody>' +
            doc.fields.map(function (field) {
                return '<tr><td class="fname">' + esc(field.name) + '</td>' +
                    '<td class="fval' + (isBlank(field.value) ? ' empty' : '') + '">' +
                    esc(fmt(field.value)) + '</td>' +
                    (withHelp ? '<td class="fhelp">' + esc(fieldHelp(field.name)) + '</td>' : '') +
                    '</tr>';
            }).join('') + '</tbody></table></div>';
    }

    function keyFieldNames(kind) {
        var source = boundDoc(kind) || pickedDoc(kind) || (kind.docs[0] || null);
        if (!source) return [];
        return source.fields.slice(0, 5).map(function (field) { return field.name; });
    }

    function valueOf(doc, name) {
        if (!doc) return undefined;
        var found = doc.fields.filter(function (field) { return field.name === name; });
        return found.length === 1 ? found[0].value : undefined;
    }

    function picker(kind) {
        if (kind.docs.length === 0) {
            return '<div class="pickwrap"><div class="sectionhead"><h3>The documents that exist ' +
                'for this kind</h3><span class="meta">' + esc(kind.table) + ' \u00b7 0 rows \u00b7 ' +
                'fixture</span></div>' +
                '<div class="notice warn"><b>This kind\u2019s curated table holds no row.</b> There ' +
                'is nothing to pick, so the kind stays unbound. The gap is in the table, not in the ' +
                'screen: the screen draws no candidate it does not have.</div></div>';
        }
        var names = keyFieldNames(kind);
        var rows = kind.docs.map(function (doc) {
            var cls = [];
            if (kind.bound === doc.id) cls.push('boundrow');
            if (kind.pick === doc.id) cls.push('pickedrow');
            return '<tr class="' + cls.join(' ') + '">' +
                '<td class="pickcell"><input type="radio" name="pick-' + esc(kind.code) +
                '" value="' + esc(doc.id) + '" data-act="pick" data-code="' + esc(kind.code) +
                '"' + (kind.pick === doc.id ? ' checked' : '') +
                ' aria-label="pick ' + esc(doc.id) + '"></td>' +
                '<td class="docid">' + esc(doc.id) +
                (kind.bound === doc.id ? ' <span class="tag ok">bound</span>' : '') + '</td>' +
                '<td class="dfield">v' + esc(doc.version) + '</td>' +
                names.map(function (name) {
                    return '<td class="dfield">' + esc(fmt(valueOf(doc, name))) + '</td>';
                }).join('') + '</tr>';
        }).join('');
        var clearRow = '<tr' + (kind.pick ? '' : ' class="pickedrow"') + '>' +
            '<td class="pickcell"><input type="radio" name="pick-' + esc(kind.code) + '" value="" ' +
            'data-act="pick" data-code="' + esc(kind.code) + '"' +
            (kind.pick ? '' : ' checked') + ' aria-label="no document bound"></td>' +
            '<td class="docid">no document bound</td><td class="dfield">\u2014</td>' +
            names.map(function () { return '<td class="dfield">\u2014</td>'; }).join('') + '</tr>';
        return '<div class="pickwrap"><div class="sectionhead"><h3>The documents that exist for ' +
            'this kind</h3><span class="meta">' + esc(kind.table) + ' \u00b7 ' + kind.docs.length +
            ' candidate' + (kind.docs.length === 1 ? '' : 's') + ' \u00b7 fixture</span></div>' +
            '<div class="table-wrap"><table class="doctable"><thead><tr><th>pick</th>' +
            '<th>document</th><th>version</th>' + names.map(function (name) {
                return '<th>' + esc(name) + '</th>';
            }).join('') + '</tr></thead><tbody>' + rows + clearRow + '</tbody></table></div>' +
            '<p class="rowfoot">The columns are the first fields the model declares for this kind, ' +
            'and they are the ones that tell one document from another. The empty cells are values ' +
            'the document leaves out; ' + esc(NO_SUBJECT_NOTE) + '</p></div>';
    }

    function unionFieldNames(kind) {
        var names = [];
        var add = function (doc) {
            if (!doc) return;
            doc.fields.forEach(function (field) {
                if (names.indexOf(field.name) < 0) names.push(field.name);
            });
        };
        add(boundDoc(kind));
        add(pickedDoc(kind));
        return names;
    }

    function diffFields(kind) {
        var before = boundDoc(kind);
        var after = pickedDoc(kind);
        return unionFieldNames(kind).filter(function (name) {
            return fmt(valueOf(before, name)) !== fmt(valueOf(after, name));
        });
    }

    function whatChanges(kind) {
        var before = boundDoc(kind);
        var after = pickedDoc(kind);
        var diffs = diffFields(kind);
        var head;
        if (!before && !after) {
            head = '<b>Nothing is bound and nothing is picked.</b> The kind stays without a ' +
                'document, so a run that reaches this family reads no terms for it.';
        } else if (!before && after) {
            head = '<b>It binds ' + esc(after.id) + ' to a kind that had no document.</b> A trade ' +
                'in this family gains the terms above, where it had none.';
        } else if (before && !after) {
            head = '<b>It unbinds the kind.</b> A run that reaches this family then reads no terms ' +
                'for it, and any figure that depends on those terms stops being comparable with one ' +
                'made while they were bound.';
        } else if (diffs.length === 0) {
            head = '<b>The document picked is the one already bound, so nothing changes.</b>';
        } else {
            head = '<b>Picking ' + esc(after.id) + ' changes ' + diffs.length + ' field' +
                (diffs.length === 1 ? '' : 's') + ':</b> ' +
                diffs.slice(0, 8).map(function (name) { return esc(name); }).join(', ') +
                (diffs.length > 8 ? ', and ' + (diffs.length - 8) + ' more' : '') + '.';
        }
        return '<div class="whatchanges">' + head +
            '<div>' + esc(SAME_TRADE) + ' A run made after the change is not comparable with one ' +
            'made before it, because the two runs did not read the same terms.</div>' +
            '<div class="same">The instrument, the trade and the market data do not change. Only the ' +
            'terms this kind supplies change.</div></div>';
    }

    function variantTag() {
        var v = variantOf(S.variant);
        return '<p class="varianttag"><span class="tag accent">variant ' + esc(v.id) + '</span>' +
            '<b>' + esc(v.name) + '</b><span>' + esc(v.gist) + '</span></p>';
    }

    /* A: the bound document first, then every field it holds, then the picker. */
    function kindA(kind) {
        var doc = boundDoc(kind);
        return '<div class="vframe">' +
            '<div class="sectionhead"><h3>The document bound today</h3>' +
            '<span class="meta">refdata.v1.conventions_documents.get \u00b7 fixture</span></div>' +
            documentCard(kind) +
            '<div class="sectionhead"><h3>Every field the bound document holds</h3>' +
            '<span class="meta">' + (doc ? doc.fields.length + ' fields \u00b7 ' +
            esc(kind.ore) : 'no document bound') + '</span></div>' +
            fieldsTable(doc, false) +
            picker(kind) +
            whatChanges(kind) +
            stepNav(kind) + '</div>';
    }

    /* B: the picker and a side-by-side compare of the bound document against the
       candidate. */
    function kindB(kind) {
        var before = boundDoc(kind);
        var after = pickedDoc(kind);
        var names = unionFieldNames(kind);
        var side = function (doc, label, cls) {
            if (!doc) {
                return '<div class="side none"><div class="sh"><span class="did">no document ' +
                    'bound</span><span class="tag warn">' + esc(label) + '</span></div>' +
                    '<p class="hint">The kind carries no document, so every field is empty.</p></div>';
            }
            return '<div class="side ' + cls + '"><div class="sh"><span class="did">' +
                esc(doc.id) + '</span><span class="tag ' + (cls === 'boundside' ? 'ok' : 'accent') +
                '">' + esc(label) + '</span></div>' +
                fieldsTable(doc, false) + '</div>';
        };
        var difflist = names.map(function (name) {
            var bv = valueOf(before, name);
            var cv = valueOf(after, name);
            var diff = fmt(bv) !== fmt(cv);
            return '<div class="diffline' + (diff ? ' diff' : '') + '">' +
                '<span class="fn">' + esc(name) + '</span>' +
                '<span class="bv' + (isBlank(bv) ? ' empty' : '') + '">' + esc(fmt(bv)) + '</span>' +
                '<span class="cv' + (isBlank(cv) ? ' empty' : '') + '">' + esc(fmt(cv)) + '</span>' +
                (diff ? '<span class="tag warn">differs</span>' : '<span class="tag muted">same</span>') +
                '</div>';
        }).join('');
        return '<div class="vframe">' +
            picker(kind) +
            '<div class="sectionhead"><h3>Bound against the candidate</h3>' +
            '<span class="meta">field by field \u00b7 ' + diffFields(kind).length + ' differ \u00b7 ' +
            'fixture</span></div>' +
            '<div class="cmp">' + side(before, 'bound today', 'boundside') +
            side(after, 'candidate', 'candside') + '</div>' +
            '<div class="difflist"><div class="diffhead">' +
            '<span class="fn">field</span><span>bound today</span><span>candidate</span>' +
            '<span></span></div>' + difflist + '</div>' +
            whatChanges(kind) +
            stepNav(kind) + '</div>';
    }

    /* C: one field at a time with the explanation beside it. The left column is
       every field of the two documents; the right column is the chosen field\u2019s
       value in each document and what the field means. */
    function kindC(kind) {
        var before = boundDoc(kind);
        var after = pickedDoc(kind);
        var names = unionFieldNames(kind);
        var total = names.length;
        var at = Math.min(Math.max(S.guide, 0), Math.max(total - 1, 0));
        var list = names.map(function (name, index) {
            var cls = index === at ? 'current' : (index < at ? 'done' : '');
            return '<div class="fitem ' + cls + '">' +
                '<span class="n">' + (index + 1) + '</span>' + esc(name) + '</div>';
        }).join('');
        var main;
        if (total === 0) {
            main = '<div class="fmain"><p class="nosettings">No field to walk: neither the bound ' +
                'document nor a candidate carries one.</p></div>';
        } else {
            var name = names[at];
            var bv = valueOf(before, name);
            var cv = valueOf(after, name);
            main = '<div class="fmain">' +
                '<p class="gcount">field ' + (at + 1) + ' of ' + total + '</p>' +
                '<div class="fexplain"><b>' + esc(name) + '.</b> ' + esc(fieldHelp(name)) + '</div>' +
                '<div class="fvalue"><span class="label">bound today' +
                (before ? ' \u00b7 ' + esc(before.id) : '') + '</span>' +
                '<span class="v' + (isBlank(bv) ? ' empty' : '') + '">' + esc(fmt(bv)) + '</span></div>' +
                '<div class="fvalue"><span class="label">candidate' +
                (after ? ' \u00b7 ' + esc(after.id) : ' \u00b7 none picked') + '</span>' +
                '<span class="v' + (isBlank(cv) ? ' empty' : '') + '">' + esc(fmt(cv)) + '</span></div>' +
                '<div class="stepnav">' +
                '<button type="button" class="btn ghost" data-act="guide-prev"' +
                (at === 0 ? ' disabled' : '') + '>Previous field</button>' +
                '<button type="button" class="btn primary ml-auto" data-act="guide-next">' +
                (at === total - 1 ? 'Back to the first field' : 'Next field') + '</button></div></div>';
        }
        return '<div class="vframe">' +
            picker(kind) +
            '<div class="sectionhead"><h3>One field at a time</h3>' +
            '<span class="meta">' + total + ' fields \u00b7 the explanation is beside each value ' +
            '\u00b7 fixture</span></div>' +
            '<div class="fieldwalk"><div class="flist">' + list + '</div>' + main + '</div>' +
            whatChanges(kind) +
            stepNav(kind) + '</div>';
    }

    function stepNav(kind) {
        return '<div class="stepnav">' +
            '<span class="hint">Step ' + stepNumberOf(kind.code) + ' of ' + rail().length +
            ' \u00b7 <span class="mono">' + esc(kind.code) + '</span> \u00b7 ' +
            esc(kind.docs.length) + ' candidate document' + (kind.docs.length === 1 ? '' : 's') +
            '</span>' +
            (kind.bound && kind.pick !== kind.bound
                ? '<button type="button" class="btn ghost" data-act="keep" data-code="' +
                  esc(kind.code) + '">Keep the bound document</button>'
                : '') +
            '</div>';
    }

    function kindStepBody(code) {
        var kind = kindOf(code);
        if (!kind) return '<p class="nosettings">No such convention kind.</p>';
        var body;
        if (S.variant === 'B') body = kindB(kind);
        else if (S.variant === 'C') body = kindC(kind);
        else body = kindA(kind);
        return variantTag() + familyBlock(kind) + body;
    }

    // ------------------------------------------------- the final step

    function readbackTable() {
        return '<div class="table-wrap"><table><thead><tr><th>kind</th><th>state</th>' +
            '<th>bound before</th><th>now</th><th>changed</th></tr></thead><tbody>' +
            S.kinds.map(function (kind) {
                var state = stateOf(kind);
                var changed = (kind.bound || '') !== (kind.pick || '');
                return '<tr' + (changed ? ' class="changed-row"' : '') + '>' +
                    '<td>' + esc(kind.name) + ' <span class="mono hint">' + esc(kind.code) +
                    '</span></td>' +
                    '<td><span class="tag ' + state.tag + '">' + esc(state.label) + '</span></td>' +
                    '<td class="mono">' + esc(kind.bound || '\u2014') + '</td>' +
                    '<td class="mono">' + esc(kind.pick || '\u2014') + '</td>' +
                    '<td>' + (changed ? '<span class="tag warn">yes</span>'
                        : '<span class="tag muted">no</span>') + '</td></tr>';
            }).join('') + '</tbody></table></div>';
    }

    function changedTable() {
        var rows = S.kinds.filter(function (kind) {
            return (kind.bound || '') !== (kind.pick || '');
        });
        if (rows.length === 0) {
            return '<div class="notice ok"><b>No kind changed.</b> Every kind that had a document ' +
                'keeps it, and no kind gained one.</div>';
        }
        return '<div class="writeblock"><div class="op"><span class="what">The kinds that ' +
            'changed</span><span class="subject">refdata.v1.conventions_documents.put</span></div>' +
            '<div class="table-wrap"><table class="changedtable"><thead><tr><th>kind</th>' +
            '<th>was</th><th>now</th><th>the step</th></tr></thead><tbody>' +
            rows.map(function (kind) {
                return '<tr><td>' + esc(kind.name) + '</td>' +
                    '<td class="mono">' + esc(kind.bound || '\u2014') + '</td>' +
                    '<td class="mono">' + esc(kind.pick || '\u2014') + '</td>' +
                    '<td class="mono">step ' + stepNumberOf(kind.code) + '</td></tr>';
            }).join('') + '</tbody></table></div></div>';
    }

    function returnHref() {
        if (S.from === 'change-what-a-report-runs') {
            return '../change-what-a-report-runs/?step=1';
        }
        return '../create-a-report/?step=2';
    }

    function returnText() {
        if (S.from === 'change-what-a-report-runs') {
            return 'Return to the report, on Change what a report runs';
        }
        return 'Return to the report, on Create a report';
    }

    function finalBody() {
        var c = counts();
        return '<div class="readback" style="display:grid;gap:14px">' +
            '<div class="banner"><span class="tag ok">read back</span>' +
            '<span>' + c.total + ' convention kinds: ' + c.bound + ' bound, ' + c.unbound +
            ' with no document, and ' + (c.changed + c.added) + ' changed in this sitting, on ' +
            esc(CONFIGURATION.name) + '.</span></div>' +
            '<div class="counts">' +
            '<div class="c ok"><span class="n">' + c.bound + '</span><span class="l">bound ' +
            'documents</span></div>' +
            '<div class="c bad"><span class="n">' + c.unbound + '</span><span class="l">kinds ' +
            'with no document</span></div>' +
            '<div class="c warn"><span class="n">' + (c.changed + c.added) + '</span>' +
            '<span class="l">changed in this sitting</span></div>' +
            '<div class="c"><span class="n">' + c.steps + '</span><span class="l">kind steps ' +
            'the rail carried</span></div></div>' +
            changedTable() +
            '<div class="writeblock"><div class="op"><span class="what">The whole set, as it now ' +
            'reads</span><span class="subject">refdata.v1.conventions_documents.get</span></div>' +
            readbackTable() + '</div>' +
            '<div class="writeblock"><div class="op"><span class="what">What a save would write, ' +
            'and what it cannot write</span>' +
            '<span class="subject">refdata.v1.conventions_documents.put</span></div>' +
            '<p class="hint">The conventions document is written whole, for the session\u2019s ' +
            'party, with <span class="mono">refdata.v1.conventions_documents.put</span>. <b>No ' +
            'operation binds a document to one kind of one reporting configuration.</b> The get and ' +
            'the put take a party and not a configuration, and the reporting configuration has no ' +
            'column for a per-kind document. The per-kind binding on this page is a fixture; a build ' +
            'needs a model and an operation before this set can be stored as it is drawn.</p></div>' +
            '<div class="notice"><b>The conventions belong to the party\u2019s conventions document, ' +
            'not to the definition.</b> ' + CONFIGURATION.boundBy + ' definitions bind ' +
            esc(CONFIGURATION.name) + ', including ' + esc(BOUND_DEFINITIONS.join(', ')) +
            '. Their next run ' +
            'reads the set this page leaves, and none of them was opened. ' + esc(SAME_TRADE) +
            '</div>' +
            '<div class="reach">' +
            '<div class="does"><h3>What this reaches</h3><ul>' +
            '<li>The next run of all ' + CONFIGURATION.boundBy + ' definitions bound to ' +
            esc(CONFIGURATION.name) + '.</li>' +
            '<li>Every instrument family a changed kind covers, and every figure read through its ' +
            'terms.</li>' +
            '<li>The day count, the calendars and the indices every analytic reads through the ' +
            'conventions.</li></ul></div>' +
            '<div class="not"><h3>What it does not reach</h3><ul>' +
            '<li>A run already produced: a run keeps the terms it read.</li>' +
            '<li>The reference data lists a convention picks from: calendars, day counts, tenors and ' +
            'indices are untouched.</li>' +
            '<li>The market data, the curves or the pricing engines: those are other configurations ' +
            'of the same report.</li></ul></div></div>' +
            '<p class="hint">The read-back is a fixture. No subject is invented for the per-kind ' +
            'binding, and the page states the model absence instead.</p></div>';
    }

    // ------------------------------------------------------- page parts

    function appHeader() {
        return '<div class="appheader"><div class="appheader-inner">' +
            '<div class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></div>' +
            '<nav class="appnav">' +
            '<a href="#" data-act="noop">Parties</a>' +
            '<a href="#" data-act="noop">Books</a>' +
            '<a href="#" class="here">Reporting</a>' +
            '<a href="#" data-act="noop">Compute</a>' +
            '</nav>' +
            '<span class="modechip">' + esc(PARTY.code) + ' \u00b7 ' + esc(PERSON.role) + '</span>' +
            '</div></div>';
    }

    function pageHead() {
        return '<div class="pagehead"><div>' +
            '<div class="crumbs"><span>Reporting</span><span>\u203a</span>' +
            '<span>' + esc(CONFIGURATION.name) + '</span><span>\u203a</span>' +
            '<span>Conventions</span></div>' +
            '<h1>Manage the conventions</h1>' +
            '<p class="lede">The conventions of <b>' + esc(CONFIGURATION.name) + '</b>, a reporting ' +
            'configuration of type conventions whose detail lives with refdata. This page owns the ' +
            'whole interaction: the set of every convention kind, a step for each kind that needs a ' +
            'decision, and a last step that reads the set back. Create a report and Change what a ' +
            'report runs link here instead of drawing it again.</p></div>' +
            '<div class="actions">' +
            '<a class="btn linklike" href="' + esc(returnHref()) + '">Back to the report</a>' +
            '<span class="updated">' + esc(fixtureClock()) + '</span>' +
            '</div></div>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>This page is a prototype, and every value on it is a ' +
            'fixture.</b> Nothing reads a server and nothing is written. The configuration, the ' +
            'definitions bound to it and every bound document are mock values. The candidate ' +
            'documents and their fields are real rows of the curated artefact tables in ' +
            'ores_dev_bright_faraday. The subjects named are the reporting and refdata services\u2019 ' +
            'own; no subject is invented for a gap.</div>';
    }

    function stepHeaderHtml() {
        return '<div class="stepheader"><div class="metaline">' +
            '<span class="nm">' + esc(CONFIGURATION.name) + '</span>' +
            '<span class="sub">type ' + esc(CONFIGURATION.type) + ' \u00b7 owned by ' +
            esc(CONFIGURATION.owningComponent) + ' \u00b7 ' + CONFIGURATION.boundBy +
            ' definitions bound</span></div></div>';
    }

    // ------------------------------------------------------------- rail

    function railHtml(list, at) {
        var firstKind = -1;
        var firstFinal = -1;
        list.forEach(function (entry, index) {
            if (firstKind < 0 && entry.kind) firstKind = index;
            if (firstFinal < 0 && entry.final) firstFinal = index;
        });
        return '<div class="railcol">' +
            '<p class="railsum">' + esc(railSummary()) + '</p>' +
            '<nav class="railnav" aria-label="Steps"><ol>' +
            list.map(function (entry, index) {
                var cls = index === at ? 'current' : (index < at ? 'done' : 'ahead');
                var group = index === firstKind || index === firstFinal ? ' railstarts' : '';
                return '<li class="railentry ' + cls + group + '"' +
                    (index === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' +
                    (index < at ? '\u2713' : String(index + 1)) + '</span>' +
                    '<span class="railtext">' + esc(entry.title) +
                    (entry.code ? ' <span class="railcode">' + esc(entry.code) + '</span>' : '') +
                    (entry.final ? ' <span class="railflag">final</span>' : '') +
                    '</span></li>';
            }).join('') + '</ol></nav></div>';
    }

    function nextOf(list, at) {
        if (at >= list.length - 1) return null;
        return { label: 'Next: ' + list[at + 1].title.toLowerCase(), enabled: true };
    }

    function backAllowed(list, at) {
        return at > 0 && list[at - 1].final !== true && list[at].final !== true;
    }

    function footHtml(step, at) {
        var list = rail();
        var next = nextOf(list, at);
        var back = backAllowed(list, at);
        if (step.final) {
            return '<div class="stepfoot">' +
                '<span class="hint">this step is final, so it offers no Back</span>' +
                '<a class="btn primary ml-auto" href="' + esc(returnHref()) + '">' +
                esc(returnText()) + '</a></div>';
        }
        if (next === null && !back) return '';
        return '<div class="stepfoot">' +
            (back ? '<button type="button" class="btn ghost" data-act="back">Back</button>' : '') +
            (next !== null
                ? '<button type="button" class="btn primary ml-auto" data-act="next">' +
                  esc(next.label) + '</button>'
                : '') + '</div>';
    }

    function stepBody(step) {
        if (step.id === 'set') return setStepBody();
        if (step.kind) return kindStepBody(step.kind);
        return finalBody();
    }

    function at() {
        var list = rail();
        for (var i = 0; i < list.length; i += 1) {
            if (list[i].id === S.step) return i;
        }
        return -1;
    }

    function pos() {
        var index = at();
        return index < 0 ? 0 : index;
    }

    function goTo(index) {
        var list = rail();
        var target = Math.min(list.length - 1, Math.max(0, index));
        S.step = list[target].id;
        S.focus = null;
        S.guide = 0;
        setActivity(list[target].final
            ? 'reading the set back'
            : 'step ' + (target + 1) + ' of ' + list.length + ' \u00b7 ' + list[target].title);
        render();
    }

    // ------------------------------------------------------------- bars

    function renderNote() {
        var position = pos();
        var list = rail();
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 fixture data, nothing reads a server, nothing is written \u00b7 ' +
            'step ' + (position + 1) + ' of ' + list.length + ', ' + list[position].title +
            ' \u00b7 variant ' + S.variant + ' \u00b7 ' + S.activity +
            ' \u00b7 ' + PERSON.name + ', ' + PERSON.role + ', party ' + PARTY.code;
    }

    function renderBar() {
        var position = pos();
        var list = rail();
        var steps = list.map(function (step, index) {
            return '<button data-act="step" data-step="' + esc(step.id) + '"' +
                (position === index ? ' class="on"' : '') + '>' + (index + 1) + ' ' +
                esc(step.title) + '</button>';
        }).join('');
        var variants = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + esc(v.id) + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.id) + ' ' +
                esc(v.name.toLowerCase()) + '</button>';
        }).join('');
        var c = counts();
        var stateLine = '<span class="label">step <b>' + (position + 1) + ' of ' + list.length +
            '</b> \u00b7 ' + esc(list[position].title) + ' \u00b7 state: <b>' +
            esc(S.activity) + '</b></span>';
        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture clock: <b>' + esc(fixtureClock()) + '</b></span>' +
            '<span>configuration: <b>' + esc(CONFIGURATION.name) + '</b></span>' +
            '<span>definitions bound: <b>' + CONFIGURATION.boundBy + '</b></span>' +
            '<span>convention kinds: <b>' + c.total + '</b></span>' +
            '<span>bound: <b>' + c.bound + '</b></span>' +
            '<span>no document: <b>' + c.unbound + '</b></span>' +
            '<span>changed: <b>' + (c.changed + c.added) + '</b></span>' +
            '<span>rail: <b>' + esc(railSummary()) + '</b></span>' +
            '<span>this step: <b>' + esc(S.step) + '</b></span>' +
            '<span>variant: <b>' + S.variant + '</b></span>' +
            '<span>return goes to: <b>' + esc(S.from) + '</b></span>' +
            '</div>' +
            '<p class="state-note">One party, because a configuration belongs to one party. Every ' +
            'bound document is a fixture; the candidate documents are real rows of the curated ' +
            'artefact tables. Nothing reads a server.</p>' +
            (S.log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + S.log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + steps +
            '<span class="sep">|</span>' +
            '<span class="label">variant</span>' + variants +
            '<span class="sep">|</span>' + stateLine +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') +
            '</button></div>' + statePanel;
    }

    function scrollRail() {
        var nav = document.querySelector('.railnav');
        var current = nav && nav.querySelector('.railentry.current');
        if (!nav || !current || nav.scrollHeight <= nav.clientHeight) return;
        var box = nav.getBoundingClientRect();
        var entry = current.getBoundingClientRect();
        if (entry.top < box.top) nav.scrollTop -= box.top - entry.top + 8;
        else if (entry.bottom > box.bottom) nav.scrollTop += entry.bottom - box.bottom + 8;
    }

    function render() {
        if (at() < 0) S.step = 'set';
        var list = rail();
        var position = pos();
        var step = list[position];
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() +
            '<main>' + pageHead() + fixtureNote() +
            '<div class="journey">' + railHtml(list, position) +
            '<section class="card">' + stepHeaderHtml() +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(step.lead) + '</p>' +
            stepBody(step) + footHtml(step, position) +
            '</section></div>' +
            '</main></div>';
        renderNote();
        renderBar();
        scrollRail();
        writeParams();
    }

    // --------------------------------------------------------- actions

    function setActivity(text) { S.activity = text; }

    function logStep(text) {
        S.log.unshift(text);
        if (S.log.length > 12) S.log.pop();
    }

    function pickDocument(code, id) {
        var kind = kindOf(code);
        if (!kind) return;
        kind.pick = id || null;
        logStep('picked ' + (kind.pick || 'no document') + ' for ' + code +
            ' \u2014 bound today is ' + (kind.bound || 'nothing'));
        if (!needsStep(kind) && S.step === 'kind-' + code) {
            S.step = 'set';
            setActivity(code + ' needs no change, so its step left the rail');
        } else if (needsStep(kind)) {
            setActivity(code + ' needs a decision \u00b7 step ' + stepNumberOf(code) + ' of ' +
                rail().length);
        }
        render();
    }

    function changeKind(code) {
        var kind = kindOf(code);
        if (!kind) return;
        var alternative = firstAlternative(kind);
        if (!alternative) {
            S.focus = code;
            setActivity('no other document exists for ' + code);
            render();
            return;
        }
        kind.pick = alternative;
        logStep('opened ' + code + ' for change \u2014 pick starts at ' + alternative);
        if (!needsStep(kind)) {
            S.step = 'set';
            setActivity(code + ' needs no change');
            render();
            return;
        }
        S.step = 'kind-' + code;
        S.guide = 0;
        setActivity(code + ' \u00b7 step ' + stepNumberOf(code) + ' of ' + rail().length +
            ' \u00b7 changed from ' + (kind.bound || 'nothing') + ' to ' + alternative);
        render();
    }

    function keepBound(code) {
        var kind = kindOf(code);
        if (!kind) return;
        kind.pick = kind.bound;
        logStep('kept the bound document ' + (kind.bound || 'nothing') + ' for ' + code +
            ' \u2014 its step leaves the rail');
        S.step = 'set';
        setActivity(code + ' needs no change, so its step left the rail');
        render();
    }

    // ------------------------------------------------------ deep links

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var from = params.get('from');
        if (from === 'create-a-report' || from === 'change-what-a-report-runs') {
            S.from = from;
        }
        var variant = params.get('variant');
        if (VARIANTS.some(function (v) { return v.id === variant; })) S.variant = variant;
        var guide = parseInt(params.get('guide'), 10);
        if (guide >= 0) S.guide = guide;
        /* A fixture deep link, so a reload can show a changed set and the final
           read-back without walking it. It picks the first alternative for the
           kind, the same change the Change control makes. */
        var changed = params.get('change');
        if (changed && kindOf(changed)) {
            var changedKind = kindOf(changed);
            var alternative = firstAlternative(changedKind);
            if (alternative) changedKind.pick = alternative;
        }
        var code = params.get('kind');
        var wanted = parseInt(params.get('step'), 10);
        if (code && kindOf(code)) {
            var kind = kindOf(code);
            if (needsStep(kind)) {
                S.step = 'kind-' + code;
            } else {
                S.step = 'set';
                S.focus = code;
                setActivity(code + ' already has a document and needs no change, so it has no ' +
                    'step: the set is where a change starts');
            }
        } else if (wanted >= 1 && wanted <= rail().length) {
            S.step = rail()[wanted - 1].id;
        }
    }

    function writeParams() {
        var params = new URLSearchParams();
        var list = rail();
        var position = pos();
        params.set('step', String(position + 1));
        if (list[position].kind) {
            params.set('kind', list[position].kind);
            if (S.variant === 'C') params.set('guide', String(S.guide));
        }
        params.set('variant', S.variant);
        params.set('from', S.from);
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        if (element.tagName !== 'BUTTON' && element.tagName !== 'A') {
            if (act === 'noop') event.preventDefault();
            return;
        }
        if (act === 'pick') return;
        if (element.tagName === 'A') {
            if (act === 'noop') event.preventDefault();
            return;
        }
        event.preventDefault();
        var code = element.getAttribute('data-code');
        var list = rail();

        if (act === 'step' || act === 'go-step') {
            var wanted = element.getAttribute('data-step');
            for (var i = 0; i < list.length; i += 1) {
                if (list[i].id === wanted) { goTo(i); return; }
            }
        } else if (act === 'go-kind') {
            for (var j = 0; j < list.length; j += 1) {
                if (list[j].id === 'kind-' + code) { goTo(j); return; }
            }
        } else if (act === 'back') {
            goTo(pos() - 1);
        } else if (act === 'next') {
            goTo(pos() + 1);
        } else if (act === 'variant') {
            S.variant = element.getAttribute('data-variant');
            S.guide = 0;
            setActivity('variant ' + S.variant + ': ' + variantOf(S.variant).name.toLowerCase() +
                ' \u2014 the set and the rail are the same in all three');
            render();
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
        } else if (act === 'change') {
            changeKind(code);
        } else if (act === 'keep') {
            keepBound(code);
        } else if (act === 'guide-next') {
            var kind = kindOf(list[pos()].kind || '');
            var total = kind ? kindCFieldCount(kind) : 0;
            S.guide = S.guide + 1 >= total ? 0 : S.guide + 1;
            render();
        } else if (act === 'guide-prev') {
            S.guide = Math.max(0, S.guide - 1);
            render();
        }
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-act="pick"]');
        if (!element) return;
        pickDocument(element.getAttribute('data-code'), element.value);
    });

    function kindCFieldCount(kind) {
        return unionFieldNames(kind).length;
    }

    S.kinds = seedKinds();
    readParams();
    render();
}());

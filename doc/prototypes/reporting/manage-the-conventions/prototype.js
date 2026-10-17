/* Manage the conventions. Throwaway prototype: plain JavaScript, no framework,
 * no modules, no fetch, no build step. It opens from file:// and reads nothing.
 *
 * THE DESIGN, IN ONE PARAGRAPH
 *
 * The screen is a table. One row per convention kind, and three columns that
 * matter: the kind, the name of the convention in use, and what its terms say.
 * A fourth column is one button, Change, which opens that kind alone. Nothing
 * in the database stores a name or a description for a convention document, so
 * both are derived from the terms: a name from the terms that tell one document
 * from another, a description by rendering the same terms in words. A kind with
 * no document of the party's own says so, and says what a run does instead.
 *
 * THE FACTS THIS PAGE IS BUILT ON (checked against ores_dev_bright_faraday)
 *
 *   - ACME, tenant b7fe3d66-2aed-4d1c-a19b-0406b689640b, owns no row in any of
 *     the 25 ores_refdata_<kind>_conventions_tbl tables. Neither does any other
 *     party: 22 of the 25 hold no row at all, and the other three hold rows of
 *     the system tenant only. So the state the screen handles first is "no
 *     document", on every row.
 *   - The only convention data is the curated set in the 25
 *     ores_dq_<kind>_conventions_artefact_tbl tables, owned by tenant
 *     ffffffff-ffff-ffff-ffff-ffffffffffff and party 00000000-....
 *   - No convention table has a name column and none has a description column.
 *     The curated tables carry an id text, which is the row's key; the party
 *     tables carry a uuid id. Neither is a field of the document.
 *   - No table has a default marker. pays_at_default_time on the CDS table is a
 *     CDS business term, not a default flag.
 *   - The terms differ by kind. Deposit carries index_based, index, calendar,
 *     convention, end_of_month, day_count_fraction and settlement_days; swap
 *     carries fixed_calendar, fixed_frequency, fixed_convention,
 *     fixed_day_count_fraction and index; FRA carries an index and nothing
 *     else; FX option carries no calendar and no day count; commodity future
 *     carries 51 term columns. A single name formula cannot span them, so the
 *     name rule is stated per kind and the page shows which fields it read.
 *
 * Every value on the page is a fixture. The term values are real rows of the
 * curated tables, copied in; which document ACME owns, the default mark and the
 * change in progress are invented for the demonstration and are labelled as
 * fixtures where they appear.
 *
 * THREE READINGS, ONE QUESTION. ?variant=A|B|C changes only the alternatives
 * block on a kind's page: A lists them described, B compares the document in
 * use against a candidate, C searches and filters for the long kinds. The rest
 * of the page is identical across the three, so a reader compares one block.
 */

(function () {
    'use strict';

    /* ------------------------------------------------------------ fixture */

    var ACME = {
        tenant: 'b7fe3d66-2aed-4d1c-a19b-0406b689640b',
        party: '00000000-0000-0000-0000-00000000acme',
        name: 'ACME',
        configuration: 'Group conventions',
        configurationId: 'cfg-conv-7d21',
        definitions: 31
    };

    /* The kinds, in the order ORE's conventions document carries them. Each one
       states how its name is derived, which is a fact about its own table:
       nameFields is read left to right, and a field the row leaves empty is
       skipped in the name and then reported beside it. */
    var KINDS = [
        {
            code: 'deposit', label: 'Deposit',
            mode: 'flag-index', indexFlag: 'index_based', indexField: 'index',
            nameFields: ['calendar', 'convention', 'day_count_fraction', 'settlement_days'],
            altCount: 163, alts: 141, termCols: 8
        },
        {
            code: 'fra', label: 'FRA',
            mode: 'index', indexField: 'index',
            nameFields: ['index'],
            altCount: 42, termCols: 2
        },
        {
            code: 'future', label: 'Future',
            mode: 'terms',
            nameFields: ['index', 'calendar', 'date_generation_rule'],
            extraFields: ['netting_type'],
            altCount: 7, termCols: 6
        },
        {
            code: 'ois', label: 'OIS',
            mode: 'terms',
            nameFields: ['fixed_calendar', 'fixed_convention', 'fixed_day_count_fraction', 'fixed_frequency'],
            extraFields: ['index', 'spot_lag', 'payment_lag', 'rule'],
            altCount: 52, termCols: 13
        },
        {
            code: 'average_ois', label: 'Average OIS',
            mode: 'terms',
            nameFields: ['fixed_calendar', 'fixed_convention', 'fixed_day_count_fraction', 'fixed_tenor'],
            extraFields: ['index', 'on_tenor', 'spot_lag'],
            altCount: 5, termCols: 11
        },
        {
            code: 'swap', label: 'Swap',
            mode: 'terms',
            nameFields: ['fixed_calendar', 'fixed_convention', 'fixed_day_count_fraction', 'fixed_frequency'],
            extraFields: ['index'],
            altCount: 320, alts: 201, termCols: 8
        },
        {
            code: 'tenor_basis_swap', label: 'Tenor basis swap',
            mode: 'terms',
            nameFields: ['short_index', 'long_index'],
            extraFields: ['pay_index', 'receive_index'],
            altCount: 37, termCols: 15
        },
        {
            code: 'tenor_basis_two_swap', label: 'Tenor basis two swap',
            mode: 'terms',
            nameFields: ['calendar', 'short_index', 'long_index', 'long_fixed_day_count_fraction'],
            extraFields: ['long_fixed_convention', 'short_fixed_convention'],
            altCount: 4, termCols: 11
        },
        {
            code: 'cross_currency_basis', label: 'Cross-currency basis',
            mode: 'terms',
            nameFields: ['settlement_calendar', 'roll_convention', 'settlement_days', 'flat_index', 'spread_index'],
            extraFields: ['eom', 'is_resettable'],
            altCount: 207, termCols: 25
        },
        {
            code: 'cross_currency_fix_float', label: 'Cross-currency fix-float',
            mode: 'terms',
            nameFields: ['settlement_calendar', 'fixed_convention', 'fixed_day_count_fraction', 'settlement_days'],
            extraFields: ['index', 'fixed_currency', 'fixed_frequency'],
            altCount: 24, termCols: 18
        },
        {
            code: 'bond_yield', label: 'Bond yield',
            mode: 'terms',
            nameFields: ['frequency', 'compounding', 'price_type'],
            extraFields: ['guess', 'accuracy', 'max_evaluations'],
            altCount: 5, alts: 1, termCols: 7
        },
        {
            code: 'cds', label: 'CDS',
            mode: 'terms',
            nameFields: ['calendar', 'payment_convention', 'day_count_fraction', 'settlement_days', 'frequency'],
            extraFields: ['rule', 'settles_accrual', 'pays_at_default_time'],
            altCount: 4, termCols: 11
        },
        {
            code: 'cms_spread_option', label: 'CMS spread option',
            mode: 'terms',
            nameFields: ['calendar', 'roll_convention', 'day_count_fraction', 'swap_tenor'],
            extraFields: ['spot_days', 'forward_start', 'fixing_days'],
            altCount: 1, termCols: 8
        },
        {
            code: 'commodity_forward', label: 'Commodity forward',
            mode: 'terms',
            nameFields: ['advance_calendar', 'business_day_convention', 'spot_days'],
            extraFields: ['outright', 'spot_relative', 'points_factor'],
            altCount: 4, alts: 1, termCols: 8
        },
        {
            code: 'commodity_future', label: 'Commodity future',
            mode: 'terms',
            nameFields: ['calendar', 'contract_frequency', 'business_day_convention'],
            extraFields: ['anchor_day_of_month', 'offset_days'],
            altCount: 790, termCols: 51
        },
        {
            code: 'fx_option', label: 'FX option',
            mode: 'terms',
            nameFields: ['fx_convention_id', 'atm_type', 'delta_type', 'switch_tenor'],
            extraFields: ['long_term_atm_type', 'long_term_delta_type', 'risk_reversal_in_favor_of', 'butterfly_style'],
            altCount: 173, termCols: 9
        },
        {
            code: 'bma_basis_swap', label: 'BMA basis swap',
            mode: 'terms',
            nameFields: ['index', 'bma_index'],
            extraFields: ['index_payment_period', 'index_settlement_days', 'overnight_lockout_days'],
            altCount: 3, termCols: 12
        },
        {
            code: 'zero', label: 'Zero',
            mode: 'terms',
            nameFields: ['spot_calendar', 'roll_convention', 'day_count_fraction'],
            extraFields: ['compounding', 'compounding_frequency', 'spot_lag', 'tenor_based'],
            altCount: 38, termCols: 10
        },
        {
            code: 'inflation_swap', label: 'Inflation swap',
            mode: 'terms',
            nameFields: ['inflation_calendar', 'inflation_convention', 'day_count_fraction', 'observation_lag'],
            extraFields: ['index', 'interpolated', 'publication_roll'],
            altCount: 22, termCols: 17
        },
        {
            code: 'intraday_power_load', label: 'Intraday power load',
            mode: 'presence',
            nameFields: ['explicit_load_profile', 'business_day_load_rules'],
            altCount: 2, termCols: 3
        },
        {
            code: 'ibor_index', label: 'IBOR index',
            mode: 'terms',
            nameFields: ['fixing_calendar', 'day_count_fraction', 'settlement_days', 'business_day_convention'],
            extraFields: ['end_of_month'],
            altCount: 10, termCols: 6
        },
        {
            code: 'overnight_index', label: 'Overnight index',
            mode: 'terms',
            nameFields: ['fixing_calendar', 'day_count_fraction', 'settlement_days'],
            altCount: 23, termCols: 4
        },
        {
            code: 'swap_index', label: 'Named swap index',
            mode: 'pointer',
            nameFields: ['conventions'],
            extraFields: ['fixing_calendar'],
            altCount: 243, termCols: 3
        },
        {
            code: 'currency_pair', label: 'Currency pair',
            mode: 'terms', keyField: 'pair_code',
            nameFields: ['pair_code', 'business_day_convention'],
            extraFields: ['advance_calendar', 'spot_relative', 'tick_size', 'pip_factor'],
            altCount: 179, termCols: 8
        },
        {
            code: 'zero_inflation_index', label: 'Zero inflation index',
            mode: 'terms',
            nameFields: ['region_name', 'currency', 'frequency'],
            extraFields: ['availability_lag', 'revised', 'region_code'],
            altCount: 9, termCols: 7
        }
    ];

    /* Real rows, copied from the curated tables of ores_dev_bright_faraday. Two
       or three per kind is a fixture: the tables hold the counts named above.
       Two swap rows and three bond-yield rows state the same terms on purpose;
       they are real duplicates in the curated data. */
    var ROWS = {
        deposit: [
            { key: 'AED-DEPOSIT', index_based: false, calendar: 'AED', convention: 'ModifiedFollowing', day_count_fraction: 'ACT/360', settlement_days: 2, end_of_month: false },
            { key: 'AED-ON-DEPOSIT', index_based: true, index: 'AED-EIBORON', oresmd_uri: 'oresmd://ir/AED?type=fixing&index=ibor&name=EIBORON' },
            { key: 'AMD-DEPOSIT', index_based: true, index: 'AMD-GENERIC', oresmd_uri: 'oresmd://ir/AMD?type=fixing&index=ibor&name=GENERIC' }
        ],
        fra: [
            { key: 'AED-3M-FRA', index: 'AED-EIBOR-3M' },
            { key: 'AUD-6M-FRA', index: 'AUD-BBSW-6M' }
        ],
        future: [
            { key: 'EURIBOR-3M-FUTURES-CONVENTIONS', index: 'EUR-EURIBOR-3M', netting_type: 'Compounding', date_generation_rule: 'IMM' },
            { key: 'JPY-LIBOR-3M-FUTURES-CONVENTIONS', index: 'JPY-LIBOR-3M' }
        ],
        ois: [
            { key: 'AED-OIS', index: 'AED-EIBORON', rule: 'Backward', spot_lag: 1, payment_lag: 2, fixed_frequency: 'Annual', fixed_convention: 'Following', fixed_day_count_fraction: 'ACT/365.FIXED', fixed_payment_convention: 'Following', end_of_month: false },
            { key: 'AED-EIBORON-OIS', index: 'AED-EIBORON', rule: 'Backward', spot_lag: 1, payment_lag: 2, fixed_frequency: 'Annual', fixed_convention: 'Following', fixed_day_count_fraction: 'ACT/365.FIXED', fixed_payment_convention: 'Following', end_of_month: false },
            { key: 'AUD-OIS', index: 'AUD-AONIA', rule: 'Backward', spot_lag: 1, payment_lag: 2, fixed_frequency: 'Annual', fixed_convention: 'Following', fixed_day_count_fraction: 'ACT/365.FIXED', fixed_payment_convention: 'Following', end_of_month: false }
        ],
        average_ois: [
            { key: 'GBP-BoEBASE-AVERAGE-OIS', index: 'GBP-BoEBASE', on_tenor: '3M', spot_lag: 0, fixed_tenor: '6M', fixed_calendar: 'UK', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/365.FIXED' },
            { key: 'HKD-AVERAGE-OIS', index: 'HKD-HONIA', on_tenor: '3M', spot_lag: 2, fixed_tenor: '3M', fixed_calendar: 'HK', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/365.FIXED' }
        ],
        swap: [
            { key: 'AED-SWAP', index: 'AED-EIBOR-3M', fixed_calendar: 'TARGET', fixed_frequency: 'Annual', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: '30/360' },
            { key: 'AED-EIBOR-3M-SWAP', index: 'AED-EIBOR-3M', fixed_calendar: 'TARGET', fixed_frequency: 'Annual', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: '30/360' },
            { key: 'AMD-GENERIC-3M-SWAP', index: 'AMD-GENERIC-3M', fixed_calendar: 'AMD', fixed_frequency: 'Semiannual', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/365.FIXED' },
            { key: 'AUD-3M-SWAP-CONVENTIONS', index: 'AUD-BBSW-3M', fixed_calendar: 'AU', fixed_frequency: 'Quarterly', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/365.FIXED' }
        ],
        tenor_basis_swap: [
            { key: 'AUD-BBSW-1M-3M-BASIS', short_index: 'AUD-BBSW-1M', long_index: 'AUD-BBSW-3M' },
            { key: 'AUD-BBSW-3M-6M-BASIS', short_index: 'AUD-BBSW-3M', long_index: 'AUD-BBSW-6M', pay_index: 'AUD-BBSW-6M' }
        ],
        tenor_basis_two_swap: [
            { key: 'EUR-EURIBOR-6M-12M-BASIS-CONVENTIONS', calendar: 'TARGET', short_index: 'EUR-EURIBOR-6M', long_index: 'EUR-EURIBOR-12M', long_minus_short: true, long_fixed_frequency: 'Annual', long_fixed_convention: 'ModifiedFollowing', short_fixed_convention: 'ModifiedFollowing', long_fixed_day_count_fraction: '30/360', short_fixed_day_count_fraction: '30/360' },
            { key: 'PLN-WIBOR-1M-3M-BASIS', calendar: 'TARGET', short_index: 'PLN-WIBOR-1M', long_index: 'PLN-WIBOR-3M', long_minus_short: true, long_fixed_frequency: 'Annual', long_fixed_convention: 'ModifiedFollowing', short_fixed_convention: 'ModifiedFollowing', long_fixed_day_count_fraction: 'ACT/ACT.ISDA', short_fixed_day_count_fraction: 'ACT/ACT.ISDA' }
        ],
        cross_currency_basis: [
            { key: 'AUD-USD-IBOR-XCCY-BASIS', flat_index: 'USD-LIBOR-3M', spread_index: 'AUD-BBSW-3M', is_resettable: true, roll_convention: 'ModifiedFollowing', settlement_days: 2, settlement_calendar: 'AU,US,UK' },
            { key: 'AUD-USD-ON-XCCY-BASIS', flat_index: 'USD-SOFR', flat_tenor: '3M', spread_index: 'AUD-BBSW-3M', spread_tenor: '3M', is_resettable: true, roll_convention: 'ModifiedFollowing', settlement_days: 2, settlement_calendar: 'AU,US' }
        ],
        cross_currency_fix_float: [
            { key: 'EUR-RON-XCCY-FIX-FLOAT-CONVENTIONS', index: 'EUR-EURIBOR-3M', fixed_currency: 'RON', fixed_frequency: 'Annual', settlement_days: 2, fixed_convention: 'ModifiedFollowing', settlement_calendar: 'TARGET,RON', fixed_day_count_fraction: 'ACT/360' },
            { key: 'USD-CLF-XCCY-FIX-FLOAT-CONVENTIONS', index: 'USD-LIBOR-6M', fixed_currency: 'CLF', fixed_frequency: 'Semiannual', settlement_days: 2, fixed_convention: 'ModifiedFollowing', settlement_calendar: 'CLF,US,UK', fixed_day_count_fraction: 'ACT/360' }
        ],
        bond_yield: [
            { key: 'CMB-USD-MMD', guess: 0.05, accuracy: 0.00000001, frequency: 'Weekly', price_type: 'Clean', compounding: 'Continuous', max_evaluations: 100 },
            { key: 'CMB-US-TBILL-HD-13W', guess: 0.05, accuracy: 0.00000001, frequency: 'Weekly', price_type: 'Clean', compounding: 'Continuous', max_evaluations: 100 },
            { key: 'CMB-US-TIPS-10Y', guess: 0.05, accuracy: 0.00000001, frequency: 'Weekly', price_type: 'Clean', compounding: 'Continuous', max_evaluations: 100 }
        ],
        cds: [
            { key: 'CDS-STANDARD-CONVENTIONS', rule: 'CDS2015', calendar: 'WeekendsOnly', frequency: 'Quarterly', settlement_days: 0, settles_accrual: true, day_count_fraction: 'ACT/360', payment_convention: 'Following', pays_at_default_time: true },
            { key: 'CDS-STANDARD-CONVENTIONS-EUR', rule: 'CDS2015', calendar: 'TARGET', frequency: 'Quarterly', settlement_days: 0, settles_accrual: true, day_count_fraction: 'ACT/360', payment_convention: 'Following', pays_at_default_time: true },
            { key: 'CDS-STANDARD-CONVENTIONS-USD', rule: 'CDS2015', calendar: 'USD', frequency: 'Quarterly', settlement_days: 0, settles_accrual: true, day_count_fraction: 'ACT/360', payment_convention: 'Following', pays_at_default_time: true }
        ],
        cms_spread_option: [
            { key: 'EUR-CMS-10Y-2Y-CONVENTION', calendar: 'TARGET', spot_days: '2D', swap_tenor: '3M', fixing_days: 2, forward_start: '0M', roll_convention: 'ModifiedFollowing', day_count_fraction: 'ACT/360' }
        ],
        commodity_forward: [
            { key: 'PM:XAGUSD', outright: false, spot_days: 2, points_factor: 100, spot_relative: true, advance_calendar: 'USD', business_day_convention: 'ModifiedFollowing' },
            { key: 'PM:XAUUSD', outright: false, spot_days: 2, points_factor: 100, spot_relative: true, advance_calendar: 'USD', business_day_convention: 'ModifiedFollowing' }
        ],
        commodity_future: [
            { key: 'BVMF:ATR', calendar: 'BRL', is_averaging: false, contract_frequency: 'Monthly', anchor_day_of_month: 1, business_day_convention: 'Following' },
            { key: 'CAISO_NP_15_DA_OPK', calendar: 'NullCalendar', peak_index: 'ICE:DPN', peak_calendar: 'US-NERC', off_peak_hours: 8, off_peak_index: 'ICE:UNP', contract_frequency: 'Daily' }
        ],
        fx_option: [
            { key: 'AUD-CAD-FXOPTION', fx_convention_id: 'AUD-CAD-FX', atm_type: 'AtmDeltaNeutral', delta_type: 'PaSpot', switch_tenor: '2Y', long_term_atm_type: 'AtmDeltaNeutral', long_term_delta_type: 'PaFwd', risk_reversal_in_favor_of: 'Call', butterfly_style: 'Broker' },
            { key: 'AUD-CHF-FXOPTION', fx_convention_id: 'AUD-CHF-FX', atm_type: 'AtmDeltaNeutral', delta_type: 'PaSpot', switch_tenor: '2Y', long_term_atm_type: 'AtmDeltaNeutral', long_term_delta_type: 'PaFwd', risk_reversal_in_favor_of: 'Call', butterfly_style: 'Broker' },
            { key: 'AUD-CNH-FXOPTION', fx_convention_id: 'AUD-CNH-FX', atm_type: 'AtmDeltaNeutral', delta_type: 'PaSpot', switch_tenor: '0D', long_term_atm_type: 'AtmDeltaNeutral', long_term_delta_type: 'PaSpot', risk_reversal_in_favor_of: 'Call', butterfly_style: 'Broker' }
        ],
        bma_basis_swap: [
            { key: 'USD-LIBOR-SIFMA-3M-BASIS-CONVENTIONS', index: 'USD-LIBOR-3M', bma_index: 'USD-SIFMA' },
            { key: 'USD-SOFR-SIFMA-BASIS-CONVENTIONS', index: 'USD-SOFR', bma_index: 'USD-SIFMA', bma_payment_lag: 0, index_payment_lag: 2, index_payment_period: '3M', index_settlement_days: 0, overnight_lockout_days: 0 }
        ],
        zero: [
            { key: 'AED-ZERO-TENOR-BASED', spot_lag: 2, compounding: 'Continuous', tenor_based: true, end_of_month: false, spot_calendar: 'AED', tenor_calendar: 'AED', roll_convention: 'Following', day_count_fraction: 'ACT/365.FIXED', compounding_frequency: 'Daily' },
            { key: 'CCY-ZERO-CONVENTIONS-TENOR-BASED', spot_lag: 2, compounding: 'Continuous', tenor_based: true, end_of_month: false, spot_calendar: 'TARGET', tenor_calendar: 'TARGET', roll_convention: 'Following', day_count_fraction: 'ACT/365.FIXED', compounding_frequency: 'Daily' }
        ],
        inflation_swap: [
            { key: 'AUCPI_INFLATIONSWAP', index: 'AUCPI', fix_calendar: 'AUD', interpolated: false, fix_convention: 'Following', observation_lag: '3M', publication_roll: 'AfterPublicationDate', day_count_fraction: '30/360', inflation_calendar: 'AUD', inflation_convention: 'Following' },
            { key: 'BECPIHL_INFLATIONSWAP', index: 'BECPIHL', fix_calendar: 'TARGET', interpolated: false, fix_convention: 'ModifiedFollowing', observation_lag: '3M', day_count_fraction: '30/360', inflation_calendar: 'TARGET', inflation_convention: 'ModifiedFollowing' }
        ],
        intraday_power_load: [
            { key: 'LOAD_PROFILE_1_EXPLICT', explicit_load_profile: '2021-01-01|0;2021-01-04|50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50' },
            { key: 'LOAD_PROFILE_1_RULEBASED', business_day_load_rules: '2020-01-01|US-NERC||' }
        ],
        ibor_index: [
            { key: 'AED-EIBOR-3M', fixing_calendar: 'AED', settlement_days: 2, day_count_fraction: 'ACT/360', business_day_convention: 'ModifiedFollowing', end_of_month: false },
            { key: 'NGN-NIBOR-1M', fixing_calendar: 'NGN', settlement_days: 2, day_count_fraction: '30/360', business_day_convention: 'ModifiedFollowing', end_of_month: false },
            { key: 'CNY-DEPO-1Y', fixing_calendar: 'CNY', settlement_days: 2, day_count_fraction: 'ACT/360', business_day_convention: 'ModifiedFollowing', end_of_month: false }
        ],
        overnight_index: [
            { key: 'AED-EIBORON', fixing_calendar: 'AED', settlement_days: 0, day_count_fraction: 'ACT/365.FIXED' },
            { key: 'BRL-CDI', fixing_calendar: 'BRL', settlement_days: 0, day_count_fraction: 'BUS/252' }
        ],
        swap_index: [
            { key: 'AED-CMS-1Y', conventions: 'AED-SWAP' },
            { key: 'AED-CMS-30Y', conventions: 'AED-SWAP' }
        ],
        currency_pair: [
            { pair_code: 'EUR/USD', tick_size: 1, pip_factor: 0.0001, end_of_month: false, spot_relative: true, advance_calendar: 'TARGET,UnitedStates.Settlement', business_day_convention: 'ModifiedFollowing' },
            { pair_code: 'GBP/USD', tick_size: 1, pip_factor: 0.0001, end_of_month: false, spot_relative: true, advance_calendar: 'UnitedKingdom.Settlement,UnitedStates.Settlement', business_day_convention: 'ModifiedFollowing' }
        ],
        zero_inflation_index: [
            { key: 'BECPIHL', revised: false, currency: 'EUR', frequency: 'Monthly', region_code: 'BE', region_name: 'Belgium', availability_lag: '1M' },
            { key: 'CLCPI', revised: false, currency: 'CLP', frequency: 'Monthly', region_code: 'CL', region_name: 'Chile', availability_lag: '1M' }
        ]
    };

    /* The three kinds whose party-owned table carries no party_id column. A
       create for one of these writes terms for the whole tenant. */
    var TENANT_WIDE_KINDS = ['currency_pair', 'ibor_index', 'overnight_index'];

    /* A fixture default per kind. No table carries this mark, so the page
       labels it a fixture wherever it appears and says where a real one would
       have to live. */
    var DEFAULT_FIXTURE = {
        deposit: 'AED-DEPOSIT',
        swap: 'AED-SWAP',
        cds: 'CDS-STANDARD-CONVENTIONS-EUR'
    };

    /* The documents ACME owns, as a fixture: kind -> the party rows a person
       would see. This state does not exist in the database. */
    var OWNED_FIXTURE = {
        deposit: [
            {
                from: 'AED-DEPOSIT', version: 2, validFrom: '2026-10-17 09:14',
                modifiedBy: 'acme.ops', reason: 'ADOPT-CURATED',
                terms: { index_based: false, calendar: 'AED', convention: 'ModifiedFollowing', day_count_fraction: 'ACT/360', settlement_days: 2, end_of_month: false }
            }
        ],
        swap: [
            {
                from: null, authored: true, version: 3, validFrom: '2026-10-17 09:20',
                modifiedBy: 'acme.ops', reason: 'HOUSE-STANDARD',
                terms: { index: 'AED-EIBOR-3M', fixed_calendar: 'TARGET', fixed_frequency: 'Annual', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/365.FIXED' }
            },
            {
                from: 'AMD-GENERIC-3M-SWAP', version: 1, validFrom: '2026-10-17 09:22',
                modifiedBy: 'acme.ops', reason: 'ADOPT-CURATED',
                terms: { index: 'AMD-GENERIC-3M', fixed_calendar: 'AMD', fixed_frequency: 'Semiannual', fixed_convention: 'ModifiedFollowing', fixed_day_count_fraction: 'ACT/365.FIXED' }
            }
        ],
        cds: [
            {
                from: 'CDS-STANDARD-CONVENTIONS-EUR', version: 1, validFrom: '2026-10-17 09:25',
                modifiedBy: 'acme.ops', reason: 'ADOPT-CURATED',
                terms: { rule: 'CDS2015', calendar: 'TARGET', frequency: 'Quarterly', settlement_days: 0, settles_accrual: true, day_count_fraction: 'ACT/360', payment_convention: 'Following', pays_at_default_time: true }
            }
        ]
    };

    /* One change in progress, as a fixture: the alternative picked for a kind
       and not written. */
    var PENDING_FIXTURE = { kind: 'swap', from: 'AUD-3M-SWAP-CONVENTIONS' };

    /* --------------------------------------------------------- term words */

    /* What each term is called when the page says it in words. A field absent
       from this table prints under its own name with the value as stated. */
    var TERM_WORDS = {
        calendar: ['the %s calendar'],
        fixed_calendar: ['the %s calendar'],
        spot_calendar: ['the %s calendar for the spot date'],
        tenor_calendar: ['the %s calendar for the tenor'],
        inflation_calendar: ['the %s calendar on the inflation leg'],
        settlement_calendar: ['%s for settlement'],
        fixing_calendar: ['%s to fix on'],
        advance_calendar: ['%s to advance on'],
        convention: ['%s as the business-day rule'],
        fixed_convention: ['%s'],
        payment_convention: ['%s to pay on'],
        settlement_convention: ['%s to settle on'],
        roll_convention: ['%s to roll on'],
        inflation_convention: ['%s on the inflation leg'],
        business_day_convention: ['%s as the business-day rule'],
        day_count_fraction: ['%s as its day count'],
        fixed_day_count_fraction: ['%s as its day count'],
        long_fixed_day_count_fraction: ['%s as the long leg day count'],
        settlement_days: ['settlement %s business days after the trade'],
        spot_days: ['spot at %s'],
        spot_lag: ['a spot lag of %s day(s)'],
        payment_lag: ['a payment lag of %s day(s)'],
        observation_lag: ['an observation lag of %s'],
        availability_lag: ['published %s after the month end'],
        index: ['fixing on %s'],
        flat_index: ['%s as the flat leg'],
        spread_index: ['%s as the spread leg'],
        short_index: ['%s as the short leg'],
        long_index: ['%s as the long leg'],
        pay_index: ['%s paid'],
        receive_index: ['%s received'],
        bma_index: ['%s as the BMA index'],
        fx_convention_id: ['the %s FX convention'],
        frequency: ['%s coupons'],
        fixed_frequency: ['%s coupons'],
        fixed_tenor: ['a %s fixed tenor'],
        contract_frequency: ['%s contracts'],
        swap_tenor: ['a %s swap tenor'],
        on_tenor: ['a %s overnight tenor'],
        switch_tenor: ['the long-term switch at %s'],
        atm_type: ['%s at the money'],
        delta_type: ['%s delta'],
        long_term_atm_type: ['%s at the money past the switch'],
        long_term_delta_type: ['%s delta past the switch'],
        risk_reversal_in_favor_of: ['a risk reversal in favour of the %s'],
        butterfly_style: ['a %s butterfly'],
        price_type: ['%s prices'],
        compounding: ['%s compounding'],
        compounding_frequency: ['compounding %s'],
        rule: ['the %s rule'],
        date_generation_rule: ['%s date generation'],
        netting_type: ['%s netting'],
        conventions: ['the %s swap terms'],
        pair_code: ['the %s pair'],
        region_name: ['%s'],
        region_code: ['region %s'],
        currency: ['in %s'],
        end_of_month: ['end of month: %s'],
        eom: ['end of month: %s'],
        settles_accrual: ['settles accrual: %s'],
        pays_at_default_time: ['pays at default time: %s'],
        is_resettable: ['resettable: %s'],
        tenor_based: ['tenor based: %s'],
        interpolated: ['interpolated: %s'],
        publication_roll: ['%s publication'],
        offset_days: ['an offset of %s day(s)'],
        anchor_day_of_month: ['anchored on day %s'],
        spot_relative: ['spot relative: %s'],
        outright: ['outright: %s'],
        points_factor: ['a points factor of %s'],
        guess: ['a guess of %s'],
        accuracy: ['accuracy %s'],
        max_evaluations: ['at most %s evaluations'],
        tick_size: ['a tick size of %s'],
        pip_factor: ['a pip factor of %s'],
        index_payment_period: ['a %s index payment period'],
        index_settlement_days: ['%s index settlement day(s)'],
        overnight_lockout_days: ['%s lockout day(s)'],
        fixed_currency: ['the fixed leg in %s'],
        peak_calendar: ['the %s peak calendar'],
        peak_index: ['%s as the peak index'],
        off_peak_index: ['%s as the off-peak index'],
        off_peak_hours: ['%s off-peak hour(s)'],
        is_averaging: ['averaged: %s'],
        tenor_based_flag: ['tenor based: %s']
    };

    var FREQUENCY_WORDS = {
        annual: 'annual', semiannual: 'semiannual', quarterly: 'quarterly',
        monthly: 'monthly', weekly: 'weekly', daily: 'daily'
    };

    var FLAG_WORDS = {
        atmdeltaneutral: 'delta neutral', paspot: 'paid on spot',
        pafwd: 'paid forward', broker: 'broker', call: 'call', put: 'put'
    };

    var BOOLEAN_FIELDS = {
        end_of_month: 1, eom: 1, settles_accrual: 1, pays_at_default_time: 1,
        is_resettable: 1, tenor_based: 1, interpolated: 1, spot_relative: 1,
        outright: 1, is_averaging: 1, revised: 1, long_minus_short: 1
    };

    /* --------------------------------------------------------------- helpers */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;');
    }

    function fmt(n) {
        return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    }

    function kindOf(code) {
        for (var i = 0; i < KINDS.length; i++) {
            if (KINDS[i].code === code) return KINDS[i];
        }
        return null;
    }

    function rowsOf(kind) {
        return ROWS[kind.code] || [];
    }

    function keyOf(kind, row) {
        return row[kind.keyField || 'key'];
    }

    function stated(field, value) {
        return !(value === null || value === undefined || value === '');
    }

    /* A term value said in words. One meaning per word: a frequency becomes a
       word a reader uses, a delta style becomes a phrase, and a code such as a
       day count or a calendar prints as the table states it. */
    function said(field, value) {
        if (!stated(field, value)) return '';
        if (typeof value === 'boolean') return value ? 'yes' : 'no';
        var text = String(value);
        if (field === 'frequency' || field === 'fixed_frequency' ||
            field === 'contract_frequency' || field === 'compounding_frequency') {
            var word = FREQUENCY_WORDS[text.toLowerCase()];
            if (word) return word;
        }
        var flag = FLAG_WORDS[text.toLowerCase()];
        if (flag && (field === 'atm_type' || field === 'delta_type' ||
            field === 'long_term_atm_type' || field === 'long_term_delta_type' ||
            field === 'butterfly_style' || field === 'risk_reversal_in_favor_of')) {
            return flag;
        }
        return text
            .replace(/ModifiedFollowing/g, 'modified following')
            .replace(/Following/g, 'following')
            .replace(/Preceding/g, 'preceding')
            .replace(/Unadjusted/g, 'unadjusted')
            .replace(/WeekendsOnly/g, 'weekends only');
    }

    function phrase(field, value) {
        var words = said(field, value);
        if (!words) return '';
        var entry = TERM_WORDS[field];
        var pattern = entry ? entry[0] : (field.replace(/_/g, ' ') + ': %s');
        return pattern.replace('%s', words);
    }

    /* The name. An index-based document is named by its index; a terms-based
       document by the terms that tell it apart, read in the order its own table
       states them; a kind whose table carries neither is named by whatever
       identifies its row. A part the row leaves empty is dropped from the name
       and reported beside it. */
    function nameOf(kind, row) {
        if (kind.mode === 'flag-index' && row[kind.indexFlag] === true) {
            return row[kind.indexField] || '';
        }
        if (kind.mode === 'index') return row[kind.indexField] || '';
        if (kind.mode === 'presence') {
            var statedFields = kind.nameFields.filter(function (field) { return row[field]; });
            if (!statedFields.length) return '';
            return statedFields.map(function (field) {
                return field === 'explicit_load_profile'
                    ? 'an explicit load profile' : 'business-day load rules';
            }).join(' and ');
        }
        var parts = [];
        kind.nameFields.forEach(function (field) {
            var value = said(field, row[field]);
            if (value) parts.push(value);
        });
        return parts.join(' · ');
    }

    /* A list said the way a person says it: one item, two items, or a run with
       or before the last. */
    function joinWords(items) {
        if (!items.length) return '';
        if (items.length === 1) return items[0];
        return items.slice(0, -1).join(', ') + ' or ' + items[items.length - 1];
    }

    var FIELD_WORDS = {
        day_count_fraction: 'day count',
        fixed_day_count_fraction: 'fixed day count',
        long_fixed_day_count_fraction: 'long leg day count',
        fx_convention_id: 'FX convention',
        settlement_days: 'settlement days',
        pair_code: 'currency pair'
    };

    function fieldWords(field) {
        return FIELD_WORDS[field] || field.replace(/_/g, ' ');
    }

    /* The description: the same terms in words, as sentences a reader can use
       without reading a column name. An index-based row is described by its
       index, and the terms it leaves empty are named, because that is the fact
       a reader needs: the index carries them. */
    function describeOf(kind, row) {
        var parts = [];
        var indexBased = kind.mode === 'index' ||
            (kind.mode === 'flag-index' && row[kind.indexFlag] === true);
        if (indexBased) {
            var indexText = phrase(kind.indexField, row[kind.indexField]);
            if (indexText) parts.push(indexText);
        }
        kind.nameFields.concat(kind.extraFields || []).forEach(function (field) {
            if (indexBased && field === kind.indexField) return;
            var text = phrase(field, row[field]);
            if (text && parts.indexOf(text) === -1) parts.push(text);
        });
        if (!parts.length) return 'The row states no terms.';
        var sentence = parts.join(', ');
        sentence = sentence.charAt(0).toUpperCase() + sentence.slice(1) + '.';
        if (indexBased) {
            var absent = kind.nameFields.filter(function (field) {
                return field !== kind.indexField && !said(field, row[field]);
            }).map(fieldWords);
            if (absent.length) {
                sentence += ' It states no ' + joinWords(absent) + ', so the index carries them.';
            }
        }
        return sentence;
    }

    /* How the name was arrived at, for the rule block on the table page. */
    function ruleExample() {
        var kind = kindOf('deposit');
        var rows = rowsOf(kind);
        return {
            index: { name: nameOf(kind, rows[1]), key: keyOf(kind, rows[1]) },
            terms: { name: nameOf(kind, rows[0]), key: keyOf(kind, rows[0]) }
        };
    }

    /* ------------------------------------------------------- ownership state */

    function ownedFor(kind, state) {
        if (state === 'acme') return [];
        return OWNED_FIXTURE[kind.code] || [];
    }

    function pendingFor(code, state) {
        if (state !== 'change' || PENDING_FIXTURE.kind !== code) return null;
        var kind = kindOf(code);
        var rows = rowsOf(kind);
        for (var i = 0; i < rows.length; i++) {
            if (keyOf(kind, rows[i]) === PENDING_FIXTURE.from) {
                return { kind: kind, row: rows[i] };
            }
        }
        return null;
    }

    function ownedRows(state) {
        var total = 0;
        KINDS.forEach(function (kind) { total += ownedFor(kind, state).length; });
        return total;
    }

    function noneKinds(state) {
        return KINDS.filter(function (kind) { return ownedFor(kind, state).length === 0; });
    }

    function defaultKeyOf(kind) {
        return DEFAULT_FIXTURE[kind.code] || null;
    }

    /* Does an owned document state the same terms as the fixture default? The
       comparison is over the fields the kind names itself by, because those are
       the fields that tell one document from another. */
    function matchesDefault(kind, owned) {
        var key = defaultKeyOf(kind);
        if (!key) return null;
        var rows = rowsOf(kind);
        for (var i = 0; i < rows.length; i++) {
            if (keyOf(kind, rows[i]) !== key) continue;
            var candidate = rows[i];
            var fields = kind.nameFields.concat(kind.extraFields || []);
            for (var j = 0; j < fields.length; j++) {
                var field = fields[j];
                if (String(owned.terms[field] || '') !== String(candidate[field] || '')) return false;
            }
            return true;
        }
        return null;
    }

    function differences(kind, owned, candidate) {
        var fields = kind.nameFields.concat(kind.extraFields || []);
        var out = [];
        fields.forEach(function (field) {
            var before = owned ? owned.terms[field] : undefined;
            var after = candidate[field];
            if (String(before || '') !== String(after || '')) out.push({ field: field });
        });
        return out;
    }

    /* ------------------------------------------------------------- the chrome */

    function chrome(here) {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span>' +
            '<span class="name">ORE Studio</span></span>' +
            '<nav class="appnav">' +
            navLink('Reports', here === 'reports') +
            navLink('Configurations', here === 'configurations') +
            navLink('Reference data', here === 'refdata') +
            navLink('Conventions', here === 'conventions') +
            '</nav>' +
            '<span class="modechip">prototype · fixture</span>' +
            '</div></header>';
    }

    function navLink(label, on) {
        return '<a href="#" class="' + (on ? 'here' : '') + '">' + esc(label) + '</a>';
    }

    function crumbs(items) {
        return '<div class="crumbs">' + items.map(function (item) {
            return item.href
                ? '<a href="' + esc(item.href) + '">' + esc(item.label) + '</a>'
                : '<span>' + esc(item.label) + '</span>';
        }).join('<span>/</span>') + '</div>';
    }

    function href(params) {
        var pairs = [];
        Object.keys(params).forEach(function (key) {
            var value = params[key];
            if (value === null || value === undefined || value === '') return;
            pairs.push(encodeURIComponent(key) + '=' + encodeURIComponent(value));
        });
        return 'index.html' + (pairs.length ? '?' + pairs.join('&') : '');
    }

    function backToReport(from) {
        if (from === 'create-a-report') {
            return { href: '../create-a-report/index.html', label: 'Back to Create a report' };
        }
        if (from === 'change-what-a-report-runs') {
            return { href: '../change-what-a-report-runs/index.html', label: 'Back to Change what a report runs' };
        }
        return { href: null, label: 'Back to the report' };
    }

    function protoNoteInner() {
        return '<div class="pn-inner">' +
            '<b>PROTOTYPE.</b> Mock data. Nothing here reads a server and nothing is written. ' +
            '<span class="pn-note">The term values are real rows of the curated convention ' +
            'tables; ownership, the default mark and the change in progress are fixtures.</span>' +
            '</div>';
    }

    function barLink(label, on, params) {
        return '<a href="' + href(params) + '"><button class="' + (on ? 'on' : '') + '">' +
            esc(label) + '</button></a>';
    }

    /* -------------------------------------------------------- the kind table */

    function nameCell(kind, state) {
        var owned = ownedFor(kind, state);
        if (!owned.length) {
            return '<div class="namecell">' +
                '<span class="nval none">none</span>' +
                '<span class="nparts">a run falls back to the curated document</span>' +
                '</div>';
        }
        var first = owned[0];
        var tags = ['<span class="tag fx">owned · fixture</span>'];
        if (owned.length > 1) tags.push('<span class="tag">' + owned.length + ' documents</span>');
        var match = matchesDefault(kind, first);
        if (match === true) tags.push('<span class="tag ok">matches the default</span>');
        if (match === false) tags.push('<span class="tag warn">not the default</span>');
        return '<div class="namecell">' +
            '<span class="nval">' + esc(nameOf(kind, first.terms)) + '</span>' +
            '<span class="ntags">' + tags.join('') + '</span>' +
            '<span class="nparts">named by ' +
            esc(kind.nameFields.join(', ').replace(/_/g, ' ')) + '</span>' +
            '</div>';
    }

    function descCell(kind, state) {
        var owned = ownedFor(kind, state);
        if (!owned.length) {
            return '<div class="desc"><span class="dnone">No terms are held.</span> ' +
                'A run that reaches the ' + esc(kind.label) + ' family reads the curated ' +
                'document, and no column says which one.</div>';
        }
        var extra = owned.length > 1
            ? ' <span class="dnone">This kind holds ' + owned.length + ' documents.</span>' : '';
        return '<div class="desc">' + esc(describeOf(kind, owned[0].terms)) + extra + '</div>';
    }

    function tableView(S) {
        var state = S.state || 'acme';
        var isAcme = state === 'acme';
        var won = ownedRows(state);
        var none = noneKinds(state).length;
        var altTotal = KINDS.reduce(function (sum, kind) { return sum + kind.altCount; }, 0);
        var back = backToReport(S.from);

        var rows = KINDS.map(function (kind) {
            var owned = ownedFor(kind, state);
            var cls = owned.length ? '' : ' class="none"';
            if (state === 'change' && PENDING_FIXTURE.kind === kind.code) cls = ' class="changed"';
            return '<tr' + cls + '>' +
                '<td><div class="kindcell"><span class="kname">' + esc(kind.label) + '</span>' +
                '<span class="kcode">' + esc(kind.code) + '</span>' +
                '<span class="ksum">' + fmt(kind.altCount) + ' curated row' +
                (kind.altCount === 1 ? '' : 's') + '</span></div></td>' +
                '<td class="kindname">' + nameCell(kind, state) + '</td>' +
                '<td>' + descCell(kind, state) + '</td>' +
                '<td class="act"><a class="btn small" href="' +
                href({ kind: kind.code, variant: S.variant, state: state, from: S.from }) +
                '">Change</a></td>' +
                '</tr>';
        }).join('');

        var banner = isAcme
            ? '<div class="notice warn loud"><b>ACME owns no convention document today.</b> ' +
              'Every row below reads <span class="mono">none</span>, and a run falls back to the ' +
              'curated document for the kind. No party owns one either: ' +
              '22 of the 25 party tables hold no row at all, and the other three hold rows of ' +
              'the system tenant. <b>This is the state the screen handles first.</b></div>'
            : '<div class="notice loud"><b>Fixture state: ACME owns ' + won +
              (won === 1 ? ' document.' : ' documents.') +
              '</b> The database holds none of them. This state exists here so the ' +
              'table can be read with a document in use, with one kind holding two, and with ' +
              'kinds holding none.</div>';

        var pending = state === 'change' ? pendingFor(PENDING_FIXTURE.kind, state) : null;
        var pendingBanner = pending
            ? '<div class="notice loud"><b>1 change in progress.</b> ' +
              esc(pending.kind.label) + ' would take <span class="mono">' +
              esc(nameOf(pending.kind, pending.row)) + '</span> in place of what it holds. ' +
              'It is not saved: nothing on this screen writes.</div>'
            : '';

        return chrome('conventions') +
            '<div class="shell">' +
            crumbs([
                { label: 'Reporting' },
                { label: 'Configurations' },
                { label: ACME.configuration },
                { label: 'Conventions' }
            ]) +
            '<div class="pagehead"><div>' +
            '<h1>The conventions this report reads</h1>' +
            '<p class="lede">One row per convention kind. The name is derived from the terms, ' +
            'because no table stores one; the description says the same terms in words. ' +
            'The configuration is <span class="mono">' + esc(ACME.configuration) + '</span>, ' +
            'which <b>' + ACME.definitions + ' definitions</b> bind, so a change here reaches ' +
            'every one of their next runs.</p>' +
            '</div><div class="actions">' +
            '<a class="btn primary" href="' +
            href({ screen: 'create', kind: 'deposit', state: state, from: S.from }) +
            '">Create a new convention</a>' +
            (back.href
                ? '<a class="btn" href="' + esc(back.href) + '">' + esc(back.label) + '</a>'
                : '<span class="updated">opened from no report</span>') +
            '</div></div>' +
            banner + pendingBanner +
            '<div class="card"><div class="counts">' +
            '<div class="c"><span class="n">' + KINDS.length +
            '</span><span class="l">convention kinds</span></div>' +
            '<div class="c ' + (won ? 'ok' : '') + '"><span class="n">' + won +
            '</span><span class="l">documents ACME owns</span></div>' +
            '<div class="c warn"><span class="n">' + none +
            '</span><span class="l">kinds with none</span></div>' +
            '<div class="c"><span class="n">' + fmt(altTotal) +
            '</span><span class="l">curated rows drawn from</span></div>' +
            '</div></div>' +
            '<div class="card">' +
            '<div class="sectionhead"><h2>The kinds</h2>' +
            '<span class="meta">ores_refdata_&lt;kind&gt;_conventions_tbl · ' +
            'ores_dq_&lt;kind&gt;_conventions_artefact_tbl · fixture</span></div>' +
            '<div class="table-wrap"><table class="kindtable"><thead><tr>' +
            '<th class="kind">Kind</th><th>The convention in use</th>' +
            '<th>What its terms say</th><th class="act"></th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">A kind with no document of the party\u2019s own reads ' +
            '<b>none</b>, and the description says what a run uses instead. ' +
            '<b>Change</b> opens that kind alone and carries no other row with it.</p>' +
            '</div>' +
            ruleCard() +
            collisionCard() +
            defaultCard(null) +
            tenantWideCard() +
            draftCard(S, state) +
            '</div>';
    }

    function ruleCard() {
        var example = ruleExample();
        return '<div class="rule neutral"><div class="rtitle">How a name is derived, and what it ' +
            'reads</div><div class="rbody">No convention table carries a name, so the name is read ' +
            'off the terms. <b>An index-based document is named by its index.</b> ' +
            '<b>A terms-based document is named by its calendar, its convention, its day count and ' +
            'its settlement days</b>, plus the payment frequency where the kind states one. ' +
            'A term the row leaves empty is dropped from the name and reported beside it. ' +
            'The fields a kind reads are the fields its own table has, so the field list differs ' +
            'by kind and the table names it under each convention.</div>' +
            '<div class="rexample neutral">index-based \u2192 ' + esc(example.index.name) +
            ' &nbsp;·&nbsp; terms-based \u2192 ' + esc(example.terms.name) + '</div>' +
            '<div class="rbody">Both are real rows of <span class="mono">ores_dq_deposit_' +
            'conventions_artefact_tbl</span>: <span class="mono">' + esc(example.index.key) +
            '</span> and <span class="mono">' + esc(example.terms.key) + '</span>.</div></div>';
    }

    function collisionCard() {
        return '<div class="rule"><div class="rtitle">A derived name does not identify a document' +
            '</div><div class="rbody">The terms identify the document, and the terms repeat. ' +
            'The curated swap table holds <b>320 rows stating 201 distinct sets of terms</b>, ' +
            'so two rows can carry one name: <span class="mono">AED-SWAP</span> and ' +
            '<span class="mono">AED-EIBOR-3M-SWAP</span> state the same TARGET, modified ' +
            'following, 30/360, annual swap. The deposit table holds 163 rows stating 141 sets, ' +
            'and its five bond-yield rows state one set between them. <b>The row key tells them ' +
            'apart and the name does not</b>, which is why the key sits beside every name on a ' +
            'kind\u2019s page.</div></div>';
    }

    /* The default. Shown short on the table and in full on a kind's page. */
    function defaultCard(kind) {
        var marked = kind ? defaultKeyOf(kind) : null;
        return '<div class="rule"><div class="rtitle">Which alternative is the default ' +
            '<span class="tag fx">fixture</span></div>' +
            '<div class="rbody"><b>None is modelled.</b> No convention table carries a default ' +
            'marker: the only <span class="mono">%default%</span> column anywhere is ' +
            '<span class="mono">pays_at_default_time</span> on the CDS terms, which is a CDS ' +
            'business term and not a default flag. ' +
            (marked
                ? 'The default marked on this page, <span class="mono">' + esc(marked) +
                  '</span>, is a fixture.'
                : 'So the fallback a run takes when a party holds nothing is not a choice ' +
                  'between documents that anyone has recorded.') +
            ' <b>A real one would have to live in one of two places:</b> a per-kind default ' +
            'column on <span class="mono">ores_refdata_&lt;kind&gt;_conventions_tbl</span>, or a ' +
            'per-kind default row keyed by tenant and kind in its own table. Neither exists, and ' +
            'this prototype picks neither.</div></div>';
    }

    function tenantWideCard() {
        return '<div class="rule"><div class="rtitle">Three kinds have no party column to write ' +
            'to</div><div class="rbody">' +
            '<span class="mono">ores_refdata_currency_pair_conventions_tbl</span>, ' +
            '<span class="mono">ores_refdata_ibor_index_conventions_tbl</span> and ' +
            '<span class="mono">ores_refdata_overnight_index_conventions_tbl</span> carry ' +
            '<b>no party_id column</b>, unlike the other 22. A document written there belongs to ' +
            'the tenant, not to a party, so ACME cannot own one of those three kinds alone.</div>' +
            '</div>';
    }

    function draftCard(S, state) {
        var pending = state === 'change' ? pendingFor(PENDING_FIXTURE.kind, state) : null;
        if (!pending) {
            return '<div class="card"><div class="sectionhead"><h3>Nothing in progress</h3>' +
                '<a class="btn small" href="' +
                href({ kind: 'swap', state: 'change', variant: S.variant, from: S.from }) +
                '">Show a change in progress (fixture)</a></div>' +
                '<p class="rowfoot">A change is a pick that has not been written. This screen has ' +
                'no Save: a pick becomes real only by writing the party\u2019s own row for the ' +
                'kind, which is the create route.</p></div>';
        }
        return '<div class="card"><div class="sectionhead"><h3>1 change in progress</h3>' +
            '<span class="meta">fixture · not written</span></div>' +
            '<div class="notice">' + esc(pending.kind.label) + ' would take ' +
            '<span class="mono">' + esc(nameOf(pending.kind, pending.row)) + '</span> in place of ' +
            'what it holds. It is not saved. <b>Nothing on this screen writes.</b> Saving it means ' +
            'writing ACME\u2019s own row for the kind, which is the create route.</div>' +
            '<div class="draftrow" style="margin-top:12px">' +
            '<a class="btn" href="' +
            href({ kind: pending.kind.code, state: state, from: S.from }) + '">Open the kind</a>' +
            '<a class="btn primary" href="' +
            href({ screen: 'create', kind: pending.kind.code, seed: PENDING_FIXTURE.from, state: state }) +
            '">Write it as a new convention \u2192</a>' +
            '<a class="btn ghost" href="' + href({ state: 'several', from: S.from }) +
            '">Discard</a>' +
            '</div></div>';
    }

    /* --------------------------------------------------------- the kind page */

    function kindView(S, kind) {
        var state = S.state || 'acme';
        var variant = S.variant || 'A';
        var owned = ownedFor(kind, state);
        var pending = pendingFor(kind.code, state);

        return chrome('conventions') +
            '<div class="shell">' +
            crumbs([
                { label: 'Reporting' },
                { label: 'Configurations' },
                { label: ACME.configuration },
                { label: 'Conventions', href: href({ state: state, from: S.from }) },
                { label: kind.label }
            ]) +
            kindHead(kind, S, state, owned) +
            currentCard(kind, S, state, owned, pending) +
            alternativesCard(kind, S, state, variant, rowsOf(kind), owned) +
            defaultCard(kind) +
            createRouteCard(kind, S, state) +
            '<div class="card"><div class="sectionhead"><h3>Back</h3></div>' +
            '<div class="draftrow">' +
            '<a class="btn" href="' + href({ state: state, from: S.from }) +
            '">\u2190 All the conventions</a>' +
            '<span class="hint">One kind owns this page. No other kind is on it.</span>' +
            '</div></div>' +
            '</div>';
    }

    function kindHead(kind, S, state, owned) {
        var sub = owned.length === 0
            ? 'ACME owns no document of this kind'
            : owned.length === 1
                ? '1 document of ACME\u2019s own'
                : owned.length + ' documents of ACME\u2019s own';
        return '<div class="pagehead"><div>' +
            '<h1>' + esc(kind.label) + ' conventions</h1>' +
            '<p class="lede">' + esc(sub) + '. The curated table for the kind holds ' +
            fmt(kind.altCount) + ' row' + (kind.altCount === 1 ? '' : 's') +
            (kind.alts ? ', stating ' + fmt(kind.alts) + ' distinct sets of terms' : '') +
            '. This page is one kind: its current document, the alternatives, and the way to ' +
            'write one.</p>' +
            '</div><div class="actions">' +
            '<a class="btn primary" href="' +
            href({ screen: 'create', kind: kind.code, state: state, from: S.from }) +
            '">Create a new convention</a>' +
            '</div></div>' +
            '<div class="varianttag"><b>Reading ' + esc(S.variant || 'A') + '</b> of the ' +
            'alternatives. <span class="faint">A lists them described; B compares one against the ' +
            'document in use; C searches and filters. The rest of the page is identical across the ' +
            'three.</span></div>';
    }

    function currentCard(kind, S, state, owned, pending) {
        var head = '<div class="sectionhead"><h2>The document in use</h2>' +
            '<span class="meta">ores_refdata_' + esc(kind.code) +
            '_conventions_tbl · fixture</span></div>';

        if (!owned.length) {
            return '<div class="card">' + head +
                '<div class="doc-card none">' +
                '<div class="dhead"><span class="dname">none</span>' +
                '<span class="tag warn">ACME owns no ' + esc(kind.label.toLowerCase()) +
                ' document</span></div>' +
                '<div class="dfrom">A run that reaches a ' + esc(kind.label.toLowerCase()) +
                ' family falls back to the curated document for the kind. Which curated row it ' +
                'takes is not modelled: the table holds ' + fmt(kind.altCount) + ' row' +
                (kind.altCount === 1 ? '' : 's') + ' and no column chooses one.</div>' +
                '<div class="fieldtable"><table><tbody>' +
                '<tr><td class="fname">term columns</td><td class="fval">' + kind.termCols +
                ' in the curated table</td></tr>' +
                '<tr><td class="fname">named by</td><td class="fval">' +
                esc(kind.nameFields.join(', ')) + '</td></tr>' +
                '</tbody></table></div></div>' +
                '<p class="rowfoot">This is the state every row of the table is in today. ' +
                'The alternatives below are the documents a run would read.</p></div>';
        }

        var doc = owned[0];
        var terms = kind.nameFields.concat(kind.extraFields || []).map(function (field) {
            var has = stated(field, doc.terms[field]);
            return '<tr><td class="fname">' + esc(field) + '</td>' +
                '<td class="fval' + (has ? '' : ' empty') + '">' +
                (has ? esc(said(field, doc.terms[field])) : 'not stated') + '</td></tr>';
        }).join('');
        var match = matchesDefault(kind, doc);
        var tags = ['<span class="tag fx">owned · fixture</span>'];
        if (match === true) tags.push('<span class="tag ok">matches the default</span>');
        if (match === false) tags.push('<span class="tag warn">deviates from the default</span>');

        var several = owned.length > 1
            ? '<div class="notice warn" style="margin-bottom:10px"><b>ACME holds ' + owned.length +
              ' ' + esc(kind.label.toLowerCase()) + ' documents.</b> This model does not say ' +
              'which one a run takes: the key of the table is ' +
              '<span class="mono">(tenant_id, id, party_id)</span>, so several rows per party and ' +
              'kind are allowed, and no column orders them.</div>'
            : '';

        var pendingNotice = pending
            ? '<div class="notice" style="margin-top:10px">A change is in progress: ' +
              '<span class="mono">' + esc(nameOf(pending.kind, pending.row)) +
              '</span> is picked and not saved.</div>'
            : '';

        return '<div class="card">' + head + several +
            '<div class="doc-card">' +
            '<div class="dhead"><span class="dname">' + esc(nameOf(kind, doc.terms)) + '</span>' +
            tags.join('') + '</div>' +
            '<div class="dmeta">' +
            '<span class="mono">ores_refdata_' + esc(kind.code) + '_conventions_tbl</span>' +
            '<span class="tag">version ' + doc.version + '</span>' +
            '<span class="tag">valid from ' + esc(doc.validFrom) + '</span>' +
            '<span>modified by <span class="mono">' + esc(doc.modifiedBy) + '</span></span>' +
            '<span>reason <span class="mono">' + esc(doc.reason) + '</span></span>' +
            '</div>' +
            '<div class="dfrom">' + esc(describeOf(kind, doc.terms)) + '</div>' +
            '<div class="fieldtable"><table><tbody>' + terms + '</tbody></table></div>' +
            '<div class="dfrom">' + (doc.from
                ? 'It was adopted from the curated row <span class="mono">' + esc(doc.from) +
                  '</span> and written into the party\u2019s own table for the kind.'
                : 'It was authored on the create route and written into the party\u2019s own ' +
                  'table for the kind. It matches no curated row: the day count is ACME\u2019s ' +
                  'own choice.') + '</div>' +
            '</div>' + pendingNotice +
            '<p class="rowfoot">The name is derived from the terms above, as on every row of the ' +
            'table. It is not stored.</p></div>';
    }

    function alternativesCard(kind, S, state, variant, rows, owned) {
        var switcher = ['A', 'B', 'C'].map(function (value) {
            return '<a class="btn small' + (variant === value ? ' primary' : '') + '" href="' +
                href({ kind: kind.code, variant: value, state: state, from: S.from }) + '">' +
                esc(value) + '</a>';
        }).join(' ');
        var intro = {
            A: 'every alternative for this kind, described, with the one in use marked.',
            B: 'the document in use beside a candidate, with the fields that differ marked.',
            C: 'search and filter, for the kinds whose curated table is long.'
        }[variant] || '';

        var body = variant === 'B'
            ? compareVariant(kind, S, state, rows, owned)
            : listVariant(kind, S, state, variant, rows, owned);

        return '<div class="card">' +
            '<div class="sectionhead"><h2>The alternatives for this kind</h2>' +
            '<span class="meta">' + switcher + '</span></div>' +
            '<p class="lead">Read from <span class="mono">ores_dq_' + esc(kind.code) +
            '_conventions_artefact_tbl</span>, the curated table for ' + esc(kind.code) + ': ' +
            fmt(kind.altCount) + ' row' + (kind.altCount === 1 ? '' : 's') + ', system owned, ' +
            'tenant <span class="mono">ffffffff-\u2026</span> and party ' +
            '<span class="mono">0000\u2026</span>. ' +
            (rows.length < kind.altCount
                ? 'This prototype carries ' + rows.length + ' of them as a fixture. '
                : '') +
            (owned.length && !owned[0].from
                ? '<b>ACME\u2019s own document matches no row below.</b> It was authored, so ' +
                  'it is not marked in the list; variant B compares it against a candidate and ' +
                  'shows the fields that differ.'
                : '') + '</p>' +
            '<div class="varianttag"><b>' + esc(variant) + '</b> ' + esc(intro) + '</div>' +
            body + '</div>';
    }

    function altRow(kind, state, row, opts) {
        var key = keyOf(kind, row);
        var tags = [];
        var cls = 'alt';
        if (opts.current) { cls += ' current'; tags.push('<span class="tag ok">in use</span>'); }
        if (opts.picked) { cls += ' picked'; tags.push('<span class="tag accent">picked</span>'); }
        if (opts.isDefault) tags.push('<span class="tag fx">default · fixture</span>');
        var action = opts.current
            ? '<span class="hint">This is the document in use. No change to make.</span>'
            : '<a class="btn small" href="' +
              href({ screen: 'create', kind: kind.code, seed: key, state: state }) +
              '">Use these terms \u2192</a>';
        return '<div class="' + cls + '"' + (opts.attrs || '') + '>' +
            '<div class="ahead"><span class="aname">' + esc(nameOf(kind, row)) + '</span>' +
            tags.join('') + '</div>' +
            '<div class="adesc">' + esc(describeOf(kind, row)) + '</div>' +
            '<div class="afoot">' +
            '<span>curated row <span class="mono">' + esc(key) + '</span></span>' +
            action +
            '</div></div>';
    }

    function listVariant(kind, S, state, variant, rows, owned) {
        var defKey = defaultKeyOf(kind);
        var currentKey = owned.length ? owned[0].from : null;
        var pending = pendingFor(kind.code, state);
        var pendingKey = pending ? keyOf(kind, pending.row) : null;
        var hasFlag = kind.mode === 'flag-index';

        var list = rows.map(function (row) {
            var key = keyOf(kind, row);
            var attrs = variant === 'C'
                ? ' data-name="' + esc(nameOf(kind, row).toLowerCase()) +
                  '" data-desc="' + esc(describeOf(kind, row).toLowerCase()) +
                  '" data-mode="' + (row.index_based === true ? 'index' : 'terms') +
                  '" data-default="' + (defKey === key ? '1' : '0') +
                  '" data-current="' + (key === currentKey || key === pendingKey ? '1' : '0') + '"'
                : '';
            return altRow(kind, state, row, {
                current: key === currentKey,
                picked: key === pendingKey,
                isDefault: defKey === key,
                attrs: attrs
            });
        }).join('');

        if (variant === 'A') {
            return '<div class="alts">' + list + '</div>' +
                '<p class="rowfoot">The one in use is marked where ACME holds a document. ' +
                'Every other row is a document a run could use, and taking one means writing ' +
                'ACME\u2019s own row, not editing the curated one.</p>';
        }

        var chips = ['<button class="filterchip on" data-filter="all">All</button>'];
        if (hasFlag) {
            chips.push('<button class="filterchip" data-filter="index">Index-based</button>');
            chips.push('<button class="filterchip" data-filter="terms">Terms-based</button>');
        }
        if (defKey) chips.push('<button class="filterchip" data-filter="default">Marked default</button>');
        if (currentKey || pendingKey) {
            chips.push('<button class="filterchip" data-filter="other">Not the one in use</button>');
        }

        return '<div class="searchbar" style="margin-bottom:10px">' +
            '<input type="search" id="alt-search" placeholder="Search the name or the terms…">' +
            chips.join('') +
            '<span class="searchcount" id="alt-count"></span>' +
            '</div>' +
            '<div class="alts" id="alt-list">' + list + '</div>' +
            '<p class="rowfoot">The search reads the derived name and the description, so it ' +
            'finds a document by a term a person would say aloud. It reads nothing else: no ' +
            'index, no ranking, no server. A kind whose curated table holds hundreds of rows ' +
            'needs this; a kind holding one row does not.</p>';
    }

    function compareVariant(kind, S, state, rows, owned) {
        var defKey = defaultKeyOf(kind);
        var current = owned.length ? owned[0] : null;
        var pending = pendingFor(kind.code, state);
        var pickedKey = S.cand || (pending ? keyOf(kind, pending.row) : null);
        if (!pickedKey) pickedKey = keyOf(kind, rows[rows.length - 1]);
        var candidate = rows[0];
        rows.forEach(function (row) { if (keyOf(kind, row) === pickedKey) candidate = row; });

        var picker = '<div class="vside"><div class="sectionhead"><h3>The candidate</h3>' +
            '<span class="meta">' + rows.length + ' shown here</span></div>' +
            '<div class="alts">' + rows.map(function (row) {
                var key = keyOf(kind, row);
                var on = key === keyOf(kind, candidate);
                return '<div class="alt' + (on ? ' picked' : '') + '">' +
                    '<div class="ahead"><span class="aname">' + esc(nameOf(kind, row)) + '</span>' +
                    (defKey === key ? '<span class="tag fx">default · fixture</span>' : '') +
                    (current && current.from === key ? '<span class="tag ok">in use</span>' : '') +
                    '</div>' +
                    '<div class="afoot"><span class="mono">' + esc(key) + '</span>' +
                    '<a class="btn small' + (on ? ' primary' : '') + '" href="' +
                    href({ kind: kind.code, variant: 'B', state: state, cand: key, from: S.from }) +
                    '">' + (on ? 'Comparing' : 'Compare') + '</a></div></div>';
            }).join('') + '</div></div>';

        var diffs = differences(kind, current, candidate);
        var fields = kind.nameFields.concat(kind.extraFields || []);
        var diffRows = fields.map(function (field) {
            var before = current ? current.terms[field] : undefined;
            var after = candidate[field];
            var differs = String(before || '') !== String(after || '');
            var beforeText = stated(field, before) ? said(field, before) : 'not stated';
            var afterText = stated(field, after) ? said(field, after) : 'not stated';
            return '<div class="diffline' + (differs ? ' diff' : '') + '">' +
                '<span class="fn">' + esc(field) + '</span>' +
                '<span class="bv' + (beforeText === 'not stated' ? ' empty' : '') + '">' +
                esc(beforeText) + '</span>' +
                '<span class="cv' + (afterText === 'not stated' ? ' empty' : '') + '">' +
                esc(afterText) + '</span>' +
                (differs ? '<span class="tag warn">differs</span>'
                    : '<span class="tag muted">same</span>') +
                '</div>';
        }).join('');

        var sides = current
            ? '<div class="cmp"><div class="side">' +
              '<div class="sh"><span class="did">' + esc(nameOf(kind, current.terms)) + '</span>' +
              '<span class="tag fx">owned · fixture</span></div>' +
              '<div class="sdesc">' + esc(describeOf(kind, current.terms)) + '</div>' +
              '<div class="sdesc faint">' + (current.from
                  ? 'Adopted from <span class="mono">' + esc(current.from) + '</span>'
                  : 'Authored on the create route; it matches no curated row') + '</div></div>' +
              '<div class="side">' +
              '<div class="sh"><span class="did">' + esc(nameOf(kind, candidate)) + '</span>' +
              '<span class="tag muted">curated</span></div>' +
              '<div class="sdesc">' + esc(describeOf(kind, candidate)) + '</div>' +
              '<div class="sdesc faint">Curated row <span class="mono">' +
              esc(keyOf(kind, candidate)) + '</span></div></div></div>'
            : '<div class="cmp"><div class="side none">' +
              '<div class="sh"><span class="did">none</span>' +
              '<span class="tag warn">no document of ACME\u2019s own</span></div>' +
              '<div class="sdesc">Nothing is held for this kind, so there is nothing to compare ' +
              'against. The candidate would become ACME\u2019s first document of the kind.</div>' +
              '</div><div class="side">' +
              '<div class="sh"><span class="did">' + esc(nameOf(kind, candidate)) + '</span>' +
              '<span class="tag muted">curated</span></div>' +
              '<div class="sdesc">' + esc(describeOf(kind, candidate)) + '</div>' +
              '<div class="sdesc faint">Curated row <span class="mono">' +
              esc(keyOf(kind, candidate)) + '</span></div></div></div>';

        var verdict = current
            ? (diffs.length
                ? '<div class="notice warn"><b>' + diffs.length +
                  (diffs.length === 1 ? ' field differs' : ' fields differ') + '</b> between ' +
                  'the document in use and the candidate: ' +
                  diffs.map(function (diff) {
                      return '<span class="mono">' + esc(diff.field) + '</span>';
                  }).join(', ') +
                  '. The two price the same trade differently, and each is correct under its own ' +
                  'terms.</div>'
                : '<div class="notice ok"><b>The candidate states the same terms as the document ' +
                  'in use.</b> Adopting it would add a second row that says nothing new. ' +
                  'The names are equal for the same reason.</div>')
            : '<div class="notice">There is no document in use to compare against, so every ' +
              'field of the candidate is new. The comparison marks them for that reason, not ' +
              'because the terms are unusual.</div>';

        return '<div class="vsplit">' + picker +
            '<div class="vside">' + verdict + sides +
            '<div class="fieldtable">' + diffRows + '</div>' +
            '<div class="draftrow">' +
            '<a class="btn primary" href="' +
            href({ screen: 'create', kind: kind.code, seed: keyOf(kind, candidate), state: state }) +
            '">Use these terms \u2192</a>' +
            '<span class="hint">Writing them means ACME owns a row for this kind, which is the ' +
            'create route.</span></div>' +
            '</div></div>';
    }

    function createRouteCard(kind, S, state) {
        return '<div class="card"><div class="sectionhead"><h3>Authoring a convention is ' +
            'another job</h3><span class="meta">leaves the report flow</span></div>' +
            '<p class="lead">The alternatives are the curated documents, and taking one is a ' +
            'choice between documents that exist. Writing a convention of ACME\u2019s own is ' +
            'not a choice: it is authoring. It has its own screen, and it does not return to ' +
            'the report when it is done.</p>' +
            '<div class="draftrow">' +
            '<a class="btn" href="' +
            href({ screen: 'create', kind: kind.code, state: state, from: S.from }) +
            '">Create a new ' + esc(kind.label.toLowerCase()) + ' convention \u2192</a>' +
            '<span class="hint">Writes ores_refdata_' + esc(kind.code) +
            '_conventions_tbl. Bitemporal. The party\u2019s own row.</span>' +
            '</div></div>';
    }

    /* ------------------------------------------------------- the create route */

    function createView(S) {
        var kind = kindOf(S.code) || kindOf('deposit');
        var seed = null;
        var rows = rowsOf(kind);
        for (var i = 0; i < rows.length; i++) {
            if (keyOf(kind, rows[i]) === S.seed) seed = rows[i];
        }
        var tenantWide = TENANT_WIDE_KINDS.indexOf(kind.code) !== -1;

        var kindOptions = KINDS.map(function (item) {
            return '<option value="' + esc(item.code) + '"' +
                (item.code === kind.code ? ' selected' : '') + '>' + esc(item.label) +
                ' (' + esc(item.code) + ')</option>';
        }).join('');

        var fields = kind.nameFields.concat(kind.extraFields || []).map(function (field) {
            var value = seed && stated(field, seed[field]) ? String(seed[field]) : '';
            return '<div class="formrow"><label>' + esc(fieldWords(field)) + '</label>' +
                '<input type="text" value="' + esc(value) + '" placeholder="' +
                (value ? '' : 'not stated') + '">' +
                '<span class="fhint">' + esc(termHint(field)) + ' <span class="mono">' +
                esc(field) + '</span></span></div>';
        }).join('');

        var seedNote = seed
            ? '<div class="notice"><b>Seeded from the curated row <span class="mono">' +
              esc(keyOf(kind, seed)) + '</span>.</b> The form below carries its terms. ' +
              'The names and descriptions on the conventions screen come from what you write ' +
              'here, so a term left empty leaves the name short.</div>'
            : '<div class="notice">The form is empty. Fill the terms for a ' +
              esc(kind.label.toLowerCase()) + ' convention, or start from a curated alternative ' +
              'on the kind\u2019s page.</div>';

        return chrome('conventions') +
            '<div class="shell">' +
            crumbs([
                { label: 'Conventions', href: href({ state: S.state, from: S.from }) },
                { label: kind.label, href: href({ kind: kind.code, state: S.state, from: S.from }) },
                { label: 'Create a convention' }
            ]) +
            '<div class="pagehead"><div>' +
            '<h1>Create a convention</h1>' +
            '<p class="lede">Authoring a convention is its own screen because it is its own job. ' +
            'The conventions table chooses between documents that exist; this page writes one ' +
            'that does not. It writes ACME\u2019s row into ' +
            '<span class="mono">ores_refdata_&lt;kind&gt;_conventions_tbl</span>, bitemporal, for ' +
            (tenantWide
                ? 'the tenant, because this kind\u2019s table carries no party column.'
                : 'the tenant and the party of the session.') + '</p>' +
            '</div><div class="actions">' +
            '<a class="btn" href="' + href({ kind: kind.code, state: S.state, from: S.from }) +
            '">Cancel</a>' +
            '</div></div>' +
            '<div class="notice warn loud"><b>This leaves the report flow.</b> A person arrives ' +
            'here from a report and does not return to it: convention management owns the ' +
            'convention from this point, and the report reads whatever the kind holds the next ' +
            'time it runs.</div>' +
            seedNote +
            '<div class="card"><div class="sectionhead"><h2>The kind and the terms</h2>' +
            '<span class="meta">fixture · nothing is written</span></div>' +
            '<div class="createform">' +
            '<div class="formrow"><label>The kind of convention</label>' +
            '<select id="kind-select">' + kindOptions + '</select>' +
            '<span class="fhint">One kind per document. The terms below are the terms this ' +
            'kind\u2019s own table carries, which is why the form changes with the kind.</span>' +
            '</div>' +
            '<div class="formgrid">' + fields + '</div>' +
            '<div class="formrow"><label>Why it changes ' +
            '<span class="mono">change_reason_code</span></label>' +
            '<select><option>ADOPT-CURATED</option><option>HOUSE-STANDARD</option>' +
            '<option>CLIENT-TERMS</option><option>CORRECTION</option></select>' +
            '<span class="fhint">A reason code is required on a bitemporal write. The list here ' +
            'is a fixture; the real one is a reference list.</span></div>' +
            '<div class="formrow"><label>Commentary ' +
            '<span class="mono">change_commentary</span></label>' +
            '<textarea placeholder="What this convention states, and why."></textarea>' +
            '<span class="fhint">Stored with the row and read back on its history.</span></div>' +
            '<div class="formrow"><label>Valid from <span class="mono">valid_from</span></label>' +
            '<input type="text" value="2026-10-17 12:00:00+00">' +
            '<span class="fhint">Bitemporal: the server closes the row this one replaces for the ' +
            'same (tenant, id, party) and opens this one. valid_to is the infinity timestamp ' +
            'until it is closed.</span></div>' +
            '</div></div>' +
            writeCard(kind, tenantWide) +
            '<div class="card"><div class="sectionhead"><h3>Write it</h3>' +
            '<span class="meta">no operation is called from here</span></div>' +
            '<div class="draftrow">' +
            '<button class="btn primary" disabled>Save the convention</button>' +
            '<span class="hint">Not wired. The prototype writes nothing and calls no ' +
            'subject.</span></div>' +
            '<p class="rowfoot">The card above says exactly what was checked about the write and ' +
            'what was not. Nothing on this page claims more than that.</p></div>' +
            '</div>';
    }

    function termHint(field) {
        var hints = {
            calendar: 'The calendar the schedule rolls on.',
            convention: 'The business-day rule.',
            day_count_fraction: 'The day count.',
            settlement_days: 'Business days from the trade to settlement.',
            index: 'The fixing index, by its ORE name.',
            fixed_calendar: 'The calendar the fixed leg rolls on.',
            fixed_frequency: 'How often the fixed leg pays.',
            fixed_convention: 'The fixed leg business-day rule.',
            fixed_day_count_fraction: 'The fixed leg day count.',
            fx_convention_id: 'The FX convention this option prices under.',
            pair_code: 'The currency pair, as the pair table states it.',
            settlement_calendar: 'The settlement calendar, or a comma-joined list.',
            fixing_calendar: 'The calendar the index fixes on.',
            business_day_convention: 'The business-day rule for the schedule.',
            short_index: 'The index the short leg fixes on.',
            long_index: 'The index the long leg fixes on.',
            domain: 'No term of this name is known to the prototype.'
        };
        return hints[field] || 'A term of this kind, said as the table states it.';
    }

    function writeCard(kind, tenantWide) {
        var subject = 'refdata.v1.' + kind.code + '_conventions.put';
        return '<div class="card"><div class="sectionhead"><h2>What this writes</h2>' +
            '<span class="meta">checked against the checkout, not against a run</span></div>' +
            '<div class="writeblock">' +
            '<div class="op"><span class="subject">' + esc(subject) + '</span>' +
            '<span class="what">the party\u2019s own row for this kind</span></div>' +
            '<table><tbody>' +
            '<tr><td>table</td><td class="val">ores_refdata_' + esc(kind.code) +
            '_conventions_tbl</td></tr>' +
            '<tr><td>carries</td><td class="val plain">tenant_id, ' +
            (tenantWide ? '<b>no party_id column</b>' : 'party_id') + ', version, ' +
            esc(kind.nameFields.join(', ')) + ', oresmd_uri</td></tr>' +
            '<tr><td>bitemporal</td><td class="val plain">modified_by, performed_by, ' +
            'change_reason_code, change_commentary, valid_from, valid_to</td></tr>' +
            '<tr><td>request</td><td class="val plain">a change: the write plus a precondition, ' +
            'and a change intent carrying the reason code and the commentary</td></tr>' +
            '<tr><td>owner</td><td class="val plain">' +
            (tenantWide ? 'the tenant, because this table has no party column'
                : 'the tenant and the party of the session') + '</td></tr>' +
            '</tbody></table></div>' +
            '<div class="rule ok" style="margin-top:12px"><div class="rtitle">What was verified ' +
            'about the write</div><div class="rbody">The subject is declared on the kind\u2019s ' +
            'protocol header and subscribed by the service\u2019s registrar: <b>26 protocol ' +
            'headers and 26 registrars</b>, one per kind including tenor, with a handler and a ' +
            'party-scoped repository behind each. <b>No run of the write is recorded here</b>, ' +
            'so this page claims that the operation exists in shape and claims nothing about ' +
            'whether it works.</div></div>' +
            (tenantWide
                ? '<div class="rule" style="margin-top:12px"><div class="rtitle">This kind ' +
                  'cannot be party-owned</div><div class="rbody"><span class="mono">' +
                  'ores_refdata_' + esc(kind.code) + '_conventions_tbl</span> has <b>no party_id ' +
                  'column</b>. A row written here belongs to the whole tenant, so ACME cannot ' +
                  'hold terms for this kind alone. The conventions table says so on the row, and ' +
                  'the form stays open, because the terms are real even when the ownership is ' +
                  'not.</div></div>' : '') +
            '</div>';
    }

    /* ------------------------------------------------------------- the router */

    function readQuery() {
        var params = new URLSearchParams(window.location.search);
        return {
            screen: params.get('screen') || '',
            code: params.get('kind') || '',
            variant: (params.get('variant') || 'A').toUpperCase(),
            state: params.get('state') || 'acme',
            from: params.get('from') || '',
            cand: params.get('cand') || '',
            seed: params.get('seed') || ''
        };
    }

    var lastSearch = '';
    var filterState = { term: '', mode: 'all' };

    function render() {
        var S = readQuery();
        lastSearch = window.location.search;
        filterState = { term: '', mode: 'all' };

        var app = document.getElementById('app');
        if (S.screen === 'create') {
            app.innerHTML = createView(S);
        } else if (S.code && kindOf(S.code)) {
            app.innerHTML = kindView(S, kindOf(S.code));
        } else {
            app.innerHTML = tableView(S);
        }
        renderBar(S);
        window.scrollTo(0, 0);
        wire(S);
    }

    function wire(S) {
        var select = document.getElementById('kind-select');
        if (select) {
            select.addEventListener('change', function () {
                window.location.search = searchOf({
                    screen: 'create', kind: select.value, state: S.state,
                    from: S.from, seed: S.seed
                });
            });
        }

        var search = document.getElementById('alt-search');
        if (!search) return;
        search.addEventListener('input', function () {
            filterState.term = search.value.toLowerCase();
            applyFilter();
        });
        Array.prototype.forEach.call(document.querySelectorAll('.filterchip'), function (chip) {
            chip.addEventListener('click', function () {
                filterState.mode = chip.getAttribute('data-filter');
                Array.prototype.forEach.call(document.querySelectorAll('.filterchip'), function (other) {
                    other.classList.toggle('on', other === chip);
                });
                applyFilter();
            });
        });
        applyFilter();
    }

    function applyFilter() {
        var list = document.getElementById('alt-list');
        var count = document.getElementById('alt-count');
        if (!list) return;
        var rows = list.querySelectorAll('.alt');
        var visible = 0;
        Array.prototype.forEach.call(rows, function (row) {
            var text = (row.getAttribute('data-name') || '') + ' ' +
                (row.getAttribute('data-desc') || '');
            var mode = row.getAttribute('data-mode');
            var show = !filterState.term || text.indexOf(filterState.term) !== -1;
            if (filterState.mode === 'index') show = show && mode === 'index';
            if (filterState.mode === 'terms') show = show && mode === 'terms';
            if (filterState.mode === 'default') show = show && row.getAttribute('data-default') === '1';
            if (filterState.mode === 'other') show = show && row.getAttribute('data-current') !== '1';
            row.style.display = show ? '' : 'none';
            if (show) visible++;
        });
        if (count) {
            count.innerHTML = 'showing <b>' + visible + '</b> of <b>' + rows.length +
                '</b> carried here';
        }
    }

    function searchOf(params) {
        var pairs = [];
        Object.keys(params).forEach(function (key) {
            var value = params[key];
            if (value === null || value === undefined || value === '') return;
            pairs.push(encodeURIComponent(key) + '=' + encodeURIComponent(value));
        });
        return pairs.length ? '?' + pairs.join('&') : '';
    }

    /* The bar states where the reader is and what the fixture is set to. It is
       drawn after the view, so no view has to carry it. */
    function renderBar(S) {
        var note = document.getElementById('proto-note');
        if (note) note.innerHTML = protoNoteInner();
        var bar = document.getElementById('proto-bar');
        if (!bar) return;

        var parts = [];
        parts.push('<span class="tag-prototype">fixture</span>');
        parts.push('<span class="label">state</span>');
        parts.push(barLink('ACME: no document', S.state === 'acme',
            { state: 'acme', kind: S.code, variant: S.variant, from: S.from }));
        parts.push(barLink('Fixture: 3 documents', S.state === 'several',
            { state: 'several', kind: S.code, variant: S.variant, from: S.from }));
        parts.push(barLink('Change in progress', S.state === 'change',
            { state: 'change', kind: S.code, variant: S.variant, from: S.from }));
        if (S.code) {
            parts.push('<span class="sep">|</span><span class="label">reading</span>');
            ['A', 'B', 'C'].forEach(function (value) {
                parts.push(barLink(value, S.variant === value,
                    { state: S.state, kind: S.code, variant: value, from: S.from }));
            });
        }
        parts.push('<span class="sep">|</span><span class="label">opened from</span>');
        parts.push(barLink('nowhere', !S.from,
            { state: S.state, kind: S.code, variant: S.variant }));
        parts.push(barLink('Create a report', S.from === 'create-a-report',
            { state: S.state, kind: S.code, variant: S.variant, from: 'create-a-report' }));
        parts.push(barLink('Change what a report runs', S.from === 'change-what-a-report-runs',
            { state: S.state, kind: S.code, variant: S.variant, from: 'change-what-a-report-runs' }));
        parts.push('<span class="sep">|</span>');
        parts.push('<a class="label" href="index.html">start again</a>');

        bar.innerHTML = '<div class="bar-line">' + parts.join(' ') + '</div>';
    }

    function boot() {
        render();
        window.addEventListener('popstate', function () {
            if (window.location.search !== lastSearch) render();
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', boot);
    } else {
        boot();
    }
})();

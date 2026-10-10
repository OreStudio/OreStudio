/* Define how tenors resolve. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * Journey 9 of the agreed ores.refdata catalogue, for a tenant administrator:
 * a flat step rail on the left, one step on the right. Every field is a field
 * the entity models carry, every operation is an operation the generated
 * protocol headers publish, and every refusal is a refusal the domain
 * resolution function in ores.refdata.api/domain/tenor_resolution.hpp throws.
 * Where the journey needs something the server does not serve, the screen says
 * so in a gap note instead of inventing it.
 *
 * States are chosen from the query string, as the other refdata prototypes do:
 *   ?state=list|tenor|convention|schedule|resolve|review|outcome|refused|history
 *   (the alias ?state=failure opens the same refused screen)
 *   ?tenor=3M ?query=ON ?kind=PERIOD|SPECIAL
 *   ?convention=RATES_SPOT_FORWARD ?cal=GB.LOIOB ?horizon=2026-03-16
 *   ?roll=Following ?schedule=ROLL_QUARTER ?rollcount=12 ?rollstart=spot
 *   ?fail=<kind>|1   the refusal to draw; 1 picks the first kind, kind_mismatch
 *   ?version=2
 * Four-eyes states, from the book controls note (four-eyes.js draws them):
 *   ?state=waiting|decide|declined   a raised request waiting, the checker's
 *                                    decision screen, and a declined request
 *   ?actor=m.risk|o.ops|...          who answers on the decision screen (decide only)
 *   ?maker=h.desk|m.risk|...         who raised the request; the maker cannot decide it
 *   ?check=resolve                   make the tenor-resolves check fail
 * The bar mirrors the same states as buttons and carries the refusal and the
 * convention as toggles. */

(function () {
    'use strict';

    /* ------------------------------------------------------------- dates */

    var MONTH_NAMES = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
    var WEEKDAY_NAMES = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

    function iso(d) {
        return d.toISOString().slice(0, 10);
    }

    function parseIso(s) {
        var m = /^(\d{4})-(\d{2})-(\d{2})$/u.exec(String(s || ''));
        if (m === null) return null;
        return new Date(Date.UTC(+m[1], +m[2] - 1, +m[3]));
    }

    function shift(d, days) {
        return new Date(d.getTime() + days * 86400000);
    }

    function fmt(d) {
        return WEEKDAY_NAMES[d.getUTCDay()] + ' ' + d.getUTCDate() + ' ' + MONTH_NAMES[d.getUTCMonth()] + ' ' + d.getUTCFullYear();
    }

    function today() {
        var n = new Date();
        return new Date(Date.UTC(n.getFullYear(), n.getMonth(), n.getDate()));
    }

    /* ------------------------------------------------------- calendars */

    var CALENDARS = {
        'GB.LOIOB': {
            code: 'GB.LOIOB', description: 'London interbank, the GBP settlement calendar.',
            holidays: ['2026-01-01', '2026-04-03', '2026-04-06', '2026-05-04', '2026-05-25', '2026-08-31', '2026-12-25', '2026-12-28',
                '2027-01-01', '2027-03-26', '2027-03-29', '2027-05-03', '2027-05-31', '2027-08-30', '2027-12-27', '2027-12-28']
        },
        'US.FOMC': {
            code: 'US.FOMC', description: 'The Federal Reserve meeting calendar, the one FOMC_MEETING reads.',
            holidays: ['2026-01-01', '2026-01-19', '2026-05-25', '2026-07-03', '2026-09-07', '2026-11-26', '2026-12-25',
                '2028-01-26', '2028-02-21', '2028-05-29']
        },
        'US.NYSE': {
            code: 'US.NYSE', description: 'The US settlement calendar.',
            holidays: ['2026-01-01', '2026-01-19', '2026-02-16', '2026-04-03', '2026-05-25', '2026-06-19', '2026-07-03',
                '2026-09-07', '2026-11-26', '2026-12-25',
                '2027-01-01', '2027-11-25', '2027-12-24']
        },
        'TARGET2': {
            code: 'TARGET2', description: 'The euro settlement calendar.',
            holidays: ['2026-01-01', '2026-04-03', '2026-04-06', '2026-05-01', '2026-12-25', '2026-12-26',
                '2027-01-01', '2027-03-26', '2027-03-29', '2027-05-01', '2027-12-25', '2027-12-26']
        }
    };

    var CCY = {
        GBP: { code: 'GBP', calendar: 'GB.LOIOB', spot_days: 2 },
        USD: { code: 'USD', calendar: 'US.NYSE', spot_days: 2 },
        EUR: { code: 'EUR', calendar: 'TARGET2', spot_days: 2 }
    };

    function holidaySet(code) {
        var cal = CALENDARS[code] || CALENDARS['GB.LOIOB'];
        var set = {};
        cal.holidays.forEach(function (d) { set[d] = true; });
        return set;
    }

    function isBusinessDay(d, hol) {
        var w = d.getUTCDay();
        return w !== 0 && w !== 6 && hol[iso(d)] !== true;
    }

    function addBusinessDays(start, n, hol) {
        var d = start;
        var left = Math.abs(n);
        var sign = n < 0 ? -1 : 1;
        while (left > 0) {
            d = shift(d, sign);
            if (isBusinessDay(d, hol)) left -= 1;
        }
        return d;
    }

    /* The roll rules the business_day_convention_type lookup holds. */
    function adjust(d, rule, hol) {
        if (rule === 'Unadjusted' || isBusinessDay(d, hol)) return d;
        if (rule === 'Preceding') return addBusinessDays(d, -1, hol);
        var f = addBusinessDays(d, 1, hol);
        if (rule === 'ModifiedFollowing' && f.getUTCMonth() !== d.getUTCMonth()) {
            return addBusinessDays(d, -1, hol);
        }
        return f;
    }

    /* ------------------------------------------------------------- data */

    var KINDS = {
        PERIOD: 'A regular nD/nW/nM/nY duration with a fixed offset from the anchor.',
        SPECIAL: 'A label with no fixed offset, resolved by rule under each convention.'
    };
    var UNITS = ['DAY', 'WEEK', 'MONTH', 'YEAR', 'NONE'];
    var ANCHORS = {
        SPOT: 'The spot date: horizon plus the currency pair\'s spot days, rolled on its calendar.',
        TODAY: 'The horizon date itself, unadjusted.',
        TOMORROW: 'The business day after the horizon, on the chosen calendar.',
        NEAR_LEG: 'The near leg of the swap, which the quote itself fixes.',
        IMM_ROLL: 'An IMM quarterly roll date, from the ROLL_QUARTER schedule.'
    };
    var ALGORITHMS = {
        ANCHOR_OFFSET: 'The anchor date plus the tenor\'s own unit and multiplier.',
        SCHEDULE_STEP: 'The anchor plus a calendar offset, then n steps along a named schedule axis.'
    };
    var ROLL_RULES = ['Following', 'ModifiedFollowing', 'Preceding', 'Unadjusted'];

    var SCHEDULES = {
        ROLL_QUARTER: {
            code: 'ROLL_QUARTER', name: 'IMM quarterly roll',
            description: 'The first business day after the 20th of March, June, September and December.',
            schedule_source: 'CLOSED_FORM', calendar_code: null, diary_entry_type: null
        },
        FOMC_MEETING: {
            code: 'FOMC_MEETING', name: 'FOMC meeting',
            description: 'The central_bank_meeting diary events named on the US.FOMC calendar.',
            schedule_source: 'EVENT_LOOKUP', calendar_code: 'US.FOMC', diary_entry_type: 'central_bank_meeting'
        }
    };

    var FOMC_DATES = [
        '2026-01-28', '2026-03-18', '2026-04-29', '2026-06-17', '2026-07-29', '2026-09-16', '2026-11-04',
        '2026-12-16', '2027-01-26', '2027-03-17', '2027-04-28', '2027-06-16', '2027-07-28', '2027-09-15',
        '2027-11-03', '2027-12-15', '2028-01-25', '2028-03-15', '2028-04-26', '2028-06-14', '2028-07-26',
        '2028-09-20', '2028-11-01', '2028-12-13', '2029-01-24', '2029-03-14'
    ];

    function scheduleDateList(kind, hol) {
        if (kind === 'ROLL_QUARTER') {
            var out = [];
            for (var y = 2026; y <= 2028; y += 1) {
                [3, 6, 9, 12].forEach(function (m) {
                    var d = new Date(Date.UTC(y, m - 1, 20));
                    var rolled = addBusinessDays(d, 1, hol);
                    /* The IMM rule is "the first business day after the 20th", so a
                       non-business day on the 20th rolls forward, never back. */
                    if (!isBusinessDay(d, hol)) rolled = addBusinessDays(d, 1, hol);
                    out.push(rolled);
                });
            }
            return out;
        }
        return FOMC_DATES.map(parseIso);
    }

    var CONVENTIONS = {
        RATES_SPOT_FORWARD: {
            code: 'RATES_SPOT_FORWARD',
            description: 'Rates spot/forward curves: regular periods measured from spot.',
            measured_from: 'SPOT', resolution_algorithm: 'ANCHOR_OFFSET', base_ccy: 'GBP',
            resolutions: [
                { tenor_code: 'O/N', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 1, schedule_code: null, schedule_step_count: null },
                { tenor_code: 'T/N', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 2, schedule_code: null, schedule_step_count: null },
                { tenor_code: 'S/W', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 7, schedule_code: null, schedule_step_count: null },
                { tenor_code: '1M', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null },
                { tenor_code: '3M', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null },
                { tenor_code: '6M', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null },
                { tenor_code: '1Y', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null },
                { tenor_code: '1Y1Y', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null }
            ]
        },
        FX_SWAP_NEAR_LEG: {
            code: 'FX_SWAP_NEAR_LEG',
            description: 'FX swap curves, quoted from the near leg: the short labels are day-counts from it.',
            measured_from: 'NEAR_LEG', resolution_algorithm: 'ANCHOR_OFFSET', base_ccy: 'GBP',
            resolutions: [
                { tenor_code: 'O/N', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 0, schedule_code: null, schedule_step_count: null },
                { tenor_code: 'T/N', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 1, schedule_code: null, schedule_step_count: null },
                { tenor_code: 'S/W', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 7, schedule_code: null, schedule_step_count: null },
                { tenor_code: 'S/N', anchor_override: null, offset_unit: 'DAY', offset_multiplier: 0, schedule_code: null, schedule_step_count: null },
                { tenor_code: '1M', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null },
                { tenor_code: '3M', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null },
                { tenor_code: '6M', anchor_override: null, offset_unit: null, offset_multiplier: null, schedule_code: null, schedule_step_count: null }
            ]
        },
        CREDIT_CDS_IMM: {
            code: 'CREDIT_CDS_IMM',
            description: 'Credit and FOMC curves: a calendar offset from spot, then n steps along a schedule axis.',
            measured_from: 'NONE', resolution_algorithm: 'SCHEDULE_STEP', base_ccy: 'GBP',
            resolutions: [
                { tenor_code: '1Y1Y', anchor_override: 'SPOT', offset_unit: 'YEAR', offset_multiplier: 1, schedule_code: 'ROLL_QUARTER', schedule_step_count: 2 },
                { tenor_code: '6M', anchor_override: 'SPOT', offset_unit: 'MONTH', offset_multiplier: 6, schedule_code: 'FOMC_MEETING', schedule_step_count: 1 },
                { tenor_code: '1Y3RQ', anchor_override: 'SPOT', offset_unit: 'YEAR', offset_multiplier: 1, schedule_code: 'FOMC_MEETING', schedule_step_count: 3 },
                { tenor_code: '1Y12RQ', anchor_override: 'SPOT', offset_unit: 'YEAR', offset_multiplier: 1, schedule_code: 'ROLL_QUARTER', schedule_step_count: 12 }
            ]
        }
    };

    /* The tenor catalog. The columns are the tenor model's own: code,
     * display_name, description, sort_order, kind, unit, multiplier. The
     * version columns stand for the row the history provider serves. */
    var TENORS = [
        { code: 'O/N', display_name: 'Overnight', description: 'The shortest money-market label. A day-count of one day under the spot/forward convention, and zero days under the swap convention.', sort_order: 10, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: 'T/N', display_name: 'Tomorrow next', description: 'Tom-next. The label the swap market quotes the near leg to one day later.', sort_order: 20, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: 'S/N', display_name: 'Spot next', description: 'Spot-next, the swap label one day past spot. It belongs to the swap convention only.', sort_order: 25, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: 'S/W', display_name: 'Spot week', description: 'One week from spot, and its own label rather than 7D.', sort_order: 30, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'j.smith', recorded_at: '2026-02-11 16:02', change_reason_code: 'DATA_FIX' },
        { code: '1W', display_name: 'One week', description: 'Seven days from the anchor.', sort_order: 40, kind: 'PERIOD', unit: 'WEEK', multiplier: 1, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: '1M', display_name: 'One month', description: 'One calendar month from the anchor.', sort_order: 50, kind: 'PERIOD', unit: 'MONTH', multiplier: 1, version: 3, modified_by: 'a.tanaka', recorded_at: '2026-03-04 11:20', change_reason_code: 'DATA_FIX' },
        { code: '3M', display_name: 'Three month', description: 'The three-month pillar. It was carried as ninety days until it was corrected to three calendar months.', sort_order: 60, kind: 'PERIOD', unit: 'MONTH', multiplier: 3, version: 2, modified_by: 'j.smith', recorded_at: '2026-03-11 08:45', change_reason_code: 'DATA_FIX' },
        { code: '6M', display_name: 'Six month', description: 'The six-month pillar.', sort_order: 70, kind: 'PERIOD', unit: 'MONTH', multiplier: 6, version: 2, modified_by: 'j.smith', recorded_at: '2026-03-11 08:47', change_reason_code: 'DATA_FIX' },
        { code: '1Y', display_name: 'One year', description: 'The one-year pillar.', sort_order: 80, kind: 'PERIOD', unit: 'YEAR', multiplier: 1, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: '2Y', display_name: 'Two year', description: 'The two-year pillar.', sort_order: 90, kind: 'PERIOD', unit: 'YEAR', multiplier: 2, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: '1Y1Y', display_name: 'One year, one year forward', description: 'A forward label: one calendar year of offset from the anchor, then two quarterly rolls.', sort_order: 100, kind: 'PERIOD', unit: 'YEAR', multiplier: 1, version: 2, modified_by: 'm.okafor', recorded_at: '2026-03-12 15:31', change_reason_code: 'NEW_LABEL' },
        { code: '1Y3RQ', display_name: 'One year, three meetings', description: 'One year of offset from spot, then three steps along the FOMC meeting schedule.', sort_order: 105, kind: 'PERIOD', unit: 'YEAR', multiplier: 1, version: 1, modified_by: 'm.okafor', recorded_at: '2026-03-12 15:33', change_reason_code: 'NEW_LABEL' },
        { code: '1Y12RQ', display_name: 'One year, twelve rolls', description: 'One year of offset from spot, then twelve quarterly rolls. Twelve is more rolls than the schedule holds from here, so it cannot resolve.', sort_order: 108, kind: 'PERIOD', unit: 'YEAR', multiplier: 1, version: 1, modified_by: 'm.okafor', recorded_at: '2026-03-12 15:35', change_reason_code: 'NEW_LABEL' },
        { code: 'SPOT', display_name: 'Spot', description: 'The spot date itself, exposed as a label so a ladder can name its first rung.', sort_order: 5, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: 'TODAY', display_name: 'Today', description: 'The horizon date, unadjusted. A label with no duration of its own.', sort_order: 1, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' },
        { code: 'TOMORROW', display_name: 'Tomorrow', description: 'The business day after the horizon on the chosen calendar.', sort_order: 2, kind: 'SPECIAL', unit: 'NONE', multiplier: null, version: 1, modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14', change_reason_code: 'SEED' }
    ];

    function tenorBy(code) {
        return TENORS.filter(function (t) { return t.code === code; })[0] || null;
    }

    /* The versions the history step reads, newest first. */
    var VERSIONS = [
        {
            version: 2, action: 'updated', modified_by: 'j.smith', recorded_at: '2026-03-11 08:45',
            change_reason_code: 'DATA_FIX', change_commentary: 'Three months is a calendar quarter, not ninety days.',
            row: { code: '3M', display_name: 'Three month', description: 'The three-month pillar. It was carried as ninety days until it was corrected to three calendar months.', sort_order: 60, kind: 'PERIOD', unit: 'MONTH', multiplier: 3 }
        },
        {
            version: 1, action: 'created', modified_by: 'tenant_admin', recorded_at: '2026-02-02 09:14',
            change_reason_code: 'SEED', change_commentary: 'Seeded with the tenant.',
            row: { code: '3M', display_name: '', description: 'Three month pillar.', sort_order: 60, kind: 'PERIOD', unit: 'DAY', multiplier: 90 }
        }
    ];

    var TENOR_FIELDS = [
        ['code', 'Code'], ['display_name', 'Display Name'], ['description', 'Description'],
        ['sort_order', 'Sort Order'], ['kind', 'Kind'], ['unit', 'Unit'], ['multiplier', 'Multiplier']
    ];

    /* ------------------------------------------------------------- state */

    var STEPS = [
        { id: 'list', title: 'Tenors', lead: 'Every tenor code the tenant reads, and what each one resolves to.' },
        { id: 'tenor', title: 'The tenor', lead: 'Its fields, and the two halves of it: the code a person reads and the resolution the system computes.' },
        { id: 'convention', title: 'Its resolution', lead: 'A tenor does not resolve the same way under every convention. This is where the anchor, the algorithm and the offsets live.' },
        { id: 'schedule', title: 'The schedule', lead: 'A schedule axis expands into dated rolls: the rolls a swap needs, each one adjusted on a calendar.' },
        { id: 'resolve', title: 'The resolved dates', lead: 'One named calendar, end to end: the code, the anchor date, the roll rule and the date that comes out.' },
        { id: 'review', title: 'Review', lead: 'Nothing is written until you confirm.' },
        { id: 'outcome', title: 'The outcome', lead: '' }
    ];
    var OFF_RAIL = [
        { id: 'refused', title: 'The refusal', lead: 'A tenor that cannot resolve on the chosen calendar is refused, with the step that refused it.' },
        { id: 'history', title: 'History', lead: 'Every version of the row, and the field-level difference from the version before.' },
        { id: 'waiting', title: 'Waiting', lead: 'The request is raised and the tenor has not changed.' },
        { id: 'decide', title: 'Decide', lead: 'The checker reads the change and answers it.' },
        { id: 'declined', title: 'Declined', lead: 'The request was declined and the tenor has not changed.' }
    ];
    var ALL = STEPS.concat(OFF_RAIL);

    /* The refusals the refusal state can draw. ?fail=1 picks the first. */
    var FAILS = ['kind_mismatch', 'exhausted', 'membership', 'unadjusted'];

    var S = {
        at: 0,
        raisedBy: 'resolve',
        query: '',
        kind: '',
        tenor: '3M',
        convention: 'RATES_SPOT_FORWARD',
        cal: 'GB.LOIOB',
        horizon: new Date(Date.UTC(2026, 2, 16)),
        roll: 'Following',
        schedule: 'ROLL_QUARTER',
        rollcount: 12,
        rollstart: 'spot',
        fail: '',
        version: 2
    };

    /* ------------------------------------------------------- resolution */

    function anchorDate(anchor, res) {
        var ccy = CCY[res.base_ccy] || CCY.GBP;
        var hol = holidaySet(ccy.calendar);
        var spot = addBusinessDays(S.horizon, ccy.spot_days, hol);
        if (anchor === 'SPOT') return spot;
        if (anchor === 'TODAY') return S.horizon;
        if (anchor === 'TOMORROW') return adjust(shift(S.horizon, 1), 'Following', hol);
        if (anchor === 'NEAR_LEG') return spot;
        return null;
    }

    function tenorOffsetOf(t, row) {
        if (t.kind === 'SPECIAL') {
            if (row === null || row.offset_unit === null || row.offset_unit === undefined) return null;
            return { unit: row.offset_unit, multiplier: row.offset_multiplier };
        }
        return { unit: t.unit, multiplier: t.multiplier };
    }

    function addOffset(d, off) {
        if (off === null) return null;
        var n = off.multiplier || 0;
        var y = d.getUTCFullYear();
        var m = d.getUTCMonth();
        var day = d.getUTCDate();
        if (off.unit === 'DAY') return shift(d, n);
        if (off.unit === 'WEEK') return shift(d, n * 7);
        if (off.unit === 'MONTH') {
            var total = m + n;
            var ny = y + Math.floor(total / 12);
            var nm = ((total % 12) + 12) % 12;
            var last = new Date(Date.UTC(ny, nm + 1, 0)).getUTCDate();
            return new Date(Date.UTC(ny, nm, Math.min(day, last)));
        }
        return new Date(Date.UTC(y + n, m, day));
    }

    function scheduleWalk(kind, from, count, hol) {
        var dates = scheduleDateList(kind, hol);
        var picked = dates.filter(function (d) { return d.getTime() >= from.getTime(); });
        return { all: dates, picked: picked, exhausted: picked.length < count };
    }

    function resolve(cfg) {
        var t = cfg.tenor, res = cfg.convention, row = cfg.row;
        var fail = cfg.fail || '';
        var roll = cfg.roll || S.roll;
        if (fail === 'kind_mismatch' && t.code === '3M') {
            return {
                ok: false, step: 'the tenor row',
                message: 'The tenor 3M cannot resolve: its kind is PERIOD but its unit is NONE, so it has no fixed offset to walk.',
                detail: 'A PERIOD tenor needs a unit of DAY, WEEK, MONTH or YEAR and a multiplier. NONE belongs to a SPECIAL tenor only.',
                source: 'ores_refdata_validate_tenor_unit_fn, on the write of the tenor row'
            };
        }
        if (row === null || row === undefined) {
            return {
                ok: false, step: 'the convention membership',
                message: 'The tenor ' + t.code + ' does not belong to ' + res.code + ', so it has no resolution there.',
                detail: 'Membership is the presence of a (convention, tenor) row in the tenor convention resolutions. A missing row is a caller error.',
                source: 'resolve_end_date: std::invalid_argument'
            };
        }
        var hol = holidaySet(cfg.cal);
        var anchorName = row.anchor_override || res.measured_from;
        var anchor = anchorDate(anchorName, res);
        if (anchor === null) {
            return {
                ok: false, step: 'the anchor',
                message: 'The anchor ' + anchorName + ' is not a reference point this build resolves to a date.',
                detail: 'Only SPOT, TODAY and TOMORROW have a date rule. IMM_ROLL and NEAR_LEG come from a schedule or from the quote itself.',
                source: 'resolve_end_date: std::invalid_argument'
            };
        }
        var turns = [{ cap: 'Horizon', what: 'The as-of date every anchor is measured from', date: S.horizon, cls: 'done' }];
        turns.push({ cap: 'Anchor', what: anchorName + ', from the convention ' + res.code, date: anchor, cls: 'done' });
        var walkStart;
        if (res.resolution_algorithm === 'SCHEDULE_STEP') {
            var off = tenorOffsetOf(t, row);
            if (off === null || off.unit === null) {
                return {
                    ok: false, step: 'the schedule',
                    message: 'The tenor ' + t.code + ' has no duration under ' + res.code + ': its resolution row is missing its offset.',
                    detail: 'A SPECIAL tenor carries no unit of its own, so its resolution row must supply offset_unit and offset_multiplier.',
                    source: 'resolve_end_date: std::invalid_argument'
                };
            }
            walkStart = addOffset(anchor, off);
            turns.push({ cap: 'Calendar offset', what: '+' + off.multiplier + ' ' + off.unit.toLowerCase() + ' from the anchor', date: walkStart, cls: 'done' });
            var count = row.schedule_step_count || 0;
            var walk = scheduleWalk(row.schedule_code, walkStart, count, hol);
            if (walk.exhausted) {
                return {
                    ok: false, step: 'the schedule',
                    message: 'The schedule ' + row.schedule_code + ' has ' + walk.picked.length + ' dates on or after ' + fmt(walkStart) +
                        ', and the tenor ' + t.code + ' asks for ' + count + '.',
                    detail: 'The walk is anchor + offset + n steps. Fewer dates on or after the walk start than the step count is a configuration error, not a refused write.',
                    source: 'resolve_end_date: std::logic_error on schedule exhaustion'
                };
            }
            for (var i = 0; i < count; i += 1) {
                turns.push({
                    cap: 'Step ' + (i + 1) + ' of ' + count, date: walk.picked[i],
                    what: 'the ' + ordinal(i + 1) + ' ' + row.schedule_code + ' date on or after the walk start',
                    cls: 'step'
                });
            }
            var rawStep = walk.picked[count - 1];
            var adjStep = fail === 'unadjusted' ? rawStep : adjust(rawStep, roll, hol);
            var rolledStep = iso(adjStep) !== iso(rawStep);
            turns.push({ cap: 'Roll', what: roll + ' on ' + cfg.cal, date: adjStep, cls: rolledStep ? 'rolled' : 'done' });
            return { ok: true, anchor: anchorName, anchorDate: anchor, walkStart: walkStart, count: count, raw: rawStep, adj: adjStep, rolled: rolledStep, turns: turns, schedule: row.schedule_code, resolution: row };
        }
        var own = tenorOffsetOf(t, row);
        if (own === null || own.unit === null) {
            return {
                ok: false, step: 'the tenor row',
                message: 'The tenor ' + t.code + ' has no unit and multiplier of its own, and its resolution row supplies none.',
                detail: 'A SPECIAL tenor resolves only where the convention says how far it runs.',
                source: 'resolve_end_date: std::invalid_argument'
            };
        }
        var raw = addOffset(anchor, own);
        var adj = fail === 'unadjusted' ? raw : adjust(raw, roll, hol);
        var rolled = iso(adj) !== iso(raw);
        turns.push({ cap: 'Offset', what: '+' + own.multiplier + ' ' + own.unit.toLowerCase() + ' from the anchor', date: raw, cls: rolled ? 'rolled' : 'done' });
        turns.push({ cap: 'Roll', what: roll + ' on ' + cfg.cal, date: adj, cls: rolled ? 'rolled' : 'done' });
        return { ok: true, anchor: anchorName, anchorDate: anchor, walkStart: anchor, raw: raw, adj: adj, rolled: rolled, turns: turns, offset: own, resolution: row };
    }

    function ordinal(n) {
        var s = ['th', 'st', 'nd', 'rd'];
        var v = n % 100;
        return n + (s[(v - 20) % 10] || s[v] || s[0]);
    }

    function activeTenor() { return tenorBy(S.tenor) || TENORS[6]; }
    function activeConvention() { return CONVENTIONS[S.convention] || CONVENTIONS.RATES_SPOT_FORWARD; }
    function rowFor(conv, code) {
        return conv.resolutions.filter(function (r) { return r.tenor_code === code; })[0] || null;
    }
    function activeRow() {
        if (S.fail === 'membership') return null;
        return rowFor(activeConvention(), activeTenor().code);
    }

    function kindFail() { return S.fail === 'kind_mismatch'; }

    function resolutionNow() {
        var t = activeTenor();
        var conv = activeConvention();
        if (S.fail === 'exhausted') {
            return resolve({ tenor: tenorBy('1Y12RQ'), convention: CONVENTIONS.CREDIT_CDS_IMM, row: rowFor(CONVENTIONS.CREDIT_CDS_IMM, '1Y12RQ'), cal: S.cal, fail: S.fail });
        }
        var shown = kindFail() ? tenorBy('3M') : t;
        return resolve({ tenor: shown, convention: conv, row: rowFor(conv, shown.code), cal: S.cal, fail: S.fail });
    }

    /* The one line that says what a tenor means, for the duo's right half. */
    function fromText(result, conv) {
        if (!result.ok) return { text: 'Cannot resolve on ' + S.cal + '.', formula: result.step };
        var t = result.resolution;
        if (t.schedule_code !== null && t.schedule_code !== undefined) {
            var off = t.offset_multiplier + ' ' + t.offset_unit.toLowerCase();
            return {
                text: 'Anchor <b>' + result.anchor + '</b>, plus <b>' + off + '</b>, then <b>' + t.schedule_step_count + ' step' +
                    (t.schedule_step_count === 1 ? '' : 's') + '</b> along <b>' + t.schedule_code + '</b>.',
                formula: t.anchor_override || conv.measured_from
            };
        }
        var t2 = tenorBy(result.resolution.tenor_code);
        var own = result.offset;
        return {
            text: 'Anchor <b>' + result.anchor + '</b>, plus <b>' + own.multiplier + ' ' + own.unit.toLowerCase() + '</b>.',
            formula: t2.code + ' = ' + result.anchor + ' + ' + own.multiplier + own.unit.charAt(0)
        };
    }

    /* ------------------------------------------------------------- utils */

    function esc(v) {
        return String(v === null || v === undefined ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function q(v) {
        return encodeURIComponent(String(v));
    }

    function matches(t) {
        var text = S.query.trim().toLowerCase();
        if (S.kind !== '' && t.kind !== S.kind) return false;
        if (text === '') return true;
        return t.code.toLowerCase().indexOf(text) >= 0 ||
            t.display_name.toLowerCase().indexOf(text) >= 0 ||
            t.description.toLowerCase().indexOf(text) >= 0;
    }

    function duo(t, result, conv, extraClass) {
        var f = fromText(result, conv);
        return '<div class="duo ' + (extraClass || '') + '">' +
            '<div class="side"><div class="cap">The code a person reads</div>' +
            '<div class="code">' + esc(t.code) + '</div>' +
            '<div class="name">' + esc(t.display_name) + '</div></div>' +
            '<div class="main"><div class="cap">The resolution the system computes</div>' +
            '<p class="resolve">' + f.text + '</p>' +
            '<div class="formula">' + esc(conv.code) + ' \u00b7 ' + esc(conv.resolution_algorithm) + ' \u00b7 ' + esc(f.formula) + '</div>' +
            '</div></div>';
    }

    function gap(text) {
        return '<div class="gapnote"><b>Gap. </b>' + text + '</div>';
    }

    /* -------------------------------------------------------- the screens */

    function listStep() {
        var rows = TENORS.filter(matches).map(function (t) {
            var conv = activeConvention();
            var result = resolve({ tenor: t, convention: conv, row: rowFor(conv, t.code), cal: S.cal, fail: S.fail });
            var here = result.ok ? '<span class="mono">' + esc(iso(result.adj || result.raw)) + '</span>' : '<span class="faint">no</span>';
            var chosen = t.code === S.tenor;
            return '<tr class="pick' + (chosen ? ' on' : '') + '" data-act="pick-tenor" data-tenor="' + esc(t.code) + '">' +
                '<td class="mono nowrap">' + esc(t.code) + '</td>' +
                '<td>' + esc(t.display_name) +
                (kindFail() && t.code === '3M' ? '<span class="sub" style="color:var(--bad)">PERIOD with unit NONE</span>' : '') + '</td>' +
                '<td>' + esc(t.kind) + '</td>' +
                '<td>' + esc(t.unit) + (t.multiplier !== null ? ' \u00d7 ' + esc(t.multiplier) : '') + '</td>' +
                '<td class="num">' + esc(t.sort_order) + '</td>' +
                '<td class="nowrap">' + here + '</td>' +
                '<td class="nowrap">v' + esc(t.version) + '</td></tr>';
        }).join('');
        if (rows === '') rows = '<tr><td colspan="7" class="faint">No tenor matches.</td></tr>';
        return '<div class="field"><span class="lbl">Search</span>' +
            '<input data-focus="q" data-q="1" placeholder="3M, one month, or part of a description" value="' + esc(S.query) + '">' +
            '<div class="hint">The search reads the code, the display name and the description.</div></div>' +
            '<div class="chips"><span class="lbl" style="align-self:center;color:var(--ink-dim);font-size:12px;margin-right:4px">Kind</span>' +
            ['', 'PERIOD', 'SPECIAL'].map(function (k) {
                return '<button type="button" class="chip' + (S.kind === k ? ' on' : '') + '" data-act="pick-kind" data-kind="' + esc(k) + '">' +
                    (k === '' ? 'all' : k) + '</button>';
            }).join('') + '</div>' +
            '<div class="tablewrap"><table class="grid"><thead><tr>' +
            '<th>Code</th><th>Display Name</th><th>Kind</th><th>Unit</th><th style="text-align:right">Sort Order</th>' +
            '<th>Resolves to (' + esc(S.convention) + ')</th><th>Version</th></tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<div class="stepfoot" style="border-top:none;margin-top:0;padding-top:0">' +
            '<button type="button" class="btn" data-act="pick-tenor" data-tenor="' + esc(S.tenor) + '">Open the tenor</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="new-tenor">Add tenor</button></div>' +
            gap('The list shows a resolution column the server does not serve, because no operation resolves a tenor over the wire: the resolution lives in <span class="mono">resolve_end_date</span> in the domain API. See the note on the resolved-dates step.');
    }

    function tenorStep() {
        var t = kindFail() ? tenorBy('3M') : activeTenor();
        var conv = activeConvention();
        var result = resolutionNow();
        var tFields = TENOR_FIELDS.map(function (f) {
            var name = f[0], label = f[1];
            var value = t[name];
            if (name === 'unit' && kindFail()) value = 'NONE';
            var bad = kindFail() && name === 'unit';
            return '<div class="field"><span class="lbl">' + esc(label) + (bad ? ' <span style="color:var(--bad)">\u2014 refused</span>' : '') + '</span>' +
                '<input value="' + esc(value === null || value === undefined ? '\u2014' : value) + '"' +
                (bad ? ' style="border-color:var(--bad)"' : '') + '></div>';
        }).join('');
        return duo(t, result, conv) +
            '<h3>Fields on the tenor row</h3>' +
            '<p class="hint" style="margin:-4px 0 12px">These seven columns are the tenor model. The anchor and the algorithm are not here: they belong to the convention that resolves the tenor.</p>' +
            '<div class="grid2">' + tFields + '</div>' +
            '<h3>Where the anchor and the algorithm come from</h3>' +
            '<div class="reviewgrid">' +
            '<dt>Convention</dt><dd class="mono">' + esc(conv.code) + '</dd>' +
            '<dt>Measured from</dt><dd class="mono">' + esc(conv.measured_from) + (result.resolution && result.resolution.anchor_override ? ' \u2192 override ' + esc(result.resolution.anchor_override) : '') + '</dd>' +
            '<dt>Resolution algorithm</dt><dd class="mono">' + esc(conv.resolution_algorithm) + '</dd>' +
            '<dt>Resolution row</dt><dd>' + (result.resolution ? 'in the set under ' + esc(conv.code) : '<span style="color:var(--bad)">no row: not in the set</span>') + '</dd>' +
            '</div>' +
            '<h3>Write the row</h3>' +
            '<div class="field"><span class="lbl">Change reason</span><select data-focus="reason">' +
            ['DATA_FIX', 'NEW_LABEL', 'SEED', 'CORRECTION'].map(function (r) { return '<option>' + esc(r) + '</option>'; }).join('') +
            '</select><div class="hint">Offered by <span class="mono">dq.v1.change_reasons.list</span>. The write validates it with <span class="mono">ores_dq_validate_change_reason_fn</span>.</div></div>' +
            '<div class="field"><span class="lbl">Change commentary</span><input data-focus="commentary" placeholder="Why this row changes"></div>' +
            '<label class="failtoggle"><input type="checkbox" data-act="fail-kind"' + (kindFail() ? ' checked' : '') + '> Prototype: write 3M with kind PERIOD and unit NONE</label>' +
            gap('The write is <span class="mono">refdata.v1.tenors.put</span>, whose body carries code, display_name, description, sort_order, kind, unit and multiplier. It carries no change reason and no commentary, though the row is validated by <span class="mono">ores_dq_validate_change_reason_fn</span>. Those two fields are drawn on this screen because the journey needs them; the wire does not carry them.');
    }

    function conventionStep() {
        var conv = activeConvention();
        var codes = Object.keys(CONVENTIONS);
        var cards = codes.map(function (c) {
            var v = CONVENTIONS[c];
            return '<button type="button" class="convcard' + (c === conv.code ? ' on' : '') + '" data-act="pick-convention" data-convention="' + esc(c) + '">' +
                '<div class="code">' + esc(c) + '</div>' +
                '<p class="sum">' + esc(v.description) + '</p>' +
                '<div class="kv">Measured from <b>' + esc(v.measured_from) + '</b> \u00b7 Algorithm <b>' + esc(v.resolution_algorithm) + '</b> \u00b7 ' + v.resolutions.length + ' tenors in the set</div></button>';
        }).join('');

        var matrix = TENORS.map(function (t) {
            var row = rowFor(conv, t.code);
            var result = resolve({ tenor: t, convention: conv, row: row, cal: S.cal, fail: S.fail });
            var anchorCell = row === null ? '<span class="faint">\u2014</span>'
                : (row.anchor_override ? '<span class="mono">' + esc(row.anchor_override) + '</span> <span class="faint">(override)</span>'
                    : '<span class="mono faint">' + esc(conv.measured_from) + '</span>');
            var offCell = row === null ? '<span class="faint">\u2014</span>'
                : (row.schedule_code ? '<span class="mono">' + esc(row.offset_multiplier + ' ' + row.offset_unit) + ' + ' + row.schedule_step_count + ' \u00d7 ' + esc(row.schedule_code) + '</span>'
                    : '<span class="mono">' + esc((row.offset_multiplier === null ? (t.multiplier + ' ' + t.unit) : (row.offset_multiplier + ' ' + row.offset_unit))) + '</span>');
            var dateCell = result.ok ? '<span class="mono">' + esc(iso(result.adj || result.raw)) + '</span>' : '<span style="color:var(--bad)">no</span>';
            var chosen = t.code === S.tenor;
            return '<tr class="' + (row === null ? 'dim' : 'pick') + (chosen ? ' on' : '') + '"' + (row === null ? '' : ' data-act="pick-tenor" data-tenor="' + esc(t.code) + '"') + '>' +
                '<td class="mono nowrap">' + esc(t.code) + '</td>' +
                '<td>' + esc(t.kind) + '</td>' +
                '<td>' + (row === null ? '' : 'in the set') + '</td>' +
                '<td>' + anchorCell + '</td>' +
                '<td>' + offCell + '</td>' +
                '<td class="nowrap">' + dateCell + '</td></tr>';
        }).join('');

        return '<div class="convcards">' + cards + '</div>' +
            '<h3>The convention itself</h3>' +
            '<div class="grid2">' +
            '<div class="field"><span class="lbl">Code</span><input value="' + esc(conv.code) + '" readonly></div>' +
            '<div class="field"><span class="lbl">Base currency</span><input value="' + esc(conv.base_ccy) + '" readonly><div class="hint">Prototype field: the tenor model carries no currency, and spot days are the pair\'s.</div></div>' +
            '<div class="field span2"><span class="lbl">Description</span><input value="' + esc(conv.description) + '"></div>' +
            '<div class="field"><span class="lbl">Measured from</span><select data-focus="measured">' +
            Object.keys(ANCHORS).map(function (a) { return '<option' + (a === conv.measured_from ? ' selected' : '') + '>' + esc(a) + '</option>'; }).join('') +
            '</select><div class="hint">' + esc(ANCHORS[conv.measured_from] || '') + '</div></div>' +
            '<div class="field"><span class="lbl">Resolution algorithm</span><select data-focus="algorithm">' +
            Object.keys(ALGORITHMS).map(function (a) { return '<option' + (a === conv.resolution_algorithm ? ' selected' : '') + '>' + esc(a) + '</option>'; }).join('') +
            '</select><div class="hint">' + esc(ALGORITHMS[conv.resolution_algorithm] || '') + '</div></div>' +
            '</div>' +
            '<h3>Every tenor, and what is in this convention\'s set</h3>' +
            '<p class="hint" style="margin:-4px 0 12px">A dim row is a tenor with no (convention, tenor) row, so the convention does not resolve it. The two models disagree on the algorithm codes: <span class="mono">tenor_convention</span> uses <span class="mono">ANCHOR_OFFSET</span> and <span class="mono">SCHEDULE_STEP</span>, and the <span class="mono">tenor_resolution_algorithm</span> lookup still lists <span class="mono">IMM_ROLL</span> as its other example. This prototype uses the convention model\'s two.</p>' +
            '<div class="tablewrap"><table class="grid"><thead><tr>' +
            '<th>Tenor</th><th>Kind</th><th>In the set</th><th>Anchor</th><th>Offset</th><th>Resolved on ' + esc(S.cal) + '</th>' +
            '</tr></thead><tbody>' + matrix + '</tbody></table></div>' +
            gap('The set, the anchor override and the offsets are a junction the server serves read-only: <span class="mono">refdata.v1.tenor_convention_resolutions.list</span>, <span class="mono">.get</span> and <span class="mono">.list_by_convention_code</span>. There is no <span class="mono">.put</span> and no <span class="mono">.delete</span>, so this screen can show membership and cannot change it.');
    }

    function scheduleStep() {
        var conv = activeConvention();
        var t = activeTenor();
        var row = rowFor(conv, t.code);
        var chosenSchedule = row !== null && row.schedule_code ? row.schedule_code : S.schedule;
        var sch = SCHEDULES[chosenSchedule] || SCHEDULES.ROLL_QUARTER;
        var hol = holidaySet(S.cal);
        var anchor = anchorDate(row !== null ? (row.anchor_override || conv.measured_from) : conv.measured_from, conv);
        if (anchor === null) anchor = S.horizon;
        var off = row !== null ? tenorOffsetOf(t, row) : null;
        var walkStart = off === null ? anchor : addOffset(anchor, off);
        var dates = scheduleDateList(chosenSchedule, hol);
        var list = dates.filter(function (d) { return d.getTime() >= walkStart.getTime(); });

        var skip = S.rollstart === 'horizon' ? S.horizon : walkStart;
        var need = S.rollcount;
        var upcoming = dates.filter(function (d) { return d.getTime() >= skip.getTime(); });
        var ladder = upcoming.slice(0, need).map(function (d, i) {
            var adj = adjust(d, S.roll, hol);
            var rolled = iso(adj) !== iso(d);
            var here = row !== null && row.schedule_code === chosenSchedule && row.schedule_step_count === (i + 1) && iso(walkStart) === iso(skip);
            return '<tr class="' + (here ? 'here' : '') + '">' +
                '<td class="num">' + (i + 1) + '</td>' +
                '<td class="mono nowrap">' + esc(iso(d)) + '</td>' +
                '<td>' + (chosenSchedule === 'ROLL_QUARTER' ? 'IMM ' + esc(MONTH_NAMES[d.getUTCMonth()]) + ' roll' : 'central_bank_meeting, US.FOMC') + '</td>' +
                '<td class="mono nowrap">' + esc(fmt(adj)) + '</td>' +
                '<td>' + (rolled ? '<span style="color:var(--warn)">' + esc(S.roll) + ' on ' + esc(S.cal) + '</span>' : '<span class="faint">a business day as it stands</span>') + '</td></tr>';
        }).join('');
        if (ladder === '') ladder = '<tr><td colspan="5" class="faint">The schedule has no dates on or after ' + esc(iso(skip)) + '.</td></tr>';

        var steps = list.slice(0, row !== null && row.schedule_step_count ? row.schedule_step_count : 4).map(function (d, i) {
            return '<li class="turn step"><span class="mark">' + (i + 1) + '</span>' +
                '<span class="what">Step ' + (i + 1) + ' along <b>' + esc(chosenSchedule) + '</b>: the next date on or after the walk start</span>' +
                '<span class="date">' + esc(iso(d)) + '</span></li>';
        }).join('');

        return '<div class="duo tall"><div class="side"><div class="cap">Schedule axis</div>' +
            '<div class="code">' + esc(sch.code) + '</div><div class="name">' + esc(sch.name) + '</div></div>' +
            '<div class="main"><div class="cap">How it produces dates</div>' +
            '<p class="resolve">' + esc(sch.description) + '</p>' +
            '<div class="formula">schedule_source ' + esc(sch.schedule_source) +
            (sch.calendar_code ? ' \u00b7 calendar_code ' + esc(sch.calendar_code) : '') +
            (sch.diary_entry_type ? ' \u00b7 diary_entry_type ' + esc(sch.diary_entry_type) : '') + '</div></div></div>' +

            '<h3>The tenor walks this axis</h3>' +
            '<ul class="turns">' +
            '<li class="turn done"><span class="mark">\u25cf</span><span class="what">Anchor <b>' + esc(row !== null ? (row.anchor_override || conv.measured_from) : conv.measured_from) + '</b> under ' + esc(conv.code) + '</span><span class="date">' + esc(iso(anchor)) + '</span></li>' +
            '<li class="turn done"><span class="mark">\u25cf</span><span class="what">Calendar offset <b>' + (off === null ? '\u2014' : '+' + off.multiplier + ' ' + off.unit.toLowerCase()) + '</b></span><span class="date">' + esc(iso(walkStart)) + '</span></li>' +
            (steps === '' ? '<li class="turn"><span class="mark">\u25cb</span><span class="what">This tenor names no schedule; pick one below to expand it</span><span class="date">\u2014</span></li>' : steps) +
            '</ul>' +

            '<h3>The swap\'s rolls</h3>' +
            '<div class="pickrow">' +
            '<div class="field"><span class="lbl">Schedule axis</span><select data-focus="sched">' +
            Object.keys(SCHEDULES).map(function (k) { return '<option' + (k === chosenSchedule ? ' selected' : '') + '>' + esc(k) + '</option>'; }).join('') +
            '</select></div>' +
            '<div class="field"><span class="lbl">Rolls the swap needs</span><select data-focus="count">' +
            [4, 8, 12, 20].map(function (n) { return '<option' + (n === S.rollcount ? ' selected' : '') + '>' + n + '</option>'; }).join('') +
            '</select></div>' +
            '<div class="field"><span class="lbl">Roll rule</span><select data-focus="roll2">' +
            ROLL_RULES.map(function (r) { return '<option' + (r === S.roll ? ' selected' : '') + '>' + esc(r) + '</option>'; }).join('') +
            '</select></div>' +
            '<div class="field"><span class="lbl">Start the ladder at</span><select data-focus="start">' +
            ['spot', 'horizon'].map(function (k) { return '<option' + (k === S.rollstart ? ' selected' : '') + '>' + esc(k) + '</option>'; }).join('') +
            '</select></div>' +
            '</div>' +
            '<div class="tablewrap"><table class="grid"><thead><tr>' +
            '<th style="text-align:right">Roll</th><th>Schedule date</th><th>What the date is</th><th>Adjusted date</th><th>Adjustment</th>' +
            '</tr></thead><tbody>' + ladder + '</tbody></table></div>' +
            gap('A swap\'s rolls are not a refdata row. <span class="mono">tenor_schedule</span> names the axis and the dates it produces, and nothing stores a ladder: the rolls a swap needs are computed where the swap is built. This screen draws that expansion so a reviewer can see the axis working.');
    }

    function resolveStep() {
        var t = activeTenor();
        var conv = activeConvention();
        if (S.fail === 'exhausted') {
            var hard = tenorBy('1Y12RQ');
            var res = resolve({ tenor: hard, convention: CONVENTIONS.CREDIT_CDS_IMM, row: rowFor(CONVENTIONS.CREDIT_CDS_IMM, '1Y12RQ'), cal: S.cal, fail: S.fail });
            return duo(hard, res, CONVENTIONS.CREDIT_CDS_IMM) +
                '<div class="notice error"><b>Refused.</b> ' + esc(res.message) + '</div>' +
                '<p class="hint">' + esc(res.detail) + '</p>' +
                '<p class="hint mono">' + esc(res.source) + '</p>' +
                '<div class="stepfoot"><button type="button" class="btn ghost" data-act="go" data-go="schedule">Back to the schedule</button>' +
                '<button type="button" class="btn primary ml-auto" data-act="go" data-go="refused">Open the refusal</button></div>';
        }
        var result = kindFail()
            ? resolve({ tenor: tenorBy('3M'), convention: conv, row: rowFor(conv, '3M'), cal: S.cal, fail: S.fail })
            : resolve({ tenor: t, convention: conv, row: activeRow(), cal: S.cal, fail: S.fail });

        var panel = '<div class="pickrow">' +
            '<div class="field"><span class="lbl">Tenor</span><select data-focus="tenor3">' +
            TENORS.map(function (x) { return '<option' + (x.code === t.code ? ' selected' : '') + '>' + esc(x.code) + '</option>'; }).join('') +
            '</select></div>' +
            '<div class="field"><span class="lbl">Convention</span><select data-focus="conv3">' +
            Object.keys(CONVENTIONS).map(function (c) { return '<option' + (c === conv.code ? ' selected' : '') + '>' + esc(c) + '</option>'; }).join('') +
            '</select></div>' +
            '<div class="field"><span class="lbl">Calendar</span><select data-focus="cal3">' +
            Object.keys(CALENDARS).map(function (c) { return '<option' + (c === S.cal ? ' selected' : '') + '>' + esc(c) + '</option>'; }).join('') +
            '</select></div>' +
            '<div class="field"><span class="lbl">Roll rule</span><select data-focus="roll3">' +
            ROLL_RULES.map(function (r) { return '<option' + (r === S.roll ? ' selected' : '') + '>' + esc(r) + '</option>'; }).join('') +
            '</select></div>' +
            '</div>' +
            '<div class="field"><span class="lbl">Horizon (the as-of date)</span>' +
            '<input data-focus="horizon" data-horizon="1" value="' + esc(iso(S.horizon)) + '">' +
            '<div class="hint">Every anchor is measured from this date. The window around 16 March 2026 is a worked example.</div></div>';

        if (!result.ok) {
            return duo(t, result, conv) + panel +
                '<div class="notice error"><b>Refused at ' + esc(result.step) + '.</b> ' + esc(result.message) + '</div>' +
                '<p class="hint">' + esc(result.detail) + '</p>' +
                '<p class="hint mono">' + esc(result.source) + '</p>';
        }

        var turns = result.turns.map(function (x) {
            return '<li class="turn ' + esc(x.cls) + '"><span class="mark">' +
                (x.cls === 'rolled' ? '\u21bb' : x.cls === 'step' ? '\u2193' : '\u25cf') + '</span>' +
                '<span class="what"><b>' + esc(x.cap) + '</b> \u2014 ' + esc(x.what) + '</span>' +
                '<span class="date">' + esc(x.date instanceof Date ? fmt(x.date) : x.date) + '</span></li>';
        }).join('');

        var finalDate = result.adj || result.raw;
        return duo(t, result, conv) + panel +
            '<h3>End to end on ' + esc(S.cal) + '</h3>' +
            '<ul class="turns">' + turns +
            '<li class="turn final done"><span class="mark">\u2713</span>' +
            '<span class="what"><b>Resolved</b> \u2014 ' + esc(t.code) + ' under ' + esc(conv.code) +
            (result.rolled ? ', after the roll rule moved it' : '') + '</span>' +
            '<span class="date">' + esc(fmt(finalDate)) + '</span></li>' +
            '</ul>' +
            '<div class="reviewgrid">' +
            '<dt>Resolution algorithm</dt><dd class="mono">' + esc(conv.resolution_algorithm) + '</dd>' +
            '<dt>Anchor</dt><dd class="mono">' + esc(result.anchor) + ' = ' + esc(iso(result.anchorDate)) + '</dd>' +
            '<dt>Before the roll</dt><dd class="mono">' + esc(iso(result.raw)) + ' \u00b7 ' + esc(WEEKDAY_NAMES[result.raw.getUTCDay()]) + '</dd>' +
            '<dt>After the roll</dt><dd class="mono">' + esc(iso(result.adj)) + ' \u00b7 ' + esc(WEEKDAY_NAMES[result.adj.getUTCDay()]) + '</dd>' +
            '<dt>Calendar</dt><dd>' + esc(CALENDARS[S.cal].description) + '</dd>' +
            '<dt>Roll rule</dt><dd>' + esc(S.roll) + ' from <span class="mono">refdata.v1.business_day_convention_types.list</span></dd>' +
            '</div>' +
            gap('No operation resolves a tenor over the wire. <span class="mono">resolve_end_date</span> is a domain function in <span class="mono">ores.refdata.api/domain/tenor_resolution.hpp</span>: it takes the horizon, the spot date and the schedule dates, and it returns a date or throws. A screen that must show a resolved date needs an operation; this prototype draws what one would return. The roll rule has no home on <span class="mono">tenor_convention</span> either: <span class="mono">business_day_convention_type</span> is a lookup with no column pointing at a convention.');
    }

    function reviewStep() {
        var t = activeTenor();
        var conv = activeConvention();
        var result = resolutionNow();
        var rows = [
            ['Tenor', t.code + ' \u2014 ' + t.display_name],
            ['Kind and unit', t.kind + ' \u00b7 ' + (t.multiplier === null ? t.unit : t.multiplier + ' ' + t.unit)],
            ['Convention', conv.code + ' (v' + '1' + ')'],
            ['Measured from', conv.measured_from],
            ['Resolution algorithm', conv.resolution_algorithm],
            ['Anchor date', result.ok ? iso(result.anchorDate) : 'refused'],
            ['Resolved date', result.ok ? iso(result.adj || result.raw) : 'refused'],
            ['Calendar', S.cal],
            ['Roll rule', S.roll],
            ['Change reason', 'DATA_FIX'],
            ['Writes', 'refdata.v1.tenors.put, then refdata.v1.tenor_conventions.put']
        ];
        var dl = rows.map(function (r) { return '<dt>' + esc(r[0]) + '</dt><dd>' + esc(r[1]) + '</dd>'; }).join('');
        var check = result.ok
            ? '<div class="notice success"><b>This resolves.</b> ' + esc(t.code) + ' on ' + esc(S.cal) + ' gives ' + esc(iso(result.adj || result.raw)) + '.</div>'
            : '<div class="notice error"><b>This cannot resolve.</b> ' + esc(result.message) + ' The write is refused.</div>';
        return reviewGate() + check + '<dl class="reviewgrid">' + dl + '</dl>' +
            '<label class="failtoggle"><input type="checkbox" data-act="fail-kind"' + (kindFail() ? ' checked' : '') + '> Prototype: leave 3M as kind PERIOD with unit NONE</label>' +
            '<div class="stepfoot"><button type="button" class="btn ghost" data-act="go" data-go="convention">Back</button>' +
            '<button type="button" class="btn primary ml-auto"' + (result.ok ? '' : ' disabled') + ' data-act="write">' + esc(FE.primaryLabel(changesOf(), 'Write the tenor and the convention')) + '</button></div>';
    }

    function outcomeStep() {
        var t = activeTenor();
        var result = resolutionNow();
        return '<div class="notice success"><b>' + (FE.S.applied ? 'Approved and written.' : 'Written.') + '</b> ' + esc(t.code) + ' is at version ' + esc(t.version + 1) +
            ' and it resolves to ' + esc(result.ok ? iso(result.adj || result.raw) : '\u2014') + ' on ' + esc(S.cal) + '.</div>' +
            '<div class="reviewgrid">' +
            '<dt>Tenor</dt><dd class="mono">' + esc(t.code) + '</dd>' +
            '<dt>New version</dt><dd>v' + esc(t.version + 1) + '</dd>' +
            '<dt>Convention</dt><dd class="mono">' + esc(activeConvention().code) + '</dd>' +
            '<dt>Resolved</dt><dd class="mono">' + esc(result.ok ? iso(result.adj || result.raw) : '\u2014') + '</dd>' +
            '<dt>Reason</dt><dd>DATA_FIX</dd>' +
            '</div>' +
            '<div class="donegrid">' +
            '<button type="button" class="donecard" data-act="go" data-go="history"><span class="nm">See the history</span><p>The two versions of this row, with the field diff.</p></button>' +
            '<button type="button" class="donecard" data-act="go" data-go="list"><span class="nm">Back to the tenors</span><p>Every code the tenant reads, with what each resolves to.</p></button>' +
            '</div>' +
            gap('The reply is <span class="mono">put_tenor_response</span>, which carries the written tenor. No operation reads a resolved date back, so the resolved date on this screen is the one the prototype computed, not one the server confirmed.');
    }

    /* The refusal the Unadjusted roll rule produces: no thrown error, but a
     * resolved date the calendar says is not a business day. */
    function unadjustedRefusal(t, conv) {
        var un = resolve({ tenor: t, convention: conv, row: activeRow(), cal: S.cal, fail: 'unadjusted' });
        if (!un.ok) return un;
        var hol = holidaySet(S.cal);
        if (isBusinessDay(un.raw, hol)) {
            return {
                step: 'the roll rule',
                message: 'Nothing to refuse: ' + iso(un.raw) + ' is already a business day on ' + S.cal + '.',
                detail: 'Unadjusted refuses a date only when the resolved date falls on a weekend or a holiday of the chosen calendar. Move the horizon so the resolved date lands on one.',
                source: 'the business_day_convention_type vocabulary, applied after resolve_end_date returns'
            };
        }
        return {
            step: 'the roll rule',
            message: t.code + ' resolves to ' + iso(un.raw) + ', a ' + WEEKDAY_NAMES[un.raw.getUTCDay()] +
                ', which ' + S.cal + ' holds as ' + (un.raw.getUTCDay() === 0 || un.raw.getUTCDay() === 6 ? 'a weekend day' : 'a holiday') +
                '. Unadjusted leaves the date where it falls.',
            detail: 'resolve_end_date returns the end date. The business day convention adjusts it afterwards, and Unadjusted adjusts nothing. A date that is not a business day is a refusal here, not a silent acceptance.',
            source: 'the business_day_convention_type vocabulary, applied after resolve_end_date returns'
        };
    }

    /* The refusal, as its own state: what was refused, why, that the record did
     * not move, and the step that raised it. */
    function refusedStep() {
        var t = activeTenor();
        var conv = activeConvention();
        var bad = S.fail === 'exhausted'
            ? { tenor: tenorBy('1Y12RQ'), convention: CONVENTIONS.CREDIT_CDS_IMM, row: rowFor(CONVENTIONS.CREDIT_CDS_IMM, '1Y12RQ') }
            : { tenor: t, convention: conv, row: activeRow() };
        var message = S.fail === 'unadjusted' ? unadjustedRefusal(t, conv) : resolve({ tenor: bad.tenor, convention: bad.convention, row: bad.row, cal: S.cal, fail: S.fail });

        var writeTarget = S.fail === 'kind_mismatch'
            ? 'refdata.v1.tenors.put \u00b7 ' + t.code + ' is still version ' + t.version + ', and its unit still reads ' + t.unit + '.'
            : 'refdata.v1.tenor_conventions.put \u00b7 ' + conv.code + ' is still version 1, and its measured-from anchor still reads ' + conv.measured_from + '.';

        var unchanged = '<div class="notice warn"><b>The record did not move.</b> Nothing was written. ' +
            esc(writeTarget) + ' The next version is still the one this screen opened.</div>';

        /* The evidence the reviewer needs to check the refusal itself. */
        var evidence = '';
        if (S.fail === 'exhausted') {
            var row = bad.row;
            var hol = holidaySet(S.cal);
            var anchorName = row.anchor_override || bad.convention.measured_from;
            var eAnchor = anchorDate(anchorName, bad.convention) || S.horizon;
            var eStart = addOffset(eAnchor, tenorOffsetOf(bad.tenor, row));
            var eDates = scheduleDateList(row.schedule_code, hol);
            var ePicked = eDates.filter(function (d) { return d.getTime() >= eStart.getTime(); });
            var steps = ePicked.slice(0, row.schedule_step_count).map(function (d, i) {
                return '<tr><td class="num">' + (i + 1) + '</td><td class="mono nowrap">' + esc(iso(d)) + '</td>' +
                    '<td class="mono nowrap">' + esc(fmt(d)) + '</td><td>becomes step ' + (i + 1) + '</td></tr>';
            }).join('');
            evidence = '<h3>The walk the schedule cannot finish</h3>' +
                '<div class="reviewgrid" style="margin-bottom:14px">' +
                '<dt>Anchor</dt><dd class="mono">' + esc(anchorName) + ' = ' + esc(iso(eAnchor)) + '</dd>' +
                '<dt>Calendar offset</dt><dd class="mono">+' + esc(row.offset_multiplier + ' ' + row.offset_unit) + '</dd>' +
                '<dt>Walk start</dt><dd class="mono">' + esc(iso(eStart)) + '</dd>' +
                '<dt>Schedule axis</dt><dd class="mono">' + esc(row.schedule_code) + '</dd>' +
                '<dt>Dates on or after the walk start</dt><dd>' + ePicked.length + '</dd>' +
                '<dt>Steps the tenor asks for</dt><dd>' + esc(row.schedule_step_count) + '</dd>' +
                '</div>' +
                '<div class="tablewrap"><table class="grid"><thead><tr>' +
                '<th style="text-align:right">Step</th><th>' + esc(row.schedule_code) + ' date</th><th>Day</th><th>Used as</th>' +
                '</tr></thead><tbody>' + (steps === '' ? '<tr><td colspan="4" class="faint">The schedule holds no date on or after the walk start.</td></tr>' : steps) +
                '</tbody></table></div>' +
                '<p class="hint">Every ' + esc(row.schedule_code) + ' date on or after ' + esc(iso(eStart)) + ' is used, and the walk still runs out ' +
                ((row.schedule_step_count || 0) - ePicked.length) + ' step' + (((row.schedule_step_count || 0) - ePicked.length) === 1 ? '' : 's') + ' short. ' +
                'The fault is the row, not the calendar: shorten the offset, lower the step count, or extend the schedule.</p>';
        } else {
            var unadj = resolve({ tenor: t, convention: conv, row: activeRow(), cal: S.cal, fail: 'unadjusted' });
            if (unadj.ok && !isBusinessDay(unadj.raw, holidaySet(S.cal))) {                evidence = '<h3>The date the roll rule decides</h3>' +
                    '<div class="tablewrap"><table class="grid"><thead><tr><th>Roll rule</th><th>Resolved date</th><th>What it did</th></tr></thead><tbody>' +
                    ROLL_RULES.map(function (r) {
                        var res = resolve({ tenor: t, convention: conv, row: activeRow(), cal: S.cal, fail: r === 'Unadjusted' ? 'unadjusted' : '' });
                        if (!res.ok) return '';
                        var d = res.adj;
                        var what = iso(d) === iso(res.raw) ? (r === 'Unadjusted' ? 'left it where it fell' : 'the date was already a business day')
                            : esc(r) + ' moved it to ' + esc(WEEKDAY_NAMES[d.getUTCDay()]);
                        var sitsOnHoliday = r === 'Unadjusted' && !isBusinessDay(d, holidaySet(S.cal));
                        return '<tr' + (r === S.roll ? ' class="on"' : '') + '><td class="mono">' + esc(r) + '</td>' +
                            '<td class="mono nowrap">' + esc(iso(d)) + ' ' + esc(WEEKDAY_NAMES[d.getUTCDay()]) + '</td>' +
                            '<td' + (sitsOnHoliday ? ' style="color:var(--warn)"' : '') + '>' + what + '</td></tr>';
                    }).join('') +
                    '</tbody></table></div>';
            }
        }

        return '<div class="notice error"><b>Refused at ' + esc(message.step) + '.</b> ' + esc(message.message) + '</div>' +
            unchanged +
            '<h3>What the server checked</h3>' +
            '<p class="hint">' + esc(message.detail) + '</p>' +
            '<p class="hint mono">' + esc(message.source) + '</p>' +
            evidence +
            duo(bad.tenor, message, bad.convention) +
            gap('The refusal the server raises today is a thrown exception, not a message: <span class="mono">resolve_end_date</span> throws <span class="mono">std::invalid_argument</span> or <span class="mono">std::logic_error</span> and no operation carries either to a screen. A screen must show the step that refused, the dates it counted, and the row that owns the fault. This prototype draws that payload.');
    }

    function historyStep() {
        var t = activeTenor();
        var v = VERSIONS.filter(function (x) { return x.version === S.version; })[0] || VERSIONS[0];
        var prev = VERSIONS.filter(function (x) { return x.version === v.version - 1; })[0] || null;
        var diffs = [];
        TENOR_FIELDS.forEach(function (f) {
            var name = f[0], label = f[1];
            var before = prev === null ? undefined : prev.row[name];
            var after = v.row[name];
            var b = before === undefined || before === null || before === '' ? null : String(before);
            var a = after === null || after === undefined || after === '' ? null : String(after);
            if (b === a) return;
            var kind = b === null ? 'added' : a === null ? 'removed' : 'changed';
            diffs.push('<tr class="diffrow"><td>' + esc(label) + '</td>' +
                '<td class="' + (b === null ? 'nul' : 'del') + '">' + esc(b === null ? '\u2014' : b) + '</td>' +
                '<td class="' + (a === null ? 'nul' : 'add') + '">' + esc(a === null ? '\u2014' : a) + '</td>' +
                '<td class="' + (kind === 'added' ? 'add' : kind === 'removed' ? 'del' : 'chg') + '">' + esc(kind) + '</td></tr>');
        });
        if (diffs.length === 0) diffs.push('<tr><td colspan="4" class="faint">This is the first version: nothing to compare it to.</td></tr>');

        var versions = VERSIONS.map(function (x) {
            return '<tr class="pick' + (x.version === v.version ? ' on' : '') + '" data-act="pick-version" data-version="' + esc(x.version) + '">' +
                '<td class="mono">v' + esc(x.version) + '</td>' +
                '<td>' + esc(x.action) + '</td>' +
                '<td class="mono">' + esc(x.modified_by) + '</td>' +
                '<td class="nowrap">' + esc(x.recorded_at) + '</td>' +
                '<td class="mono">' + esc(x.change_reason_code) + '</td>' +
                '<td>' + esc(x.change_commentary) + '</td></tr>';
        }).join('');

        return '<div class="versionline"><span class="lbl" style="color:var(--ink-dim);font-size:12px">Version shown</span>' +
            VERSIONS.map(function (x) {
                return '<button type="button" class="chip' + (x.version === v.version ? ' on' : '') + '" data-act="pick-version" data-version="' + esc(x.version) + '">v' + esc(x.version) + '</button>';
            }).join('') + '</div>' +
            '<div class="tablewrap"><table class="grid"><thead><tr>' +
            '<th>Version</th><th>Action</th><th>Modified By</th><th>Recorded At</th><th>Change Reason</th><th>Commentary</th>' +
            '</tr></thead><tbody>' + versions + '</tbody></table></div>' +
            '<h3>What changed in v' + esc(v.version) + '</h3>' +
            '<div class="tablewrap"><table class="grid"><thead><tr>' +
            '<th>Field</th><th>Version before</th><th>This version</th><th>Kind</th>' +
            '</tr></thead><tbody>' + diffs.join('') + '</tbody></table></div>' +
            '<h3>The row this version writes</h3>' +
            '<div class="reviewgrid">' +
            '<dt>Code</dt><dd class="mono">' + esc(v.row.code) + '</dd>' +
            '<dt>Display name</dt><dd>' + esc(v.row.display_name || '\u2014') + '</dd>' +
            '<dt>Sort order</dt><dd>' + esc(v.row.sort_order) + '</dd>' +
            '<dt>Kind</dt><dd>' + esc(v.row.kind) + '</dd>' +
            '<dt>Unit</dt><dd>' + esc(v.row.unit) + '</dd>' +
            '<dt>Multiplier</dt><dd>' + esc(v.row.multiplier) + '</dd>' +
            '</div>' +
            '<div class="stepfoot"><button type="button" class="btn ghost" data-act="go" data-go="tenor">Back to the tenor</button>' +
            '<button type="button" class="btn ml-auto" data-act="revert">Revert to this version</button></div>' +
            gap('History is read with <span class="mono">refdata.v1.history.get</span> for the entity <span class="mono">ores.refdata.tenor</span>: it serves every version\'s field render and the field-level difference from the version before. The per-entity <span class="mono">refdata.v1.tenors_versions.list</span> and <span class="mono">.get</span> also exist, and return the version rows without a diff. A revert is another <span class="mono">put</span>: history is never rewritten.');
    }

    /* ---------------------------------------------------------- the shell */

    function rail(at) {
        return '<nav class="railnav" aria-label="Journey steps"><ol>' +
            ALL.map(function (s, i) {
                var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
                return '<li class="railentry ' + cls + '"' + (i === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (i < at ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(s.title) + '</li>';
            }).join('') + '</ol></nav>';
    }

    /* The changes the write would make. A tenor and a convention decide the
       date a trade, a curve or a report reads, so Market Risk decides both, from
       the book controls note. */
    function changesOf() {
        var t = activeTenor();
        var conv = activeConvention();
        return [
            { what: 'Write the tenor ' + t.code, who: t.display_name, from: null, to: null, decider: 'Market Risk' },
            { what: 'Write the convention ' + conv.code, who: conv.description || conv.code, from: null, to: null, decider: 'Market Risk' }
        ];
    }

    var FE = FourEyes.create({
        getChanges: changesOf,
        subject: function () { return activeTenor().code; },
        checks: function (actor, changes, check) {
            var result = resolutionNow();
            var ok = result.ok && check !== 'resolve';
            return [{ ok: ok, label: 'The tenor resolves on the chosen calendar',
                detail: ok ? activeTenor().code + ' resolves to ' + iso(result.adj || result.raw) + ' on ' + S.cal + '.' : 'The tenor cannot resolve on ' + S.cal + '.' }];
        }
    });

    var FE_IDS = ['waiting', 'decide', 'declined'];

    function reviewGate() {
        var changes = changesOf();
        return FE.reviewNotice(changes) +
            '<ul class="fe-changes" style="margin-bottom:16px">' + changes.map(function (c) {
                return '<li><div class="fe-what">' + esc(c.what) + FE.badge(c) + '</div><div class="fe-who">' + esc(c.who) + '</div></li>';
            }).join('') + '</ul>';
    }

    function stepBody(step) {
        if (FE_IDS.indexOf(step.id) >= 0) return FE.html(step.id);
        if (step.id === 'list') return listStep();
        if (step.id === 'tenor') return tenorStep();
        if (step.id === 'convention') return conventionStep();
        if (step.id === 'schedule') return scheduleStep();
        if (step.id === 'resolve') return resolveStep();
        if (step.id === 'review') return reviewStep();
        if (step.id === 'outcome') return outcomeStep();
        if (step.id === 'refused') return refusedStep();
        return historyStep();
    }

    function stepHeader() {
        var t = activeTenor();
        return '<div class="stepheader"><div>' +
            '<div class="nm">Tenant Acme Capital \u00b7 tenant_admin</div>' +
            '<div class="sub">Refdata \u203a Tenors \u00b7 ' + esc(t.code) + ' under ' + esc(activeConvention().code) +
            ' \u00b7 horizon ' + esc(iso(S.horizon)) + ' \u00b7 ' + esc(S.cal) + ' \u00b7 ' + esc(S.roll) + '</div>' +
            '</div></div>';
    }

    function foot(step) {
        var i = ALL.indexOf(step);
        var prev = i > 0 ? ALL[i - 1] : null;
        var next = i >= 0 && i < ALL.length - 1 ? ALL[i + 1] : null;
        if (FE_IDS.indexOf(step.id) >= 0) return '';
        if (step.id === 'refused') {
            var reason = S.raisedBy === 'tenor' ? 'Back to the tenor' : 'Back to the resolved dates';
            return '<div class="stepfoot">' +
                '<button type="button" class="btn ghost" data-act="go" data-go="' + esc(S.raisedBy) + '">' + esc(reason) + '</button>' +
                '<button type="button" class="btn ml-auto" data-act="next-fail">Show another refusal</button></div>';
        }
        if (step.id === 'review' || step.id === 'history' || step.id === 'list') return '';
        var back = step.id === 'outcome' ? 'review' : (prev ? prev.id : 'list');
        var backLabel = step.id === 'outcome' ? 'Back to review' : (prev ? 'Back' : '');
        return '<div class="stepfoot">' +
            (backLabel === '' ? '' : '<button type="button" class="btn ghost" data-act="go" data-go="' + esc(back) + '">' + esc(backLabel) + '</button>') +
            (next === null ? '' : '<button type="button" class="btn primary ml-auto" data-act="go" data-go="' + esc(next.id) + '">' + esc(next.title) + '</button>') +
            '</div>';
    }

    function render() {
        var step = ALL[S.at];
        var lead = step.id === 'outcome'
            ? activeTenor().code + ' is written, and it resolves on ' + S.cal + '.'
            : step.lead;
        document.getElementById('app').innerHTML =
            '<div class="page"><h1>Define how tenors resolve</h1>' +
            '<div class="journey">' + rail(S.at) +
            '<section class="card">' + stepHeader() +
            '<h2>' + esc(step.title) + '</h2>' +
            (lead === '' ? '' : '<p class="lead">' + esc(lead) + '</p>') +
            stepBody(step) + foot(step) + '</section></div></div>';
        renderNote();
        renderBar();
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey Define how tenors resolve \u00b7 ' +
            S.tenor + ' under ' + S.convention + ' \u00b7 ' + S.cal + ' \u00b7 ' + S.roll +
            ' \u00b7 state ' + ALL[S.at].id + (S.fail === '' ? '' : ' \u00b7 fail ' + S.fail);
    }

    function renderBar() {
        var steps = STEPS.map(function (s) {
            var i = ALL.indexOf(s);
            return '<button data-act="go" data-go="' + s.id + '"' + (S.at === i ? ' class="on"' : '') + '>' + esc(s.id) + '</button>';
        }).join('');
        var extras = OFF_RAIL.map(function (s) {
            var i = ALL.indexOf(s);
            return '<button data-act="go" data-go="' + s.id + '"' + (S.at === i ? ' class="on"' : '') + '>' + esc(s.id) + '</button>';
        }).join('');
        var fails = [''].concat(FAILS).map(function (f) {
            return '<button data-act="fail" data-fail="' + esc(f) + '"' + (S.fail === f ? ' class="on"' : '') + '>' +
                (f === '' ? 'none' : f) + '</button>';
        }).join('');
        var convs = Object.keys(CONVENTIONS).map(function (c) {
            return '<button data-act="pick-convention" data-convention="' + esc(c) + '"' + (S.convention === c ? ' class="on"' : '') + '>' + esc(c) + '</button>';
        }).join('');
        var cals = Object.keys(CALENDARS).map(function (c) {
            return '<button data-act="pick-cal" data-cal="' + esc(c) + '"' + (S.cal === c ? ' class="on"' : '') + '>' + esc(c) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + steps + '<span class="sep">|</span>' + extras +
            '<span class="sep">|</span><span class="label">fail</span>' + fails +
            '<span class="sep">|</span><span class="label">convention</span>' + convs +
            '<span class="sep">|</span><span class="label">calendar</span>' + cals;
    }

    /* --------------------------------------------------------- behaviour */

    function goTo(id) {
        if (id === 'failure') id = 'refused';
        var i = ALL.map(function (s) { return s.id; }).indexOf(id);
        if (i < 0) return;
        if (id === 'refused') armFail();
        S.at = i;
    }

    function rerender() {
        var ae = document.activeElement;
        var key = ae && ae.getAttribute ? ae.getAttribute('data-focus') : null;
        var start = ae && typeof ae.selectionStart === 'number' ? ae.selectionStart : null;
        var end = ae && typeof ae.selectionEnd === 'number' ? ae.selectionEnd : null;
        render();
        if (key) {
            var el = document.querySelector('[data-focus="' + key + '"]');
            if (el) {
                el.focus();
                if (start !== null && el.setSelectionRange) {
                    try { el.setSelectionRange(start, end); } catch (e) { /* not a text input */ }
                }
            }
        }
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        FE.params(p);
        var state = p.get('state') || p.get('step');
        if (state) goTo(state);
        if (p.get('tenor') !== null && tenorBy(p.get('tenor')) !== null) S.tenor = p.get('tenor');
        if (p.get('query') !== null) S.query = p.get('query');
        if (p.get('kind') !== null) S.kind = p.get('kind');
        if (p.get('convention') !== null && CONVENTIONS[p.get('convention')] !== undefined) S.convention = p.get('convention');
        if (p.get('cal') !== null && CALENDARS[p.get('cal')] !== undefined) S.cal = p.get('cal');
        if (p.get('horizon') !== null) {
            var h = parseIso(p.get('horizon'));
            if (h !== null) S.horizon = h;
        }
        if (p.get('roll') !== null && ROLL_RULES.indexOf(p.get('roll')) >= 0) S.roll = p.get('roll');
        if (p.get('schedule') !== null && SCHEDULES[p.get('schedule')] !== undefined) S.schedule = p.get('schedule');
        if (p.get('rollcount') !== null && +p.get('rollcount') > 0) S.rollcount = +p.get('rollcount');
        if (p.get('rollstart') !== null) S.rollstart = p.get('rollstart');
        if (p.get('fail') !== null) {
            var f = p.get('fail');
            /* ?fail=1 picks the first refusal kind. */
            S.fail = f === '1' ? FAILS[0] : (FAILS.indexOf(f) >= 0 ? f : '');
        }
        if (p.get('version') !== null) S.version = +p.get('version');
    }

    function clearFail() {
        goTo(S.raisedBy);
    }

    /* Arm the refusal: it belongs to the step that raised it. */
    function armFail() {
        if (S.fail === '') S.fail = FAILS[0];
        var walk = ['tenor', 'resolve', 'review'];
        var at = walk.indexOf(ALL[S.at].id);
        S.raisedBy = at >= 0 ? ALL[S.at].id : 'resolve';
    }

    document.addEventListener('change', function (ev) {
        var el = ev.target;
        if (!el || !el.getAttribute) return;
        var key = el.getAttribute('data-focus');
        if (key === 'sched') S.schedule = el.value;
        else if (key === 'count') S.rollcount = +el.value;
        else if (key === 'roll2') S.roll = el.value;
        else if (key === 'roll3') S.roll = el.value;
        else if (key === 'start') S.rollstart = el.value;
        else if (key === 'cal3') S.cal = el.value;
        else if (key === 'conv3') S.convention = el.value;
        else if (key === 'tenor3') S.tenor = el.value;
        else if (key === 'measured') { /* the picker shows the model field */ }
        else return;
        rerender();
    });

    document.addEventListener('input', function (ev) {
        var el = ev.target;
        if (!el || !el.getAttribute) return;
        if (el.getAttribute('data-q') !== null) {
            S.query = el.value;
            rerender();
            return;
        }
        if (el.getAttribute('data-horizon') !== null) {
            var h = parseIso(el.value);
            if (h !== null) {
                S.horizon = h;
                rerender();
            }
            return;
        }
        if (el.getAttribute('data-act') === 'fail-kind') {
            S.fail = el.checked ? 'kind_mismatch' : '';
            rerender();
        }
    });

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'go') {
            ev.preventDefault();
            goTo(el.getAttribute('data-go'));
        } else if (act === 'pick-tenor') {
            ev.preventDefault();
            var code = el.getAttribute('data-tenor');
            if (tenorBy(code) !== null) S.tenor = code;
            goTo('tenor');
        } else if (act === 'pick-convention') {
            ev.preventDefault();
            var c = el.getAttribute('data-convention');
            if (CONVENTIONS[c] !== undefined) S.convention = c;
        } else if (act === 'pick-cal') {
            ev.preventDefault();
            var cal = el.getAttribute('data-cal');
            if (CALENDARS[cal] !== undefined) S.cal = cal;
        } else if (act === 'pick-kind') {
            ev.preventDefault();
            S.kind = el.getAttribute('data-kind');
        } else if (act === 'pick-version') {
            ev.preventDefault();
            S.version = +el.getAttribute('data-version');
        } else if (act === 'fail') {
            ev.preventDefault();
            S.fail = el.getAttribute('data-fail');
            if (S.fail !== '') goTo('refused');
            else if (ALL[S.at].id === 'refused') goTo(S.raisedBy);
        } else if (act === 'next-fail') {
            ev.preventDefault();
            var at = FAILS.indexOf(S.fail);
            S.fail = FAILS[(at + 1) % FAILS.length];
        } else if (act === 'fail-kind') {
            return;
        } else if (act === 'clear-fail') {
            ev.preventDefault();
            clearFail();
        } else if (act === 'write') {
            ev.preventDefault();
            var changes = changesOf();
            if (FE.gated(changes).length > 0) {
                FE.raise(changes);
                goTo('waiting');
            } else {
                FE.reset();
                goTo('outcome');
            }
        } else if (act === 'new-tenor') {
            ev.preventDefault();
            S.tenor = 'O/N';
            goTo('tenor');
        } else if (act === 'revert') {
            ev.preventDefault();
            goTo('outcome');
        } else {
            return;
        }
        rerender();
    });

    readParams();
    if (ALL[S.at].id === 'refused') armFail();
    render();

    /* The four-eyes screens: their own controls, not the journey's. */
    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-fe]');
        if (!el) return;
        ev.preventDefault();
        var to = FE.click(el);
        if (to === null || to === 'stay') return;
        goTo(to);
        rerender();
    });

    function onFeInput(ev) {
        var el = ev.target;
        if (el && el.getAttribute && FE.input(el)) rerender();
    }

    document.addEventListener('input', onFeInput);
    document.addEventListener('change', onFeInput);
})();

/* Change what a report runs. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_change_what_a_report_runs.org.
 *
 * The journey's question is how a person changes the set of analytics a report
 * runs without losing what the change costs. All three variants carry the same
 * fixture and the same write, and disagree only about structure: A a switchboard
 * of every analytic, B the current set beside the proposed one, C a question
 * about the missing measure answered in the context of the runs it strands.
 *
 * Five facts the copy never hides:
 *
 *   - An analytic is turned off by setting `active` to false, never by deleting
 *     the row, so the history shows it was turned off.
 *   - pfe reads a cube that simulation writes, so pfe on without simulation
 *     cannot work. No service refuses it: the rule lives in the ACME seeder, so
 *     this page refuses the change itself and says the server would not.
 *   - A configuration is shared, so the change reaches every definition bound to
 *     it, not only the one the person opened.
 *   - A run keeps the configuration it ran with.
 *   - A run made after the change is not comparable with one made before it, and
 *     nothing says which configuration version a run used, so the run placement
 *     here is an inference from start times. */

(function () {
    'use strict';

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };

    var DEFINITION = {
        id: 'rpt-8f31a0c4',
        name: 'Group Consolidated Exposure',
        reportType: 'risk'
    };

    /* The five configuration types a definition binds. The fifth is where the
       analytics live, and it is shared: three other definitions bind it too. */
    var BINDINGS = [
        { type: 'conventions', name: 'ORE conventions 2026' },
        { type: 'pricing_engines', name: 'ACME pricing engines' },
        { type: 'todays_market', name: 'London close 2026-10-16' },
        { type: 'curve_configuration', name: 'ACME curve set' },
        { type: 'risk_report_config', name: 'Group risk configuration', shared: true }
    ];

    var SHARED_WITH = ['Board Risk Dashboard', 'Group DV01 by book', 'Treasury Sensitivity'];

    var CONFIGURATION = { id: 'cfg-2d61f0a8', name: 'Group risk configuration' };

    /* The fourteen analytic types, in the order the page draws them. */
    var ANALYTIC_TYPES = [
        { code: 'npv', name: 'Net present value' },
        { code: 'sensitivity', name: 'Sensitivity' },
        { code: 'cashflow', name: 'Cashflows' },
        { code: 'curves', name: 'Curves' },
        { code: 'simulation', name: 'Simulation' },
        { code: 'saccr', name: 'SA-CCR' },
        { code: 'parametricVar', name: 'Parametric VaR' },
        { code: 'portfolioDetails', name: 'Portfolio details' },
        { code: 'creditcurves', name: 'Credit curves' },
        { code: 'pfe', name: 'Potential future exposure' },
        { code: 'calibration', name: 'Calibration' },
        { code: 'pnlExplain', name: 'P&L explain' },
        { code: 'historicalSimulationVar', name: 'Historical simulation VaR' },
        { code: 'xva', name: 'XVA' }
    ];

    /* The configuration as stored today. Eight types carry a row; the other six
       carry none, so "on the configuration" and "on" are two different things. */
    var ROWS = [
        { code: 'npv', active: 'Y', order: 1, params: [] },
        {
            code: 'sensitivity', active: 'Y', order: 2, params: [
                { name: 'shift_size', value: '0.0001' },
                { name: 'shift_type', value: 'Absolute' }
            ]
        },
        { code: 'cashflow', active: 'N', order: 3, params: [] },
        { code: 'portfolioDetails', active: 'Y', order: 4, params: [] },
        {
            code: 'simulation', active: 'Y', order: 5, params: [
                { name: 'time_grid', value: '1D' }
            ]
        },
        {
            code: 'pfe', active: 'N', order: 6, params: [
                { name: 'horizon', value: '10' },
                { name: 'quantile', value: '0.95' }
            ]
        },
        { code: 'saccr', active: 'N', order: 7, params: [] },
        { code: 'calibration', active: 'N', order: 8, params: [] }
    ];

    /* What B opens with, so the difference is on the page rather than one click
       away: one analytic turned on, one turned off, one parameter changed. */
    var SEED = {
        pnlExplain: { code: 'pnlExplain', active: 'Y', order: 9, params: [] },
        portfolioDetails: { code: 'portfolioDetails', active: 'N', order: 4, params: [] },
        sensitivity: {
            code: 'sensitivity', active: 'Y', order: 2, params: [
                { name: 'shift_size', value: '0.00005' },
                { name: 'shift_type', value: 'Absolute' }
            ]
        }
    };

    var VERSIONS = [
        { version: 6, at: '2026-09-30 09:12', note: 'the analytics as they are stored now' },
        { version: 5, at: '2026-08-14 16:40', note: 'the schedule change' },
        { version: 4, at: '2026-07-02 11:05', note: 'the first official release' }
    ];

    var NEXT_VERSION = 7;

    var RUNS = [
        { id: 'run 41c9f0a7', at: '2026-10-16 06:04', state: 'completed', errors: 0 },
        { id: 'run 5b02e6d3', at: '2026-10-15 06:03', state: 'completed', errors: 2 },
        { id: 'run 9d13b7e0', at: '2026-10-14 06:04', state: 'completed', errors: 0 }
    ];

    var VARIANTS = [
        {
            id: 'A',
            name: 'Switchboard',
            gist: 'Every analytic is a row with an on/off control, and the parameters sit inline under the ones that have them, so the whole set is visible and a toggle is one click.'
        },
        {
            id: 'B',
            name: 'Before and after',
            gist: 'The stored set sits beside the proposed one, the difference is stated as added, removed and changed, and nothing is written until the person reviews it.'
        },
        {
            id: 'C',
            name: 'A question first',
            gist: 'The screen asks what measure is missing, narrows the analytic types that would produce it, then shows the change in the context of the runs it strands.'
        }
    ];

    var MEASURES = [
        { id: 'pfe', q: 'Potential future exposure', codes: ['pfe'], needs: ['simulation'] },
        { id: 'pnl', q: 'P&L explain', codes: ['pnlExplain'], needs: [] },
        { id: 'var', q: 'A VaR I can explain to the board', codes: ['parametricVar', 'historicalSimulationVar'], needs: ['curves'] },
        { id: 'saccr', q: 'SA-CCR', codes: ['saccr'], needs: [] },
        { id: 'xva', q: 'XVA', codes: ['xva'], needs: ['simulation', 'curves'] }
    ];

    /* The five statements the page repeats wherever they are needed. */
    var NO_DELETE = 'An analytic is turned off by setting active to false, not by deleting the row. ' +
        'The row keeps its display_order and its parameters and reads as off, so the history shows the ' +
        'report used to produce it. No control on this page removes a row.';

    var PFE_RULE = 'pfe reads a cube that simulation writes, so turning pfe on without simulation cannot ' +
        'work. The rule lives in the ACME seeder, in generate_risk_config_data.py, function ' +
        'check_exposure_needs_simulation, and in no service. Nothing validates it when a configuration is ' +
        'written, so the server would accept this set. This page refuses it itself, and a build would have ' +
        'to do the same or state the failure it cannot prevent.';

    var PAST_RUNS = 'A run keeps the configuration it ran with. Its outputs, its errors and its figures ' +
        'are that run\u2019s own record, and they do not change when this configuration changes. The runs ' +
        'already produced stay listed.';

    var INCOMPARABLE = 'A run made after this change is not comparable with one made before it. The two ' +
        'runs did not produce the same measures, so the difference between them would be a difference of ' +
        'configuration and not a difference of the book. The results screen compares a run with a previous ' +
        'run of the same definition and marks the comparison partial where a side carried errors; it cannot ' +
        'mark this one, because no operation says which configuration version a run used. This page places ' +
        'a run by its own start time against the definition\u2019s version history, and that is an ' +
        'inference, not a fact.';

    var SHARED = 'A configuration is shared. Three other definitions bind Group risk configuration, so the ' +
        'next run of each of them produces the new set too, and none of them was opened here.';

    var GAPS = [
        {
            title: 'Nothing validates the pfe rule',
            body: 'pfe needs simulation, and that rule lives in the ACME seeder and nowhere else. reporting.v1.configuration_analytics.put_many accepts pfe without simulation, so the failure arrives in the run and not in the write. This page refuses the pair; a build cannot rely on the server doing it.'
        },
        {
            title: 'No operation says which runs used which configuration',
            body: 'A run keeps the configuration it ran with, and nothing reads that configuration back with the run. The page can only compare a run\u2019s start time with the definition\u2019s version history, which is a history of a different row: the analytics live on the configuration. "This run predates my change" is an inference here and nowhere a fact.'
        },
        {
            title: 'No read of a configuration\u2019s version history',
            body: 'A configuration carries a version, and no operation in this journey\u2019s set returns the versions of one configuration, or the version a definition bound at a time. report_definitions_versions reads the definition, and the analytics are not on the definition.'
        },
        {
            title: 'The comparison is marked partial for errors, not for a changed set',
            body: 'The results screen marks a comparison partial where a side carried errors. It cannot mark it invalid because the analytics changed, because nothing tells it that they did. A person reading across this change reads two different sets of measures side by side and sees the difference as a change in the figures.'
        },
        {
            title: 'No route and no screen',
            body: 'No BFF route serves reporting and the application has no reporting UI. The analytics panel, the review and the run placement are new work, and this page is the first drawing of them.'
        },
        {
            title: 'The allowed values of a parameter are not readable here',
            body: 'A parameter row is a parameter_name and a value. The kind of value comes from the definition behind it, and no operation in this journey\u2019s set returns that definition\u2019s value domain, so the page shows a value as stored and cannot say whether the analytic accepts it.'
        }
    ];

    var S = {
        variant: 'A',
        rows: null,
        drafts: { A: {}, B: {}, C: {} },
        staged: { A: false, B: false, C: false },
        confirm: null,
        refusal: null,
        wish: null,
        saved: false,
        readback: null,
        showState: false,
        activity: 'rest',
        log: []
    };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function clone(value) {
        return JSON.parse(JSON.stringify(value));
    }

    function typeOf(code) {
        var found = ANALYTIC_TYPES.filter(function (entry) { return entry.code === code; });
        return found.length === 1 ? found[0] : { code: code, name: code };
    }

    function variant() {
        var found = VARIANTS.filter(function (entry) { return entry.id === S.variant; });
        return found.length === 1 ? found[0] : VARIANTS[0];
    }

    function draft() {
        return S.drafts[S.variant];
    }

    function storedRow(code) {
        var found = S.rows.filter(function (row) { return row.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function proposedRow(code) {
        var held = draft()[code];
        if (held) return held;
        return storedRow(code);
    }

    function nextOrder() {
        var top = 0;
        ANALYTIC_TYPES.forEach(function (entry) {
            var row = proposedRow(entry.code);
            if (row && row.order > top) top = row.order;
        });
        return top + 1;
    }

    function activeOf(code) {
        var row = proposedRow(code);
        return row !== null && row.active === 'Y';
    }

    function ruleState() {
        var pfeOn = activeOf('pfe');
        var simOn = activeOf('simulation');
        if (pfeOn && !simOn) return 'violated';
        if (pfeOn && simOn) return 'satisfied';
        return 'idle';
    }

    function paramDiff(stored, proposed) {
        var changed = [];
        (proposed ? proposed.params : []).forEach(function (param) {
            var was = null;
            (stored ? stored.params : []).forEach(function (old) {
                if (old.name === param.name) was = old.value;
            });
            if (was !== param.value) {
                changed.push({ name: param.name, was: was, will: param.value });
            }
        });
        return changed;
    }

    /* The difference between the stored set and the proposed one. A row that is
       turned off is "removed" from what the run produces and not from the
       configuration, which is the distinction the page exists to make. */
    function diff() {
        var changes = [];
        ANALYTIC_TYPES.forEach(function (entry) {
            var stored = storedRow(entry.code);
            var proposed = proposedRow(entry.code);
            if (!proposed) return;
            if (!stored) {
                changes.push({ kind: 'added', code: entry.code, proposed: proposed });
                return;
            }
            if (stored.active !== proposed.active) {
                changes.push({
                    kind: proposed.active === 'Y' ? 'turnedOn' : 'turnedOff',
                    code: entry.code,
                    proposed: proposed
                });
                return;
            }
            var params = paramDiff(stored, proposed);
            if (params.length > 0) {
                changes.push({ kind: 'parameter', code: entry.code, params: params });
            }
        });
        return changes;
    }

    function writes() {
        var list = [];
        diff().forEach(function (change) {
            if (change.kind === 'parameter') {
                change.params.forEach(function (param) {
                    list.push({
                        entity: 'configuration_analytic_parameter',
                        field: change.code + '.' + param.name,
                        was: param.was === null ? '\u2014' : param.was,
                        will: param.will
                    });
                });
                return;
            }
            var stored = storedRow(change.code);
            if (change.kind === 'added') {
                list.push({
                    entity: 'configuration_analytic',
                    field: change.code + '.active',
                    was: 'no row',
                    will: 'Y, display_order ' + change.proposed.order
                });
            } else if (change.kind === 'turnedOn') {
                list.push({
                    entity: 'configuration_analytic',
                    field: change.code + '.active',
                    was: 'N',
                    will: 'Y'
                });
            } else {
                list.push({
                    entity: 'configuration_analytic',
                    field: change.code + '.active',
                    was: 'Y',
                    will: 'N, row kept at display_order ' + stored.order
                });
            }
        });
        return list;
    }

    function logStep(text) {
        S.log.push(text);
    }

    function setActivity(text) {
        S.activity = text;
    }

    // ------------------------------------------------------------ staging

    function refuse(message) {
        S.refusal = message;
        logStep('refused: ' + message);
        setActivity('refused');
    }

    function stageToggle(code) {
        S.refusal = null;
        var proposed = proposedRow(code);
        var wantOn = proposed === null || proposed.active !== 'Y';

        /* The one rule between analytics. Test the set as it would be, not the
           row on its own, because either side of the pair can be the change. */
        var pfeOn = code === 'pfe' ? wantOn : activeOf('pfe');
        var simOn = code === 'simulation' ? wantOn : activeOf('simulation');
        if (pfeOn && !simOn) {
            refuse(code + ' cannot be on without simulation');
            render();
            return;
        }

        if (!wantOn && storedRow(code) === null) {
            delete draft()[code];
            setActivity('removed the staged add of ' + code + ' \u00b7 nothing was stored');
            logStep('unstaged the new row for ' + code);
            render();
            return;
        }

        var row;
        if (proposed === null) {
            row = { code: code, active: 'Y', order: nextOrder(), params: [] };
        } else {
            row = clone(proposed);
            row.active = wantOn ? 'Y' : 'N';
        }
        setDraft(code, row);
        setActivity((wantOn ? 'turned on ' : 'turned off ') + code + ' \u00b7 not yet written');
        logStep('staged ' + code + ' active=' + row.active);
        render();
    }

    function stageParam(code, name, value) {
        S.refusal = null;
        var proposed = proposedRow(code);
        if (!proposed) return;
        var row = clone(proposed);
        var found = false;
        row.params.forEach(function (param) {
            if (param.name === name) {
                param.value = value;
                found = true;
            }
        });
        if (!found) row.params.push({ name: name, value: value });
        setDraft(code, row);
        setActivity('changed ' + code + '.' + name + ' \u00b7 not yet written');
        logStep('staged ' + code + '.' + name + ' = ' + value);
        render();
    }

    /* A draft entry equal to the stored row is not a change, so it is dropped and
       the difference stays honest. */
    function setDraft(code, row) {
        var stored = storedRow(code);
        if (stored && stored.active === row.active) {
            var params = paramDiff(stored, row);
            var reorder = row.order !== stored.order;
            if (params.length === 0 && !reorder) {
                delete draft()[code];
                return;
            }
        }
        draft()[code] = row;
    }

    function seed() {
        if (S.staged[S.variant]) return;
        if (S.variant === 'A' || S.variant === 'B') {
            draft().pnlExplain = clone(SEED.pnlExplain);
            draft().portfolioDetails = clone(SEED.portfolioDetails);
            draft().sensitivity = clone(SEED.sensitivity);
            logStep('variant ' + S.variant + ' opens with a change already staged');
        }
        if (S.variant === 'C') {
            S.wish = 'pfe';
            draft().pfe = {
                code: 'pfe', active: 'Y', order: 6, params: clone(storedRow('pfe').params)
            };
            logStep('variant C opens with the question answered, and pfe turned on, as a fixture');
        }
        S.staged[S.variant] = true;
    }

    function save() {
        var list = writes();
        Object.keys(draft()).forEach(function (code) {
            var row = draft()[code];
            var stored = storedRow(code);
            if (stored) {
                stored.active = row.active;
                stored.order = row.order;
                stored.params = clone(row.params);
            } else {
                S.rows.push(clone(row));
            }
        });
        S.drafts[S.variant] = {};
        S.staged[S.variant] = true;
        S.readback = {
            rows: S.rows.length,
            version: NEXT_VERSION,
            list: list
        };
        S.saved = true;
        S.confirm = null;
        setActivity('written \u00b7 ' + list.length + ' row(s), definition version ' + NEXT_VERSION);
        logStep('would send reporting.v1.configuration_analytics.put_many and ' +
            'configuration_analytic_parameters.put \u2014 nothing is sent');
        render();
    }

    // ------------------------------------------------------------ fixtures

    function rowsInOrder() {
        var withRow = [];
        ANALYTIC_TYPES.forEach(function (entry) {
            var row = proposedRow(entry.code);
            if (row) withRow.push(row);
        });
        return withRow.slice().sort(function (left, right) {
            if (left.order === right.order) return left.code.localeCompare(right.code);
            return left.order - right.order;
        });
    }

    // --------------------------------------------------------- shared parts

    function appHeader() {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Prototypes</a>' +
            '<a href="#" class="here">Reporting</a></nav>' +
            '<span class="modechip">' + esc(PARTY.code) + ' \u00b7 ' + esc(DEFINITION.reportType) + ' report</span>' +
            '</div></header>';
    }

    function pageHead() {
        var change = diff().length;
        return '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>/</span>' +
            '<span>Reporting</span><span>/</span><span>Change what a report runs</span></div>' +
            '<h1>What this report runs</h1>' +
            '<p class="lede">' + esc(DEFINITION.name) + ' is a <b>' + esc(DEFINITION.reportType) +
            '</b> report of party <b>' + esc(PARTY.code) + '</b>. It binds five configurations, and the ' +
            'analytics this journey changes belong to the fifth, <b>' + esc(CONFIGURATION.name) + '</b>, ' +
            'which three other definitions bind too. Turning one off writes <b>active = false</b> and keeps ' +
            'the row. Changing the set makes every run made after it incomparable with the runs before it.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Fixture clock 2026-10-17 10:20</span>' +
            '<button class="btn" data-act="toggle-state">Show state</button>' +
            '<a class="btn linklike" href="../index.org">Back to prototypes</a>' +
            '</div></header>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>PROTOTYPE, MOCK DATA.</b> ' +
            'Every analytic, parameter, run, version and date on this page is a fixture, and nothing reads a ' +
            'server. Two statements on it are not fixture: <b>an analytic is turned off by setting active to ' +
            'false, not by deleting the row</b>, and <b>pfe cannot run without simulation</b>. The first is ' +
            'how the model works; the second is a rule the ACME seeder enforces and no service does, so this ' +
            'page enforces it itself. <b>No operation says which runs were made with which configuration ' +
            'version</b>: the runs below are placed by their own start time against the definition\u2019s ' +
            'version history, which is an inference, and the page says so where it draws them. There is no ' +
            'BFF route for reporting and no reporting UI in the application.</div>';
    }

    function bindingsCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The configuration this definition binds</h2>' +
            '<span class="meta">reporting.v1.report_configurations.list \u00b7 ' +
            '<span class="mono">' + esc(DEFINITION.id) + '</span></span></header>' +
            '<div class="defline"><b>' + esc(DEFINITION.name) + '</b>' +
            '<span class="id">' + esc(DEFINITION.id) + '</span>' +
            '<span class="tag accent">' + esc(DEFINITION.reportType) + '</span>' +
            '<span class="tag muted">party ' + esc(PARTY.code) + '</span></div>' +
            '<div class="bindings">' + BINDINGS.map(function (binding) {
                return '<span class="binding' + (binding.shared ? ' here' : '') + '"><b>' +
                    esc(binding.type) + '</b>' + esc(binding.name) +
                    (binding.shared ? ' \u00b7 shared by 4 definitions' : '') + '</span>';
            }).join('') + '</div>' +
            '<p class="rowfoot">A definition binds configurations of exactly these five types. The analytics ' +
            'are not on the definition: they are rows of the configuration in the fifth slot. ' +
            esc(SHARED) + ' To change another configuration, or to rebind the definition to a different one, ' +
            'is a different change, and this page does not offer it.</p></section>';
    }

    function ruleStrip() {
        var state = ruleState();
        var cls = state === 'violated' ? ' err' : (state === 'satisfied' ? ' ok' : '');
        var title;
        if (state === 'violated') {
            title = '<span class="tag bad">this set cannot run</span> pfe is on and simulation is off';
        } else if (state === 'satisfied') {
            title = '<span class="tag ok">rule satisfied</span> pfe and simulation are both on';
        } else {
            title = '<span class="tag warn">rule between analytics</span> pfe is off, so the rule does not bind yet';
        }
        return '<div class="rule' + cls + '">' +
            '<div class="rtitle">' + title + '</div>' +
            '<div class="rbody"><b>pfe reads a cube that simulation writes</b>, so turning pfe on without ' +
            'simulation cannot work. The rule lives in the ACME seeder, in ' +
            '<span class="mono">generate_risk_config_data.py</span>, function ' +
            '<span class="mono">check_exposure_needs_simulation</span>, and in no service: nothing validates ' +
            'it when a configuration is written. This page refuses the pair. A build that does not refuse ' +
            'it has to say that the run will fail instead of stating a validation it does not have.</div>' +
            '</div>';
    }

    function refusalNote() {
        if (!S.refusal) return '';
        return '<div class="notice err"><b>Refused here, not by the server.</b> ' +
            esc(S.refusal) + '. ' + esc(PFE_RULE) + '</div>';
    }

    function switchFor(code) {
        var row = proposedRow(code);
        if (!row) {
            return '<button class="btn small" data-act="toggle" data-code="' + esc(code) + '">' +
                'Add row</button>';
        }
        var on = row.active === 'Y';
        return '<button class="sw" role="switch" aria-checked="' + (on ? 'true' : 'false') + '" ' +
            'data-act="toggle" data-code="' + esc(code) + '" title="Writes active ' + (on ? 'N' : 'Y') +
            '. The row is never deleted.">' +
            '<span class="track"><span class="knob"></span></span>' +
            '<span class="statelbl ' + (on ? 'on' : 'off') + '">' + (on ? 'Y' : 'N') + '</span></button>';
    }

    function paramsOf(row, editable) {
        if (!row || row.params.length === 0) return '';
        return '<div class="an-params"><span class="hint">parameters</span>' +
            row.params.map(function (param) {
                var value = editable
                    ? '<input value="' + esc(param.value) + '" data-act="param" data-code="' +
                      esc(row.code) + '" data-name="' + esc(param.name) + '" aria-label="' +
                      esc(param.name) + ' of ' + esc(row.code) + '">'
                    : esc(param.value);
                return '<span class="p"><span class="pn">' + esc(param.name) + '</span>' +
                    '<span class="pv">' + value + '</span></span>';
            }).join('') + '</div>';
    }

    function analyticRow(code, editable) {
        var stored = storedRow(code);
        var row = proposedRow(code);
        var change = diff().filter(function (entry) { return entry.code === code; });
        var cls = 'anrow';
        if (!row) cls += ' norow';
        else if (row.active !== 'Y') cls += ' isoff';
        if (change.length > 0) cls += ' changed';

        var name = typeOf(code);
        var stateTag = '';
        if (!row) {
            stateTag = '<span class="tag muted">no row on this configuration</span>';
        } else if (row.active !== 'Y') {
            stateTag = '<span class="tag muted">off \u00b7 row kept at display_order ' + row.order + '</span>';
        }
        var changeTag = change.length === 0 ? '' : '<span class="tag accent">staged</span>';

        return '<div class="' + cls + '">' +
            '<div class="an-top">' +
            switchFor(code) +
            '<span class="an-name">' + esc(name.name) +
            '<span class="codename">' + esc(name.code) + '</span></span>' +
            '<span class="an-order">' + (row ? 'order ' + row.order : '\u2014') + '</span>' +
            '</div>' +
            paramsOf(row, editable) +
            ((stateTag || changeTag)
                ? '<div class="cellgroup" style="margin: 2px 0 0 96px">' + stateTag + changeTag + '</div>'
                : '') +
            '</div>';
    }

    function writesBlock() {
        var list = writes();
        if (list.length === 0) {
            return '<div class="notice">No change is staged. Turn an analytic on or off, or change a ' +
                'parameter, and the write appears here before anything is sent.</div>';
        }
        return '<div class="writes">' +
            '<div class="wh"><span class="ent">entity</span><span class="fld">field</span>' +
            '<span class="was">was</span><span class="will">will be</span></div>' +
            list.map(function (write) {
            return '<div class="w"><span class="ent">' + esc(write.entity) + '</span>' +
                '<span class="fld">' + esc(write.field) + '</span>' +
                '<span class="was">' + esc(write.was) + '</span>' +
                '<span class="will"><span class="arrow">\u2192 </span>' + esc(write.will) + '</span></div>';
        }).join('') + '</div>';
    }

    function reviewBar() {
        var list = writes();
        var rows = list.filter(function (write) { return write.entity === 'configuration_analytic'; }).length;
        var params = list.length - rows;
        return '<div class="reviewbar">' +
            '<span class="count"><b>' + rows + '</b> analytic row(s) and <b>' + params +
            '</b> parameter row(s) will be written to ' + esc(CONFIGURATION.name) +
            '. No row is deleted and no run is started.</span>' +
            '<button class="btn primary" data-act="ask-save"' + (list.length === 0 ? ' disabled' : '') +
            '>Review and save</button>' +
            '<button class="btn ghost" data-act="reset"' + (list.length === 0 ? ' disabled' : '') +
            '>Discard</button></div>';
    }

    function readbackNote() {
        if (!S.readback) return '';
        return '<div class="notice ok"><b>Read back from the configuration.</b> ' +
            S.readback.rows + ' analytic rows are stored. ' +
            'The screen reads the configuration again and draws the stored values, not the values the person ' +
            'typed. The write would be recorded as definition version ' + S.readback.version +
            '; the older versions stay readable. No run has produced the new set yet.</div>';
    }

    // ------------------------------------------------------------ variant A

    function variantA() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The switchboard: every analytic type, one row each</h2>' +
            '<span class="meta">reporting.v1.configuration_analytics.list_by_configuration_id \u00b7 ' +
            '14 types \u00b7 8 rows \u00b7 fixture</span></header>' +
            ruleStrip() + refusalNote() +
            '<div class="anlist">' + ANALYTIC_TYPES.map(function (entry) {
                return analyticRow(entry.code, true);
            }).join('') + '</div>' +
            '<p class="rowfoot">' + esc(NO_DELETE) + ' The six types with no row were never asked for by this ' +
            'configuration, so turning one on adds a row (reporting.v1.configuration_analytics.put). Every ' +
            'other toggle writes an existing row.</p>' +
            '</section>' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Review: exactly what will be written</h2>' +
            '<span class="meta">configuration_analytics.put_many \u00b7 configuration_analytic_parameters.put ' +
            '\u00b7 fixture</span></header>' +
            writesBlock() + reviewBar() + readbackNote() +
            '<p class="rowfoot">The set is sent in one call, so a turned-on analytic, a turned-off one and a ' +
            'changed parameter land together. A build then reads the configuration back and draws the stored ' +
            'values.</p>' +
            '</section>';
    }

    // ------------------------------------------------------------ variant B

    function setList(rows, editable) {
        if (rows.length === 0) {
            return '<p class="hint">The configuration carries no analytic rows.</p>';
        }
        return '<div class="set">' + rows.map(function (row) {
            var stored = storedRow(row.code);
            var cls = 'item';
            var change = editable
                ? diff().filter(function (entry) { return entry.code === row.code; })[0]
                : null;
            if (change) {
                cls += ' ' + (change.kind === 'turnedOff' ? 'removed'
                    : (change.kind === 'parameter' ? 'changed' : 'added'));
            }
            var params = row.params.map(function (param) {
                return param.name + ' = ' + param.value;
            }).join(', ');
            var control = editable ? switchFor(row.code) : '';
            return '<div class="' + cls + '">' +
                '<span class="nm">' + esc(row.code) + '</span>' +
                '<span class="act">active ' + esc(row.active) + '</span>' +
                (params ? '<span class="prm">' + esc(params) + '</span>' : '') +
                (control ? '<span style="margin-left:auto">' + control + '</span>' : '') +
                '</div>' +
                (editable && row.params.length > 0 ? paramsOf(row, true) : '') +
                (editable && !stored
                    ? '<div class="prm">a new row on this configuration</div>'
                    : '');
        }).join('') + '</div>';
    }

    function variantB() {
        var current = S.rows.slice().sort(function (left, right) { return left.order - right.order; });
        var proposed = rowsInOrder();
        var changes = diff();
        var added = changes.filter(function (entry) { return entry.kind === 'added'; });
        var removed = changes.filter(function (entry) { return entry.kind === 'turnedOff'; });
        var turnedOn = changes.filter(function (entry) { return entry.kind === 'turnedOn'; });
        var params = changes.filter(function (entry) { return entry.kind === 'parameter'; });

        function names(list) {
            return list.length === 0 ? 'none' : list.map(function (entry) { return entry.code; }).join(', ');
        }

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Before and after: the stored set, and the one on the table</h2>' +
            '<span class="meta">left is what recovery reads \u00b7 right is what a write would store \u00b7 fixture</span>' +
            '</header>' +
            '<div class="ba">' +
            '<div class="side"><div class="sidehead"><h3>Stored now</h3>' +
            '<span class="hint">' + current.length + ' rows</span></div>' +
            setList(current, false) + '</div>' +
            '<div class="side proposed"><div class="sidehead"><h3>Proposed</h3>' +
            '<span class="hint">' + proposed.length + ' rows \u00b7 toggles here</span></div>' +
            setList(proposed, true) + '</div>' +
            '</div>' +
            '<div class="sectionhead" style="margin-top:14px"><h3>The difference, stated</h3>' +
            '<span class="hint">staged, not written</span></div>' +
            '<div class="diff">' +
            '<span class="k">Added</span><span class="v add">' + esc(names(added)) + '</span>' +
            '<span class="k">Removed</span><span class="v rem">' + esc(names(removed)) +
            (removed.length ? ' \u00b7 turned off, the row stays' : '') + '</span>' +
            '<span class="k">Turned on</span><span class="v add">' + esc(names(turnedOn)) + '</span>' +
            '<span class="k">Changed</span><span class="v chg">' + (params.length === 0 ? 'none' :
                params.map(function (entry) {
                    return entry.code + '.' + entry.params.map(function (param) {
                        return param.name + ' ' + (param.was === null ? '\u2014' : param.was) +
                            ' \u2192 ' + param.will;
                    }).join(', ');
                }).join('; ')) + '</span>' +
            '</div>' +
            '<p class="rowfoot">"Removed" means removed from what the next run produces, not from the ' +
            'configuration. ' + esc(NO_DELETE) + '</p>' +
            ruleStrip() + refusalNote() +
            '</section>' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Review before the save</h2>' +
            '<span class="meta">the write, stated exactly \u00b7 fixture</span></header>' +
            writesBlock() + reviewBar() + readbackNote() +
            '</section>';
    }

    // ------------------------------------------------------------ variant C

    function candidates(measure) {
        return measure.codes.map(function (code) {
            var row = proposedRow(code);
            var stored = storedRow(code);
            var on = row !== null && row.active === 'Y';
            return '<div class="c">' +
                '<span><span class="ct">' + esc(typeOf(code).name) + '</span>' +
                '<span class="cd"> \u00b7 <span class="mono">' + esc(code) + '</span> \u00b7 ' +
                (stored ? (on ? 'on, display_order ' + row.order : 'off, display_order ' + row.order) :
                    'not on this configuration') + '</span></span>' +
                switchFor(code) +
                '</div>' +
                paramsOf(row, true);
        }).join('');
    }

    function needsLine(measure) {
        if (measure.needs.length === 0) {
            return '<p class="hint">No other analytic has to be on for this one.</p>';
        }
        return '<p class="hint">It needs ' + measure.needs.map(function (code) {
            var on = activeOf(code);
            return '<span class="mono">' + esc(code) + '</span> ' + (on ? '(on)' : '(off)');
        }).join(' and ') + '. pfe reads a cube that simulation writes, so a pair that is not both on ' +
            'cannot work; this page refuses it and the server would not.</p>';
    }

    function variantC() {
        var measure = null;
        MEASURES.forEach(function (entry) {
            if (entry.id === S.wish) measure = entry;
        });
        var changes = diff();
        var body = '<div class="ask">' +

            '<div class="answer"><span class="stepno">1</span>' +
            '<h3>What is the report not producing?</h3>' +
            '<p class="hint">Answer in the words of the person reading the run, not in analytic codes. ' +
            'The screen narrows the codes afterwards. This page opens with potential future exposure ' +
            'chosen and pfe already staged, as a fixture, so the narrowing and the runs are on screen; ' +
            'choose another answer to narrow a different set.</p>' +
            '<div class="choices">' + MEASURES.map(function (entry) {
                return '<button class="choice' + (S.wish === entry.id ? ' on' : '') +
                    '" data-act="wish" data-id="' + esc(entry.id) + '">' + esc(entry.q) + '</button>';
            }).join('') + '</div></div>';

        if (measure) {
            body += '<div class="answer"><span class="stepno">2</span>' +
                '<h3>Analytic types that would produce that</h3>' +
                '<div class="cand">' + candidates(measure) + '</div>' +
                needsLine(measure) + '</div>';

            body += '<div class="answer"><span class="stepno">3</span>' +
                '<h3>What that does to the runs you already have</h3>' +
                (changes.length === 0
                    ? '<p class="hint">Nothing is staged yet, so no run is stranded. Turn a candidate on, and ' +
                      'the runs this change makes incomparable appear here.</p>'
                    : '<p class="dim" style="font-size:13px">' + esc(INCOMPARABLE) + '</p>' +
                      writesBlock() + reviewBar()) +
                '</div>';
        }

        return '<section class="card">' +
            '<header class="sectionhead"><h2>A question first, then the change</h2>' +
            '<span class="meta">measure \u2192 analytic types \u2192 runs \u00b7 fixture</span></header>' +
            body + ruleStrip() + refusalNote() + readbackNote() +
            '</section>';
    }

    // ------------------------------------------------------------ shared tail

    function runsCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The runs this change strands</h2>' +
            '<span class="meta">report_instances.list \u00b7 report_definitions_versions.list \u00b7 ' +
            'placed by start time \u00b7 inference</span></header>' +
            '<div class="versions">' + VERSIONS.map(function (version) {
                return '<div class="vrow"><span class="vno">v' + version.version + '</span>' +
                    '<span class="vat">' + esc(version.at) + '</span>' +
                    '<span class="vnote">' + esc(version.note) + '</span></div>';
            }).join('') +
            '<div class="vrow"><span class="vno">v' + NEXT_VERSION + '</span>' +
            '<span class="vat">not yet written</span>' +
            '<span class="vnote">this change, if it is saved</span></div>' +
            '</div>' +
            '<div class="runs" style="margin-top:12px">' + RUNS.map(function (run) {
                return '<div class="runrow">' +
                    '<span class="rid">' + esc(run.id) + '</span>' +
                    '<span class="rat">' + esc(run.at) + '</span>' +
                    '<span class="rstate">' + esc(run.state) + ', ' + run.errors + ' error(s) \u00b7 ' +
                    'started after v6, so it ran the stored set (an inference)' +
                    '<span class="tag ' + (S.saved ? 'bad' : 'warn') + '">' +
                    (S.saved ? 'incomparable' : 'becomes incomparable') + '</span></span>' +
                    '</div>';
            }).join('') + '</div>' +
            '<div class="reach" style="margin-top:12px">' +
            '<div class="does"><h3>What the change reaches</h3><ul>' +
            '<li>The next run of every definition bound to ' + esc(CONFIGURATION.name) + ', including ' +
            esc(SHARED_WITH.join(', ')) + '.</li>' +
            '<li>The configuration\u2019s analytic rows and their parameter rows.</li>' +
            '<li>The comparison between a run made after it and a run made before it, which stops being a ' +
            'comparison.</li></ul></div>' +
            '<div class="not"><h3>What it does not reach</h3><ul>' +
            '<li>A run already produced: it keeps the configuration it ran with, with its outputs and its ' +
            'errors.</li>' +
            '<li>The definition\u2019s schedule, concurrency policy, processing phases and its other four ' +
            'configurations.</li>' +
            '<li>The configuration\u2019s market roles, which this page reads and does not write.</li>' +
            '<li>Any run in flight: no operation in this journey stops one.</li></ul></div>' +
            '</div>' +
            '<p class="rowfoot">' + esc(PAST_RUNS) + ' ' + esc(INCOMPARABLE) + '</p>' +
            '</section>';
    }

    function gapCard() {
        return '<section class="card gaps">' +
            '<header class="sectionhead"><h2>What this screen cannot show</h2>' +
            '<span class="faint" style="font-size:12px">the journey\u2019s own list of what is missing</span>' +
            '</header>' +
            '<dl>' + GAPS.map(function (gap) {
                return '<div><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    // ------------------------------------------------------------ dialog

    function dialog() {
        if (S.confirm !== 'save') return '';
        var list = writes();
        return '<div class="backdrop"><div class="dialog" role="dialog" aria-modal="true">' +
            '<h2>Write these analytics?</h2>' +
            '<p class="confirmline"><b>' + esc(DEFINITION.name) + '</b> \u00b7 def ' +
            '<span class="mono">' + esc(DEFINITION.id) + '</span> \u00b7 party <b>' + esc(PARTY.code) +
            '</b> \u00b7 configuration <b>' + esc(CONFIGURATION.name) + '</b> ' +
            '<span class="mono">' + esc(CONFIGURATION.id) + '</span></p>' +
            '<div class="sectionhead"><h3>Exactly what will be written</h3>' +
            '<span class="hint">' + list.length + ' row(s)</span></div>' +
            writesBlock() +
            '<div class="notice">' + esc(NO_DELETE) + '</div>' +
            '<div class="notice">' + esc(SHARED) + '</div>' +
            '<div class="notice warn">' + esc(PAST_RUNS) + ' ' + esc(INCOMPARABLE) + '</div>' +
            '<div class="notice">Rule between analytics: ' + (ruleState() === 'violated'
                ? '<b>pfe is on and simulation is off, so this page refuses the change.</b>'
                : (ruleState() === 'satisfied'
                    ? 'pfe and simulation are both on, so it is satisfied.'
                    : 'pfe is off, so it does not bind.')) + ' ' + esc(PFE_RULE) + '</div>' +
            '<p class="hint">Operation: ' +
            '<span class="mono">reporting.v1.configuration_analytics.put_many</span> for the analytic rows, ' +
            'and <span class="mono">reporting.v1.configuration_analytic_parameters.put</span> for the ' +
            'parameter rows. This prototype sends nothing.</p>' +
            '<div class="actions">' +
            '<button class="btn" data-act="confirm-cancel">Cancel</button>' +
            '<button class="btn primary" data-act="confirm-go"' +
            (ruleState() === 'violated' ? ' disabled' : '') + '>Confirm write</button>' +
            '</div>' +
            '</div></div>';
    }

    // --------------------------------------------------------------- bars

    function renderBar() {
        var variantButtons = VARIANTS.map(function (entry) {
            return '<button data-act="variant" data-variant="' + entry.id + '"' +
                (S.variant === entry.id ? ' class="on"' : '') + '>' + esc(entry.id) + '</button>';
        }).join('');

        var stateLine = '<span class="label">variant <b>' + esc(S.variant) + '</b> \u00b7 ' +
            esc(variant().name) + ' \u00b7 state: <b>' + esc(S.activity) + '</b> \u00b7 ' +
            esc(DEFINITION.name) + ' \u00b7 party ' + esc(PARTY.code) + '</span>';

        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>analytic types: <b>' + ANALYTIC_TYPES.length + '</b></span>' +
            '<span>rows stored: <b>' + S.rows.length + '</b></span>' +
            '<span>staged writes: <b>' + writes().length + '</b></span>' +
            '<span>pfe / simulation: <b>' + (activeOf('pfe') ? 'on' : 'off') + ' / ' +
                (activeOf('simulation') ? 'on' : 'off') + '</b></span>' +
            '<span>rule: <b>' + ruleState() + '</b></span>' +
            '<span>definition version: <b>' + (S.saved ? NEXT_VERSION : 'not written') + '</b></span>' +
            '<span>runs: <b>' + RUNS.length + '</b></span>' +
            '</div>' +
            '<p class="state-note">The rows, the parameters, the runs and the versions are a fixture. The ' +
            'rules are not: active false keeps the row, and pfe needs simulation. No service checks the ' +
            'second, and no operation says which configuration version a run used.</p>' +
            (S.log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + S.log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + variantButtons +
            '<span class="sep">|</span>' + stateLine +
            '<span class="gist">' + esc(variant().gist) + '</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>' +
            '</div>' + statePanel;
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 variant ' + S.variant + ' live \u00b7 ' +
            'state ' + S.activity + ' \u00b7 ' + DEFINITION.name + ', party ' + PARTY.code;
    }

    function render() {
        var body = pageHead() + fixtureNote() + bindingsCard() +
            (S.variant === 'A' ? variantA() : (S.variant === 'B' ? variantB() : variantC())) +
            runsCard() + gapCard();
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() + '<main>' + body + '</main></div>' + dialog();
        renderNote();
        renderBar();
    }

    // ------------------------------------------------------------- params

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var wanted = (params.get('variant') || '').toUpperCase();
        if (VARIANTS.some(function (entry) { return entry.id === wanted; })) S.variant = wanted;
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('variant', S.variant);
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    // ------------------------------------------------------------ actions

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        if (element.tagName !== 'BUTTON') return;
        event.preventDefault();
        var act = element.getAttribute('data-act');
        var code = element.getAttribute('data-code');

        if (act === 'variant') {
            S.variant = element.getAttribute('data-variant');
            S.confirm = null;
            S.refusal = null;
            S.wish = null;
            S.readback = null;
            S.saved = false;
            seed();
            setActivity('rest');
            logStep('switched to variant ' + S.variant);
        } else if (act === 'toggle') {
            stageToggle(code);
            writeParams();
            return;
        } else if (act === 'param') {
            return;
        } else if (act === 'wish') {
            S.wish = element.getAttribute('data-id');
            S.confirm = null;
            S.refusal = null;
            setActivity('narrowing the types that would produce it');
            logStep('asked what would produce ' + S.wish);
        } else if (act === 'ask-save') {
            if (writes().length === 0) return;
            S.confirm = 'save';
            setActivity('reviewing the write');
            logStep('opened the review of ' + writes().length + ' row(s)');
        } else if (act === 'confirm-cancel') {
            S.confirm = null;
            setActivity('rest');
            logStep('cancelled, nothing sent');
            render();
            return;
        } else if (act === 'confirm-go') {
            save();
            writeParams();
            return;
        } else if (act === 'reset') {
            S.drafts[S.variant] = {};
            S.confirm = null;
            S.refusal = null;
            S.readback = null;
            S.saved = false;
            setActivity('rest');
            logStep('discarded the staged change');
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
            return;
        }
        render();
        writeParams();
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element || element.tagName !== 'INPUT') return;
        if (element.getAttribute('data-act') !== 'param') return;
        stageParam(element.getAttribute('data-code'), element.getAttribute('data-name'), element.value);
        writeParams();
    });

    document.addEventListener('keydown', function (event) {
        if (event.key !== 'Escape') return;
        if (S.confirm === null) return;
        S.confirm = null;
        setActivity('rest');
        render();
    });

    S.rows = clone(ROWS);
    readParams();
    seed();
    render();
    writeParams();
}());

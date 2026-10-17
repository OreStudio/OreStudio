/* Create a report. Self-contained: plain JavaScript, mock data, no framework,
 * no build step, and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_create_a_report.org.
 *
 * The journey's question is what shape the act of creating a report has, when a
 * report is a definition plus five bindings plus a set of analytics plus a
 * scope. All three variants carry the same fixture and the same writes, and
 * disagree only about structure: A one form with every section visible, B a
 * four-step wizard with a rail, C a checklist where the definition is the
 * product of what the report must satisfy.
 *
 * Three absences the copy never hides:
 *
 *  - the portfolio scope is a column of the bound risk report config and no
 *    operation in this journey's set writes it, so the scope is chosen by
 *    choosing the configuration that carries it;
 *  - pfe reads a cube simulation writes, and the rule that pairs them lives in
 *    the ACME seeder (generate_risk_config_data.py, check_exposure_needs_simulation)
 *    and not in the reporting service, so the refusal on this page is the
 *    screen's own rule;
 *  - nothing answers what a report needs in order to run beyond the
 *    configuration types its report type requires.
 *
 * Nothing here sends a message. The operations named are the journey's own. */

(function () {
    'use strict';

    // ------------------------------------------------------------- fixture

    var NOW = new Date(2026, 9, 20, 14, 32, 0);

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation' };
    var TENANT = 'ACME Group (the session\u2019s tenant)';
    var PERSON = { name: 'Anneke Vos', role: 'Tenant administrator' };

    /* The whole configuration vocabulary: five types, and no more. `supplies`
       is the sentence the screen uses, because a person choosing between two
       configurations needs to know what the choice decides. */
    var CONFIG_TYPES = [
        {
            code: 'conventions',
            supplies: 'The instrument conventions the trades are read with, one document for each convention kind.'
        },
        {
            code: 'pricing_engines',
            supplies: 'The pricing model of each product, and its parameters.'
        },
        {
            code: 'todays_market',
            supplies: 'The market data a run prices against, by market role.'
        },
        {
            code: 'curve_configuration',
            supplies: 'The curves: their definitions, bootstrapping, segments and quotes, and the volatility and correlation configuration.'
        },
        {
            code: 'risk_report_config',
            supplies: 'The run settings the party shares: base currency, observation model, VaR and SIMM settings, and threading. It also carries the scope.'
        }
    ];

    /* The fourteen analytic types the picker offers. `params` are the settings a
       configuration row carries for that analytic, as the ACME seeder writes
       them; a type with no settings asks for nothing. */
    var ANALYTICS = [
        { code: 'npv', params: [['baseCurrency', 'GBP']] },
        { code: 'sensitivity', params: [['recalibrateModels', 'Y'], ['outputSensitivityThreshold', '0.000001'], ['parSensitivity', 'Y']] },
        { code: 'cashflow', params: [] },
        { code: 'curves', params: [['configuration', 'default'], ['grid', '240,1M']] },
        { code: 'simulation', params: [['baseCurrency', 'GBP'], ['storeFlows', 'Y'], ['amc', 'N']] },
        { code: 'saccr', params: [['simmVersion', '2.1']] },
        { code: 'parametricVar', params: [['quantiles', '0.99'], ['method', 'DeltaGammaNormal'], ['breakdown', 'Y']] },
        { code: 'portfolioDetails', params: [] },
        { code: 'creditcurves', params: [['configuration', 'default'], ['grid', '240,1M']] },
        { code: 'pfe', params: [['exposureProfiles', 'Y'], ['exposureProfilesByTrade', 'N'], ['quantile', '0.95'], ['calculationType', 'Symmetric'], ['allocationMethod', 'None'], ['exerciseNextBreak', 'N'], ['baseCurrency', 'GBP']] },
        { code: 'calibration', params: [] },
        { code: 'pnlExplain', params: [['parSensitivity', 'Y']] },
        { code: 'historicalSimulationVar', params: [['historicalPeriod', '2008-09-01,2009-03-31'], ['mporDays', '10'], ['quantiles', '0.99'], ['includeExpectedShortfall', 'Y']] },
        { code: 'xva', params: [['exposureProfiles', 'Y'], ['cva', 'Y'], ['dva', 'Y'], ['fva', 'Y'], ['quantile', '0.95'], ['baseCurrency', 'GBP']] }
    ];

    /* The portfolios the party holds. The scope is not a field of the
       definition: it is a column of the risk report config in the slot, so a
       portfolio resolves to the configurations scoped to it. */
    var PORTFOLIOS = [
        { name: 'Group Treasury Portfolio', scopedBy: ['cfg-rrc-exposure-gtp', 'cfg-rrc-intraday-gtp'] },
        { name: 'Trading Book', scopedBy: ['cfg-rrc-exposure-tb'] }
    ];

    /* Configurations the party holds, by type. A risk report config carries the
       analytic set too, which is why the analytics picker edits it. */
    var CONFIGS = [
        { id: 'cfg-conv-group', type: 'conventions', name: 'Group conventions', detail: '24 convention kinds', scope: null, boundBy: 31, analytics: null },
        { id: 'cfg-pe-group', type: 'pricing_engines', name: 'Group pricing engines', detail: '42 products', scope: null, boundBy: 31, analytics: null },
        { id: 'cfg-tm-group', type: 'todays_market', name: 'Group today\u2019s market', detail: '3 collections, 118 entries', scope: null, boundBy: 31, analytics: null },
        { id: 'cfg-cc-group', type: 'curve_configuration', name: 'Group curve configuration', detail: '2 sections, 61 curves', scope: null, boundBy: 31, analytics: null },
        {
            id: 'cfg-rrc-exposure-gtp', type: 'risk_report_config',
            name: 'Exposure simulation - Group Treasury Portfolio',
            detail: 'base currency GBP, 4 threads', scope: 'Group Treasury Portfolio', boundBy: 6,
            analytics: [
                { code: 'npv', active: true, order: 1 },
                { code: 'simulation', active: true, order: 2 },
                { code: 'pfe', active: true, order: 3 }
            ]
        },
        {
            id: 'cfg-rrc-exposure-tb', type: 'risk_report_config',
            name: 'Exposure simulation - Trading Book',
            detail: 'base currency GBP, 4 threads', scope: 'Trading Book', boundBy: 3,
            analytics: [
                { code: 'npv', active: true, order: 1 },
                { code: 'simulation', active: true, order: 2 },
                { code: 'pfe', active: true, order: 3 },
                { code: 'saccr', active: false, order: 4 }
            ]
        },
        {
            id: 'cfg-rrc-intraday-gtp', type: 'risk_report_config',
            name: 'Intraday risk - Group Treasury Portfolio',
            detail: 'base currency GBP, 2 threads', scope: 'Group Treasury Portfolio', boundBy: 4,
            analytics: [
                { code: 'npv', active: true, order: 1 },
                { code: 'sensitivity', active: true, order: 2 },
                { code: 'parametricVar', active: true, order: 3 }
            ]
        }
    ];

    /* The names the party already uses. A definition's name is unique within its
       party, so the screen refuses one of these. */
    var TAKEN_NAMES = [
        'Exposure', 'CVA/DVA/FVA', 'Intraday Risk Monitor', 'Large Exposures',
        'Board Risk Dashboard', 'Headline Position', 'Bucketed DV01', 'Stressed VaR',
        'Leverage Ratio', 'Cashflows'
    ];

    var CONCURRENCY = [
        { code: 'skip', label: 'Skip: a firing is skipped while another run is in flight' },
        { code: 'queue', label: 'Queue: a firing waits until the run in flight ends' },
        { code: 'fail', label: 'Fail: a firing is recorded as failed, and does not run' }
    ];

    var VARIANTS = [
        {
            id: 'A',
            name: 'One form',
            gist: 'Creating a report is one page of sections: identity, what the type needs, the five configurations, the analytics, the scope, and the review, all visible at once.'
        },
        {
            id: 'B',
            name: 'Wizard',
            gist: 'Creating a report is four steps on a rail: what it is, what it reads, what it runs, and review and save. One decision at a time, and the rail says what is still open.'
        },
        {
            id: 'C',
            name: 'Checklist',
            gist: 'Creating a report is the list of what a report must satisfy, each item met or not, with the fields inline under the ones that are not. The definition is the product of the list.'
        }
    ];

    var PFE_RULE = 'pfe reads a cube that simulation writes, so a report that enables pfe without ' +
        'simulation cannot work. The rule lives in the ACME seeder, in ' +
        'generate_risk_config_data.py at check_exposure_needs_simulation, and not in the reporting ' +
        'service: nothing validates it when a definition is written.';

    var CHECKLIST_WAIVER = 'Nothing answers what a report needs in order to run beyond the ' +
        'configuration types its report type requires, so this list is everything a screen can ' +
        'check today, and not everything a run needs.';

    var S = {
        variant: 'A',
        step: 1,
        draft: null,
        saved: null,
        dialog: null,
        activity: 'new, nothing written',
        showState: false,
        openInline: {},
        log: [],
        touchedAnalytics: false
    };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function configById(id) {
        var found = CONFIGS.filter(function (entry) { return entry.id === id; });
        return found.length === 1 ? found[0] : null;
    }

    function configsOf(type) {
        return CONFIGS.filter(function (entry) { return entry.type === type; });
    }

    function analyticsOf(code) {
        var found = ANALYTICS.filter(function (entry) { return entry.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function portfolioOf(name) {
        var found = PORTFOLIOS.filter(function (entry) { return entry.name === name; });
        return found.length === 1 ? found[0] : null;
    }

    function fixtureClock() {
        var days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
        var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        function pad(n) { return (n < 10 ? '0' : '') + n; }
        return days[NOW.getDay()] + ' ' + NOW.getDate() + ' ' + months[NOW.getMonth()] + ' ' +
            NOW.getFullYear() + ' ' + pad(NOW.getHours()) + ':' + pad(NOW.getMinutes()) +
            ' installation clock';
    }

    // --------------------------------------------------------- the draft

    function newDraft() {
        var draft = {
            name: '',
            description: '',
            reportType: 'risk',
            official: true,
            concurrency: 'skip',
            binds: {},
            scope: null,
            analytics: []
        };
        CONFIG_TYPES.forEach(function (type) {
            var candidates = configsOf(type.code);
            draft.binds[type.code] = candidates.length > 0 ? candidates[0].id : null;
        });
        draft.scope = configById(draft.binds.risk_report_config).scope;
        draft.analytics = copyAnalytics(configById(draft.binds.risk_report_config));
        return draft;
    }

    function copyAnalytics(config) {
        return config.analytics.map(function (row) {
            var type = analyticsOf(row.code);
            var values = {};
            var positions = {};
            type.params.forEach(function (pair) { values[pair[0]] = pair[1]; });
            return {
                code: row.code,
                active: row.active,
                order: row.order,
                params: type.params.map(function (pair, index) {
                    positions[pair[0]] = index + 1;
                    return { name: pair[0], value: values[pair[0]], position: index + 1 };
                })
            };
        });
    }

    function boundRiskConfig() {
        return configById(S.draft.binds.risk_report_config);
    }

    function analyticsRow(code) {
        var found = S.draft.analytics.filter(function (row) { return row.code === code; });
        return found.length === 1 ? found[0] : null;
    }

    function activeAnalytics() {
        return S.draft.analytics.filter(function (row) { return row.active; })
            .sort(function (a, b) { return a.order - b.order; });
    }

    function pfeWithoutSimulation() {
        var pfe = analyticsRow('pfe');
        var simulation = analyticsRow('simulation');
        return !!(pfe && pfe.active) && !(simulation && simulation.active);
    }

    // ------------------------------------------------------------ checks

    /* The list of what a report must satisfy. Variant C draws it as the screen;
       A and B use it for the review and the rail. `inline` names the editor the
       checklist opens under the item. */
    function checks() {
        var nameTaken = TAKEN_NAMES.some(function (taken) {
            return taken.toLowerCase() === String(S.draft.name || '').trim().toLowerCase();
        });
        var list = [
            {
                id: 'name',
                what: 'It has a name the party does not already use',
                why: 'A definition\u2019s name is unique within its party, and the front door reads it. ' +
                    TAKEN_NAMES.slice(0, 4).join(', ') + ' and five more are taken.',
                met: String(S.draft.name || '').trim() !== '' && !nameTaken,
                state: String(S.draft.name || '').trim() === ''
                    ? 'no name yet'
                    : (nameTaken ? 'the party already uses this name' : 'named'),
                inline: 'name'
            },
            {
                id: 'description',
                what: 'It says what it is for',
                why: 'The definition carries a description, and it is the only sentence a reader ' +
                    'sees beside the name on the front door.',
                met: String(S.draft.description || '').trim() !== '',
                state: String(S.draft.description || '').trim() === '' ? 'no description yet' : 'described',
                inline: 'description'
            },
            {
                id: 'type',
                what: 'It is a risk report or a grid report',
                why: 'There are exactly two report types, risk and grid, and the type decides what ' +
                    'the report is rather than how it runs.',
                met: true,
                state: S.draft.reportType,
                inline: 'type'
            },
            {
                id: 'official',
                what: 'It states whether it is official',
                why: 'An official report never reads a sandbox: its scope cannot name a sandbox ' +
                    'portfolio or a virtual book. A definition defaults to official.',
                met: true,
                state: S.draft.official ? 'official' : 'not official, and may read a sandbox',
                inline: 'official'
            },
            {
                id: 'concurrency',
                what: 'It says what happens when two runs collide',
                why: 'The concurrency policy decides the state a run is born in: skip, queue, or fail.',
                met: !!S.draft.concurrency,
                state: S.draft.concurrency || 'not chosen',
                inline: 'concurrency'
            }
        ];

        CONFIG_TYPES.forEach(function (type) {
            var bound = configById(S.draft.binds[type.code]);
            list.push({
                id: 'slot-' + type.code,
                what: 'It binds a ' + type.code + ' configuration',
                why: type.supplies,
                met: !!bound,
                state: bound ? bound.name : 'nothing bound',
                inline: 'slot',
                slot: type.code
            });
        });

        var active = activeAnalytics();
        list.push({
            id: 'analytics',
            what: 'It runs at least one analytic',
            why: 'A run with no analytic switched on produces nothing. An analytic is turned off by ' +
                'setting active false, never by deleting its row.',
            met: active.length > 0,
            state: active.length === 0 ? 'no analytic is on' : active.length + ' on: ' +
                active.map(function (row) { return row.code; }).join(', '),
            inline: 'analytics'
        });

        list.push({
            id: 'pfe',
            what: 'pfe is not enabled without simulation',
            why: PFE_RULE,
            met: !pfeWithoutSimulation(),
            state: pfeWithoutSimulation() ? 'pfe is on and simulation is off, so a save is refused'
                : (analyticsRow('pfe') && analyticsRow('pfe').active
                    ? 'pfe and simulation are both on'
                    : 'pfe is off, so the rule does not apply'),
            inline: 'pfe'
        });

        var emptySettings = active.filter(function (row) {
            return row.params.some(function (param) { return String(param.value).trim() === ''; });
        });
        list.push({
            id: 'settings',
            what: 'The analytics that carry settings carry them',
            why: 'An analytic\u2019s settings are configuration_analytic_parameter rows of a ' +
                'parameter_name and a value, in positions.',
            met: emptySettings.length === 0,
            state: emptySettings.length === 0 ? 'every setting has a value'
                : 'a value is empty for ' + emptySettings.map(function (row) { return row.code; }).join(', '),
            inline: 'settings'
        });

        list.push({
            id: 'scope',
            what: 'It reads a portfolio',
            why: 'The scope is a column of the bound risk report config, not a field of the ' +
                'definition, and no operation in this journey\u2019s set writes it.',
            met: !!S.draft.scope,
            state: S.draft.scope || 'nothing chosen',
            inline: 'scope'
        });

        var open = list.filter(function (item) { return !item.met; }).length;
        list.push({
            id: 'review',
            what: 'Every value it will write has been read back',
            why: 'The review names the field values rather than summarising them, so what is saved ' +
                'is what was read.',
            met: open === 0,
            state: open === 0 ? 'ready to save' : open + ' item' + (open === 1 ? '' : 's') + ' still open',
            inline: 'review'
        });

        return list;
    }

    function openCount() {
        return checks().filter(function (item) { return !item.met; }).length;
    }

    function ready() {
        return openCount() === 0;
    }

    // ------------------------------------------------------------- write plan

    /* What a save writes, in the words of the operations. Every field of the
       definition is named, including the ones that stay empty. */
    function definitionFields() {
        return [
            ['id', 'a new identifier, assigned by the service', false],
            ['tenant_id', TENANT, false],
            ['name', S.draft.name || '', !S.draft.name],
            ['party_id', PARTY.code + ' (from the session)', false],
            ['description', S.draft.description || '', !S.draft.description],
            ['report_type', S.draft.reportType, false],
            ['fsm_state_id', 'empty until first activation', true],
            ['schedule_expression', 'empty: the definition is on demand', true],
            ['concurrency_policy', S.draft.concurrency, false],
            ['scheduler_job_id', 'empty: no schedule', true],
            ['run_grant_id', 'empty: created at admission, by scheduling or by a person asking', true],
            ['pre_processing', 'execute', false],
            ['prepared_input_key', 'empty: set only when pre_processing is substitute', true],
            ['post_processing', 'execute', false],
            ['is_official', S.draft.official ? 'true' : 'false', false]
        ];
    }

    function bindingRows() {
        return CONFIG_TYPES.map(function (type) {
            var config = configById(S.draft.binds[type.code]);
            return [type.code, config ? config.name : '', !config];
        });
    }

    function analyticRows() {
        return S.draft.analytics.slice().sort(function (a, b) { return a.order - b.order; })
            .map(function (row) {
                return [row.code, String(row.order), row.active ? 'true' : 'false'];
            });
    }

    function parameterRows() {
        var rows = [];
        activeAnalytics().forEach(function (row) {
            row.params.forEach(function (param) {
                rows.push([row.code + '.' + param.name, param.value, param.position]);
            });
        });
        return rows;
    }

    // --------------------------------------------------------- components

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
        return '<div class="pagehead">' +
            '<div>' +
            '<div class="crumbs">Reporting / Reports / <span class="mono">new definition</span></div>' +
            '<h1>Create a report</h1>' +
            '<p class="lede">Name the report, bind the five configurations it reads, choose the ' +
            'analytics it runs and their settings, and scope it to a portfolio. The review states ' +
            'every field value before it is written.</p>' +
            '</div>' +
            '<div class="actions">' +
            '<span class="tag warn">fixture</span>' +
            '<span class="updated">installation clock ' + esc(fixtureClock()) + '</span>' +
            '</div></div>';
    }

    function fixtureNote() {
        return '<div class="notice warn">This page is a fixture. Nothing reads a server and nothing ' +
            'is written. The party, the configurations, the analytics and their settings are mock ' +
            'data drawn from the ACME seed. <b>No BFF route serves reporting today</b>, and nothing ' +
            'in the application creates a report.</div>';
    }

    function nameField() {
        var takenBy = TAKEN_NAMES.some(function (taken) {
            return taken.toLowerCase() === String(S.draft.name || '').trim().toLowerCase();
        });
        return '<div class="field">' +
            '<label for="f-name">Name</label>' +
            '<input id="f-name" type="text" data-act="name" value="' + esc(S.draft.name) + '"' +
            (takenBy ? ' class="bad"' : '') + ' placeholder="Intraday Exposure">' +
            '<span class="note">' + (takenBy
                ? 'The party already uses this name.'
                : 'Unique within ' + esc(PARTY.code) + '.') + '</span>' +
            '</div>';
    }

    function descriptionField() {
        return '<div class="field">' +
            '<label for="f-desc">Description</label>' +
            '<input id="f-desc" type="text" data-act="description" value="' + esc(S.draft.description) + '"' +
            ' placeholder="Exposure profiles for the trading book">' +
            '<span class="note">One sentence a reader sees beside the name.</span>' +
            '</div>';
    }

    function typeField() {
        return '<div class="field">' +
            '<label>Report type</label>' +
            '<div class="radios">' +
            ['risk', 'grid'].map(function (code) {
                return '<label><input type="radio" name="rtype" data-act="type" value="' + code + '"' +
                    (S.draft.reportType === code ? ' checked' : '') + '> <span class="mono">' + code + '</span></label>';
            }).join('') +
            '</div>' +
            '<span class="note">There are exactly two types, risk and grid.</span>' +
            '</div>';
    }

    function concurrencyField() {
        return '<div class="field">' +
            '<label for="f-conc">Concurrency policy</label>' +
            '<select id="f-conc" data-act="concurrency">' +
            CONCURRENCY.map(function (entry) {
                return '<option value="' + entry.code + '"' +
                    (S.draft.concurrency === entry.code ? ' selected' : '') + '>' +
                    esc(entry.code) + ' \u2014 ' + esc(entry.label) + '</option>';
            }).join('') +
            '</select>' +
            '<span class="note">What a second firing does while a run is in flight.</span>' +
            '</div>';
    }

    function officialField() {
        return '<div class="field wide">' +
            '<label class="check"><input type="checkbox" data-act="official"' +
            (S.draft.official ? ' checked' : '') + '> Is official</label>' +
            '<span class="note">An official report never reads a sandbox: its scope cannot name a ' +
            'sandbox portfolio or a virtual book. The party is ' + esc(PARTY.code) +
            ', from the session, and is not a choice.</span>' +
            '</div>';
    }

    function identityFields() {
        return '<div class="formgrid">' + nameField() + descriptionField() + typeField() +
            concurrencyField() + officialField() + '</div>';
    }

    function configSlots(onlySlot) {
        return '<div class="slots">' + CONFIG_TYPES.filter(function (type) {
            return !onlySlot || type.code === onlySlot;
        }).map(function (type) {
            var bound = configById(S.draft.binds[type.code]);
            var candidates = configsOf(type.code);
            return '<div class="slot">' +
                '<div class="type"><code>' + esc(type.code) + '</code>' +
                '<span class="req">required by ' + esc(S.draft.reportType) + '</span></div>' +
                '<div class="supplies">' + esc(type.supplies) + '</div>' +
                '<div class="pick">' +
                '<select data-act="bind" data-slot="' + esc(type.code) + '">' +
                '<option value=""' + (bound ? '' : ' selected') + '>not bound</option>' +
                candidates.map(function (config) {
                    return '<option value="' + esc(config.id) + '"' +
                        (bound && bound.id === config.id ? ' selected' : '') + '>' +
                        esc(config.name) + '</option>';
                }).join('') +
                '</select>' +
                '<span class="shared">' + (bound
                    ? esc(bound.detail) + ' \u00b7 bound by <b>' + bound.boundBy + ' definitions</b>'
                    : 'nothing is bound in this slot, and a ' + esc(type.code) + ' configuration is required') +
                '</span>' +
                '</div>' +
                '</div>';
        }).join('') + '</div>';
    }

    function analyticsPicker(includeRule) {
        var broken = pfeWithoutSimulation();
        var rows = ANALYTICS.map(function (type) {
            var row = analyticsRow(type.code);
            var on = !!(row && row.active);
            var locked = broken && ((type.code === 'pfe' && on) || type.code === 'simulation');
            var params = row && row.params.length > 0
                ? row.params.slice(0, 2).map(function (param) {
                    return esc(param.name) + ' <span class="pv">' + esc(param.value) + '</span>';
                }).join(' \u00b7 ') + (row.params.length > 2 ? ' \u00b7 +' + (row.params.length - 2) + ' more' : '')
                : 'no settings';
            return '<div class="pickrow' + (locked ? ' locked' : '') + '">' +
                '<button class="switch' + (locked ? ' bad' : '') + '" role="switch" aria-checked="' +
                (on ? 'true' : 'false') + '" data-act="toggle-analytic" data-code="' + esc(type.code) +
                '" aria-label="' + esc(type.code) + (on ? ' active' : ' inactive') + '"></button>' +
                '<span class="acode' + (on ? ' on' : '') + '">' + esc(type.code) + '</span>' +
                '<span class="params">' + params + '</span>' +
                '<span class="order field"><input type="number" min="1" max="14" data-act="order" data-code="' +
                esc(type.code) + '" value="' + (row ? row.order : '') + '"' +
                (on ? '' : ' disabled') + ' title="display_order"></span>' +
                '<span class="rowact">' + (on
                    ? '<button class="btn small linklike" data-act="params" data-code="' + esc(type.code) + '">Settings</button>'
                    : '') + '</span>' +
                '</div>';
        }).join('');

        return '<div class="picker">' + rows + '</div>' +
            '<p class="rowfoot">Every analytic is offered, active or not. Turning one off sets ' +
            '<span class="mono">active</span> false: the row and its settings stay.</p>' +
            (includeRule === false ? '' :
                '<div class="rule" style="margin-top:12px"><b>The rule between two analytics.</b> ' +
                esc(PFE_RULE) + '</div>') +
            (broken
                ? '<div class="notice err" style="margin-top:12px"><b>Refused.</b> pfe is on and ' +
                  'simulation is off. Turn simulation on, or turn pfe off, before saving. No ' +
                  'service would refuse this write: the screen is the only place that checks it.' +
                  '</div>'
                : '');
    }

    function scopePicker() {
        var options = PORTFOLIOS.map(function (portfolio) {
            var config = configById(S.draft.binds.risk_report_config);
            var carried = config && portfolio.scopedBy.indexOf(config.id) !== -1;
            return '<option value="' + esc(portfolio.name) + '"' +
                (S.draft.scope === portfolio.name ? ' selected' : '') + '>' +
                esc(portfolio.name) + (carried ? ' \u2014 scoped by the bound configuration' : '') +
                '</option>';
        }).join('');
        var config = boundRiskConfig();
        return '<div class="formgrid">' +
            '<div class="field">' +
            '<label for="f-scope">Portfolio the report reads</label>' +
            '<select id="f-scope" data-act="scope"><option value="">not chosen</option>' + options + '</select>' +
            '<span class="note">' +
            (config && config.scope
                ? 'The bound configuration ' + esc(config.name) + ' is scoped to ' + esc(config.scope) + '.'
                : 'No risk report config is bound, so no scope comes with it.') +
            '</span>' +
            '</div>' +
            '<div class="field">' +
            '<label>What carries the scope</label>' +
            '<p class="dim" style="font-size:13px">The scope is not a field of the definition. It is a ' +
            'column of the risk report config in the slot, in its portfolio and book junction tables, ' +
            'so choosing a portfolio means choosing the configuration scoped to it. ' +
            '<b>No operation in this journey\u2019s set writes the scope.</b></p>' +
            '</div>' +
            '</div>';
    }

    function reviewPanel() {
        var open = checks().filter(function (item) { return !item.met; });

        function fieldTable(rows) {
            return '<table><tbody>' + rows.map(function (row) {
                return '<tr><td>' + esc(row[0]) + '</td><td class="val' + (row[2] ? ' empty' : '') + '">' +
                    esc(row[1] || '\u2014') + '</td></tr>';
            }).join('') + '</tbody></table>';
        }

        var definition = '<div class="writeblock">' +
            '<div class="op"><span class="what">The definition</span>' +
            '<span class="subject">reporting.v1.report_definitions.put</span></div>' +
            fieldTable(definitionFields()) + '</div>';

        var bindings = '<div class="writeblock">' +
            '<div class="op"><span class="what">One binding for each of the five configuration types</span>' +
            '<span class="subject">reporting.v1.report_configurations.put</span></div>' +
            '<table><thead><tr><th>configuration_type_code</th><th>configuration</th></tr></thead><tbody>' +
            bindingRows().map(function (row) {
                return '<tr><td class="code">' + esc(row[0]) + '</td><td class="val' + (row[2] ? ' empty' : '') + '">' +
                    esc(row[1] || 'nothing bound') + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<p class="hint">The bindings hang off the definition. The analytics below hang off the ' +
            'risk report config, which other definitions bind too.</p></div>';

        var analytics = '<div class="writeblock">' +
            '<div class="op"><span class="what">The analytic rows</span>' +
            '<span class="subject">reporting.v1.configuration_analytics.put_many</span></div>' +
            '<table><thead><tr><th>analytic_type_code</th><th>display_order</th><th>active</th></tr></thead><tbody>' +
            analyticRows().map(function (row) {
                return '<tr><td class="code">' + esc(row[0]) + '</td><td class="code">' + esc(row[1]) +
                    '</td><td class="code">' + esc(row[2]) + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<p class="hint">A row is written for every analytic the person turned on or off. ' +
            'Nothing is deleted.</p></div>';

        var params = parameterRows();
        var parameters = '<div class="writeblock">' +
            '<div class="op"><span class="what">The settings of the analytics that are on</span>' +
            '<span class="subject">reporting.v1.configuration_analytic_parameters.put</span></div>' +
            (params.length === 0
                ? '<p class="dim">No analytic that is on carries a setting.</p>'
                : '<table><thead><tr><th>parameter_name</th><th>value</th><th>position</th></tr></thead><tbody>' +
                  params.map(function (row) {
                      return '<tr><td class="code">' + esc(row[0]) + '</td><td class="val">' + esc(row[1]) +
                          '</td><td class="code">' + esc(row[2]) + '</td></tr>';
                  }).join('') + '</tbody></table>') +
            '</div>';

        var scope = '<div class="writeblock">' +
            '<div class="op"><span class="what">The scope</span>' +
            '<span class="subject">no operation</span></div>' +
            '<table><tbody><tr><td>portfolio</td><td class="val' + (S.draft.scope ? '' : ' empty') + '">' +
            esc(S.draft.scope || 'nothing chosen') + '</td></tr>' +
            '<tr><td>carried by</td><td class="val">' +
            esc(boundRiskConfig() ? boundRiskConfig().name : 'no configuration bound') + '</td></tr></tbody></table>' +
            '<p class="hint">No operation in this journey\u2019s set writes or reads the scope: it ' +
            'comes with the configuration, and the seeder builds that configuration.</p></div>';

        var openBox = open.length === 0
            ? '<div class="notice ok"><b>Ready to save.</b> Everything the checklist can check is met. ' +
              'The checklist is what a screen can check, not what a run needs.</div>'
            : '<div class="notice err"><b>Not ready.</b> ' + open.length + ' item' +
              (open.length === 1 ? '' : 's') + ' still open: ' +
              open.map(function (item) { return esc(item.what); }).join('; ') + '. A save is refused ' +
              'while any is open, because the definition would read back unlike the review.</div>';

        return '<div class="review">' + openBox + definition + bindings + analytics + parameters + scope +
            '<p class="hint">' + esc(CHECKLIST_WAIVER) + '</p></div>';
    }

    function readbackPanel() {
        if (!S.saved) return '';
        var saved = S.saved;
        return '<div class="card readback">' +
            '<div class="sectionhead"><h2>Read back after the write</h2>' +
            '<span class="meta"><span class="tag ok">stored</span>' +
            '<span>reporting.v1.report_definitions.get</span></span></div>' +
            '<div class="banner"><b>' + esc(saved.name) + '</b> reads back as written. ' +
            '<span class="mono">' + esc(saved.id) + '</span> \u00b7 version 1</div>' +
            '<div class="statgrid" style="margin-top:14px">' +
            '<div><span class="lbl">report_type</span><span class="val mono">' + esc(saved.reportType) + '</span></div>' +
            '<div><span class="lbl">party_id</span><span class="val mono">' + esc(PARTY.code) + '</span></div>' +
            '<div><span class="lbl">schedule_expression</span><span class="val dim">empty, on demand</span></div>' +
            '<div><span class="lbl">scheduler_job_id</span><span class="val dim">empty</span></div>' +
            '<div><span class="lbl">run_grant_id</span><span class="val dim">empty until a run needs it</span></div>' +
            '<div><span class="lbl">fsm_state_id</span><span class="val dim">empty until first activation</span></div>' +
            '<div><span class="lbl">is_official</span><span class="val mono">' + (saved.official ? 'true' : 'false') + '</span></div>' +
            '<div><span class="lbl">concurrency_policy</span><span class="val mono">' + esc(saved.concurrency) + '</span></div>' +
            '<div><span class="lbl">runs</span><span class="val"><span class="tag muted">never run</span></span></div>' +
            '</div>' +
            '<table class="kv" style="margin-top:14px"><tbody>' +
            saved.bindings.map(function (row) {
                return '<tr><td>bound ' + esc(row[0]) + '</td><td>' + esc(row[1]) + '</td></tr>';
            }).join('') +
            '<tr><td>portfolio scope</td><td>' + esc(saved.scope) + ', carried by the bound configuration</td></tr>' +
            '<tr><td>analytic rows</td><td>' + saved.analytics + '</td></tr>' +
            '<tr><td>parameter rows</td><td>' + saved.params + '</td></tr>' +
            '</tbody></table>' +
            '<p class="rowfoot">The definition is bitemporal: an edit writes a new version and no past ' +
            'run changes. It has never run, so the front door draws it as never run. ' +
            '<b>Nothing was sent.</b></p>' +
            '<div class="actions" style="display:flex;gap:8px;margin-top:12px;flex-wrap:wrap">' +
            '<button class="btn" data-act="leave">Leave it on demand</button>' +
            '<button class="btn" data-act="schedule">Choose when it runs</button>' +
            '</div></div>';
    }

    function reachCard() {
        return '<div class="card"><div class="sectionhead"><h2>What a save reaches, and what it does not</h2></div>' +
            '<div class="reach">' +
            '<div class="does"><h3>It writes</h3><ul>' +
            '<li>the definition, on demand, with no schedule expression and no scheduler job</li>' +
            '<li>one binding for each of the five configuration types</li>' +
            '<li>the analytic rows of the bound risk report config</li>' +
            '<li>the settings of the analytics that are on</li>' +
            '</ul></div>' +
            '<div class="not"><h3>It does not</h3><ul>' +
            '<li>create a run, or produce anything</li>' +
            '<li>write a schedule, a scheduler job or a grant</li>' +
            '<li>write the portfolio scope, which the configuration carries</li>' +
            '<li>write the run settings inside a risk report config</li>' +
            '<li>change a run that has already happened</li>' +
            '</ul></div></div></div>';
    }

    function gapCard() {
        return '<div class="card gaps"><div class="sectionhead"><h2>What this page cannot show</h2></div>' +
            '<dl>' +
            '<dt>The analytic rule is the screen\u2019s own</dt>' +
            '<dd>' + esc(PFE_RULE) + ' The refusal above is drawn because the journey asks for it, and ' +
            'a build would have to carry the rule in the screen.</dd>' +
            '<dt>No operation answers what a report needs</dt>' +
            '<dd>report_type_configuration_types.list names the configuration types a report type ' +
            'cannot start without, and that is all. Nothing names the analytics a run must enable, or ' +
            'the inputs each analytic reads, so a screen cannot show the full checklist a definition ' +
            'must satisfy.</dd>' +
            '<dt>No operation writes the scope</dt>' +
            '<dd>The scope lives on the risk report config, in its portfolio and book junction ' +
            'tables, and no operation in this set touches them. The configurations on this page, with ' +
            'their scopes, are the seeder\u2019s.</dd>' +
            '<dt>No route, and nothing creates a report</dt>' +
            '<dd>No BFF route serves reporting, and nothing in the application creates a report today: ' +
            'every definition that exists was seeded.</dd>' +
            '<dt>No write permission is named</dt>' +
            '<dd>No permission code names the right to create a definition or to change a shared ' +
            'configuration\u2019s analytics. The tenant administrator holds *, and reaches both.</dd>' +
            '</dl></div>';
    }

    // ------------------------------------------------------- variants

    function sectionCard(id, title, meta, body) {
        return '<div class="card" id="' + id + '"><div class="sectionhead"><h2>' + esc(title) + '</h2>' +
            '<span class="meta">' + meta + '</span></div>' + body + '</div>';
    }

    function variantA() {
        var open = openCount();
        return pageHead() + fixtureNote() +
            sectionCard('sec-identity', '1. Identity', '<span class="tag muted">the definition</span>',
                identityFields()) +
            sectionCard('sec-needs', '2. What a ' + S.draft.reportType + ' report needs',
                '<span class="mono">reporting.v1.report_type_configuration_types.list</span>',
                '<table><thead><tr><th>configuration_type_code</th><th>required by ' +
                esc(S.draft.reportType) + '</th></tr></thead><tbody>' +
                CONFIG_TYPES.map(function (type) {
                    return '<tr><td class="code">' + esc(type.code) + '</td><td>required</td></tr>';
                }).join('') + '</tbody></table>' +
                '<p class="rowfoot">This list is the one read that answers part of what the report ' +
                'needs. It names configuration types, and nothing names the analytics a run must enable.</p>') +
            sectionCard('sec-configs', '3. The configurations it binds',
                '<span class="mono">reporting.v1.report_configurations.put</span>' + ' \u00b7 ' +
                '<span class="mono">reporting.v1.configurations.list_by_configuration_type_code</span>',
                configSlots()) +
            sectionCard('sec-analytics', '4. The analytics, and their settings',
                '<span class="mono">reporting.v1.analytic_types.list</span> \u00b7 ' +
                '<span class="mono">reporting.v1.configuration_analytics.put_many</span>',
                analyticsPicker()) +
            sectionCard('sec-scope', '5. The scope', '<span class="tag warn">no operation</span>',
                scopePicker()) +
            sectionCard('sec-review', '6. Review and save',
                '<span class="tag ' + (open === 0 ? 'ok' : 'bad') + '">' +
                (open === 0 ? 'ready' : open + ' open') + '</span>',
                reviewPanel() +
                '<div class="actions" style="display:flex;gap:8px;margin-top:16px;flex-wrap:wrap">' +
                '<button class="btn primary" data-act="save"' + (ready() ? '' : ' disabled') +
                '>Save the report</button>' +
                '<button class="btn" data-act="discard">Start again</button>' +
                '<span class="hint">' + (ready()
                    ? 'Five writes, in the order shown above.'
                    : 'A save is refused while an item is open.') + '</span>' +
                '</div>') +
            readbackPanel() + reachCard() + gapCard();
    }

    var STEPS = [
        { n: 1, name: 'What it is', sub: 'name, type, official', ids: ['name', 'description', 'type', 'official', 'concurrency'] },
        { n: 2, name: 'What it reads', sub: 'five configurations, the scope', ids: ['slot-conventions', 'slot-pricing_engines', 'slot-todays_market', 'slot-curve_configuration', 'slot-risk_report_config', 'scope'] },
        { n: 3, name: 'What it runs', sub: 'the analytics and their settings', ids: ['analytics', 'pfe', 'settings'] },
        { n: 4, name: 'Review and save', sub: 'what will be written', ids: ['review'] }
    ];

    function stepState(step) {
        var list = checks();
        var items = list.filter(function (item) { return step.ids.indexOf(item.id) !== -1; });
        var open = items.filter(function (item) { return !item.met; }).length;
        return { items: items, open: open, met: open === 0 };
    }

    function rail() {
        return '<div class="rail">' + STEPS.map(function (step) {
            var state = stepState(step);
            return '<button class="railitem' + (S.step === step.n ? ' here' : '') +
                (state.met ? ' met' : '') + '" data-act="step" data-step="' + step.n + '">' +
                '<span class="num">' + step.n + '</span>' +
                '<span class="head">' + esc(step.name) + '</span>' +
                '<span class="sub">' + (state.met ? esc(step.sub) : state.open + ' open \u00b7 ' + esc(step.sub)) +
                '</span></button>';
        }).join('') +
            '<div class="railfoot">A step marked open has an item a save would refuse. The rail reads ' +
            'the same checklist variant C draws as the screen.</div></div>';
    }

    function stepBody() {
        if (S.step === 1) {
            return '<div class="card"><div class="sectionhead"><h2>What it is</h2>' +
                '<span class="meta"><span class="tag muted">step 1 of 4</span></span></div>' +
                identityFields() + '</div>';
        }
        if (S.step === 2) {
            return '<div class="card"><div class="sectionhead"><h2>What it reads</h2>' +
                '<span class="meta"><span class="tag muted">step 2 of 4</span></span></div>' +
                '<p class="dim" style="margin-bottom:12px;font-size:13px">Five configuration types, and ' +
                'what each supplies. A ' + esc(S.draft.reportType) + ' report needs all five.</p>' +
                configSlots() +
                '<h3 style="margin:18px 0 10px;font-size:14px">The scope</h3>' + scopePicker() +
                '</div>';
        }
        if (S.step === 3) {
            return '<div class="card"><div class="sectionhead"><h2>What it runs</h2>' +
                '<span class="meta"><span class="tag muted">step 3 of 4</span></span></div>' +
                analyticsPicker() + '</div>';
        }
        return '<div class="card"><div class="sectionhead"><h2>Review and save</h2>' +
            '<span class="meta"><span class="tag muted">step 4 of 4</span></span></div>' +
            reviewPanel() + '</div>' + readbackPanel();
    }

    function variantB() {
        var open = openCount();
        return pageHead() + fixtureNote() +
            '<div class="wizard">' + rail() +
            '<div class="wizardbody">' + stepBody() +
            '<div class="card"><div class="wizardfoot">' +
            '<span class="hint">' + (open === 0
                ? 'Nothing is open. The review names every field value.'
                : open + ' item' + (open === 1 ? '' : 's') + ' open, across ' +
                  STEPS.filter(function (step) { return !stepState(step).met; }).length + ' steps.') +
            '</span>' +
            '<span class="right">' +
            '<button class="btn" data-act="step" data-step="' + Math.max(1, S.step - 1) + '"' +
            (S.step === 1 ? ' disabled' : '') + '>Back</button>' +
            '<button class="btn" data-act="step" data-step="' + Math.min(4, S.step + 1) + '"' +
            (S.step === 4 ? ' disabled' : '') + '>Next</button>' +
            '<button class="btn primary" data-act="save"' + (ready() ? '' : ' disabled') +
            '>Save the report</button>' +
            '</div></div></div>' +
            '</div></div>' +
            reachCard() + gapCard();
    }

    function checklistInline(item) {
        if (item.inline === 'name') return '<div class="formgrid">' + nameField() + '</div>';
        if (item.inline === 'description') return '<div class="formgrid">' + descriptionField() + '</div>';
        if (item.inline === 'type') return '<div class="formgrid">' + typeField() + '</div>';
        if (item.inline === 'official') return '<div class="formgrid">' + officialField() + '</div>';
        if (item.inline === 'concurrency') return '<div class="formgrid">' + concurrencyField() + '</div>';
        if (item.inline === 'slot') return configSlots(item.slot);
        if (item.inline === 'analytics') return analyticsPicker(false);
        if (item.inline === 'pfe') {
            return '<div class="rule">' + esc(PFE_RULE) + '</div>' +
                (pfeWithoutSimulation()
                    ? '<div style="margin-top:8px;display:flex;gap:8px;flex-wrap:wrap">' +
                      '<button class="btn small" data-act="toggle-analytic" data-code="simulation">' +
                      'Turn simulation on</button>' +
                      '<button class="btn small" data-act="toggle-analytic" data-code="pfe">Turn pfe off</button>' +
                      '</div>'
                    : '');
        }
        if (item.inline === 'settings') {
            return '<p class="dim" style="font-size:13px">Every setting is edited from the picker ' +
                'above, with Settings beside the analytic that carries it.</p>';
        }
        if (item.inline === 'scope') return scopePicker();
        if (item.inline === 'review') return reviewPanel();
        return '';
    }

    var CHANGEABLE = ['name', 'description', 'type', 'official', 'concurrency', 'slot', 'scope'];

    function variantC() {
        var list = checks();
        var met = list.filter(function (item) { return item.met; }).length;
        var body = list.map(function (item) {
            var mark = item.met ? '\u2713' : '';
            var changeable = item.met && CHANGEABLE.indexOf(item.inline) !== -1;
            var showInline = !item.met || item.id === 'review' || item.id === 'analytics' ||
                item.id === 'pfe' || S.openInline[item.id] === true;
            return '<div class="chk ' + (item.met ? 'met' : 'bad') + '">' +
                '<span class="mark">' + mark + '</span>' +
                '<div><div class="what">' + esc(item.what) + '</div>' +
                '<div class="why">' + esc(item.why) + '</div></div>' +
                '<div class="state"><span class="tag ' + (item.met ? 'ok' : 'bad') + '">' +
                (item.met ? 'met' : 'not met') + '</span>' +
                '<span class="dim" style="font-size:12px">' + esc(item.state) + '</span>' +
                (changeable
                    ? '<button class="btn small ghost" data-act="change" data-id="' + esc(item.id) + '">' +
                      (S.openInline[item.id] ? 'Hide' : 'Change') + '</button>'
                    : '') +
                '</div>' +
                (showInline ? '<div class="inline">' + checklistInline(item) + '</div>' : '') +
                '</div>';
        }).join('');

        return pageHead() + fixtureNote() +
            '<div class="card"><div class="sectionhead"><h2>The list a report must satisfy</h2>' +
            '<span class="meta"><span class="tag ' + (met === list.length ? 'ok' : 'bad') + '">' +
            met + ' of ' + list.length + ' met</span>' +
            '<span class="mono" style="font-size:11px">the definition is the product of this list</span>' +
            '</span></div>' +
            '<p class="dim" style="font-size:13px">Every item the screen can check is here, in the ' +
            'order it is decided, and the fields sit under the item they satisfy. ' +
            esc(CHECKLIST_WAIVER) + '</p>' +
            '<div class="progressbar"><span style="width:' + Math.round(100 * met / list.length) +
            '%"></span></div>' +
            '<div class="checklist" style="margin-top:14px">' + body + '</div>' +
            '<div style="display:flex;gap:8px;margin-top:16px;flex-wrap:wrap">' +
            '<button class="btn primary" data-act="save"' + (ready() ? '' : ' disabled') +
            '>Save the report</button>' +
            '<button class="btn" data-act="discard">Start again</button>' +
            '<span class="hint">' + (ready() ? 'Everything is met.'
                : 'A save is refused while an item is not met.') + '</span>' +
            '</div></div>' +
            readbackPanel() + reachCard() + gapCard();
    }

    // -------------------------------------------------------------- dialog

    function settingsDialog() {
        if (!S.dialog) return '';
        var row = analyticsRow(S.dialog.code);
        if (!row) return '';
        return '<div class="backdrop"><div class="dialog" role="dialog" aria-modal="true">' +
            '<h2>Settings of <span class="mono">' + esc(row.code) + '</span></h2>' +
            '<p class="confirmline">One row per setting: a <b>parameter_name</b> and a <b>value</b>, in ' +
            'positions. The values below are the ACME seeder\u2019s, and a fixture.</p>' +
            '<div class="paramgrid">' +
            '<span class="head">parameter_name</span><span class="head">value</span><span class="head">pos</span>' +
            row.params.map(function (param, index) {
                return '<input type="text" data-act="param-name" data-code="' + esc(row.code) +
                    '" data-index="' + index + '" value="' + esc(param.name) + '" aria-label="parameter name ' +
                    (index + 1) + '">' +
                    '<input type="text" data-act="param-value" data-code="' + esc(row.code) +
                    '" data-index="' + index + '" value="' + esc(param.value) + '" aria-label="value ' +
                    (index + 1) + '">' +
                    '<input class="num" type="number" min="1" data-act="param-pos" data-code="' + esc(row.code) +
                    '" data-index="' + index + '" value="' + param.position + '" aria-label="position ' +
                    (index + 1) + '">';
            }).join('') +
            '</div>' +
            (row.params.length === 0
                ? '<p class="dim">This analytic takes no setting. The picker still offers it.</p>'
                : '') +
            '<div class="actions">' +
            '<button class="btn" data-act="param-add" data-code="' + esc(row.code) + '">Add a setting</button>' +
            '<button class="btn primary" data-act="param-close">Done</button>' +
            '</div>' +
            '<p class="hint">A build would write these with ' +
            '<span class="mono">reporting.v1.configuration_analytic_parameters.put</span>. Nothing here ' +
            'is sent.</p>' +
            '</div></div>';
    }

    // --------------------------------------------------------------- bars

    function variant() {
        var found = VARIANTS.filter(function (entry) { return entry.id === S.variant; });
        return found.length === 1 ? found[0] : VARIANTS[0];
    }

    function renderBar() {
        var variantButtons = VARIANTS.map(function (entry) {
            return '<button data-act="variant" data-variant="' + entry.id + '"' +
                (S.variant === entry.id ? ' class="on"' : '') + '>' + esc(entry.id) + '</button>';
        }).join('');

        var stateLine = '<span class="label">variant <b>' + esc(S.variant) + '</b> \u00b7 ' +
            esc(variant().name) + (S.variant === 'B' ? ' \u00b7 step <b>' + S.step + '</b> of 4' : '') +
            ' \u00b7 state: <b>' + esc(S.activity) + '</b> \u00b7 ' + esc(PERSON.name) + ', ' +
            esc(PERSON.role) + ' on party ' + esc(PARTY.code) + '</span>';

        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture clock: <b>' + esc(fixtureClock()) + '</b></span>' +
            '<span>report type: <b>' + esc(S.draft.reportType) + '</b></span>' +
            '<span>name: <b>' + esc(S.draft.name || 'none') + '</b></span>' +
            '<span>slots bound: <b>' + CONFIG_TYPES.filter(function (type) {
                return !!S.draft.binds[type.code];
            }).length + ' of 5</b></span>' +
            '<span>analytics on: <b>' + activeAnalytics().length + ' of 14</b></span>' +
            '<span>scope: <b>' + esc(S.draft.scope || 'none') + '</b></span>' +
            '<span>checklist open: <b>' + openCount() + '</b></span>' +
            '<span>pfe without simulation: <b>' + (pfeWithoutSimulation() ? 'yes' : 'no') + '</b></span>' +
            '</div>' +
            '<p class="state-note">One party, because a definition belongs to one party. The ' +
            'configurations, their scopes and every analytic setting are a fixture. Nothing reads a ' +
            'server, no operation writes the scope, the pfe rule lives in the seeder, and nothing ' +
            'answers what a report needs beyond its configuration types.</p>' +
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
            'PROTOTYPE \u00b7 fixture data, nothing reads a server, nothing is written \u00b7 variant ' +
            S.variant + ' \u00b7 ' + S.activity + ' \u00b7 ' + PERSON.name + ', ' + PERSON.role +
            ', party ' + PARTY.code;
    }

    function render() {
        var body = S.variant === 'A' ? variantA() : (S.variant === 'B' ? variantB() : variantC());
        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() + '<main>' + body + '</main></div>' + settingsDialog();
        renderNote();
        renderBar();
    }

    // ------------------------------------------------------------ actions

    function setActivity(text) {
        S.activity = text;
    }

    function logStep(text) {
        S.log.unshift(text);
        if (S.log.length > 12) S.log.pop();
    }

    function toggleAnalytic(code) {
        var row = analyticsRow(code);
        if (!row) {
            var type = analyticsOf(code);
            var highest = S.draft.analytics.reduce(function (acc, entry) {
                return Math.max(acc, entry.order);
            }, 0);
            S.draft.analytics.push({
                code: code,
                active: true,
                order: highest + 1,
                params: type.params.map(function (pair, index) {
                    return { name: pair[0], value: pair[1], position: index + 1 };
                })
            });
            logStep('turned ' + code + ' on \u2014 ' + (type.params.length === 0
                ? 'it takes no setting'
                : 'it carries ' + type.params.length + ' setting' + (type.params.length === 1 ? '' : 's')));
        } else {
            row.active = !row.active;
            logStep('set ' + code + ' active ' + row.active + ' \u2014 the row stays, nothing is deleted');
        }
        S.touchedAnalytics = true;
        setActivity(pfeWithoutSimulation()
            ? 'pfe on without simulation: a save is refused'
            : (S.draft.name ? 'editing ' + S.draft.name : 'new, nothing written'));
        render();
        writeParams();
    }

    function loadAnalytics(config) {
        if (!config) {
            S.draft.analytics = [];
            return;
        }
        S.draft.analytics = copyAnalytics(config);
    }

    function bindSlot(slot, id) {
        S.draft.binds[slot] = id || null;
        var config = configById(id);
        if (slot === 'risk_report_config') {
            loadAnalytics(config);
            S.draft.scope = config ? config.scope : null;
            logStep('bound ' + (config ? config.name : 'nothing') + ' in risk_report_config \u2014 it ' +
                'carries ' + (config ? 'the analytic set and the scope ' + config.scope : 'nothing'));
        } else {
            logStep('bound ' + (config ? config.name : 'nothing') + ' in ' + slot);
        }
        setActivity(pfeWithoutSimulation()
            ? 'pfe on without simulation: a save is refused'
            : 'editing ' + (S.draft.name || 'a new report'));
        render();
        writeParams();
    }

    function chooseScope(name) {
        S.draft.scope = name || null;
        if (!name) {
            logStep('cleared the scope');
            render();
            writeParams();
            return;
        }
        var portfolio = portfolioOf(name);
        var current = configById(S.draft.binds.risk_report_config);
        if (!current || portfolio.scopedBy.indexOf(current.id) === -1) {
            var replacement = configById(portfolio.scopedBy[0]);
            S.draft.binds.risk_report_config = replacement.id;
            loadAnalytics(replacement);
            logStep('chose ' + name + ' \u2014 and bound ' + replacement.name + ' in risk_report_config, ' +
                'because the scope is a column of the configuration and not of the definition');
            setActivity('scope ' + name + ' \u00b7 ' + replacement.name);
        } else {
            logStep('chose ' + name + ', already carried by ' + current.name);
            setActivity('scope ' + name);
        }
        render();
        writeParams();
    }

    function save() {
        var open = checks().filter(function (item) { return !item.met; });
        if (open.length > 0) {
            logStep('save refused: ' + open.map(function (item) { return item.id; }).join(', '));
            setActivity('save refused');
            render();
            return;
        }
        var config = boundRiskConfig();
        var rows = S.draft.analytics;
        S.saved = {
            id: 'rpt-' + (0x1000000 + Math.floor(Math.random() * 0xeffffff)).toString(16),
            name: S.draft.name,
            reportType: S.draft.reportType,
            official: S.draft.official,
            concurrency: S.draft.concurrency,
            scope: S.draft.scope,
            bindings: bindingRows().map(function (row) { return [row[0], row[1]]; }),
            analytics: rows.length + ' rows on ' + (config ? config.name : 'no configuration'),
            params: parameterRows().length + ' settings'
        };
        logStep('would send reporting.v1.report_definitions.put for ' + S.draft.name +
            ' \u2014 nothing is sent');
        logStep('would send reporting.v1.report_configurations.put for five slots');
        logStep('would send reporting.v1.configuration_analytics.put_many for ' + rows.length +
            ' rows on ' + (config ? config.name : 'no configuration'));
        logStep('would send reporting.v1.configuration_analytic_parameters.put for ' +
            parameterRows().length + ' settings');
        if (config && config.boundBy > 0) {
            logStep(config.boundBy + ' other definitions bind ' + config.name +
                ', and run the analytic set this save wrote');
        }
        setActivity('saved (nothing sent) \u00b7 never run');
        render();
        writeParams();
    }

    function discard() {
        S.draft = newDraft();
        S.saved = null;
        S.touchedAnalytics = false;
        S.openInline = {};
        S.step = 1;
        logStep('started again from the fixture');
        setActivity('new, nothing written');
        render();
        writeParams();
    }

    // --------------------------------------------------------------- boot

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var wanted = (params.get('variant') || '').toUpperCase();
        if (VARIANTS.some(function (entry) { return entry.id === wanted; })) S.variant = wanted;
        var step = parseInt(params.get('step'), 10);
        if (step >= 1 && step <= 4) S.step = step;
        if (params.get('pfe') === 'without-simulation') {
            S.draft.binds.risk_report_config = 'cfg-rrc-exposure-tb';
            loadAnalytics(configById('cfg-rrc-exposure-tb'));
            S.draft.scope = 'Trading Book';
            var simulation = analyticsRow('simulation');
            if (simulation) simulation.active = false;
            S.touchedAnalytics = true;
            setActivity('pfe on without simulation: a save is refused');
        }
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('variant', S.variant);
        if (S.variant === 'B') params.set('step', String(S.step));
        if (pfeWithoutSimulation() && paramSeedBroken()) params.set('pfe', 'without-simulation');
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    /* Whether the broken pair came from the URL, so a person who turns
       simulation on does not have the parameter put back on a reload. */
    function paramSeedBroken() {
        return S.seededBroken === true;
    }

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        if (element.tagName !== 'BUTTON') {
            if (act === 'noop') event.preventDefault();
            return;
        }
        event.preventDefault();
        var code = element.getAttribute('data-code');

        if (act === 'variant') {
            S.variant = element.getAttribute('data-variant');
            render();
            writeParams();
        } else if (act === 'step') {
            S.variant = 'B';
            S.step = parseInt(element.getAttribute('data-step'), 10);
            setActivity(S.step === 4 ? 'reviewing what will be written' : 'step ' + S.step + ' of 4');
            render();
            writeParams();
        } else if (act === 'change') {
            var id = element.getAttribute('data-id');
            S.openInline[id] = !S.openInline[id];
            render();
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
        } else if (act === 'toggle-analytic') {
            toggleAnalytic(code);
        } else if (act === 'params') {
            S.dialog = { code: code };
            setActivity('editing the settings of ' + code);
            render();
        } else if (act === 'param-close') {
            S.dialog = null;
            render();
        } else if (act === 'param-add') {
            var row = analyticsRow(code);
            row.params.push({ name: '', value: '', position: row.params.length + 1 });
            logStep('added a setting to ' + code);
            render();
        } else if (act === 'save') {
            save();
        } else if (act === 'discard') {
            discard();
        } else if (act === 'leave') {
            logStep('left ' + (S.saved ? S.saved.name : 'the definition') + ' on demand \u2014 the ' +
                'schedule journey is where a recurrence starts');
            setActivity('left on demand');
            render();
        } else if (act === 'schedule') {
            logStep('would hand ' + (S.saved ? S.saved.name : 'the definition') +
                ' to Choose when a report runs \u2014 nothing is sent');
            setActivity('handed to the schedule journey');
            render();
        }
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        var act = element.getAttribute('data-act');
        var code = element.getAttribute('data-code');
        var index = parseInt(element.getAttribute('data-index'), 10);

        if (act === 'name') {
            S.draft.name = element.value;
            S.saved = null;
            render();
            writeParams();
        } else if (act === 'description') {
            S.draft.description = element.value;
            S.saved = null;
            render();
            writeParams();
        } else if (act === 'type') {
            S.draft.reportType = element.value;
            logStep('report type ' + element.value + ' \u2014 it decides what the report is');
            render();
            writeParams();
        } else if (act === 'official') {
            S.draft.official = element.checked;
            logStep('is_official ' + element.checked);
            render();
            writeParams();
        } else if (act === 'concurrency') {
            S.draft.concurrency = element.value;
            logStep('concurrency_policy ' + element.value);
            render();
            writeParams();
        } else if (act === 'bind') {
            bindSlot(element.getAttribute('data-slot'), element.value);
        } else if (act === 'scope') {
            chooseScope(element.value);
        } else if (act === 'order') {
            var ordered = analyticsRow(code);
            if (ordered) {
                ordered.order = Math.max(1, parseInt(element.value, 10) || 1);
                logStep('display_order of ' + code + ' is ' + ordered.order);
            }
            render();
        } else if (act === 'param-name' || act === 'param-value' || act === 'param-pos') {
            var row = analyticsRow(code);
            if (!row || !row.params[index]) return;
            if (act === 'param-name') row.params[index].name = element.value;
            if (act === 'param-value') row.params[index].value = element.value;
            if (act === 'param-pos') row.params[index].position = Math.max(1, parseInt(element.value, 10) || 1);
            S.saved = null;
            render();
        }
    });

    S.draft = newDraft();
    S.seededBroken = new URLSearchParams(window.location.search).get('pfe') === 'without-simulation';
    readParams();
    render();
    writeParams();
}());

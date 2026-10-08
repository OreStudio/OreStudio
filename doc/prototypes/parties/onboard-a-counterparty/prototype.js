/* Onboard a counterparty journey prototype. Self-contained: plain JavaScript,
 * mock data, no framework, no build step, and nothing that outlives the page.
 *
 * The journey the tenant administrator walks: the counterparty list, then the
 * onboarding steps, then the outcome. Every field comes from a modelling file
 * under projects/ores.refdata/modeling/, and every operation drawn on a screen
 * comes from the generated protocol header under
 * projects/ores.refdata/api/include/ores.refdata.api/messaging/. Nothing is
 * invented; a design gap is drawn and marked with a proto-hint.
 *
 * States are chosen from the query string, as the navigation prototype does:
 *   ?state=landing|counterparty|identifiers|contacts|agreements|review|done|history|refused
 *   ?list=active|closed|all                the list tab on the landing screen
 *   ?query=Northwind                       the search, as typed
 *   ?cp=NWCAP                              the counterparty, already on board
 *   ?scheme=LEI                            the identifier scheme under the cursor
 *   ?value=549300NWCAPITAL00001            the identifier value, as typed
 *   ?contact=Legal                         the contact type under the cursor
 *   ?agreement=ISDA-2026-014               the netting agreement under the cursor
 *   ?set=NS-NWCAP-IRS                      the netting set under the cursor
 *   ?fail=duplicate|missing|duplicate_set_id   the server's refusal; ?fail=1 is the first kind
 *   ?refuse=duplicate                      an alias of ?fail
 *   ?version=3                             the history version to read
 * The bar mirrors the same states as buttons. */

(function () {
    'use strict';

    /* --------------------------------------------------------------- mock data */

    var PARTY_ID_SCHEMES = ['LEI', 'BIC', 'MIC', 'NATIONAL_ID', 'CEDB',
        'NATURAL_PERSON', 'ACER', 'DTCC_PARTICIPANT_ID', 'MPID', 'INTERNAL'];
    var NETTING_SET_SCHEMES = ['ORE', 'INTERNAL'];
    var PARTY_TYPES = ['Bank', 'Corporate', 'Fund', 'Insurance', 'Sovereign', 'CentralCounterparty'];
    var PARTY_STATUSES = ['Active', 'Closed', 'Pending'];
    var CONTACT_TYPES = ['Legal', 'Operations', 'Settlement', 'Billing'];
    var AGREEMENT_TYPES = ['ISDA Master Agreement', 'GMRA', 'GMSLA', 'Cleared Derivatives Execution Agreement'];
    var GOVERNING_LAWS = ['English law', 'New York law', 'French law', 'German law', 'Japanese law'];
    var CURRENCY_CODES = ['EUR', 'USD', 'GBP', 'JPY', 'CHF'];
    var BILATERALS = ['Bilateral', 'CallOnly', 'PostOnly'];

    var BUSINESS_CENTRES = [
        { code: 'GBLO', city_name: 'London', country_alpha2_code: 'GB' },
        { code: 'USNY', city_name: 'New York', country_alpha2_code: 'US' },
        { code: 'FRPA', city_name: 'Paris', country_alpha2_code: 'FR' },
        { code: 'DEFF', city_name: 'Frankfurt', country_alpha2_code: 'DE' },
        { code: 'JPTO', city_name: 'Tokyo', country_alpha2_code: 'JP' },
        { code: 'SGSN', city_name: 'Singapore', country_alpha2_code: 'SG' },
        { code: 'WRLD', city_name: 'Worldwide', country_alpha2_code: '' }
    ];

    var PARTIES = [
        { id: 'p-ores', short_name: 'Ore Capital Markets', full_name: 'Ore Capital Markets Ltd', centre: 'GBLO' },
        { id: 'p-oref', short_name: 'Ore Funding', full_name: 'Ore Funding plc', centre: 'GBLO' }
    ];

    var LIST_ROWS = [
        {
            code: 'NWCAP', name: 'Northwind Capital Ltd', type: 'Bank', status: 'Active',
            centre: 'GBLO', version: 4, modified_by: 'a.tanaka', recorded_at: '2026-10-06 09:14',
            transliterated: '', parent: '', ids: [
                { scheme: 'LEI', value: '549300NWCAPITAL00001' },
                { scheme: 'BIC', value: 'NWCAGB2L' }],
            contacts: [{ type: 'Legal' }, { type: 'Operations' }]
        },
        {
            code: 'MRDN', name: 'Meridian Bank AG', type: 'Bank', status: 'Active',
            centre: 'DEFF', version: 2, modified_by: 'j.smith', recorded_at: '2026-09-28 16:02',
            transliterated: '', parent: '', ids: [{ scheme: 'LEI', value: '529900MRIDIANBANK7' }],
            contacts: [{ type: 'Legal' }]
        },
        {
            code: 'HLDN', name: 'Holding North America Inc', type: 'Corporate', status: 'Closed',
            centre: 'USNY', version: 7, modified_by: 'm.okafor', recorded_at: '2026-08-11 11:41',
            transliterated: '', parent: '', ids: [], contacts: []
        }
    ];

    var STEP_ORDER = ['counterparty', 'identifiers', 'contacts', 'agreements', 'review'];

    var STEPS = [
        { id: 'counterparty', title: 'Identity', lead: 'The legal entity and where it is registered.' },
        { id: 'identifiers', title: 'Identifiers', lead: 'How the tenant recognises this counterparty.' },
        { id: 'contacts', title: 'Contacts', lead: 'Who to reach, and where.' },
        { id: 'agreements', title: 'Legal agreements', lead: 'The netting agreements, their netting sets, and the collateral terms.' },
        { id: 'review', title: 'Review', lead: 'Nothing is written until you confirm.' },
        { id: 'done', title: 'Outcome', lead: '', final: true },
        { id: 'history', title: 'History', lead: 'Who changed it, when, and the field-level difference from the version before.', final: true }
    ];

    var RAIL = ['landing', 'counterparty', 'identifiers', 'contacts', 'agreements', 'review', 'done'];

    /* Every state the bar can reach, in the order the bar lists them. */
    var BAR_STATES = ['landing', 'counterparty', 'identifiers', 'contacts', 'agreements',
        'review', 'done', 'history', 'refused'];

    /* The first kind ?fail=1 selects. */
    var FAIL_KINDS = ['duplicate', 'missing', 'duplicate_set_id'];

    /* The refusals the server can return, in the words the models state. */
    var REFUSALS = {
        missing: {
            on: 'identifiers',
            title: 'A counterparty needs one authoritative identifier.',
            subject: 'Northwind Capital Ltd',
            detail: ['INSERT INTO ores_refdata_counterparty_identifiers_tbl \u2014 ' +
                'no identifier under a scheme the tenant treats as authoritative'],
            actions: ['Add an LEI, or state why the counterparty has none', 'Cancel this onboarding']
        },
        duplicate: {
            on: 'identifiers',
            title: 'That identifier is already on this counterparty.',
            subject: 'LEI 549300NWCAPITAL00001',
            detail: ['tenant_id, id_scheme, id_value is already taken on NWCAP ' +
                '(counterparty_id, id_scheme, id_value is the natural key)',
                'An ORE alias is unique across the tenant: id_scheme = \'ORE\''],
            actions: ['Correct the value', 'Keep the identifier already stored']
        },
        duplicate_set_id: {
            on: 'agreements',
            title: 'That netting set code is already in use.',
            subject: 'NS-NWCAP-IRS',
            detail: ['code is the natural key of ores_refdata_netting_sets_tbl',
                'The ORE id is a row of its own: netting_set_id, id_scheme = \'ORE\', id_value'],
            actions: ['Choose another code', 'Open the set already stored']
        }
    };

    var HISTORY = [
        {
            version: 4, actor: 'a.tanaka', at: '2026-10-06 09:14',
            reason: 'Opened NS-NWCAP-IRS under the ISDA agreement',
            changes: [
                ['Netting set', '\u2014', 'NS-NWCAP-IRS (added)'],
                ['Netting set identifier', '\u2014', 'ORE NS-NWCAP-IRS (added)']
            ],
            identifiers: [{ scheme: 'LEI', value: '549300NWCAPITAL00001' }],
            contacts: [{ type: 'Legal' }, { type: 'Operations' }]
        },
        {
            version: 3, actor: 'a.tanaka', at: '2026-10-06 09:02',
            reason: 'Onboarding, step 4: the ISDA agreement recorded',
            changes: [
                ['Business center', 'WRLD', 'GBLO'],
                ['Netting agreement', '\u2014', 'ISDA-2026-014 (added)'],
                ['Governing law', '\u2014', 'English law']
            ],
            identifiers: [{ scheme: 'LEI', value: '549300NWCAPITAL00001' }],
            contacts: [{ type: 'Legal' }, { type: 'Operations' }]
        },
        {
            version: 2, actor: 'j.smith', at: '2026-10-05 17:31',
            reason: 'Onboarding, step 3: the operations contact added',
            changes: [['Contact (Operations)', '\u2014', 'ops@northwind.example (added)']],
            identifiers: [{ scheme: 'LEI', value: '549300NWCAPITAL00001' }],
            contacts: [{ type: 'Legal' }, { type: 'Operations' }]
        },
        {
            version: 1, actor: 'j.smith', at: '2026-10-05 17:20',
            reason: 'Onboarding, step 2: the LEI recorded',
            changes: [
                ['Short code', '\u2014', 'NWCAP'],
                ['Full name', '\u2014', 'Northwind Capital'],
                ['Identifier (LEI)', '\u2014', '549300NWCAPITAL00001 (added)']
            ],
            identifiers: [{ scheme: 'LEI', value: '549300NWCAPITAL00001' }],
            contacts: [{ type: 'Legal' }]
        }
    ];

    /* ------------------------------------------------------------------- state */

    var S = {
        state: 'landing',
        list: 'active',
        query: '',
        draft: null,
        picked: undefined,
        scheme: 'LEI',
        value: '',
        contact: 'Legal',
        agreement: '',
        set: '',
        fail: '',
        version: 0,
        resolvedParties: ['p-ores'],
        live: false,
        refused: false
    };

    function blankDraft() {
        return {
            short_code: '',
            full_name: '',
            transliterated_name: '',
            party_type: 'Bank',
            status: 'Active',
            business_center_code: 'GBLO',
            parent_counterparty_id: '',
            image_id: '',
            identifiers: [],
            contacts: [
                { type: 'Legal', street_line_1: '', street_line_2: '', city: '', state: '',
                    country_code: '', postal_code: '', phone: '', email: '', web_page: '' },
                { type: 'Operations', street_line_1: '', street_line_2: '', city: '', state: '',
                    country_code: '', postal_code: '', phone: '', email: '', web_page: '' }
            ],
            agreements: [newAgreement('ISDA-2026-014', 'ISDA Master Agreement', 'English law'),
                newAgreement('GMRA-2026-002', 'GMRA', 'English law')],
            newAgreementCode: '',
            newAgreementType: 'ISDA Master Agreement',
            newAgreementLaw: 'English law',
            newAgreementDescription: ''
        };
    }

    function newSet(code) {
        return {
            code: code,
            call_type: 'Bilateral',
            initial_margin_type: 'Bilateral',
            risk_weight: '',
            description: '',
            identifiers: [],
            csa: {
                is_active: true, bilateral: 'Bilateral', csa_currency: 'EUR', index_name: 'EUR-EONIA',
                threshold_pay: '', threshold_receive: '', minimum_transfer_amount_pay: '',
                minimum_transfer_amount_receive: '', independent_amount_held: '',
                independent_amount_type: '', call_frequency: '1D', post_frequency: '1D',
                margin_period_of_risk: '2W', collateral_compounding_spread_receive: '',
                collateral_compounding_spread_pay: '', apply_initial_margin: false,
                initial_margin_type: '', calculate_im_amount: false, calculate_vm_amount: false,
                non_exempt_im_regulations: ''
            },
            eligible: [{ currency_code: 'EUR', position: 0 }]
        };
    }

    function newAgreement(number, type, law) {
        return {
            agreement_number: number, agreement_type: type, governing_law: law, description: '',
            sets: [newSet('NS-NWCAP-IRS')]
        };
    }

    /* An identifier list for a row that is already on board. */
    function identifiersFor(row) {
        return row.ids.map(function (id) {
            return { scheme: id.scheme, value: id.value, description: '' };
        });
    }

    function loadRow(row) {
        var d = blankDraft();
        d.short_code = row.code;
        d.full_name = row.name;
        d.transliterated_name = row.transliterated;
        d.party_type = row.type;
        d.status = row.status;
        d.business_center_code = row.centre;
        d.parent_counterparty_id = '';
        d.identifiers = identifiersFor(row);
        d.contacts = row.contacts.map(function (c) {
            return blankContact(c.type);
        });
        return d;
    }

    function blankContact(type) {
        return { type: type, street_line_1: '', street_line_2: '', city: '', state: '',
            country_code: '', postal_code: '', phone: '', email: '', web_page: '' };
    }

    /* --------------------------------------------------------------- vocabulary */

    function schemeList() {
        return PARTY_ID_SCHEMES;
    }

    function centre(code) {
        return BUSINESS_CENTRES.filter(function (b) { return b.code === code; })[0];
    }

    function partyName(id) {
        var p = PARTIES.filter(function (x) { return x.id === id; })[0];
        return p === undefined ? id : p.short_name;
    }

    function agreementOf(number) {
        if (S.draft === null) return undefined;
        return S.draft.agreements.filter(function (a) {
            return a.agreement_number === number;
        })[0];
    }

    function setOf(number, code) {
        var a = agreementOf(number);
        if (a === undefined) return undefined;
        return a.sets.filter(function (s) { return s.code === code; })[0];
    }

    function currentAgreement() {
        return agreementOf(S.agreement);
    }

    function currentSet() {
        return setOf(S.agreement, S.set);
    }

    /* ---------------------------------------------------------------- utilities */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function mono(value) {
        return '<span class="mono">' + esc(value) + '</span>';
    }

    function dash(value) {
        return value === '' || value === undefined || value === null ? '\u2014' : value;
    }

    function tag(text, tone) {
        return '<span class="tag' + (tone ? ' ' + tone : '') + '">' + esc(text) + '</span>';
    }

    /* A form value that lives outside the counterparty draft, such as the
       fields of an add form that is not yet a row. */
    function formVal(key) {
        return S[key] === undefined ? '' : S[key];
    }

    function formSet(key, value) {
        S[key] = value;
    }

    function field(label, key, value, opts) {
        opts = opts || {};
        var required = opts.required ? ' <span class="req">*</span>' : '';
        var attr = ' data-field="' + key + '"';
        if (opts.scope) attr = ' data-scope="' + opts.scope + '" data-field="' + key + '"';
        var control;
        if (opts.options) {
            control = '<select' + attr + '>' + opts.options.map(function (o) {
                var v = typeof o === 'string' ? o : o.value;
                var l = typeof o === 'string' ? o : o.label;
                return '<option value="' + esc(v) + '"' + (String(value) === String(v) ? ' selected' : '') +
                    '>' + esc(l) + '</option>';
            }).join('') + '</select>';
        } else if (opts.textarea) {
            control = '<textarea' + attr + ' placeholder="' + esc(opts.placeholder || '') + '">' +
                esc(value) + '</textarea>';
        } else {
            control = '<input' + attr + ' value="' + esc(value) + '" placeholder="' +
                esc(opts.placeholder || '') + '"' + (opts.bad ? ' class="bad"' : '') + '>';
        }
        return '<label class="field' + (opts.span ? ' span2' : '') + '">' +
            '<span class="lbl">' + esc(label) + required + '</span>' + control +
            (opts.hint ? '<span class="hint">' + opts.hint + '</span>' : '') + '</label>';
    }

    /* ------------------------------------------------------------- read params */

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var state = p.get('state');
        if (state !== null) S.state = state;
        var list = p.get('list');
        if (list !== null) S.list = list;
        var query = p.get('query');
        if (query !== null) S.query = query;
        var cp = p.get('cp');
        if (cp !== null && cp !== '') {
            var row = rowByCode(cp);
            if (row !== undefined) {
                S.draft = loadRow(row);
                S.picked = cp;
                S.state = p.get('state') === null ? 'counterparty' : p.get('state');
            }
        }
        ['scheme', 'value', 'contact', 'agreement', 'set'].forEach(function (k) {
            if (p.get(k) !== null) S[k] = p.get(k);
        });
        var fail = p.get('fail');
        if (fail === null) fail = p.get('refuse');
        if (fail !== null) S.fail = failParam(fail);
        var version = p.get('version');
        if (version !== null) S.version = parseInt(version, 10) || 0;
        if (S.draft === null) S.draft = blankDraft();
        if (S.agreement === '') {
            S.agreement = S.draft.agreements.length > 0 ? S.draft.agreements[0].agreement_number : '';
        }
        if (S.set === '') {
            var a = currentAgreement();
            S.set = a !== undefined && a.sets.length > 0 ? a.sets[0].code : '';
        }
        if (S.fail !== '' && REFUSALS[S.fail] !== undefined) S.refused = true;
        if (S.state === 'done') S.live = true;
    }

    /* ?fail= accepts a kind by name, or 1 for the first kind. */
    function failParam(raw) {
        var value = String(raw);
        if (REFUSALS[value] !== undefined) return value;
        var n = parseInt(value, 10);
        if (!isNaN(n) && n >= 1 && n <= FAIL_KINDS.length) return FAIL_KINDS[n - 1];
        return value === '1' ? FAIL_KINDS[0] : '';
    }

    function rowByCode(code) {
        return LIST_ROWS.filter(function (r) {
            return r.code.toLowerCase() === String(code).toLowerCase();
        })[0];
    }

    function isListed(code) {
        return rowByCode(code) !== undefined || (S.picked !== undefined &&
            String(S.picked).toLowerCase() === String(code).toLowerCase());
    }

    function listRows() {
        var rows = LIST_ROWS.filter(function (r) {
            if (S.list === 'active') return r.status !== 'Closed';
            if (S.list === 'closed') return r.status === 'Closed';
            return true;
        });
        var q = S.query.trim().toLowerCase();
        if (q === '') return rows;
        return rows.filter(function (r) {
            return r.code.toLowerCase().indexOf(q) >= 0 ||
                r.name.toLowerCase().indexOf(q) >= 0 ||
                r.ids.some(function (i) { return i.value.toLowerCase().indexOf(q) >= 0; });
        });
    }

    /* ------------------------------------------------------- identifier validation */

    function authoritativeIdentifier(ids) {
        for (var i = 0; i < ids.length; i += 1) {
            if (ids[i].scheme === 'LEI') return ids[i];
        }
        for (var j = 0; j < ids.length; j += 1) {
            if (ids[j].scheme === 'BIC') return ids[j];
        }
        return undefined;
    }

    function identifierProblems(d) {
        var out = [];
        if (d.identifiers.length === 0) {
            out.push({ key: 'missing', text: 'No identifier yet. A counterparty needs one authoritative identifier.' });
        } else if (authoritativeIdentifier(d.identifiers) === undefined) {
            out.push({
                key: 'missing',
                text: 'No LEI and no BIC. The tenant treats an LEI, then a BIC, as authoritative.'
            });
        }
        d.identifiers.forEach(function (id) {
            if (id.value.trim() === '') {
                out.push({ key: 'empty', text: 'An identifier has no value.' });
                return;
            }
            var same = d.identifiers.filter(function (x) {
                return x !== id && x.scheme === id.scheme && x.value.trim().toLowerCase() ===
                    id.value.trim().toLowerCase();
            });
            if (same.length > 0) {
                out.push({
                    key: 'duplicate',
                    text: 'This counterparty already holds ' + id.scheme + ' ' + id.value + '. ' +
                        'counterparty_id, id_scheme, id_value is the natural key.'
                });
            }
            if (id.scheme === 'ORE' && oreAliasTaken(id.value)) {
                out.push({
                    key: 'duplicate',
                    text: 'The ORE alias ' + id.value + ' names another counterparty in this ' +
                        'tenant. An ORE alias is unique per tenant.'
                });
            }
        });
        return out;
    }

    function oreAliasTaken(value) {
        var v = value.trim().toLowerCase();
        if (v === '') return false;
        return LIST_ROWS.some(function (r) {
            return r.code.toLowerCase() !== String(S.picked || '').toLowerCase() &&
                r.ids.some(function (i) {
                    return i.scheme === 'ORE' && i.value.toLowerCase() === v;
                });
        });
    }

    function contactProblems(d) {
        var out = [];
        var seen = {};
        d.contacts.forEach(function (c) {
            if (seen[c.type] === true) {
                out.push({
                    key: 'duplicate',
                    text: 'Two contacts of type ' + c.type + '. The natural key is ' +
                        'counterparty_id, contact_type: one contact row per type.'
                });
            }
            seen[c.type] = true;
        });
        return out;
    }

    /* -------------------------------------------------------------- landing view */

    function landing() {
        var rows = listRows();
        var body;
        if (rows.length === 0) {
            body = '<tr><td colspan="6"><div class="empty">No counterparty matches ' +
                mono(S.query) + '.</div></td></tr>';
        } else {
            body = rows.map(function (r) {
                var ids = r.ids.length === 0 ? '<span class="meta">No identifier</span>' :
                    r.ids.map(function (i) {
                        return tag(i.scheme + ' ' + i.value, i.scheme === 'LEI' ? 'accent' : '');
                    }).join(' ');
                var b = centre(r.business_center_code);
                var statusTone = r.status === 'Active' ? 'ok' : 'bad';
                return '<tr class="click" data-act="open" data-code="' + esc(r.code) + '">' +
                    '<td class="mono">' + esc(r.code) + '</td>' +
                    '<td><span class="nm">' + esc(r.name) + '</span></td>' +
                    '<td>' + esc(r.type) + '</td>' +
                    '<td>' + tag(r.status, statusTone) + '</td>' +
                    '<td>' + (b === undefined ? esc(r.business_center_code) :
                        esc(r.business_center_code) + '<span class="sub">' + esc(b.city_name) +
                        '</span>') + '</td>' +
                    '<td>' + ids + '</td>' +
                    '<td><span class="meta">' + esc(r.contacts.length) + ' contact' +
                    (r.contacts.length === 1 ? '' : 's') + '</span></td>' +
                    '<td class="num">' + esc(r.version) + '</td>' +
                    '<td><span class="mono">' + esc(r.modified_by) + '</span>' +
                    '<span class="sub">' + esc(r.recorded_at) + '</span></td></tr>';
            }).join('');
        }
        var tabs = ['active', 'closed', 'all'].map(function (t) {
            return '<button data-act="tab" data-list="' + t + '"' +
                (S.list === t ? ' class="on"' : '') + '>' + t + '</button>';
        }).join('');
        return '<div class="lhead"><div>' +
            '<div class="brand">Reference data \u00b7 counterparties</div>' +
            '<h1 style="margin-top:8px">Counterparties</h1>' +
            '<p class="desc">The legal entities this tenant trades with.</p></div>' +
            '<div class="actions">' +
            '<button class="btn primary" data-act="start">Onboard a counterparty</button>' +
            '</div></div>' +
            '<section class="card">' +
            '<div class="toolbar">' +
            '<div class="grow"><input data-focus="query" data-q="1" ' +
            'placeholder="Search by code, name or identifier value" value="' + esc(S.query) + '"></div>' +
            '<div class="tabs">' + tabs + '</div></div>' +
            '<table class="grid"><thead><tr><th>Code</th><th>Name</th><th>Type</th>' +
            '<th>Status</th><th>Business center</th><th>Identifiers</th><th>Contacts</th>' +
            '<th class="num">Version</th><th>Modified by</th></tr></thead>' +
            '<tbody>' + body + '</tbody></table>' +
            '<p class="proto-hint"><b>Gap:</b> the landing list is one call \u2014 ' +
            mono('refdata.v1.counterparties.list') + ' with offset, limit, order and a filter. ' +
            'The identifier and contact columns read from the owning rows: ' +
            mono('refdata.v1.counterparty_identifiers.list_by_counterparty_id') + ' and ' +
            mono('refdata.v1.counterparty_contact_informations.list_by_counterparty_id') + ', ' +
            'one call per row, because the list response carries the counterparty alone.</p>' +
            '</section>';
    }

    /* ----------------------------------------------------------------- the rail */

    function rail(state) {
        var at = RAIL.indexOf(state);
        if (at < 0) at = 0;
        return '<nav class="railnav" aria-label="Onboarding steps"><ol>' +
            RAIL.map(function (id, i) {
                var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
                var label = id === 'landing' ? 'Counterparties' : STEPS.filter(function (s) {
                    return s.id === id;
                })[0].title;
                return '<li class="railentry ' + cls + '"' + (i === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (i < at ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(label) + '</li>';
            }).join('') + '</ol></nav>';
    }

    function stepHeader() {
        var d = S.draft;
        var crest = d.short_code === '' ? '\u2014' : d.short_code.slice(0, 3).toUpperCase();
        return '<div class="stepheader">' +
            '<span class="crest">' + esc(crest) + '</span><div>' +
            '<div class="nm">' + esc(d.full_name === '' ? 'New counterparty' : d.full_name) + '</div>' +
            '<div class="sub">' + (d.short_code === '' ? 'No short code yet' : mono(d.short_code)) +
            (S.picked !== undefined && isListed(S.picked) ?
                ' \u00b7 already on board, version ' + esc(versionOf(S.picked)) : ' \u00b7 not yet created') +
            '</div></div></div>';
    }

    function versionOf(code) {
        var r = rowByCode(code);
        return r === undefined ? '1' : String(r.version);
    }

    /* ------------------------------------------------------------ identity step */

    function identityStep() {
        var d = S.draft;
        var duplicateCode = LIST_ROWS.some(function (r) {
            return r.code.toLowerCase() === d.short_code.trim().toLowerCase() &&
                r.code.toLowerCase() !== String(S.picked || '').toLowerCase() &&
                d.short_code.trim() !== '';
        });
        var codeBad = duplicateCode || (S.fail === 'duplicate_code');
        var typeOptions = PARTY_TYPES;
        var statusOptions = PARTY_STATUSES;
        var parents = [{ value: '', label: 'No parent' }].concat(LIST_ROWS.map(function (r) {
            return { value: r.code, label: r.name + ' (' + r.code + ')' };
        }));
        var centres = BUSINESS_CENTRES.map(function (b) {
            return { value: b.code, label: b.code + ' \u2014 ' + b.city_name };
        });
        var warn = '';
        if (S.fail === 'duplicate_code') {
            warn = refusalBlock('duplicate_code', 'That short code is taken.',
                d.short_code,
                ['short_code is the key of ores_refdata_counterparties_tbl, unique per tenant'],
                ['Choose another code', 'Open the counterparty already stored']);
        } else if (duplicateCode) {
            warn = '<div class="notice warn"><h3>Short code already used</h3>' +
                'Another counterparty in this tenant holds ' + mono(d.short_code) +
                '. Correct the code before you continue.</div>';
        }
        return warn +
            '<div class="grid2">' +
            field('Short Code', 'short_code', d.short_code, {
                required: true, bad: codeBad, placeholder: 'Enter short code',
                hint: 'The key of the counterparty, unique in the tenant.'
            }) +
            field('Full Name', 'full_name', d.full_name, {
                required: true, placeholder: 'Enter full name',
                hint: 'The registered legal name. It need not be unique: branches share a name under distinct short codes.'
            }) +
            field('Transliterated Name', 'transliterated_name', d.transliterated_name, {
                placeholder: 'Latin-script rendering, when the name is not Latin',
                hint: 'Optional. For a counterparty whose registered name uses another script.'
            }) +
            field('Party Type', 'party_type', d.party_type, {
                options: typeOptions, hint: 'References the <b>party_type</b> lookup table.'
            }) +
            field('Status', 'status', d.status, {
                options: statusOptions, hint: 'References the <b>party_status</b> lookup table. A new counterparty starts Active.'
            }) +
            field('Business Center', 'business_center_code', d.business_center_code, {
                options: centres,
                hint: 'The primary trading location, an FpML-style code. It decides the holiday calendar.'
            }) +
            field('Parent Counterparty', 'parent_counterparty_id', d.parent_counterparty_id, {
                options: parents, span: true,
                hint: 'Optional. Counterparties form a group hierarchy through <b>parent_counterparty_id</b>.'
            }) +
            '</div>' +
            '<div class="panel-soft" style="margin-top:4px">' +
            '<h3>Where it trades</h3>' +
            '<p class="hint" style="margin:0 0 10px">The centre the model carries is one column, ' +
            '<b>business_center_code</b>.</p>' +
            '<div class="centerpick">' + BUSINESS_CENTRES.map(function (b) {
                return '<span class="checkline">' + tag(b.code, b.code === d.business_center_code ? 'accent' : '') +
                    '<span>' + esc(b.city_name) + '</span></span>';
            }).join('') + '</div>' +
            '<p class="proto-hint"><b>Gap:</b> a counterparty carries one business centre. This journey ' +
            'must show every centre it deals through, and there is no counterparty-to-centre row. Either ' +
            'the single column is the answer, or a junction like ' +
            mono('party_country_junction') + ' is needed. A second centre is drawn as a gap, not as a field.</p>' +
            '</div>';
    }

    /* -------------------------------------------------------- identifiers step */

    function identifierRow(id, index) {
        var auth = authoritativeIdentifier(S.draft.identifiers);
        var isAuth = auth !== undefined && auth === id;
        var dup = S.draft.identifiers.some(function (x) {
            return x !== id && x.scheme === id.scheme &&
                x.value.trim().toLowerCase() === id.value.trim().toLowerCase();
        });
        return '<li' + (id.value !== '' && (isAuth || dup) ? ' class="on"' : '') + '>' +
            '<span class="id">' + tag(id.scheme, isAuth ? 'accent' : '') + '</span>' +
            '<span class="grow val">' + (id.value === '' ? '<span class="meta">no value</span>' : esc(id.value)) + '</span>' +
            (isAuth ? tag('authoritative', 'ok') : '') +
            (dup ? tag('duplicate', 'bad') : '') +
            (id.description !== '' ? '<span class="meta">' + esc(id.description) + '</span>' : '') +
            '<button class="btn ghost small" data-act="delid" data-i="' + index + '">Remove</button>' +
            '</li>';
    }

    function identifiersStep() {
        var d = S.draft;
        var problems = identifierProblems(d);
        var auth = authoritativeIdentifier(d.identifiers);
        var warn = '';
        if (S.fail === 'missing') {
            warn = refusalBlock('missing', REFUSALS.missing.title, d.full_name,
                REFUSALS.missing.detail, REFUSALS.missing.actions);
        } else if (S.fail === 'duplicate') {
            warn = refusalBlock('duplicate', REFUSALS.duplicate.title,
                S.scheme + ' ' + (S.value || '549300NWCAPITAL00001'),
                REFUSALS.duplicate.detail, REFUSALS.duplicate.actions);
        } else if (problems.length > 0) {
            warn = '<div class="notice warn"><h3>Not ready to continue</h3><ul>' +
                problems.map(function (p) { return '<li>' + esc(p.text) + '</li>'; }).join('') +
                '</ul></div>';
        } else {
            warn = '<div class="notice info"><h3>Authoritative identifier</h3>' +
                'The system takes the <b>LEI</b> first, then the <b>BIC</b>, as the identifier it ' +
                'resolves this counterparty by. Other schemes are kept and searched, and none of them ' +
                'decides which legal entity this is.</div>';
        }
        var rows = d.identifiers.length === 0 ?
            '<li><span class="meta">No identifier yet.</span></li>' :
            d.identifiers.map(identifierRow).join('');
        var schemes = schemeList().map(function (s) {
            return '<option value="' + esc(s) + '"' + (S.scheme === s ? ' selected' : '') + '>' +
                esc(s) + '</option>';
        }).join('');
        return warn +
            '<ul class="rowlist">' + rows + '</ul>' +
            '<div class="panel-soft" style="margin-top:16px">' +
            '<h3>Add an identifier</h3>' +
            '<div class="grid2">' +
            '<label class="field"><span class="lbl">Scheme</span>' +
            '<select data-pick="scheme">' + schemes + '</select>' +
            '<span class="hint">References the <b>party_id_scheme</b> lookup table. The scheme ' +
            'carries <b>max_cardinality</b> and <b>display_order</b>.</span></label>' +
            field('Value', 'value', S.value, {
                placeholder: 'Enter the identifier value',
                hint: 'counterparty_id, id_scheme, id_value is the natural key.'
            }) +
            '</div>' +
            '<button class="btn small" data-act="addid">Add identifier</button>' +
            '</div>' +
            '<div class="panel-soft" style="margin-top:16px">' +
            '<h3>How this counterparty resolves</h3>' +
            '<table class="datatable"><tbody>' +
            '<tr><th>Authoritative</th><td>' + (auth === undefined ?
                '<span class="meta">none yet</span>' :
                tag(auth.scheme, 'accent') + ' ' + mono(auth.value)) + '</td></tr>' +
            '<tr><th>ORE alias</th><td>' + (d.identifiers.filter(function (i) {
                return i.scheme === 'ORE';
            }).map(function (i) {
                return mono(i.value);
            }).join(', ') || '<span class="meta">none. An ORE document names ' +
                'this counterparty by its short code ' + esc(d.short_code === '' ? '\u2014' : d.short_code) +
                ' until an ORE alias is added.</span>') + '</td></tr>' +
            '<tr><th>Visible to</th><td>' + S.resolvedParties.map(function (p) {
                return tag(partyName(p), 'accent');
            }).join(' ') + ' <span class="meta">\u2014 the parties that may trade with it</span></td></tr>' +
            '</tbody></table>' +
            '<p class="proto-hint"><b>Gap:</b> the model stores no authoritative flag. The precedence ' +
            'above is inferred from the scheme. If the tenant must state which identifier is ' +
            'authoritative, the column does not exist. Visibility is a row of ' +
            mono('ores_refdata_party_counterparties_tbl') + ' and the junction calls ' +
            mono('refdata.v1.party_counterparties.put') + ' and ' +
            mono('refdata.v1.party_counterparties.list_by_party_id') + ' exist; the counterparty ' +
            'protocol has no call that reads them from this side.</p>' +
            '</div>';
    }

    /* ------------------------------------------------------------ contacts step */

    function contactStep() {
        var d = S.draft;
        var problems = contactProblems(d);
        var warn = problems.length === 0 ? '' :
            '<div class="notice warn"><h3>Not ready to continue</h3><ul>' +
            problems.map(function (p) { return '<li>' + esc(p.text) + '</li>'; }).join('') +
            '</ul></div>';
        var seen = {};
        var rows = d.contacts.map(function (c, i) {
            var dup = seen[c.type] === true;
            seen[c.type] = true;
            var where = [c.street_line_1, c.street_line_2, c.city, c.state, c.country_code,
                c.postal_code].filter(function (x) { return x !== ''; }).join(', ');
            return '<li data-act="pickcontact" data-i="' + i + '"' +
                (S.contact === c.type && !dup ? ' class="on"' : '') + '>' +
                '<span class="id">' + tag(c.type, S.contact === c.type && !dup ? 'accent' : '') + '</span>' +
                (dup ? tag('refused: second ' + c.type, 'bad') + ' ' : '') +
                '<span class="grow">' + (where === '' ? '<span class="meta">No address yet</span>' :
                    esc(where)) + '</span>' +
                '<span class="meta">' + esc(c.phone || '\u2014') + '</span>' +
                '<span class="val">' + esc(c.email || '\u2014') + '</span>' +
                '<button class="btn ghost small" data-act="delcontact" data-i="' + i + '">Remove</button>' +
                '</li>';
        }).join('');
        var first = false;
        var c = d.contacts.filter(function (x) {
            if (x.type !== S.contact) return false;
            var isFirst = !first;
            first = true;
            return isFirst;
        })[0];
        var editor = '';
        if (c === undefined) {
            editor = '<div class="notice info">No contact of type ' + esc(S.contact) +
                '. Add one, or pick another type.</div>';
        } else {
            var countries = ['', 'GB', 'US', 'FR', 'DE', 'JP', 'SG'].map(function (x) {
                return { value: x, label: x === '' ? 'Not stated' : x };
            });
            editor = '<div class="grid2" data-scope="contact">' +
                field('Street Line 1', 'street_line_1', c.street_line_1, { placeholder: 'Enter street line 1' }) +
                field('Street Line 2', 'street_line_2', c.street_line_2, { placeholder: 'Enter street line 2' }) +
                field('City', 'city', c.city, { placeholder: 'Enter city' }) +
                field('State', 'state', c.state, { placeholder: 'Enter state' }) +
                field('Country Code', 'country_code', c.country_code, { options: countries }) +
                field('Postal Code', 'postal_code', c.postal_code, { placeholder: 'Enter postal code' }) +
                field('Phone', 'phone', c.phone, { placeholder: 'Enter phone' }) +
                field('Email', 'email', c.email, { placeholder: 'Enter email' }) +
                field('Web Page', 'web_page', c.web_page, { placeholder: 'Enter web page', span: true }) +
                '</div>';
        }
        var types = CONTACT_TYPES.map(function (t) {
            return '<option value="' + esc(t) + '"' + (S.contact === t ? ' selected' : '') + '>' +
                esc(t) + '</option>';
        }).join('');
        return warn +
            '<ul class="rowlist">' + rows + '</ul>' +
            '<div class="panel-soft" style="margin-top:16px">' +
            '<h3>Contact of type ' + esc(S.contact) + '</h3>' +
            '<label class="field"><span class="lbl">Type</span>' +
            '<select data-pick="contact">' + types + '</select>' +
            '<span class="hint">References the <b>contact_type</b> lookup table. The natural key is ' +
            'counterparty_id, contact_type: one contact row per type.</span></label>' +
            editor +
            '<button class="btn small" data-act="addcontact">Add this contact type</button>' +
            '</div>';
    }

    /* ---------------------------------------------------------- agreements step */

    function agreementsStep() {
        var d = S.draft;
        if (d.agreements.length === 0) {
            return '<div class="notice info">No netting agreement yet. A counterparty with no ' +
                'agreement still trades: every trade sits in a netting set of its own, and a set with ' +
                'no agreement holds trades that do not net.</div>' + newAgreementForm();
        }
        var warn = '';
        if (S.fail === 'duplicate_set_id') {
            warn = refusalBlock('duplicate_set_id', REFUSALS.duplicate_set_id.title, S.set,
                REFUSALS.duplicate_set_id.detail, REFUSALS.duplicate_set_id.actions);
        }
        var sets = currentAgreement();
        var body = sets === undefined ? '' : sets.sets.map(function (s) {
            var on = s.code === S.set;
            var ids = s.identifiers.length === 0 ?
                '<span class="meta">No netting set identifier yet. An ORE document names this set ' +
                'by its code ' + mono(s.code) + ' until one is added.</span>' :
                '<div class="chips">' + s.identifiers.map(function (i) {
                    return tag(i.scheme + ' ' + i.value, i.scheme === 'ORE' ? 'accent' : '');
                }).join('') + '</div>';
            var csa = s.csa;
            return '<li' + (on ? ' class="on"' : '') + '>' +
                '<div class="sethead">' +
                '<span class="code mono">' + esc(s.code) + '</span>' +
                (csa.is_active ? tag('CSA active', 'ok') : tag('CSA off', 'warn')) +
                '<span class="grow"></span>' +
                '<button class="btn ghost small" data-act="pickset" data-code="' + esc(s.code) + '">' +
                (on ? 'Editing' : 'Edit') + '</button>' +
                '</div>' +
                '<div class="setbody">' +
                '<table class="datatable"><tbody>' +
                '<tr><th>Names it answers to</th><td>' + ids + '</td></tr>' +
                '<tr><th>Collateral (CSA)</th><td>' + esc(dash(csa.bilateral)) + ' \u00b7 ' +
                esc(dash(csa.csa_currency)) + ' \u00b7 ' + esc(dash(csa.index_name)) + ' \u00b7 MPOR ' +
                esc(dash(csa.margin_period_of_risk)) + '</td></tr>' +
                '<tr><th>Eligible collateral</th><td>' + (csa === undefined || s.eligible.length === 0 ?
                    '<span class="meta">none</span>' :
                    '<div class="chips">' + s.eligible.map(function (e) {
                        return tag(e.currency_code + ' \u00b7 position ' + e.position);
                    }).join('') + '</div>') + '</td></tr>' +
                '<tr><th>Call type / IM type</th><td>' + esc(dash(s.call_type)) + ' / ' +
                esc(dash(s.initial_margin_type)) + '</td></tr>' +
                '<tr><th>Risk weight</th><td>' + esc(dash(s.risk_weight)) + '</td></tr>' +
                '</tbody></table>' +
                (on ? setEditor(s) : '') +
                '</div></li>';
        }).join('');
        return warn +
            '<div class="panel-soft">' +
            '<h3>Netting agreement ' + mono(S.agreement) + '</h3>' +
            '<table class="datatable"><tbody>' +
            '<tr><th>Agreement number</th><td class="mono">' + esc(sets === undefined ? '' : sets.agreement_number) + '</td></tr>' +
            '<tr><th>Type</th><td>' + esc(sets === undefined ? '' : sets.agreement_type) + '</td></tr>' +
            '<tr><th>Governing law</th><td>' + esc(sets === undefined ? '' : dash(sets.governing_law)) + '</td></tr>' +
            '<tr><th>Parties</th><td>' + tag(partyName('p-ores'), 'accent') + ' ' +
            tag(d.full_name === '' ? 'this counterparty' : d.full_name, 'accent') +
            ' <span class="meta">\u2014 both columns are fixed and never change across versions</span></td></tr>' +
            '<tr><th>Description</th><td>' + esc(sets === undefined ? '' : dash(sets.description)) + '</td></tr>' +
            '</tbody></table></div>' +
            '<div class="chips" style="margin:14px 0">' + d.agreements.map(function (a) {
                return '<button class="btn small' + (a.agreement_number === S.agreement ? ' primary' : '') +
                    '" data-act="pickagreement" data-number="' + esc(a.agreement_number) + '">' +
                    esc(a.agreement_number) + '</button>';
            }).join('') + '</div>' +
            '<h2 style="font-size:14px">Netting sets under this agreement</h2>' +
            '<ul class="sets">' + body + '</ul>' +
            newSetForm() + newAgreementForm() +
            '<p class="proto-hint"><b>Gap:</b> the agreements and the sets are written as one ' +
            'composite over ' + mono('refdata.v1.netting_agreements.put') + ', ' +
            mono('refdata.v1.netting_sets.put') + ' and ' +
            mono('refdata.v1.netting_set_identifiers.put') + '; there is no call that stages them ' +
            'together, and no call reads a counterparty\u2019s agreements and sets in one reply. The ' +
            'CSA belongs to the set, not to the counterparty: ' +
            mono('refdata.v1.csas.list_by_netting_set_id') + ' reads it.</p>';
    }

    function setEditor(s) {
        var c = s.csa;
        var bil = BILATERALS.map(function (b) { return { value: b, label: b }; });
        var currencies = [''].concat(CURRENCY_CODES).map(function (x) {
            return { value: x, label: x === '' ? 'Not stated' : x };
        });
        return '<div class="panel-soft" style="margin-top:12px">' +
            '<h3>Identifiers of ' + mono(s.code) + '</h3>' +
            '<ul class="rowlist">' + (s.identifiers.length === 0 ?
                '<li><span class="meta">None yet.</span></li>' :
                s.identifiers.map(function (i, k) {
                    return '<li><span class="id">' + tag(i.scheme, 'accent') + '</span>' +
                        '<span class="grow val">' + esc(i.value) + '</span>' +
                        '<button class="btn ghost small" data-act="delsetid" data-i="' + k + '">Remove</button></li>';
                }).join('')) + '</ul>' +
            '<div class="grid2" style="margin-top:10px">' +
            '<label class="field"><span class="lbl">Scheme</span><select data-pick="setscheme">' +
            NETTING_SET_SCHEMES.map(function (x) {
                return '<option value="' + esc(x) + '"' + (S.setscheme === x ? ' selected' : '') + '>' +
                    esc(x) + '</option>';
            }).join('') + '</select>' +
            '<span class="hint">A set uses the schemes that name things inside one system.</span></label>' +
            field('Value', 'setidvalue', formVal('setidvalue'), {
                placeholder: 'CPTY_A, as the ORE document writes it'
            }) +
            '</div><button class="btn small" data-act="addsetid">Add netting set id</button>' +
            '<div class="grid2" style="margin-top:12px">' +
            field('Description', 'description', s.description, {
                scope: 'set', span: true, placeholder: 'Not stated'
            }) +
            '</div>' +
            '<h3 style="margin-top:18px">Collateral terms (CSA)</h3>' +
            '<div class="grid2">' +
            field('Bilateral', 'csa_bilateral', c.bilateral, { options: bil }) +
            field('Currency', 'csa_currency', c.csa_currency, { options: currencies }) +
            field('Index', 'csa_index_name', c.index_name, { placeholder: 'EUR-EONIA' }) +
            field('MPOR', 'csa_mpor', c.margin_period_of_risk, { placeholder: 'Period, such as 2W' }) +
            field('Threshold, pay', 'csa_threshold_pay', c.threshold_pay, { placeholder: 'Not stated' }) +
            field('Threshold, receive', 'csa_threshold_receive', c.threshold_receive, { placeholder: 'Not stated' }) +
            field('MTA, pay', 'csa_mta_pay', c.minimum_transfer_amount_pay, { placeholder: 'Not stated' }) +
            field('MTA, receive', 'csa_mta_receive', c.minimum_transfer_amount_receive, { placeholder: 'Not stated' }) +
            field('Independent amount', 'csa_independent_amount', c.independent_amount_held, { placeholder: 'Not stated' }) +
            field('Independent amount type', 'csa_independent_amount_type', c.independent_amount_type, { options: ['', 'FIXED'] }) +
            field('Call frequency', 'csa_call_frequency', c.call_frequency, { placeholder: '1D' }) +
            field('Post frequency', 'csa_post_frequency', c.post_frequency, { placeholder: '1D' }) +
            field('Compounding spread, receive', 'csa_spread_receive', c.collateral_compounding_spread_receive, { placeholder: 'Not stated' }) +
            field('Compounding spread, pay', 'csa_spread_pay', c.collateral_compounding_spread_pay, { placeholder: 'Not stated' }) +
            field('Non-exempt IM regulations', 'csa_non_exempt', c.non_exempt_im_regulations, { placeholder: 'Not stated', span: true }) +
            '</div>' +
            '<div class="span2">' +
            '<label class="checkline"><input type="checkbox" data-check="csa_is_active"' +
            (c.is_active ? ' checked' : '') + '> CSA in force (is_active)</label>' +
            '<label class="checkline"><input type="checkbox" data-check="csa_apply_im"' +
            (c.apply_initial_margin ? ' checked' : '') + '> Apply initial margin</label>' +
            '<label class="checkline"><input type="checkbox" data-check="csa_calc_im"' +
            (c.calculate_im_amount ? ' checked' : '') + '> Calculate the IM amount</label>' +
            '<label class="checkline"><input type="checkbox" data-check="csa_calc_vm"' +
            (c.calculate_vm_amount ? ' checked' : '') + '> Calculate the VM amount</label>' +
            '</div>' +
            '<h3 style="margin-top:18px">Eligible collateral</h3>' +
            '<div class="chips">' + s.eligible.map(function (e) {
                return tag(e.currency_code + ' \u00b7 position ' + e.position);
            }).join('') + '</div>' +
            '<p class="hint">Each currency is a row of its own, and the position keeps the order ' +
            'ORE lists them in. A set has at most one active CSA; an inactive one keeps its terms.</p>' +
            '</div>';
    }

    function newSetForm() {
        return '<details class="panel-soft" style="margin-top:14px">' +
            '<summary style="cursor:pointer;font-size:13px;font-weight:600">Add a netting set</summary>' +
            '<div class="grid2" style="margin-top:12px">' +
            field('Code', 'newset_code', formVal('newset_code'), {
                placeholder: 'NS-NWCAP-IRS',
                hint: 'The netting set id, as ORE names it.'
            }) +
            field('Call type', 'newset_call_type', formVal('newset_call_type'), { options: BILATERALS }) +
            field('Initial margin type', 'newset_im_type', formVal('newset_im_type'), { options: BILATERALS }) +
            field('Risk weight', 'newset_risk_weight', formVal('newset_risk_weight'), { placeholder: 'Not stated' }) +
            '</div>' +
            '<button class="btn small" data-act="addset">Add netting set</button>' +
            '</details>';
    }

    function newAgreementForm() {
        return '<details class="panel-soft" style="margin-top:14px">' +
            '<summary style="cursor:pointer;font-size:13px;font-weight:600">Add a netting agreement</summary>' +
            '<div class="grid2" style="margin-top:12px">' +
            field('Agreement number', 'newagreement_number', formVal('newagreement_number'), {
                placeholder: 'ISDA-2026-014',
                hint: 'The reference the two parties give the agreement.'
            }) +
            field('Agreement type', 'newagreement_type', formVal('newagreement_type'),
                { options: AGREEMENT_TYPES }) +
            field('Governing law', 'newagreement_law', formVal('newagreement_law'),
                { options: GOVERNING_LAWS }) +
            field('Description', 'newagreement_description', formVal('newagreement_description'), {
                placeholder: 'Not stated', span: true
            }) +
            '</div>' +
            '<button class="btn small" data-act="addagreement">Add netting agreement</button>' +
            '<p class="hint">A set opened under this agreement copies its counterparty and its legal ' +
            'entity, and the copy is pinned to the agreement.</p>' +
            '</details>';
    }

    /* -------------------------------------------------------------- review step */

    function reviewStep() {
        var d = S.draft;
        var ids = d.identifiers.map(function (i) {
            return i.scheme + ' ' + i.value;
        }).join(', ') || 'none';
        var contacts = d.contacts.map(function (c) { return c.type; }).join(', ') || 'none';
        var sets = [];
        var csas = [];
        d.agreements.forEach(function (a) {
            a.sets.forEach(function (s) {
                sets.push(a.agreement_number + ' / ' + s.code);
                csas.push(s.code + ': ' + (s.csa.is_active ? 'in force' : 'off') + ', ' +
                    dash(s.csa.bilateral) + ' ' + dash(s.csa.csa_currency) + ', MPOR ' +
                    dash(s.csa.margin_period_of_risk) + ', eligible ' +
                    (s.eligible.map(function (e) { return e.currency_code; }).join('/') || 'none'));
            });
        });
        var rows = [
            ['Short code', d.short_code],
            ['Full name', d.full_name],
            ['Transliterated name', dash(d.transliterated_name)],
            ['Party type', d.party_type],
            ['Status', d.status],
            ['Business center', d.business_center_code + ' \u2014 ' +
                (centre(d.business_center_code) === undefined ? '' : centre(d.business_center_code).city_name)],
            ['Parent counterparty', dash(d.parent_counterparty_id)],
            ['Identifier, authoritative', (function () {
                var a = authoritativeIdentifier(d.identifiers);
                return a === undefined ? 'none' : a.scheme + ' ' + a.value;
            })()],
            ['Identifiers', ids],
            ['Contacts', contacts],
            ['Netting agreements', d.agreements.map(function (a) {
                return a.agreement_number;
            }).join(', ') || 'none'],
            ['Netting sets', sets.join(', ') || 'none'],
            ['Collateral (CSA)', csas.join('; ') || 'none'],
            ['Visible to', S.resolvedParties.map(partyName).join(', ')]
        ];
        var dl = rows.map(function (r) {
            return '<dt>' + esc(r[0]) + '</dt><dd>' + esc(r[1] === '' ? '\u2014' : r[1]) + '</dd>';
        }).join('');
        var steps = [
            ['refdata.v1.counterparties.put', '1 row \u2014 the counterparty'],
            ['refdata.v1.counterparty_identifiers.put_many', d.identifiers.length + ' row(s)'],
            ['refdata.v1.counterparty_contact_informations.put_many', d.contacts.length + ' row(s)'],
            ['refdata.v1.netting_agreements.put', d.agreements.length + ' row(s)'],
            ['refdata.v1.netting_sets.put', sets.length + ' row(s)'],
            ['refdata.v1.netting_set_identifiers.put_many',
                d.agreements.reduce(function (n, a) {
                    return n + a.sets.reduce(function (m, s) { return m + s.identifiers.length; }, 0);
                }, 0) + ' row(s)'],
            ['refdata.v1.csas.put', sets.length + ' row(s)'],
            ['refdata.v1.csa_eligible_currencies.put_many',
                d.agreements.reduce(function (n, a) {
                    return n + a.sets.reduce(function (m, s) { return m + s.eligible.length; }, 0);
                }, 0) + ' row(s)'],
            ['refdata.v1.party_counterparties.put', S.resolvedParties.length + ' row(s)']
        ];
        return '<dl class="reviewgrid">' + dl + '</dl>' +
            '<h2 style="font-size:14px;margin-top:22px">What the confirm writes</h2>' +
            '<table class="datatable"><tbody>' + steps.map(function (r) {
                return '<tr><th>' + mono(r[0]) + '</th><td>' + esc(r[1]) + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<label class="failtoggle"><input type="checkbox" data-failtoggle="1"' +
            (S.fail !== '' ? ' checked' : '') +
            '> Prototype: make the confirm refuse' +
            (S.fail === '' ? ' (nothing is refused until you choose a fail kind from the bar)' : '') +
            '</label>';
    }

    /* ---------------------------------------------------------------- the outcome */

    function doneStep() {
        var d = S.draft;
        var auth = authoritativeIdentifier(d.identifiers);
        var cards = [
            ['Trade with it', 'Its netting sets and collateral terms are ready for a trade.'],
            ['See who can see it', 'The parties that may trade with ' + (d.full_name || 'it') + '.'],
            ['Onboard another', 'Start this journey again from the counterparty list.']
        ].map(function (c) {
            return '<button class="handoffcard"><span class="nm">' + esc(c[0]) + '</span>' +
                '<p>' + esc(c[1]) + '</p></button>';
        }).join('');
        return '<div class="notice success"><h3>' +
            esc(d.full_name === '' ? 'The counterparty' : d.full_name) + ' is on board.</h3>' +
            'The tenant can trade with it. ' +
            (auth === undefined ? 'It has no authoritative identifier yet.' :
                'It resolves by ' + esc(auth.scheme) + ' ' + mono(auth.value) + '.') + '</div>' +
            '<table class="datatable"><tbody>' +
            '<tr><th>Short code</th><td class="mono">' + esc(d.short_code) + '</td></tr>' +
            '<tr><th>Identifiers</th><td>' + (d.identifiers.length === 0 ? '<span class="meta">none</span>' :
                d.identifiers.map(function (i) {
                    return tag(i.scheme + ' ' + i.value, i === auth ? 'ok' : '');
                }).join(' ')) + '</td></tr>' +
            '<tr><th>Contacts</th><td>' + esc(d.contacts.map(function (c) { return c.type; }).join(', ')) + '</td></tr>' +
            '<tr><th>Netting agreements</th><td>' + esc(d.agreements.map(function (a) {
                return a.agreement_number;
            }).join(', ')) + '</td></tr>' +
            '<tr><th>Version written</th><td class="mono">1</td></tr>' +
            '</tbody></table>' +
            '<div class="handoffcards" style="margin-top:18px">' + cards + '</div>' +
            '<p class="proto-hint"><b>Gap:</b> the confirm is one act over eight subjects. The ' +
            'server offers each write alone, so a failure half way leaves a counterparty with some ' +
            'of its rows. The prototype draws the outcome as one act; the server cannot yet promise it.</p>';
    }

    /* ---------------------------------------------------------------- history step */

    function historyStep() {
        var at = S.version;
        if (at < 0 || at >= HISTORY.length) at = 0;
        var v = HISTORY[at];
        var versions = HISTORY.map(function (h, i) {
            return '<li><button data-act="version" data-i="' + i + '"' +
                (i === at ? ' class="on"' : '') + '>' +
                '<span class="v">Version ' + esc(h.version) + '</span>' +
                '<span class="who">' + esc(h.actor) + ' \u00b7 ' + esc(h.at) + '</span></button></li>';
        }).join('');
        var diff = v.changes.map(function (c) {
            var from = c[1] === '\u2014' ? '<span class="meta">not set</span>' :
                '<span class="mono">' + esc(c[1]) + '</span>';
            return '<tr><td class="fld">' + esc(c[0]) + '</td>' +
                '<td class="from">' + from + '</td>' +
                '<td class="to"><span class="mono">' + esc(c[2]) + '</span></td></tr>';
        }).join('');
        return '<div class="hist">' +
            '<ul class="versions">' + versions + '</ul>' +
            '<div>' +
            '<div class="panel-soft">' +
            '<h3>Version ' + esc(v.version) + ' \u00b7 ' + esc(v.reason) + '</h3>' +
            '<p class="hint" style="margin:0 0 10px">Written by ' + mono(v.actor) + ' at ' +
            esc(v.at) + '. Reverting writes the old values as a new version; history is never rewritten.</p>' +
            '<table class="difftable"><thead><tr><th>Field</th><th>From</th><th>To</th></tr></thead>' +
            '<tbody>' + diff + '</tbody></table>' +
            '<div style="margin-top:14px" class="chips">' +
            '<button class="btn small" data-act="revert">Revert to this version</button>' +
            '<button class="btn ghost small" data-act="openversion">Open read-only</button>' +
            '</div></div>' +
            '<div class="panel-soft" style="margin-top:14px">' +
            '<h3>As it stood in this version</h3>' +
            '<table class="datatable"><tbody>' +
            '<tr><th>Identifiers</th><td>' + (v.identifiers.length === 0 ? '<span class="meta">none</span>' :
                v.identifiers.map(function (i) { return tag(i.scheme + ' ' + i.value); }).join(' ')) + '</td></tr>' +
            '<tr><th>Contacts</th><td>' + v.contacts.map(function (c) {
                return tag(c.type);
            }).join(' ') + '</td></tr>' +
            '</tbody></table>' +
            '<p class="hint">This panel reads ' +
            mono('refdata.v1.counterparties.composite_as_of') + ' with the version and shows ' +
            'the counterparty with its identifiers and contacts as they stood during that version\u2019s ' +
            'window.</p></div>' +
            '<p class="proto-hint"><b>Gap:</b> version history is read with ' +
            mono('refdata.v1.counterparties_versions.list') + ' and ' +
            mono('refdata.v1.counterparties_versions.get') + ', and the diff is computed by the ' +
            'screen: no reply carries a field-level difference. The child rows have their own ' +
            'history subjects, so a full diff of identifiers and contacts needs a read each.</p>' +
            '</div></div>';
    }

    /* -------------------------------------------------------------- refusal block */

    function refusalBlock(kind, title, subject, detail, actions) {
        return '<div class="notice error"><h3>' + esc(title) + '</h3>' +
            'The server refused ' + mono(subject === '' ? 'the write' : subject) +
            '. Nothing was written. The step keeps what you typed.' +
            '<ul>' + detail.map(function (d) { return '<li>' + esc(d) + '</li>'; }).join('') +
            '</ul><div style="margin-top:10px" class="chips">' +
            actions.map(function (a, i) {
                return '<button class="btn ' + (i === 0 ? 'primary ' : 'ghost ') + 'small" data-act="fixrefusal">' +
                    esc(a) + '</button>';
            }).join('') + '</div>' +
            '<p class="hint" style="margin-top:10px">Prototype: this is the refusal the journey must ' +
            'handle. Choose another state from the bar to leave it.</p></div>';
    }

    /* The refusal as its own screen. It states what was refused, that the
       record did not change, and the way back to the step that raised it. */
    function refusedStep() {
        var r = REFUSALS[S.fail];
        var back = r === undefined ? '' : r.on;
        var step = stepById(back);
        var title = step === undefined ? 'the onboarding step' : step.title;
        if (r === undefined) {
            return '<div class="notice info"><h3>Nothing was refused.</h3>' +
                'This screen states a refusal the server returned. Choose a kind from the bar to ' +
                'draw one, or walk back to the step that raises it.</div>' +
                '<div class="chips"><button class="btn small" data-act="state" data-state="review">' +
                'Back to Review</button></div>';
        }
        var code = S.draft.short_code === '' ? 'the counterparty' : mono(S.draft.short_code);
        var version = S.picked === undefined ? '1 (nothing written yet)' : versionOf(S.picked);
        return '<div class="notice error"><h3>' + esc(r.title) + '</h3>' +
            'The server refused ' + mono(r.subject) + ' on ' +
            (step === undefined ? 'the write' : esc(step.title)) + '.' +
            '<ul>' + r.detail.map(function (d) { return '<li>' + esc(d) + '</li>'; }).join('') +
            '</ul></div>' +
            '<table class="datatable"><tbody>' +
            '<tr><th>Refused</th><td>' + mono(r.subject) + '</td></tr>' +
            '<tr><th>Raised by</th><td>' + esc(title) +
            ' <span class="meta">\u2014 the step that sent the write</span></td></tr>' +
            '<tr><th>Record</th><td>' + code + '</td></tr>' +
            '<tr><th>Version</th><td>' + esc(version) +
            ' <span class="meta">\u2014 unchanged. A refused write leaves no version and no row.</span>' +
            '</td></tr>' +
            '<tr><th>Your work</th><td>Kept. Nothing you typed on ' + esc(title) + ' is lost.</td></tr>' +
            '</tbody></table>' +
            '<div class="chips" style="margin-top:16px">' +
            (back === '' ? '' :
                '<button class="btn primary small" data-act="state" data-state="' + back + '">' +
                'Back to ' + esc(title) + '</button> ') +
            '<button class="btn ghost small" data-act="state" data-state="history">See the history</button>' +
            '</div>' +
            '<p class="proto-hint"><b>Gap:</b> each refusal arrives as a plain error on one subject. ' +
            'No reply names the record, the version it left behind, or the field that collided, so ' +
            'this screen composes those three from the step that failed.</p>';
    }

    /* ------------------------------------------------------------ the shell */

    function stepById(id) {
        return STEPS.filter(function (s) { return s.id === id; })[0];
    }

    function stepBody(id) {
        if (id === 'counterparty') return identityStep();
        if (id === 'identifiers') return identifiersStep();
        if (id === 'contacts') return contactStep();
        if (id === 'agreements') return agreementsStep();
        if (id === 'review') return reviewStep();
        if (id === 'done') return doneStep();
        if (id === 'history') return historyStep();
        return '';
    }

    function stepState(id) {
        if (id === 'counterparty') return S.draft.short_code.trim() !== '' &&
            S.draft.full_name.trim() !== '';
        if (id === 'identifiers') return identifierProblems(S.draft).length === 0;
        if (id === 'contacts') return contactProblems(S.draft).length === 0;
        return true;
    }

    function nextOf(id) {
        if (id === 'review') return { label: 'Confirm and write', enabled: true };
        if (id === 'done' || id === 'history') return null;
        var i = STEP_ORDER.indexOf(id);
        if (i < 0) return null;
        return { label: 'Continue', enabled: stepState(id) };
    }

    function errorFor(id) {
        var r = REFUSALS[S.fail];
        if (r === undefined || r.on !== id) return '';
        return refusalBlock(S.fail, r.title, r.subject, r.detail, r.actions);
    }

    function render() {
        if (S.state === 'landing') {
            document.getElementById('app').innerHTML =
                '<div class="page">' + landing() + '</div>';
        } else if (S.state === 'refused') {
            document.getElementById('app').innerHTML =
                '<div class="page">' +
                '<h1>Refused \u2014 ' +
                esc(S.draft.full_name === '' ? 'new counterparty' : S.draft.full_name) + '</h1>' +
                '<div class="journey">' + rail(refusalStep()) +
                '<section class="card">' + stepHeader() +
                '<h2>' + (S.fail === '' ? 'No refusal' : 'The write was refused') + '</h2>' +
                '<p class="lead">What was refused, what the record kept, and the way back to the ' +
                'step that raised it.</p>' + refusedStep() + '</section></div></div>';
        } else {
            var id = stepById(S.state) === undefined ? 'counterparty' : S.state;
            var step = stepById(id);
            var next = nextOf(id);
            var foot = next === null ? '' :
                '<div class="stepfoot">' +
                '<button class="btn ghost" data-act="back">Back</button>' +
                '<button class="btn primary ml-auto" data-act="next"' +
                (next.enabled ? '' : ' disabled') + '>' + esc(next.label) + '</button></div>';
            var railNav = id === 'done' || id === 'history' ? rail('review') : rail(id);
            var lead = id === 'done' ?
                (S.draft.full_name === '' ? 'The counterparty is on board.' :
                    S.draft.full_name + ' is on board.') : step.lead;
            var warn = errorFor(id);
            document.getElementById('app').innerHTML =
                '<div class="page"><h1>' +
                (id === 'history' ? 'History \u2014 ' : 'Onboard a counterparty \u2014 ') +
                esc(S.draft.full_name === '' ? 'new counterparty' : S.draft.full_name) + '</h1>' +
                '<div class="journey">' + railNav +
                '<section class="card">' + stepHeader() +
                '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(lead) + '</p>' +
                warn + stepBody(id) + foot + '</section></div></div>';
        }
        renderNote();
        renderBar();
    }

    /* The step a refusal was raised by, for the refused rail. */
    function refusalStep() {
        var r = REFUSALS[S.fail];
        return r === undefined ? 'review' : r.on;
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey Onboard a counterparty \u00b7 ' +
            (S.draft.short_code === '' ? 'new counterparty' : S.draft.short_code + ' \u2014 ' +
                S.draft.full_name) + ' \u00b7 state ' + S.state +
            (S.fail === '' ? '' : ' \u00b7 fail ' + S.fail);
    }

    function renderBar() {
        var states = BAR_STATES.map(function (s) {
            return '<button data-act="state" data-state="' + s + '"' +
                (S.state === s ? ' class="on"' : '') + '>' + s + '</button>';
        }).join('');
        var lists = '<span class="label">list</span>' + ['active', 'closed', 'all'].map(function (t) {
            return '<button data-act="tab" data-list="' + t + '"' +
                (S.list === t ? ' class="on"' : '') + '>' + t + '</button>';
        }).join('');
        var refusals = FAIL_KINDS.map(function (r, i) {
            return '<button data-act="fail" data-fail="' + r + '"' +
                (S.fail === r ? ' class="on"' : '') + '>' + (i === 0 ? '1 ' : '') + r + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + states +
            '<span class="sep">|</span>' + lists +
            '<span class="sep">|</span><span class="label">fail</span>' + refusals +
            '<span class="sep">|</span><button data-act="fail" data-fail="">none</button>';
    }

    /* --------------------------------------------------------------- behaviour */

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

    function setState(id) {
        S.state = id;
        if (id === 'done' && S.fail !== '') S.fail = '';
    }

    function goNext() {
        var id = S.state;
        if (id === 'review') { setState('done'); return; }
        var i = STEP_ORDER.indexOf(id);
        if (i >= 0 && i < STEP_ORDER.length - 1) S.state = STEP_ORDER[i + 1];
    }

    function goBack() {
        var id = S.state;
        if (id === 'done' || id === 'history') { setState('review'); return; }
        var i = STEP_ORDER.indexOf(id);
        if (i > 0) S.state = STEP_ORDER[i - 1];
    }

    /* Selecting a fail kind records it and opens the step that raises it; the
       refused state then draws the same refusal as its own screen. */
    function applyFail(kind) {
        S.fail = kind;
        S.refused = kind !== '' && REFUSALS[kind] !== undefined;
        if (kind === '') return;
        var r = REFUSALS[kind];
        if (r !== undefined) S.state = r.on;
        if (kind === 'missing') S.draft.identifiers = [];
    }

    function optionAt(select) {
        return select.options[select.selectedIndex].value;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'state') {
            setState(el.getAttribute('data-state'));
        } else if (act === 'tab') {
            S.list = el.getAttribute('data-list');
            S.state = 'landing';
        } else if (act === 'fail' || act === 'refuse') {
            applyFail(el.getAttribute('data-fail') !== null ?
                el.getAttribute('data-fail') : el.getAttribute('data-refuse'));
        } else if (act === 'start') {
            S.draft = blankDraft();
            S.picked = undefined;
            S.resolvedParties = ['p-ores'];
            S.fail = '';
            setState('counterparty');
        } else if (act === 'open') {
            var row = rowByCode(el.getAttribute('data-code'));
            if (row !== undefined) {
                S.draft = loadRow(row);
                S.picked = row.code;
                S.agreement = S.draft.agreements.length > 0 ?
                    S.draft.agreements[0].agreement_number : '';
                S.set = currentAgreement() === undefined ? '' : currentAgreement().sets[0].code;
                S.resolvedParties = ['p-ores'];
                S.fail = '';
                setState('counterparty');
            }
        } else if (act === 'next') {
            if (!el.disabled) goNext();
        } else if (act === 'back') {
            goBack();
        } else if (act === 'addid') {
            if (S.value.trim() !== '') {
                S.draft.identifiers.push({
                    scheme: S.scheme, value: S.value.trim(), description: ''
                });
                S.value = '';
                S.fail = '';
            }
        } else if (act === 'delid') {
            S.draft.identifiers.splice(parseInt(el.getAttribute('data-i'), 10), 1);
        } else if (act === 'addcontact') {
            S.draft.contacts.push(blankContact(S.contact));
        } else if (act === 'delcontact') {
            S.draft.contacts.splice(parseInt(el.getAttribute('data-i'), 10), 1);
        } else if (act === 'pickcontact') {
            S.contact = S.draft.contacts[parseInt(el.getAttribute('data-i'), 10)].type;
        } else if (act === 'pickagreement') {
            S.agreement = el.getAttribute('data-number');
            var a = currentAgreement();
            S.set = a !== undefined && a.sets.length > 0 ? a.sets[0].code : '';
        } else if (act === 'pickset') {
            S.set = el.getAttribute('data-code');
            S.fail = '';
        } else if (act === 'addset') {
            var code = String(S.newset_code || '').trim();
            var cur = currentAgreement();
            if (code !== '' && cur !== undefined &&
                cur.sets.every(function (s) { return s.code !== code; })) {
                cur.sets.push(newSet(code));
                S.set = code;
                S.newset_code = '';
            }
        } else if (act === 'addagreement') {
            var number = String(S.newagreement_number || '').trim();
            if (number !== '' && agreementOf(number) === undefined) {
                var ag = newAgreement(number, S.newagreement_type, S.newagreement_law);
                ag.description = S.newagreement_description || '';
                ag.sets = [];
                S.draft.agreements.push(ag);
                S.agreement = number;
                S.set = '';
                S.newagreement_number = '';
            }
        } else if (act === 'addsetid') {
            var s = currentSet();
            var v = String(S.setidvalue || '').trim();
            if (s !== undefined && v !== '') {
                s.identifiers.push({ scheme: S.setscheme || 'ORE', value: v });
                S.setidvalue = '';
                S.fail = '';
            }
        } else if (act === 'delsetid') {
            var st = currentSet();
            if (st !== undefined) st.identifiers.splice(parseInt(el.getAttribute('data-i'), 10), 1);
        } else if (act === 'version') {
            S.version = parseInt(el.getAttribute('data-i'), 10);
        } else if (act === 'revert' || act === 'openversion' || act === 'fixrefusal') {
            /* The prototype cannot write. The button is where the real screen acts. */
        } else {
            return;
        }
        rerender();
    });

    function inputChanged(el) {
        var attr = el.getAttribute ? el.getAttribute('data-field') : null;
        if (attr !== null) {
            var scope = el.getAttribute('data-scope');
            if (scope === 'contact') {
                var c = S.draft.contacts.filter(function (x) { return x.type === S.contact; })[0];
                if (c !== undefined) c[attr] = el.value;
            } else if (scope === 'set') {
                var st = currentSet();
                if (st !== undefined) st[attr] = el.value;
            } else if (scope === 'form' || S.draft[attr] === undefined) {
                formSet(attr, el.value);
            } else {
                S.draft[attr] = el.value;
            }
            return true;
        }
        var pick = el.getAttribute ? el.getAttribute('data-pick') : null;
        if (pick !== null) {
            S[pick] = optionAt(el);
            return true;
        }
        var check = el.getAttribute ? el.getAttribute('data-check') : null;
        if (check !== null) {
            var set = currentSet();
            if (set !== undefined) {
                if (check === 'csa_is_active') set.csa.is_active = el.checked;
                if (check === 'csa_apply_im') set.csa.apply_initial_margin = el.checked;
                if (check === 'csa_calc_im') set.csa.calculate_im_amount = el.checked;
                if (check === 'csa_calc_vm') set.csa.calculate_vm_amount = el.checked;
            }
            return true;
        }
        if (el.getAttribute && el.getAttribute('data-failtoggle') !== null) {
            if (!el.checked) {
                S.fail = '';
                S.refused = false;
            }
            return true;
        }
        if (el.getAttribute && el.getAttribute('data-q') !== null) {
            S.query = el.value;
            S.state = 'landing';
            return true;
        }
        return false;
    }

    function onInput(ev) {
        var el = ev.target;
        if (el && el.getAttribute && inputChanged(el)) rerender();
    }

    document.addEventListener('input', onInput);
    document.addEventListener('change', onInput);

    readParams();
    render();
})();

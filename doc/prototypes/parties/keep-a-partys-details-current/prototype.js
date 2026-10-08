/* Keep a party's details current prototype. Self-contained: plain JavaScript,
 * mock data, no framework, no build step, no fetch, and nothing that outlives
 * the page.
 *
 * The tenant administrator's journey: the party list with search, then the
 * walk over the party's own record, its identifiers, its contacts, the
 * countries and currencies it operates in, and the business units and
 * counterparties it is linked to, then the review, the outcome, the refusal
 * and the history.
 *
 * Every field below is a column the models carry:
 *   ores.refdata.party, party_identifier, party_contact_information,
 *   party_country, party_currency, party_counterparty, business_unit,
 *   business_unit_type, party_id_scheme, party_type, party_status, contact_type.
 * Every operation is a subject the generated protocol headers declare. Where
 * the journey needs something the server does not provide, the screen draws it
 * and says so in a gap note.
 *
 * States are chosen from the query string, as the sibling prototypes do:
 *   ?fail=<kind>                       which refusal the failure state draws:
 *                                      level|cardinality|stale, or 1|2|3 for
 *                                      the same kinds in that order (1 = level)
 *   ?state=list|overview|identifiers|contacts|memberships|structure|review|outcome|refused|history
 *   ?party=NWCAP                       the open party, by short code
 *   ?query=Northwind                   the party list search, as typed
 *   ?name=Northwind%20Capital%20Holdings%20Ltd   the corrected legal name
 *   ?shortname=NWCAP                   the corrected short code
 *   ?scheme=LEI                        the identifier scheme being corrected
 *   ?value=549300NWCAPITAL00002        the identifier value being recorded
 *   ?contact=Operations                the contact shown in full
 *   ?primary=Operations                the contact the screen marks primary (a gap)
 *   ?close=SG                          the country membership being closed
 *   ?closeccy=CHF                      the currency membership being closed
 *   ?bu=Rates%20Trading                the business unit in the structure step
 *   ?unittype=BRANCH                   the business unit type chosen for it
 *   ?type=Corporate                    the edited party type
 *   ?status=Active                     the edited party status
 *   ?parent=NWCAP                      the edited parent party
 *   ?with=1                            the registration-default flag
 *   ?reason=common.rectification       the change reason code
 *   ?version=7                         the history version whose diff is shown
 * The bar mirrors the same states as buttons. */

(function () {
    'use strict';

    /* ------------------------------------------------------------ mock data */

    var SCHEMES = [
        { code: 'LEI', name: 'Legal Entity Identifier', max: 1 },
        { code: 'BIC', name: 'Business Identifier Code', max: 3 },
        { code: 'MIC', name: 'Market Identifier Code', max: 2 },
        { code: 'NATIONAL_ID', name: 'National registration number', max: 1 },
        { code: 'INTERNAL', name: 'Internal reference', max: 5 }
    ];

    var PARTY_TYPES = ['Corporate', 'Branch', 'Fund', 'SPV', 'Government'];

    var PARTY_STATUSES = ['Active', 'Inactive', 'Pending', 'Closed'];

    var BUSINESS_CENTRES = [
        { code: 'WRLD', name: 'Global (sentinel)' },
        { code: 'GBLO', name: 'London' },
        { code: 'USNY', name: 'New York' },
        { code: 'SGSL', name: 'Singapore' },
        { code: 'DEFR', name: 'Frankfurt' },
        { code: 'IEDU', name: 'Dublin' }
    ];

    var UNIT_TYPES = [
        { code: 'DIVISION', name: 'Division', level: 0 },
        { code: 'BRANCH', name: 'Branch', level: 0 },
        { code: 'DESK', name: 'Trading Desk', level: 1 },
        { code: 'TEAM', name: 'Team', level: 2 }
    ];

    var CONTACT_TYPES = ['Legal', 'Operations', 'Settlement', 'Billing'];

    var COUNTRIES = {
        GB: 'United Kingdom', US: 'United States', SG: 'Singapore', DE: 'Germany',
        JP: 'Japan', CA: 'Canada', AU: 'Australia', IE: 'Ireland', LU: 'Luxembourg',
        CH: 'Switzerland', FR: 'France', NL: 'Netherlands'
    };

    var CURRENCIES = {
        GBP: 'Pound sterling', USD: 'US dollar', EUR: 'Euro', SGD: 'Singapore dollar',
        JPY: 'Yen', CHF: 'Swiss franc', CAD: 'Canadian dollar', AUD: 'Australian dollar'
    };

    var COUNTERPARTIES = {
        'CP-ATLAS': 'Atlas Clearing House Ltd',
        'CP-MERIDIAN': 'Meridian Bank plc',
        'CP-KESTREL': 'Kestrel Futures S.A.',
        'CP-VANTAGE': 'Vantage Securities LLC',
        'CP-HARBOR': 'Harbor Reinsurance AG'
    };

    var PARTIES = [
        { code: 'NWCAP', name: 'Northwind Capital Ltd', short: 'Northwind Capital',
            codename: 'brave_harbor_af', translit: null, category: 'Operational',
            type: 'Corporate', parent: null, bc: 'GBLO', status: 'Active',
            regDefault: true, version: 7, by: 'tenant_admin', at: '2026-09-30 11:04' },
        { code: 'NWMKT', name: 'Northwind Markets LLC', short: 'Northwind Markets',
            codename: 'swift_anchor_bc', translit: null, category: 'Operational',
            type: 'Corporate', parent: 'NWCAP', bc: 'USNY', status: 'Active',
            regDefault: false, version: 3, by: 'j.silva', at: '2026-08-21 15:12' },
        { code: 'NWAPAC', name: 'Northwind Asia Pacific Pte Ltd', short: 'Northwind APAC',
            codename: 'quiet_harbour_cd', translit: null, category: 'Operational',
            type: 'Corporate', parent: 'NWCAP', bc: 'SGSL', status: 'Pending',
            regDefault: false, version: 2, by: 'a.reyes', at: '2026-07-04 09:47' },
        { code: 'NWFUND', name: 'Northwind Alpha Fund ICAV', short: 'Alpha Fund',
            codename: 'amber_beacon_de', translit: null, category: 'Operational',
            type: 'Fund', parent: 'NWCAP', bc: 'IEDU', status: 'Active',
            regDefault: false, version: 1, by: 'tenant_admin', at: '2026-06-11 13:20' },
        { code: 'NWBRCH', name: 'Northwind Frankfurt Branch', short: 'Frankfurt Branch',
            codename: 'grey_meridian_ef', translit: null, category: 'Operational',
            type: 'Branch', parent: 'NWCAP', bc: 'DEFR', status: 'Inactive',
            regDefault: false, version: 4, by: 'm.okafor', at: '2026-05-02 17:35' }
    ];

    var DETAIL = {
        NWCAP: {
            primary: 'Operations',
            identifiers: [
                { scheme: 'LEI', value: '549300NWCAPITAL00001', desc: 'GLEIF LEI, registered 2014-06-12', version: 3 },
                { scheme: 'BIC', value: 'NWCAGB2L', desc: 'Head office BIC', version: 2 },
                { scheme: 'MIC', value: 'XNWC', desc: 'Northwind Capital, London', version: 1 },
                { scheme: 'NATIONAL_ID', value: '08874521', desc: 'UK Companies House registration', version: 5 }
            ],
            contacts: [
                { type: 'Legal', line1: '1 Fenchurch Avenue', line2: '', city: 'London', state: '',
                    country: 'GB', postal: 'EC3M 5AD', phone: '+44 20 7000 0000',
                    email: 'legal@northwind.example', web: 'https://northwind.example', version: 2 },
                { type: 'Operations', line1: '25 Bank Street', line2: 'Canary Wharf', city: 'London', state: '',
                    country: 'GB', postal: 'E14 5JP', phone: '+44 20 7000 0100',
                    email: 'operations@northwind.example', web: 'https://northwind.example/ops', version: 4 },
                { type: 'Settlement', line1: '25 Bank Street', line2: 'Canary Wharf', city: 'London', state: '',
                    country: 'GB', postal: 'E14 5JP', phone: '+44 20 7000 0120',
                    email: 'settlements@northwind.example', web: '', version: 3 },
                { type: 'Billing', line1: '1 Fenchurch Avenue', line2: '', city: 'London', state: '',
                    country: 'GB', postal: 'EC3M 5AD', phone: '+44 20 7000 0140',
                    email: 'billing@northwind.example', web: '', version: 1 }
            ],
            countries: ['GB', 'US', 'SG', 'DE', 'JP'],
            currencies: ['GBP', 'USD', 'EUR', 'SGD', 'JPY', 'CHF'],
            counterparties: ['CP-ATLAS', 'CP-MERIDIAN', 'CP-KESTREL'],
            units: [
                { name: 'Markets', code: 'MKT', type: 'DIVISION', parent: null, bc: 'GBLO', status: 'Active' },
                { name: 'Rates Trading', code: 'RATES', type: 'DESK', parent: 'Markets', bc: 'GBLO', status: 'Active' },
                { name: 'FX Options', code: 'FXOPT', type: 'DESK', parent: 'Markets', bc: 'GBLO', status: 'Active' },
                { name: 'Finance', code: 'FIN', type: 'DIVISION', parent: null, bc: 'GBLO', status: 'Active' },
                { name: 'Treasury', code: 'TRSY', type: 'TEAM', parent: 'Finance', bc: 'GBLO', status: 'Active' }
            ]
        },
        NWMKT: {
            primary: 'Legal',
            identifiers: [
                { scheme: 'LEI', value: '549300NWMARKETS00002', desc: 'GLEIF LEI', version: 1 },
                { scheme: 'BIC', value: 'NWMKUS33', desc: 'New York BIC', version: 1 },
                { scheme: 'MIC', value: 'XNWM', desc: 'Northwind Markets, New York', version: 1 }
            ],
            contacts: [
                { type: 'Legal', line1: '200 Vesey Street', line2: '', city: 'New York', state: 'NY',
                    country: 'US', postal: '10281', phone: '+1 212 555 0100',
                    email: 'legal.us@northwind.example', web: '', version: 1 },
                { type: 'Operations', line1: '200 Vesey Street', line2: '', city: 'New York', state: 'NY',
                    country: 'US', postal: '10281', phone: '+1 212 555 0110',
                    email: 'ops.us@northwind.example', web: '', version: 2 }
            ],
            countries: ['US', 'GB', 'CA'],
            currencies: ['USD', 'GBP', 'CAD'],
            counterparties: ['CP-VANTAGE'],
            units: [
                { name: 'US Markets', code: 'USMKT', type: 'DIVISION', parent: null, bc: 'USNY', status: 'Active' },
                { name: 'Rates Desk', code: 'USRATES', type: 'DESK', parent: 'US Markets', bc: 'USNY', status: 'Active' }
            ]
        },
        NWAPAC: {
            primary: 'Legal',
            identifiers: [
                { scheme: 'LEI', value: '549300NWASIAPAC00003', desc: 'GLEIF LEI', version: 1 },
                { scheme: 'NATIONAL_ID', value: '201412345K', desc: 'ACRA registration, Singapore', version: 1 }
            ],
            contacts: [
                { type: 'Legal', line1: '8 Marina View', line2: 'Asia Square Tower 1', city: 'Singapore', state: '',
                    country: 'SG', postal: '018960', phone: '+65 6800 0100',
                    email: 'legal.apac@northwind.example', web: '', version: 1 },
                { type: 'Billing', line1: '8 Marina View', line2: 'Asia Square Tower 1', city: 'Singapore', state: '',
                    country: 'SG', postal: '018960', phone: '+65 6800 0140',
                    email: 'billing.apac@northwind.example', web: '', version: 1 }
            ],
            countries: ['SG', 'JP', 'AU'],
            currencies: ['SGD', 'JPY', 'USD'],
            counterparties: [],
            units: [
                { name: 'Asia Pacific', code: 'APAC', type: 'DIVISION', parent: null, bc: 'SGSL', status: 'Active' }
            ]
        },
        NWFUND: {
            primary: 'Settlement',
            identifiers: [
                { scheme: 'LEI', value: '549300NWALPHA00004', desc: 'GLEIF LEI', version: 1 }
            ],
            contacts: [
                { type: 'Legal', line1: '10 Earlsfort Terrace', line2: '', city: 'Dublin', state: '',
                    country: 'IE', postal: 'D02 T380', phone: '+353 1 555 0100',
                    email: 'legal.fund@northwind.example', web: '', version: 1 },
                { type: 'Settlement', line1: '10 Earlsfort Terrace', line2: '', city: 'Dublin', state: '',
                    country: 'IE', postal: 'D02 T380', phone: '+353 1 555 0120',
                    email: 'settlements.fund@northwind.example', web: '', version: 1 }
            ],
            countries: ['IE', 'GB', 'LU'],
            currencies: ['EUR', 'GBP'],
            counterparties: ['CP-MERIDIAN'],
            units: [
                { name: 'Fund Administration', code: 'FUNDADMIN', type: 'DIVISION', parent: null, bc: 'IEDU', status: 'Active' }
            ]
        },
        NWBRCH: {
            primary: 'Legal',
            identifiers: [
                { scheme: 'LEI', value: '549300NWFRANK00005', desc: 'GLEIF LEI', version: 1 }
            ],
            contacts: [
                { type: 'Legal', line1: 'Neue Mainzer Strasse 52', line2: '', city: 'Frankfurt', state: '',
                    country: 'DE', postal: '60311', phone: '+49 69 555 0100',
                    email: 'legal.de@northwind.example', web: '', version: 1 }
            ],
            countries: ['DE'],
            currencies: ['EUR'],
            counterparties: ['CP-HARBOR'],
            units: [
                { name: 'Frankfurt Branch', code: 'FRA', type: 'BRANCH', parent: null, bc: 'DEFR', status: 'Inactive' }
            ]
        }
    };

    /* The party's own versions, as refdata.v1.history.get returns them: the
       full field render and the diff from the version before. Codename is
       immutable, so it reads the same in every version. */
    var VERSIONS = [
        { v: 7, by: 'j.silva', at: '2026-09-30 11:04', reason: 'common.activation',
            commentary: 'Party activated when its essential data was published.',
            unchanged: 6,
            diff: [
                ['full_name', 'Full name', 'Northwind Capital Ltd', 'Northwind Capital Ltd', 'same'],
                ['status', 'Status', 'Pending', 'Active', 'changed'],
                ['is_registration_default', 'Registration default', 'no', 'yes', 'changed'],
                ['business_center_code', 'Business center', 'GBLO', 'GBLO', 'same'],
                ['party_type', 'Party type', 'Corporate', 'Corporate', 'same'],
                ['short_code', 'Short code', 'NWCAP', 'NWCAP', 'same']
            ] },
        { v: 6, by: 'tenant_admin', at: '2026-08-14 09:31', reason: 'common.rectification',
            commentary: 'Primary location corrected from the sentinel to London.',
            unchanged: 6,
            diff: [
                ['business_center_code', 'Business center', 'WRLD', 'GBLO', 'changed'],
                ['full_name', 'Full name', 'Northwind Capital Ltd', 'Northwind Capital Ltd', 'same'],
                ['status', 'Status', 'Pending', 'Pending', 'same'],
                ['short_code', 'Short code', 'NWCAP', 'NWCAP', 'same']
            ] },
        { v: 5, by: 'tenant_admin', at: '2026-07-02 16:48', reason: 'common.rectification',
            commentary: 'ASCII transliteration recorded for the register extract.',
            unchanged: 6,
            diff: [
                ['transliterated_name', 'Transliterated name', '\u2014', 'Northwind Capital', 'added'],
                ['full_name', 'Full name', 'Northwind Capital Ltd', 'Northwind Capital Ltd', 'same'],
                ['business_center_code', 'Business center', 'WRLD', 'WRLD', 'same']
            ] },
        { v: 4, by: 'a.reyes', at: '2026-05-19 10:12', reason: 'common.rectification',
            commentary: 'Short code aligned with the trading system mnemonic.',
            unchanged: 7,
            diff: [
                ['short_code', 'Short code', 'NWHLD', 'NWCAP', 'changed'],
                ['full_name', 'Full name', 'Northwind Capital Ltd', 'Northwind Capital Ltd', 'same']
            ] },
        { v: 3, by: 'tenant_admin', at: '2026-03-01 08:05', reason: 'common.rectification',
            commentary: 'Party type corrected from the provisional classification.',
            unchanged: 7,
            diff: [
                ['party_type', 'Party type', 'SPV', 'Corporate', 'changed'],
                ['full_name', 'Full name', 'Northwind Capital Ltd', 'Northwind Capital Ltd', 'same']
            ] },
        { v: 2, by: 'tenant_admin', at: '2026-01-20 14:40', reason: 'common.rectification',
            commentary: 'Registered name changed at Companies House.',
            unchanged: 7,
            diff: [
                ['full_name', 'Full name', 'Northwind Holdings Ltd', 'Northwind Capital Ltd', 'changed'],
                ['short_code', 'Short code', 'NWHLD', 'NWHLD', 'same']
            ] },
        { v: 1, by: 'system', at: '2025-11-03 09:00', reason: 'common.creation',
            commentary: 'Party created by tenant provisioning.', unchanged: 0, diff: [] }
    ];

    /* The tenant-wide counterparties the party could be linked to, and the
       change reasons dq.v1.change_reasons.list offers. */
    var UNLINKED = ['CP-VANTAGE', 'CP-HARBOR'];

    var REASONS = [
        { code: 'common.rectification', name: 'Rectification' },
        { code: 'common.activation', name: 'Activation' },
        { code: 'common.deactivation', name: 'Deactivation' },
        { code: 'common.reorganisation', name: 'Reorganisation' },
        { code: 'common.revert', name: 'Revert to an earlier version' }
    ];

    var STEPS = [
        { id: 'overview', title: 'Overview',
            lead: 'The party\'s own record: its names, its type and its status. A correction writes a new version of this record; it never replaces it.' },
        { id: 'identifiers', title: 'Identifiers',
            lead: 'The schemes this party is known by, and why a change of legal name and a change of identifier do not read the same on screen.' },
        { id: 'contacts', title: 'Contacts',
            lead: 'Who to reach for what. The party keeps more than one contact, and one of them is the primary.' },
        { id: 'memberships', title: 'Memberships',
            lead: 'The countries and currencies this party operates in. Closing one ends the party\'s access; the definition stays with the tenant.' },
        { id: 'structure', title: 'Structure',
            lead: 'The counterparties this party can see and the business units it is made of, each classified by a business unit type.' },
        { id: 'review', title: 'Review',
            lead: 'What will change, and the reason it is recorded under. Nothing is written until you confirm.' },
        { id: 'outcome', title: 'Outcome', lead: '' }
    ];

    var FAILS = ['level', 'cardinality', 'stale'];

    /* ---------------------------------------------------------------- state */

    var S = {
        view: 'list',
        at: 0,
        query: '',
        code: 'NWCAP',
        name: '',
        short: '',
        type: '',
        status: '',
        bc: '',
        parent: '',
        regDefault: false,
        scheme: 'LEI',
        newValue: '',
        idDesc: '',
        contact: '',
        primary: '',
        closeCountry: '',
        closeCurrency: '',
        bu: '',
        unitType: '',
        reason: 'common.rectification',
        commentary: '',
        version: 8,
        fail: 'level',
        saved: false,
        reverted: false
    };

    function party() {
        return byCode(S.code);
    }

    function byCode(code) {
        for (var i = 0; i < PARTIES.length; i++) {
            if (PARTIES[i].code === code) return PARTIES[i];
        }
        return PARTIES[0];
    }

    function detail(code) {
        var d = DETAIL[code];
        if (d === undefined) return { identifiers: [], contacts: [], countries: [], currencies: [], counterparties: [], units: [], primary: '' };
        return d;
    }

    function scheme(code) {
        for (var i = 0; i < SCHEMES.length; i++) {
            if (SCHEMES[i].code === code) return SCHEMES[i];
        }
        return SCHEMES[0];
    }

    function unitType(code) {
        for (var i = 0; i < UNIT_TYPES.length; i++) {
            if (UNIT_TYPES[i].code === code) return UNIT_TYPES[i];
        }
        return undefined;
    }

    function unit(name) {
        var us = detail(S.code).units;
        for (var i = 0; i < us.length; i++) {
            if (us[i].name === name) return us[i];
        }
        return undefined;
    }

    function identifierFor(code) {
        var ids = detail(S.code).identifiers;
        for (var i = 0; i < ids.length; i++) {
            if (ids[i].scheme === code) return ids[i];
        }
        return undefined;
    }

    function contactRow(type) {
        var cs = detail(S.code).contacts;
        for (var i = 0; i < cs.length; i++) {
            if (cs[i].type === type) return cs[i];
        }
        return cs.length ? cs[0] : undefined;
    }

    function partyName(code) {
        if (code === '' || code === null || code === undefined) return '\u2014';
        return byCode(code).name;
    }

    function statusClass(status) {
        if (status === 'Active') return 'active';
        if (status === 'Pending') return 'pending';
        return 'inactive';
    }

    /* -------------------------------------------------------------- parameters */

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var code = p.get('party');
        if (code !== null && code !== '') S.code = byCode(code).code;
        openParty(S.code);
        seedDemo();

        if (p.get('query') !== null) S.query = p.get('query');
        if (p.get('name') !== null) S.name = p.get('name');
        if (p.get('shortname') !== null) S.short = p.get('shortname');
        if (p.get('type') !== null) S.type = p.get('type');
        if (p.get('status') !== null) S.status = p.get('status');
        if (p.get('parent') !== null) S.parent = p.get('parent');
        if (p.get('with') !== null) S.regDefault = p.get('with') === '1';
        if (p.get('scheme') !== null && scheme(p.get('scheme'))) S.scheme = scheme(p.get('scheme')).code;
        if (p.get('value') !== null) S.newValue = p.get('value');
        if (p.get('contact') !== null) S.contact = p.get('contact');
        if (p.get('primary') !== null) S.primary = p.get('primary');
        if (p.get('close') !== null) S.closeCountry = p.get('close');
        if (p.get('closeccy') !== null) S.closeCurrency = p.get('closeccy');
        if (p.get('bu') !== null) S.bu = p.get('bu');
        if (p.get('unittype') !== null) S.unitType = p.get('unittype');
        if (p.get('reason') !== null) S.reason = p.get('reason');
        var v = parseInt(p.get('version'), 10);
        if (!isNaN(v) && v > 0) S.version = v;
        var f = p.get('fail');
        if (f !== null && f !== '') {
            /* ?fail=<kind> names the refusal; ?fail=1|2|3 selects the same
               kinds in order, so ?fail=1 is "level". */
            var idx = parseInt(f, 10);
            if (!isNaN(idx) && idx >= 1 && idx <= FAILS.length) S.fail = FAILS[idx - 1];
            else if (FAILS.indexOf(f) >= 0) S.fail = f;
        }

        setState(p.get('state'));
        if (S.view === 'refused') armFail();
    }

    function setState(id) {
        for (var i = 0; i < STEPS.length; i++) {
            if (STEPS[i].id === id) { S.view = 'walk'; S.at = i; return; }
        }
        if (id === 'refused') { S.view = 'refused'; return; }
        if (id === 'history') { S.view = 'history'; return; }
        S.view = 'list';
    }

    /* Arm the refusal state: each refusal belongs to the step that raises it. */
    function armFail() {
        if (S.fail === 'level') {
            S.bu = 'Rates Trading';
            S.unitType = 'BRANCH';
            S.at = 4;
        } else if (S.fail === 'cardinality') {
            S.scheme = 'LEI';
            S.at = 1;
        } else {
            S.at = 0;
        }
    }

    function openParty(code) {
        S.code = byCode(code).code;
        var p = party();
        var d = detail(S.code);
        S.name = p.name;
        S.short = p.short;
        S.type = p.type;
        S.status = p.status;
        S.bc = p.bc;
        S.parent = p.parent === null ? '' : p.parent;
        S.regDefault = p.regDefault;
        S.primary = d.primary;
        S.closeCountry = '';
        S.closeCurrency = '';
        S.newValue = '';
        S.idDesc = '';
        S.scheme = 'LEI';
        S.contact = d.contacts.length ? d.contacts[0].type : '';
        S.bu = d.units.length ? d.units[0].name : '';
        S.unitType = d.units.length ? d.units[0].type : '';
        S.saved = false;
        S.reverted = false;
        S.fail = 'level';
        S.version = p.version;
    }

    /* The demo correction the walk carries: the legal name after the
       reorganisation, and the Singapore membership closed. */
    function seedDemo() {
        if (S.code !== 'NWCAP') return;
        S.name = 'Northwind Capital Holdings Ltd';
        S.closeCountry = 'SG';
    }

    /* ----------------------------------------------------------- change set */

    function changes() {
        var p = party();
        var d = detail(S.code);
        var out = [];
        function add(what, from, to, op) {
            out.push({ what: what, from: from, to: to, op: op });
        }
        var putParty = 'refdata.v1.parties.put';
        if (S.name !== p.name) add('Legal name (full_name)', p.name, S.name, putParty);
        if (S.short !== p.short) add('Short code (short_code)', p.short, S.short, putParty);
        if (S.type !== p.type) add('Party type (party_type)', p.type, S.type, putParty);
        if (S.status !== p.status) add('Status (status)', p.status, S.status, putParty);
        if (S.bc !== p.bc) add('Business center (business_center_code)', p.bc, S.bc, putParty);
        if (S.parent !== (p.parent === null ? '' : p.parent)) {
            add('Parent party (parent_party_id)', partyName(p.parent), partyName(S.parent), putParty);
        }
        if (S.regDefault !== p.regDefault) {
            add('Registration default', p.regDefault ? 'yes' : 'no', S.regDefault ? 'yes' : 'no', putParty);
        }
        if (S.primary !== d.primary) {
            add('Primary contact', d.primary, S.primary,
                'none \u2014 party_contact_information carries no primary flag (gap)');
        }
        if (S.closeCountry !== '' && d.countries.indexOf(S.closeCountry) >= 0) {
            add('Country membership', COUNTRIES[S.closeCountry] + ' (' + S.closeCountry + ')', 'closed',
                'refdata.v1.party_countries.delete');
        }
        if (S.closeCurrency !== '' && d.currencies.indexOf(S.closeCurrency) >= 0) {
            add('Currency membership', CURRENCIES[S.closeCurrency] + ' (' + S.closeCurrency + ')', 'closed',
                'refdata.v1.party_currencies.delete');
        }
        if (S.newValue.trim() !== '') {
            var existing = identifierFor(S.scheme);
            add('Identifier value (id_value, scheme ' + S.scheme + ')',
                existing ? existing.value : '\u2014 none \u2014',
                S.newValue.trim(),
                'refdata.v1.party_identifiers.delete + .put');
        }
        var u = unit(S.bu);
        if (u !== undefined && S.unitType !== u.type) {
            add('Business unit type \u00b7 ' + S.bu, u.type, S.unitType,
                'refdata.v1.business_units.put');
        }
        return out;
    }

    function save() {
        S.saved = true;
        S.version = Math.max(S.version, party().version + 1);
    }

    /* -------------------------------------------------------------- utilities */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function badge(text, cls) {
        return '<span class="badge ' + esc(cls) + '">' + esc(text) + '</span>';
    }

    function opts(values, current, blank) {
        var html = blank === undefined ? '' : '<option value="">' + esc(blank) + '</option>';
        return html + values.map(function (v) {
            return '<option value="' + esc(v) + '"' + (v === current ? ' selected' : '') + '>' + esc(v) + '</option>';
        }).join('');
    }

    function optsNamed(list, current) {
        return list.map(function (o) {
            return '<option value="' + esc(o.code) + '"' + (o.code === current ? ' selected' : '') + '>' +
                esc(o.code + ' \u2014 ' + o.name) + '</option>';
        }).join('');
    }

    function tag(status) {
        return badge(status, statusClass(status));
    }

    /* ------------------------------------------------------------------ render */

    function render() {
        var app = document.getElementById('app');
        if (S.view === 'list') {
            app.innerHTML = listScreen();
        } else {
            app.innerHTML = '<div class="page"><h1>Party details</h1>' +
                '<div class="journey">' + rail() +
                '<section class="card">' + header() +
                '<h2>' + esc(cardTitle()) + '</h2>' +
                (cardLead() === '' ? '' : '<p class="lead">' + esc(cardLead()) + '</p>') +
                body() + foot() + '</section></div></div>';
        }
        renderNote();
        renderBar();
    }

    function cardTitle() {
        if (S.view === 'history') return 'History';
        if (S.view === 'refused') return 'Refused';
        return STEPS[S.at].title;
    }

    function cardLead() {
        if (S.view === 'history') {
            return 'Every version of the party, each with the field difference from the version before. Read with refdata.v1.history.get, entity_type ores.refdata.party.';
        }
        if (S.view === 'refused') return 'The write did not land. The record is exactly as it was.';
        if (STEPS[S.at].id === 'outcome') {
            return 'The correction is written as a new version of the same party.';
        }
        return STEPS[S.at].lead;
    }

    function header() {
        var p = party();
        var v = S.saved ? S.version : p.version;
        return '<div class="stepheader"><div>' +
            '<div class="nm">' + esc(p.name) + '</div>' +
            '<div class="sub mono">' + esc(p.code) + ' \u00b7 v' + esc(v) +
            ' \u00b7 ' + esc(p.codename) + '</div></div>' +
            '<div class="ml-auto">' + badge(p.type, 'key') + tag(p.status) + '</div></div>';
    }

    function rail() {
        var entries = STEPS.map(function (s, i) {
            var cls;
            if (S.view === 'walk') cls = i === S.at ? 'current' : (i < S.at ? 'done' : 'ahead');
            else cls = i === S.at ? 'current' : 'ahead';
            return '<li class="railentry ' + cls + '"' + (cls === 'current' ? ' aria-current="step"' : '') + '>' +
                '<span class="railmark ' + cls + '">' + (cls === 'done' ? '\u2713' : String(i + 1)) + '</span>' +
                esc(s.title) + '</li>';
        }).join('');
        var side = '<div class="side">' +
            '<div class="railentry ' + (S.view === 'history' ? 'current' : '') + '">' +
            '<span class="railmark ' + (S.view === 'history' ? 'current' : 'ahead') + '">\u21ba</span>History</div>' +
            '<div class="railentry ' + (S.view === 'refused' ? 'current' : '') + '">' +
            '<span class="railmark ' + (S.view === 'refused' ? 'current' : 'ahead') + '">\u26a0</span>Refused</div>' +
            '</div>';
        return '<nav class="railnav" aria-label="Journey steps"><ol>' + entries + '</ol>' + side + '</nav>';
    }

    function body() {
        if (S.view === 'history') return historyStep();
        if (S.view === 'refused') return refusedStep();
        var id = STEPS[S.at].id;
        if (id === 'overview') return overviewStep();
        if (id === 'identifiers') return identifiersStep();
        if (id === 'contacts') return contactsStep();
        if (id === 'memberships') return membershipsStep();
        if (id === 'structure') return structureStep();
        if (id === 'review') return reviewStep();
        return outcomeStep();
    }

    /* ------------------------------------------------------------- the list */

    function matches() {
        var q = S.query.trim().toLowerCase();
        if (q === '') return PARTIES;
        return PARTIES.filter(function (p) {
            return p.name.toLowerCase().indexOf(q) >= 0 ||
                p.short.toLowerCase().indexOf(q) >= 0 ||
                p.code.toLowerCase().indexOf(q) === 0 ||
                p.codename.indexOf(q) >= 0;
        });
    }

    function listScreen() {
        var rows = matches().map(function (p) {
            return '<tr class="clickable' + (p.code === S.code ? ' on' : '') +
                '" data-act="party" data-code="' + esc(p.code) + '">' +
                '<td class="mono nowrap">' + esc(p.code) + '</td>' +
                '<td>' + esc(p.name) + '</td>' +
                '<td>' + badge(p.type, 'key') + '</td>' +
                '<td>' + tag(p.status) + '</td>' +
                '<td class="mono">' + esc(p.bc) + '</td>' +
                '<td class="num">' + esc(p.version) + '</td>' +
                '<td class="dim">' + esc(p.by) + '</td>' +
                '<td class="actions"><button class="btn small" data-act="party" data-code="' +
                esc(p.code) + '">Open</button></td></tr>';
        }).join('');
        if (rows === '') {
            rows = '<tr><td colspan="8" class="faint">No party matches. Clear the search, or add a party.</td></tr>';
        }
        return '<div class="page"><h1>Parties</h1>' +
            '<p class="lede">The legal entities this tenant trades as. A party is corrected in place: ' +
            'it keeps its identity and its codename, and each correction is a new version of the same record.</p>' +
            '<div class="searchrow">' +
            '<label class="field"><span class="lbl">Search parties</span>' +
            '<input data-focus="query" data-q="1" placeholder="Name, short code or codename" value="' +
            esc(S.query) + '"></label>' +
            '<button class="btn" data-act="search">Search</button>' +
            '<button class="btn primary" data-act="newparty">New party</button>' +
            '</div>' +
            '<p class="countline">' + esc(matches().length) + ' of ' + esc(PARTIES.length) +
            ' parties \u00b7 the tenant\'s System party is excluded: party_category is not user-editable.</p>' +
            '<div class="card"><table class="table">' +
            '<caption>refdata.v1.parties.list \u00b7 page 1 of 1 \u00b7 order by short_code</caption>' +
            '<thead><tr><th>Code</th><th>Name</th><th>Type</th><th>Status</th>' +
            '<th>Business Center</th><th>Version</th><th>Modified By</th><th></th></tr></thead>' +
            '<tbody>' + rows + '</tbody></table></div>' +
            '<div class="gapnote"><b>Gap.</b> The list filter is <span class="mono">refdata.v1.parties.list</span> ' +
            'with a search term. The party model has no "as of" column here, so the list reads current rows; ' +
            'a dated list would need the versions read.</div></div>';
    }

    /* --------------------------------------------------------- overview step */

    function overviewStep() {
        var p = party();
        var parents = PARTIES.filter(function (x) { return x.code !== p.code; });
        var namePending = S.name !== p.name;
        var shortPending = S.short !== p.short;
        return '<div class="notice info"><b>A correction, not a replacement.</b> ' +
            'This write states the version it read (' + esc(p.version) + '), so a record that moved on is ' +
            'refused instead of overwritten. The codename is immutable and Party Category is set by provisioning; ' +
            'neither can be typed here. <span class="mono">refdata.v1.parties.put</span> with ' +
            '<span class="mono">precondition = must_match_version ' + esc(p.version) + '</span>.</div>' +
            '<div class="grid2">' +
            fieldText('Short Code', 'shortname', S.short, 'short_code \u00b7 natural key \u00b7 required', shortPending) +
            fieldText('Full Name', 'name', S.name, 'full_name \u00b7 the registered legal name', namePending) +
            fieldStatic('Codename (codename)', p.codename, 'Immutable once assigned; generated if left blank.') +
            fieldStatic('Transliterated Name', p.translit === null ? '\u2014' : p.translit,
                'ASCII form, filled from GLEIF for a non-Latin name.') +
            '<label class="field"><span class="lbl">Party Type</span>' +
            '<select data-type="1">' + opts(PARTY_TYPES, S.type) + '</select>' +
            '<div class="hint">party_type lookup \u00b7 refdata.v1.party_types.list</div></label>' +
            '<label class="field"><span class="lbl">Status</span>' +
            '<select data-status="1">' + opts(PARTY_STATUSES, S.status) + '</select>' +
            '<div class="hint">party_status lookup \u00b7 refdata.v1.party_statuses.list</div></label>' +
            '<label class="field"><span class="lbl">Parent Party</span>' +
            '<select data-parent="1">' +
            '<option value=""' + (S.parent === '' ? ' selected' : '') + '>No Parent</option>' +
            optsNamed(parents.map(function (x) { return { code: x.code, name: x.name }; }), S.parent) + '</select>' +
            '<div class="hint">parent_party_id \u00b7 the tenant\'s hierarchy. One root party per tenant.</div></label>' +
            '<label class="field"><span class="lbl">Business Center</span>' +
            '<select data-bc="1">' + optsNamed(BUSINESS_CENTRES, S.bc) + '</select>' +
            '<div class="hint">business_center_code \u00b7 FpML business centre</div></label>' +
            '<div class="span2">' +
            '<label class="checkline"><input type="checkbox" data-regdefault="1"' +
            (S.regDefault ? ' checked' : '') + '><span>New self-registered accounts join this party ' +
            '<span class="mono">(is_registration_default)</span>. At most one live party per tenant may hold it.</span>' +
            '</label>' +
            '</div>' +
            fieldStatic('Party Category', p.category,
                'System or Operational. Only tenant provisioning writes a System party; the field has no form control.') +
            fieldStatic('Logo (image_id)', '\u2014 none \u2014',
                'Optional image in the assets store. Nothing in this journey uploads one.') +
            '</div>' +
            '<div class="panel-soft" style="margin-top:6px"><h3>Audit</h3>' +
            '<p class="mono">version ' + esc(p.version) + ' \u00b7 modified_by ' + esc(p.by) +
            ' \u00b7 recorded_at ' + esc(p.at) + '</p>' +
            '<p>The row keeps modified_by, version and recorded_at. The full field diff per version is on the ' +
            'History step.</p></div>' +
            pendingLine();
    }

    function fieldText(label, key, value, hint, pending) {
        return '<label class="field"><span class="lbl">' + esc(label) + '</span>' +
            '<input data-focus="' + esc(key) + '" data-' + (key === 'name' ? 'name' : 'shortname') +
            '="1" value="' + esc(value) + '">' +
            '<div class="hint">' + esc(hint) + (pending ? ' \u00b7 <span class="pending">pending correction</span>' : '') +
            '</div></label>';
    }

    function fieldStatic(label, value, hint) {
        return '<label class="field"><span class="lbl">' + esc(label) + '</span>' +
            '<input value="' + esc(value) + '" disabled>' +
            '<div class="hint">' + esc(hint) + '</div></label>';
    }

    function pendingLine() {
        var p = party();
        if (S.name === p.name && S.short === p.short && S.bc === p.bc && S.type === p.type &&
            S.status === p.status && S.parent === (p.parent === null ? '' : p.parent)) {
            return '<div class="hint" style="margin-top:12px">No field on this step is edited yet. ' +
                'The walk carries the edits forward to Review.</div>';
        }
        return '<div class="notice warn" style="margin-top:12px;margin-bottom:0">Pending corrections on this ' +
            'step: legal name, and any other field you changed. Review states them before anything is written.</div>';
    }

    /* ------------------------------------------------------ identifiers step */

    function identifiersStep() {
        var d = detail(S.code);
        var rows = d.identifiers.map(function (id) {
            var sc = scheme(id.scheme);
            var isChanging = S.newValue.trim() !== '' && S.scheme === id.scheme;
            return '<li' + (isChanging ? ' class="closing"' : '') + '>' +
                '<span class="grow"><span class="val mono">' + esc(id.value) + '</span>' +
                '<span class="sub"> \u00b7 ' + esc(id.desc) + '</span>' +
                '<div class="sub">scheme <b>' + esc(id.scheme) + '</b> \u00b7 max cardinality ' +
                esc(sc.max) + ' per party \u00b7 version ' + esc(id.version) +
                (isChanging ? ' \u00b7 <span class="badge closing">retiring</span>' : '') + '</div></span>' +
                '<span class="acts">' +
                '<button class="btn small" data-act="pick-scheme" data-scheme="' + esc(id.scheme) + '">' +
                'Record a new value</button></span></li>';
        }).join('');
        if (rows === '') rows = '<li class="faint">This party holds no identifier.</li>';

        var existing = identifierFor(S.scheme);
        return '<p class="dim" style="margin-top:0">Identifiers are rows, keyed by ' +
            '<span class="mono">(party_id, id_scheme, id_value)</span>. A value is part of that key, so it is ' +
            'retired and replaced, never edited in place. The party\'s version is bumped by the child write.</p>' +
            '<ul class="rowlist">' + rows + '</ul>' +
            '<fieldset style="margin-top:18px"><legend>Record a new identifier value</legend>' +
            '<div class="grid2">' +
            '<label class="field"><span class="lbl">Scheme</span>' +
            '<select data-scheme="1">' + optsNamed(SCHEMES, S.scheme) + '</select>' +
            '<div class="hint">party_id_scheme \u00b7 refdata.v1.party_id_schemes.list \u00b7 ' +
            'max cardinality ' + esc(scheme(S.scheme).max) + ' per party</div></label>' +
            '<label class="field"><span class="lbl">New value</span>' +
            '<input data-focus="value" data-value="1" placeholder="' +
            (existing ? 'replaces ' + esc(existing.value) : 'the identifier value') + '" value="' +
            esc(S.newValue) + '"></label>' +
            '<label class="field span2"><span class="lbl">Description</span>' +
            '<input data-desc="1" placeholder="Where the value came from" value="' + esc(S.idDesc) + '"></label>' +
            '</div>' +
            '<div class="hint">' + (existing
                ? 'This scheme already holds <b>' + esc(existing.value) + '</b>. Writing a new value retires it with ' +
                  '<span class="mono">refdata.v1.party_identifiers.delete</span> and writes the new one with ' +
                  '<span class="mono">.put</span>.'
                : 'This party holds no value under ' + esc(S.scheme) + ', so the write is a plain ' +
                  '<span class="mono">refdata.v1.party_identifiers.put</span>.') + '</div>' +
            '</fieldset>' +
            '<div class="compare2">' +
            '<div class="comparecard"><h3>A change of legal name</h3>' +
            '<p>It is one field on the party row. The party keeps its id, its codename and every identifier.</p>' +
            '<ol>' +
            '<li>Edit <span class="mono">full_name</span> on the Overview step.</li>' +
            '<li>Write the party: <span class="op">refdata.v1.parties.put</span>.</li>' +
            '<li>The party moves from v' + esc(party().version) + ' to v' + esc(party().version + 1) +
            '; one row changes.</li>' +
            '<li>No identifier is touched. The LEI still means the same legal person.</li>' +
            '</ol></div>' +
            '<div class="comparecard"><h3>A change of identifier</h3>' +
            '<p>It is a row of its own, and the value is part of that row\'s natural key, so it cannot be ' +
            'edited. The old row is closed and a new one is written.</p>' +
            '<ol>' +
            '<li>Retire the old value: <span class="op">refdata.v1.party_identifiers.delete</span>.</li>' +
            '<li>Write the new value: <span class="op">refdata.v1.party_identifiers.put</span>.</li>' +
            '<li>The scheme\'s <span class="mono">max_cardinality</span> limits how many values of that scheme ' +
            'one party may hold at once.</li>' +
            '<li>The party\'s version is bumped by the child write, so a stale party write is refused.</li>' +
            '</ol></div></div>' +
            '<div class="gapnote"><b>Read the difference on the History step.</b> ' +
            '<span class="mono">refdata.v1.parties.composite_as_of</span> returns the party with its identifiers ' +
            'and contacts as they stood in one version window, which is what makes the two changes comparable ' +
            'after the fact. No operation reads one identifier row\'s own field diff; the versions subjects ' +
            '(party_identifiers_versions.list/.get) return raw rows only.</div>';
    }

    /* --------------------------------------------------------- contacts step */

    function contactsStep() {
        var d = detail(S.code);
        var rows = d.contacts.map(function (c) {
            var isPrimary = c.type === S.primary;
            var open = c.type === S.contact;
            return '<li class="grow"' + (open ? ' style="border-color:#24344d"' : '') + '>' +
                '<div style="display:flex;align-items:center;gap:10px">' +
                '<span class="val">' + esc(c.type) + '</span>' +
                (isPrimary ? badge('primary', 'primary') : '') +
                '<span class="sub">version ' + esc(c.version) + '</span>' +
                '<span class="ml-auto acts">' +
                '<button class="btn small" data-act="primary" data-primary="' + esc(c.type) + '"' +
                (isPrimary ? ' disabled' : '') + '>Mark primary</button>' +
                '<button class="btn small" data-act="open-contact" data-contact="' + esc(c.type) + '">' +
                (open ? 'Shown' : 'Show') + '</button>' +
                '</span></div>' +
                '<div class="sub">' + esc(c.line1) + (c.line2 ? ', ' + esc(c.line2) : '') + ' \u00b7 ' +
                esc(c.city) + (c.state ? ', ' + esc(c.state) : '') + ' ' + esc(c.postal) + ' \u00b7 ' +
                esc(c.country) + '</div>' +
                '<div class="sub mono">' + esc(c.phone) + ' \u00b7 ' + esc(c.email) +
                (c.web ? ' \u00b7 ' + esc(c.web) : '') + '</div>' +
                '</li>';
        }).join('');
        if (rows === '') rows = '<li class="faint">This party holds no contact information.</li>';

        var c = contactRow(S.contact);
        var full = '';
        if (c !== undefined) {
            full = '<fieldset style="margin-top:20px"><legend>Contact \u00b7 ' + esc(c.type) + '</legend>' +
                '<div class="grid2">' +
                contactField('Type', 'contact_type', c.type) +
                contactField('Street Line 1', 'street_line_1', c.line1) +
                contactField('Street Line 2', 'street_line_2', c.line2) +
                contactField('City', 'city', c.city) +
                contactField('State', 'state', c.state) +
                contactField('Country Code', 'country_code', c.country) +
                contactField('Postal Code', 'postal_code', c.postal) +
                contactField('Phone', 'phone', c.phone) +
                contactField('Email', 'email', c.email) +
                contactField('Web Page', 'web_page', c.web) +
                '</div>' +
                '<div class="hint">Writes with <span class="mono">refdata.v1.party_contact_informations.put</span> ' +
                'and the row\'s own version as its precondition. One row per contact type per party.</div>' +
                '</fieldset>';
        }
        return '<p class="dim" style="margin-top:0">The party keeps four contacts, one per purpose. ' +
            'Each is a row in <span class="mono">party_contact_informations</span>; ' +
            '<span class="mono">contact_type</span> references the tenant\'s contact-type list.</p>' +
            '<ul class="rowlist">' + rows + '</ul>' + full +
            '<div class="gapnote"><b>Gap drawn here.</b> ' +
            '<span class="mono">party_contact_information</span> carries no primary flag. The model gives one row ' +
            'per <span class="mono">contact_type</span> (Legal, Operations, Settlement, Billing), so "primary" is ' +
            'not a column the server has. This screen marks <b>' + esc(S.primary) + '</b> as primary as a client ' +
            'convention, and the Review step records that choice with no operation behind it. The candidate is an ' +
            '<span class="mono">is_primary</span> column with a one-per-party constraint.</div>';
    }

    function contactField(label, col, value) {
        return '<label class="field"><span class="lbl">' + esc(label) + '</span>' +
            '<input value="' + esc(value) + '" disabled>' +
            '<div class="hint mono">' + esc(col) + '</div></label>';
    }

    /* ------------------------------------------------------ memberships step */

    function membershipList(kind, codes, names) {
        return codes.map(function (code) {
            var closing = kind === 'country' ? S.closeCountry === code : S.closeCurrency === code;
            return '<li' + (closing ? ' class="closing"' : '') + '>' +
                '<span class="grow"><span class="val mono">' + esc(code) + '</span>' +
                ' <span class="sub">' + esc(names[code]) + '</span>' +
                '<div class="sub">' + (closing
                    ? '<span class="badge closing">closing</span> the junction row is closed, not deleted'
                    : 'open \u00b7 the party sees this ' + esc(kind)) + '</div></span>' +
                '<span class="acts">' +
                (closing
                    ? '<button class="btn small" data-act="reopen" data-kind="' + esc(kind) + '">Keep open</button>'
                    : '<button class="btn small danger" data-act="close" data-kind="' + esc(kind) +
                      '" data-code="' + esc(code) + '">Close</button>') +
                '</span></li>';
        }).join('');
    }

    function membershipsStep() {
        var d = detail(S.code);
        var cRows = d.countries.length ? membershipList('country', d.countries, COUNTRIES) :
            '<li class="faint">No country membership.</li>';
        var mRows = d.currencies.length ? membershipList('currency', d.currencies, CURRENCIES) :
            '<li class="faint">No currency membership.</li>';
        return '<div class="notice info">A membership is a visibility link, not a definition. The country and the ' +
            'currency are the tenant\'s shared reference data; the junction decides whether <b>this</b> party ' +
            'sees it. Closing one writes a closed version of the junction row, so the party stops seeing it and ' +
            'every other party keeps it.</div>' +
            '<div class="compare2">' +
            '<div class="comparecard"><h3>Countries this party operates in</h3>' +
            '<p class="mono" style="font-size:12px">refdata.v1.party_countries.list_by_party_id</p>' +
            '<ul class="rowlist">' + cRows + '</ul>' +
            '<p class="hint">Close writes <span class="mono">refdata.v1.party_countries.delete</span>.</p></div>' +
            '<div class="comparecard"><h3>Currencies this party operates in</h3>' +
            '<p class="mono" style="font-size:12px">refdata.v1.party_currencies.list_by_party_id</p>' +
            '<ul class="rowlist">' + mRows + '</ul>' +
            '<p class="hint">Close writes <span class="mono">refdata.v1.party_currencies.delete</span>.</p></div>' +
            '</div>' +
            '<div class="panel-soft" style="margin-top:16px"><h3>What closing a membership means</h3>' +
            '<p>The party stops seeing the country or the currency: it disappears from this party\'s pickers and ' +
            'from the trades this party books.</p>' +
            '<p>The definition itself is untouched. <span class="mono">ores_refdata_countries_tbl</span> and ' +
            '<span class="mono">ores_refdata_currencies_tbl</span> are tenant-level rows, and every other party ' +
            'that holds the same membership keeps it.</p>' +
            '<p>Nothing already booked is rewritten. A trade keeps the country and currency it was booked with; ' +
            'the membership only decides what this party may choose next.</p>' +
            '<p>A closed membership is reversible: linking it again writes the junction row once more.</p></div>' +
            '<div class="gapnote"><b>Gap drawn here.</b> The junction tables keep closed versions, but no ' +
            'operation reads them: the generated protocol carries ' +
            '<span class="mono">party_countries.list/.get/.put/.delete/.list_by_party_id</span> and no versions ' +
            'subject. So a closed membership leaves this panel with no on-screen record of when it closed or who ' +
            'closed it. The candidate is a versions read for the two junctions, in the shape the entity models ' +
            'already have.</div>';
    }

    /* -------------------------------------------------------- structure step */

    function structureStep() {
        var d = detail(S.code);
        var linked = d.counterparties.map(function (code) {
            return '<li><span class="grow"><span class="val">' + esc(COUNTERPARTIES[code]) + '</span>' +
                '<div class="sub mono">' + esc(code) + ' \u00b7 party_counterparties row</div></span>' +
                '<span class="acts"><button class="btn small danger" data-act="unlink" data-code="' + esc(code) +
                '">Remove link</button></span></li>';
        }).join('');
        if (linked === '') linked = '<li class="faint">No counterparty is linked to this party.</li>';
        var available = UNLINKED.filter(function (code) {
            return d.counterparties.indexOf(code) < 0;
        }).map(function (code) {
            return '<li><span class="grow"><span class="val">' + esc(COUNTERPARTIES[code]) + '</span>' +
                '<div class="sub mono">' + esc(code) + ' \u00b7 tenant-wide</div></span>' +
                '<span class="acts"><button class="btn small" data-act="link" data-code="' + esc(code) +
                '">Link to this party</button></span></li>';
        }).join('');
        if (available === '') available = '<li class="faint">Every tenant counterparty is already linked.</li>';

        var units = d.units.map(function (u) {
            var chosen = u.name === S.bu ? S.unitType : u.type;
            var t = unitType(chosen);
            var parentType = '';
            if (u.parent !== null) {
                var pu = unit(u.parent);
                parentType = pu === undefined ? '' : (pu.type + ' (level ' + unitType(pu.type).level + ')');
            }
            return '<li><span class="grow"><span class="val">' +
                (u.parent === null ? '' : '<span class="indent">\u2514 </span>') + esc(u.name) + '</span>' +
                '<div class="sub mono">' + esc(u.code) + ' \u00b7 ' +
                (u.parent === null ? 'top-level unit' : 'under ' + esc(u.parent) +
                    (parentType ? ' \u00b7 parent type ' + esc(parentType) : '')) +
                ' \u00b7 ' + esc(u.bc) + ' \u00b7 ' + esc(u.status) + '</div></span>' +
                '<span class="acts typepick"><select data-unit="' + esc(u.name) + '">' +
                UNIT_TYPES.map(function (ut) {
                    return '<option value="' + esc(ut.code) + '"' + (ut.code === chosen ? ' selected' : '') +
                        '>' + esc(ut.code + ' \u2014 level ' + ut.level) + '</option>';
                }).join('') + '</select></span></li>';
        }).join('');
        if (units === '') units = '<li class="faint">This party has no business unit.</li>';

        var ladder = UNIT_TYPES.map(function (ut) {
            return '<div class="lvl"><b>' + esc(ut.code) + '</b><span>level ' + esc(ut.level) +
                ' \u00b7 ' + esc(ut.name) + '</span></div>';
        }).join('');

        return '<div class="panel-soft" style="margin-bottom:16px"><h3>Party hierarchy</h3>' +
            '<p><b>' + esc(party().name) + '</b> sits under ' +
            (party().parent === null ? 'nothing: it is the tenant\'s root party' :
                '<b>' + esc(partyName(party().parent)) + '</b>') +
            '. <span class="mono">parent_party_id</span>, written on the Overview step.</p></div>' +
            '<div class="compare2">' +
            '<div class="comparecard"><h3>Counterparties this party can see</h3>' +
            '<p>Tenant-wide counterparty identity, linked per party by <span class="mono">party_counterparties' +
            '</span>. Unlinking hides the counterparty from this party and leaves the counterparty alone.</p>' +
            '<ul class="rowlist">' + linked + '</ul>' +
            '<p class="hint" style="margin-top:10px">Available to link</p>' +
            '<ul class="rowlist">' + available + '</ul>' +
            '<p class="hint">Writes <span class="mono">refdata.v1.party_counterparties.put</span> / ' +
            '<span class="mono">.delete</span>.</p></div>' +
            '<div class="comparecard"><h3>Business units this party is made of</h3>' +
            '<p>A unit is classified by a business unit type, and the type carries a level. A child\'s level must ' +
            'be strictly greater than its parent\'s.</p>' +
            '<ul class="rowlist">' + units + '</ul>' +
            '<p class="hint">Writes <span class="mono">refdata.v1.business_units.put</span>; the type picker reads ' +
            '<span class="mono">refdata.v1.business_unit_types.list</span>.</p>' +
            '<div class="levelladder" style="margin-top:10px">' + ladder + '</div></div></div>' +
            '<div class="gapnote"><b>The rule the server enforces.</b> A unit type level is checked by the ' +
            'business-unit insert trigger: when both the unit and its parent carry a type, the child\'s level must ' +
            'be strictly greater than the parent\'s. A choice that breaks that rule is refused \u2014 the Refused ' +
            'state draws it. Business unit types are classified by <span class="mono">coding_scheme_code</span> ' +
            '(ORES-ORG), which the tenant does not edit here.</div>';
    }

    /* ----------------------------------------------------------- review step */

    function reviewStep() {
        var cs = changes();
        var rows = cs.map(function (c) {
            return '<tr><td>' + esc(c.what) + '</td>' +
                '<td class="dfrom">' + esc(c.from) + '</td>' +
                '<td class="dto">' + esc(c.to) + '</td>' +
                '<td class="mono faint">' + esc(c.op) + '</td></tr>';
        }).join('');
        if (rows === '') {
            rows = '<tr><td colspan="4" class="faint">Nothing is edited. Go back and correct a field, ' +
                'close a membership, or record an identifier.</td></tr>';
        }
        var p = party();
        return '<div class="notice warn">Nothing is written until you confirm. The party write states ' +
            '<span class="mono">must_match_version ' + esc(p.version) + '</span>; a record that moved on is ' +
            'refused, and the refusal names the version it found.</div>' +
            '<table class="table"><thead><tr><th>What</th><th>Before</th><th>After</th><th>Operation</th></tr>' +
            '</thead><tbody>' + rows + '</tbody></table>' +
            '<div class="grid2" style="margin-top:20px">' +
            '<label class="field"><span class="lbl">Change reason</span>' +
            '<select data-reason="1">' + optsNamed(REASONS, S.reason) + '</select>' +
            '<div class="hint">Offered by <span class="mono">dq.v1.change_reasons.list</span>, carried in ' +
            '<span class="mono">change_intent.reason_code</span>.</div></label>' +
            '<label class="field"><span class="lbl">Commentary</span>' +
            '<input data-commentary="1" placeholder="Why the record changed" value="' + esc(S.commentary) + '">' +
            '<div class="hint">change_intent.commentary \u00b7 kept with the version</div></label>' +
            '</div>' +
            '<div class="panel-soft"><h3>What the server will do</h3>' +
            '<p>Each row above is one operation. The party write carries the version precondition; a child write ' +
            'bumps the party version on its own. The reason and the commentary are written with every row, and a ' +
            'new version is appended \u2014 history is never rewritten.</p></div>' +
            '<div class="gapnote"><b>Gap drawn here.</b> The primary-contact row has no operation behind it: ' +
            'the model has no primary flag (see the Contacts step). It is listed so the reviewer sees the choice ' +
            'the screen makes, and it would write nothing.</div>';
    }

    /* ---------------------------------------------------------- outcome step */

    function outcomeStep() {
        var p = party();
        var cs = changes();
        var written = cs.map(function (c) {
            return '<li><span class="grow"><span class="val">' + esc(c.what) + '</span>' +
                '<div class="sub">' + esc(c.from) + ' \u2192 ' + esc(c.to) + '</div></span>' +
                '<span class="acts mono sub">' + esc(c.op) + '</span></li>';
        }).join('');
        if (written === '') written = '<li class="faint">No operation was written.</li>';
        return '<div class="notice success"><b>Saved.</b> ' + esc(p.name) + ' is now version ' +
            esc(S.version) + '. The party kept its id and its codename; the correction is a new version of the ' +
            'same record, and the previous version is untouched.</div>' +
            '<ul class="rowlist">' + written + '</ul>' +
            '<div class="grid2" style="margin-top:18px">' +
            '<div class="panel-soft"><h3>What reads differently now</h3>' +
            '<p>The list, the picker and every screen that names this party read the corrected values. ' +
            'The published essential data is unchanged: this was a correction, not a re-provision.</p></div>' +
            '<div class="panel-soft"><h3>What changed as a side effect</h3>' +
            '<p>A child write (an identifier, a contact, a business unit) bumps the party\'s version. A party ' +
            'write that was prepared against the old version is therefore refused, which is the point of the ' +
            'precondition.</p></div></div>' +
            '<div class="stepfoot" style="border-top:none;padding-top:0">' +
            '<button class="btn" data-act="state" data-state="list">Back to the list</button>' +
            '<button class="btn primary ml-auto" data-act="state" data-state="history">Open its history</button>' +
            '</div>';
    }

    /* ---------------------------------------------------------- history step */

    /* The versions a read returns. Until the correction is saved the newest is
       the party's current version; after the save the version the walk has just
       written is prepended, built from the same change set Review showed. */
    function versionList() {
        if (!S.saved) return VERSIONS;
        var cs = changes();
        var diff = cs.map(function (c) {
            return ['', c.what, c.from, c.to, 'changed'];
        });
        return [{
            v: S.version, by: 'tenant_admin', at: '2026-10-06 14:22', reason: S.reason,
            commentary: S.commentary !== '' ? S.commentary :
                'Correction written from the review step of this journey.',
            unchanged: 6, diff: diff
        }].concat(VERSIONS);
    }

    function historyStep() {
        var list = versionList();
        var sel = list.filter(function (v) { return v.v === S.version; })[0];
        if (sel === undefined) sel = list[0];
        var rows = sel.diff.map(function (d) {
            return '<tr class="' + esc(d[4]) + '"><td>' + esc(d[1]) + '</td>' +
                '<td>' + (d[4] === 'changed' || d[4] === 'removed' ? '<span class="dfrom">' + esc(d[2]) + '</span>' : esc(d[2])) + '</td>' +
                '<td>' + (d[4] === 'changed' || d[4] === 'added' ? '<span class="dto">' + esc(d[3]) + '</span>' : esc(d[3])) + '</td>' +
                '<td class="dkind">' + esc(d[4]) + '</td></tr>';
        }).join('');
        if (rows === '') {
            rows = '<tr class="same"><td colspan="4" class="faint">Version 1 is the creation: every field was ' +
                'written as it stands, and there is no version before it to diff against.</td></tr>';
        }
        var versions = list.map(function (v) {
            return '<button class="btn small' + (v.v === sel.v ? ' on' : '') + '" data-act="version" data-version="' +
                esc(v.v) + '">v' + esc(v.v) + '</button>';
        }).join('');

        var comp = '<ul class="rowlist">' +
            detail(S.code).identifiers.map(function (id) {
                return '<li><span class="grow"><span class="val mono">' + esc(id.value) + '</span>' +
                    '<div class="sub">' + esc(id.scheme) + ' \u00b7 as of v' + esc(sel.v) + '</div></span></li>';
            }).join('') + '</ul>';
        if (detail(S.code).identifiers.length === 0) comp = '<p class="faint">No identifier as of that version.</p>';

        return '<div class="notice info">Read with <span class="mono">refdata.v1.history.get</span> ' +
            '(<span class="mono">entity_type = ores.refdata.party</span>): every version with its actor and the ' +
            'field diff from the version before. The per-entity ' +
            '<span class="mono">refdata.v1.parties_versions.list/.get</span> return the raw rows; this screen ' +
            'needs the diff, so it uses the generic read.</div>' +
            '<div class="versionrow">' + versions + '</div>' +
            '<div class="panel-soft" style="margin-top:14px"><h3>Version ' + esc(sel.v) + '</h3>' +
            '<p class="mono">modified_by ' + esc(sel.by) + ' \u00b7 recorded_at ' + esc(sel.at) +
            ' \u00b7 reason ' + esc(sel.reason) + '</p>' +
            '<p>' + esc(sel.commentary) + '</p></div>' +
            '<table class="difftable" style="margin-top:14px">' +
            '<thead><tr><th>Field</th><th>Before</th><th>After</th><th></th></tr></thead>' +
            '<tbody>' + rows + '</tbody></table>' +
            (sel.unchanged ? '<p class="hint">' + esc(sel.unchanged) + ' further fields are unchanged in this ' +
                'version and are not listed.</p>' : '') +
            '<div class="compare2" style="margin-top:16px">' +
            '<div class="comparecard"><h3>Identifiers and contacts as of v' + esc(sel.v) + '</h3>' +
            '<p>Read with <span class="mono">refdata.v1.parties.composite_as_of</span>. It returns the party with ' +
            'its identifiers and contacts as they stood during that version\'s window, which is why a legal-name ' +
            'change and an identifier change stay comparable after the fact.</p>' +
            comp + '</div>' +
            '<div class="comparecard"><h3>Revert</h3>' +
            '<p>There is no revert subject. Choosing an older version writes its values back with ' +
            '<span class="mono">refdata.v1.parties.put</span> as a new version, with a reason. History is ' +
            'append-only.</p>' +
            '<button class="btn" data-act="revert" data-version="' + esc(sel.v) + '">Revert to v' +
            esc(sel.v) + '</button>' +
            (S.reverted ? '<div class="hint pending">Revert prepared: the values of v' + esc(sel.v) +
                ' will be written as v' + esc(party().version + 1) + '. This is a client convention; no revert ' +
                'subject exists.</div>' : '') +
            '</div></div>';
    }

    /* ---------------------------------------------------------- refused step */

    function refusal() {
        if (S.fail === 'cardinality') {
            return {
                title: 'Scheme ' + S.scheme + ' permits at most ' + scheme(S.scheme).max +
                    ' identifier per party',
                what: 'A new ' + S.scheme + ' identifier value for ' + party().name + ' was refused.',
                rule: 'ores_refdata_validate_party_id_scheme_fn reads party_id_schemes.max_cardinality ' +
                    'and counts the live identifiers of that scheme for this party.',
                subject: 'refdata.v1.party_identifiers.put',
                body: 'This party already holds <span class="mono">' +
                    esc(identifierFor(S.scheme) ? identifierFor(S.scheme).value : 'a value') + '</span> under ' +
                    esc(S.scheme) + ', and the scheme allows ' + esc(scheme(S.scheme).max) + '. ' +
                    'Nothing was written.',
                fix: 'Retire the existing value first, which writes ' +
                    '<span class="mono">refdata.v1.party_identifiers.delete</span>, and then record the new one. ' +
                    'Or record the new value under a scheme whose cardinality permits another.'
            };
        }
        if (S.fail === 'stale') {
            return {
                title: 'This party moved on: expected version ' + party().version +
                    ', the record is now version ' + (party().version + 1),
                what: 'The correction prepared against version ' + party().version + ' was refused.',
                rule: 'The party write states precondition = must_match_version. The service compares it with ' +
                    'the current row and refuses a mismatch instead of overwriting a concurrent edit.',
                subject: 'refdata.v1.parties.put',
                body: 'The correction was prepared against version ' + esc(party().version) + '. Another session ' +
                    'wrote version ' + esc(party().version + 1) + ' while this screen was open. Nothing was written.',
                fix: 'Reload the record so the screen holds version ' + esc(party().version + 1) +
                    ', then apply the correction again. The other session\'s change is kept.'
            };
        }
        return {
            title: 'Business unit type level 0 cannot be contained by a unit of the same or higher level 0',
            what: 'Unit Type BRANCH on the business unit Rates Trading was refused.',
            rule: 'The business-unit insert trigger checks that a child unit\'s type level is strictly greater ' +
                'than its parent\'s. Business unit type levels run 0 = top, higher = lower in the hierarchy.',
            subject: 'refdata.v1.business_units.put',
            body: 'You set Unit Type <b>BRANCH</b> (level 0) on <b>Rates Trading</b>. Its parent <b>Markets</b> ' +
                'is a <b>DIVISION</b> (level 0). A child cannot sit at the same level as its parent, so the write ' +
                'was refused. Nothing was written: the unit still reads DESK, and Markets is unchanged.',
            fix: 'Choose a type whose level is greater than the parent\'s \u2014 DESK (level 1) or TEAM (level 2) ' +
                'under Markets. If Rates Trading really belongs at level 0, move it out from under Markets first.'
        };
    }

    function refusedStep() {
        var r = refusal();
        return '<div class="notice error"><b>Refused: ' + esc(r.title) + '</b>' +
            '<div style="margin-top:6px">' + esc(r.what) + '</div></div>' +
            '<div class="panel-soft" style="margin-bottom:16px;border-color:#5c2a24">' +
            '<h3>The record is unchanged</h3>' +
            '<p><b>' + esc(party().name) + '</b> is still version <b>' + esc(party().version) + '</b>. The refused ' +
            'write left no version behind: no new row, no bumped version, and no history entry.</p>' +
            '<p class="mono" style="font-size:12px">' + esc(r.subject) + ' \u00b7 result outcome = refused \u00b7 ' +
            'current version ' + esc(party().version) + '</p></div>' +
            '<div class="grid2">' +
            '<div class="panel-soft"><h3>What the server checked</h3><p>' + r.rule + '</p></div>' +
            '<div class="panel-soft"><h3>What the screen shows</h3><p>' + r.body + '</p></div>' +
            '</div>' +
            '<div class="panel-soft" style="margin-top:16px"><h3>What to do instead</h3><p>' + r.fix + '</p>' +
            '<p>The dependent record was not touched. The refusal names the field, the rule and the value it ' +
            'found, so the person knows which unit or identifier to correct rather than guessing.</p></div>' +
            '<div class="gapnote"><b>Drawn as a state, not a toast.</b> The refusal keeps the screen, the unit and ' +
            'its parent visible, so the person can see the dependent record the change would have broken. The ' +
            'candidate is a typed refusal from the server that carries the field and the rule; today the message ' +
            'is the trigger\'s text.</div>';
    }

    /* ---------------------------------------------------------------- footer */

    function foot() {
        if (S.view === 'refused') {
            var to = STEPS[S.at].id;
            var next = FAILS[(FAILS.indexOf(S.fail) + 1) % FAILS.length];
            return '<div class="stepfoot">' +
                '<button class="btn ghost" data-act="state" data-state="' + esc(to) + '">Back to ' +
                esc(STEPS[S.at].title) + '</button>' +
                '<button class="btn ml-auto" data-act="fail" data-fail="' + esc(next) + '">Show another ' +
                'refusal</button>' +
                '</div>';
        }
        if (S.view === 'history') {
            return '<div class="stepfoot">' +
                '<button class="btn ghost" data-act="state" data-state="review">Back to review</button>' +
                '<button class="btn ml-auto" data-act="state" data-state="list">Back to the list</button></div>';
        }
        var step = STEPS[S.at].id;
        if (step === 'outcome') return '';
        var back = S.at > 0 ?
            '<button class="btn ghost" data-act="back">Back</button>' :
            '<button class="btn ghost" data-act="state" data-state="list">Parties</button>';
        var label = step === 'review' ? 'Save correction' : 'Continue';
        var enabled = step !== 'review' || changes().length > 0;
        return '<div class="stepfoot">' + back +
            '<button class="btn primary ml-auto" data-act="' + (step === 'review' ? 'confirm' : 'next') + '"' +
            (enabled ? '' : ' disabled') + '>' + esc(label) + '</button></div>';
    }

    /* ---------------------------------------------------------------- chrome */

    function renderNote() {
        var state = S.view === 'walk' ? STEPS[S.at].id : S.view;
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey Keep a party\'s details current \u00b7 party ' +
            S.code + ' \u00b7 state ' + state;
    }

    function renderBar() {
        var states = ['list'].concat(STEPS.map(function (s) { return s.id; })).concat(['refused', 'history']);
        var current = S.view === 'walk' ? STEPS[S.at].id : S.view;
        var buttons = states.map(function (id) {
            return '<button data-act="state" data-state="' + esc(id) + '"' +
                (current === id ? ' class="on"' : '') + '>' + esc(id) + '</button>';
        }).join('');
        var fails = FAILS.map(function (f) {
            return '<button data-act="fail" data-fail="' + esc(f) + '"' +
                (S.view === 'refused' && S.fail === f ? ' class="on"' : '') + '>' + esc(f) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + buttons +
            '<span class="sep">|</span><span class="label">fail</span>' + fails;
    }

    /* ------------------------------------------------------------ behaviour */

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

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el || el.disabled) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'state') {
            var id = el.getAttribute('data-state');
            if (id === 'outcome' && !S.saved) save();
            setState(id);
        } else if (act === 'party') {
            openParty(el.getAttribute('data-code'));
            seedDemo();
            setState('overview');
        } else if (act === 'search') {
            setState('list');
        } else if (act === 'newparty') {
            setState('list');
        } else if (act === 'next') {
            S.at += 1;
        } else if (act === 'back') {
            if (S.at > 0) S.at -= 1;
        } else if (act === 'confirm') {
            save();
            setState('outcome');
        } else if (act === 'close') {
            var kind = el.getAttribute('data-kind');
            var code = el.getAttribute('data-code');
            if (kind === 'country') S.closeCountry = code;
            else S.closeCurrency = code;
        } else if (act === 'reopen') {
            if (el.getAttribute('data-kind') === 'country') S.closeCountry = '';
            else S.closeCurrency = '';
        } else if (act === 'primary') {
            S.primary = el.getAttribute('data-primary');
        } else if (act === 'open-contact') {
            S.contact = el.getAttribute('data-contact');
        } else if (act === 'pick-scheme') {
            S.scheme = el.getAttribute('data-scheme');
            S.newValue = '';
        } else if (act === 'version') {
            S.version = parseInt(el.getAttribute('data-version'), 10);
            S.reverted = false;
        } else if (act === 'revert') {
            S.reverted = true;
        } else if (act === 'fail') {
            S.fail = el.getAttribute('data-fail');
            armFail();
            S.view = 'refused';
        } else if (act === 'link' || act === 'unlink') {
            /* The mock keeps the linked set fixed; the action is drawn. */
        } else {
            return;
        }
        rerender();
    });

    function applyInput(el) {
        if (el.getAttribute('data-q') !== null) { S.query = el.value; return true; }
        if (el.getAttribute('data-name') !== null) { S.name = el.value; return true; }
        if (el.getAttribute('data-shortname') !== null) { S.short = el.value; return true; }
        if (el.getAttribute('data-value') !== null) { S.newValue = el.value; return true; }
        if (el.getAttribute('data-desc') !== null) { S.idDesc = el.value; return true; }
        if (el.getAttribute('data-commentary') !== null) { S.commentary = el.value; return true; }
        if (el.getAttribute('data-scheme') !== null) { S.scheme = el.value; return true; }
        if (el.getAttribute('data-reason') !== null) { S.reason = el.value; return true; }
        if (el.getAttribute('data-type') !== null) { S.type = el.value; return true; }
        if (el.getAttribute('data-status') !== null) { S.status = el.value; return true; }
        if (el.getAttribute('data-parent') !== null) { S.parent = el.value; return true; }
        if (el.getAttribute('data-bc') !== null) { S.bc = el.value; return true; }
        if (el.getAttribute('data-regdefault') !== null) { S.regDefault = el.checked; return true; }
        var unitName = el.getAttribute('data-unit');
        if (unitName !== null) { S.bu = unitName; S.unitType = el.value; return true; }
        return false;
    }

    function onInput(ev) {
        var el = ev.target;
        if (el && el.getAttribute && applyInput(el)) rerender();
    }

    document.addEventListener('input', onInput);
    document.addEventListener('change', onInput);

    readParams();
    render();
})();

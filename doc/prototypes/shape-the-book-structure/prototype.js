/* Shape the book structure journey prototype. Self-contained: plain
 * JavaScript, mock data, no framework, no build step, and nothing that
 * outlives the page.
 *
 * The tenant administrator walks the portfolio tree, opens or creates a
 * book, classifies it, sets the portfolio rights, reviews the write and
 * reads the outcome. Every field is the one the model carries, and every
 * operation is the subject the generated protocol header carries:
 *
 *   portfolios      refdata.v1.portfolios.list / .get / .put / .put_many / .delete
 *   books           refdata.v1.books.list / .get / .put / .put_many / .delete
 *   portfolio_right refdata.v1.portfolio_rights.list / .put / .put_many / .delete
 *   lookups         refdata.v1.<plural>.list          (statuses, purpose types, ...)
 *   history         refdata.v1.history.get            (entity_type = ores.refdata.book)
 *
 * The book record the server holds carries book_status and
 * regulatory_book_type but neither book_purpose_type nor ledger_feed_type.
 * The prototype draws those two pickers because book classification says a
 * book carries them, and marks them as a gap on the screen.
 *
 * States are chosen from the query string, as the new-party prototype does:
 *   ?state=tree|portfolio|book|classify|rights|review|outcome|refused|history
 *   ?query=EUR                            the search, as typed
 *   ?portfolio=RATES.EUR                  the portfolio node already chosen
 *   ?book=EUR_SWAPS_01                    the book already open
 *   ?new=1                                author a new book, not an existing one
 *   ?status=Active|Frozen|Closed          the book status being set
 *   ?purpose=Trading|Reserve|...          the book purpose type being set
 *   ?regulatory=Trading|Banking           the Basel book classification
 *   ?ledger=None|Automatic|Manual         how the ledger balance is fed
 *   ?sweepable=0|1                        the spot-sweep eligibility flag
 *   ?account=m.okafor                     the account whose rights are shown
 *   ?right=read|open_sandbox|none         the right being considered
 *   ?fail=open_activity|transition|1      which refusal the server returns
 *                                          (1 selects the first kind)
 *   ?version=2                            the history version under review
 * The bar mirrors the same states as buttons, plus the two refusals. */

(function () {
    'use strict';

    /* --------------------------------------------------------- mock data */

    var TENANT = 'Northwind Capital Ltd';
    var ACCOUNTS = [
        { id: 'tenant_admin', name: 'tenant_admin' },
        { id: 'j.smith', name: 'j.smith' },
        { id: 'a.tanaka', name: 'a.tanaka' },
        { id: 'm.okafor', name: 'm.okafor' }
    ];

    /* The portfolio tree. A portfolio is a folder: it holds books and other
     * portfolios and never a deal. GLOBAL is the root, so every book is
     * reachable. purpose_type and aggregation_ccy are the portfolio's own. */
    var PORTFOLIOS = [
        { id: 'GLOBAL', name: 'Global Trading', parent: null, purpose: 'Risk', ccy: 'USD',
          unit: 'Group Treasury', status: 'Active', virtual: false, sandbox: null,
          description: 'The global portfolio tree. Every book is reachable from here.' },
        { id: 'RATES', name: 'Global Rates', parent: 'GLOBAL', purpose: 'Risk', ccy: 'USD',
          unit: 'Rates Desk', status: 'Active', virtual: false, sandbox: null,
          description: 'The rates desks, grouped for risk management.' },
        { id: 'RATES.EUR', name: 'EUR Rates', parent: 'RATES', purpose: 'Risk', ccy: 'EUR',
          unit: 'EUR Rates Desk', status: 'Active', virtual: false, sandbox: null,
          description: 'EUR-denominated rates activity.' },
        { id: 'RATES.USD', name: 'USD Rates', parent: 'RATES', purpose: 'Risk', ccy: 'USD',
          unit: 'USD Rates Desk', status: 'Active', virtual: false, sandbox: null,
          description: 'USD-denominated rates activity.' },
        { id: 'CREDIT', name: 'APAC Credit', parent: 'GLOBAL', purpose: 'Regulatory', ccy: 'USD',
          unit: 'APAC Credit Desk', status: 'Active', virtual: false, sandbox: null,
          description: 'Credit risk in the Asia Pacific region.' },
        { id: 'CREDIT.CN', name: 'China Credit', parent: 'CREDIT', purpose: 'Regulatory', ccy: 'CNY',
          unit: 'China Credit Desk', status: 'Active', virtual: false, sandbox: null,
          description: 'Mainland China credit activity.' }
    ];

    /* The books. A book is a ledger leaf: it holds trades, belongs to exactly
     * one portfolio, and is this tenant's own data. openActivity is the count
     * of trades the ledger still shows open against the book. */
    var BOOKS = [
        { name: 'EUR_SWAPS_01', portfolio: 'RATES.EUR', description: 'Vanilla EUR interest rate swaps.',
          ccy: 'EUR', gl: 'GL-10150-IRS', cost: 'CC-110', unit: 'EUR Rates Desk', status: 'Active',
          regulatory: 'Trading', purpose: 'Trading', ledger: 'Automatic', sweepable: false,
          centre: 'GBLO', openActivity: 14 },
        { name: 'EUR_VOL_01', portfolio: 'RATES.EUR', description: 'EUR swaption volatility book.',
          ccy: 'EUR', gl: 'GL-10150-VOL', cost: 'CC-111', unit: 'EUR Rates Desk', status: 'Active',
          regulatory: 'Trading', purpose: 'Trading', ledger: 'None', sweepable: false,
          centre: 'GBLO', openActivity: 3 },
        { name: 'USD_SWAPS_01', portfolio: 'RATES.USD', description: 'USD interest rate swaps.',
          ccy: 'USD', gl: 'GL-10150-USD', cost: 'CC-120', unit: 'USD Rates Desk', status: 'Active',
          regulatory: 'Trading', purpose: 'Trading', ledger: 'Automatic', sweepable: false,
          centre: 'USNY', openActivity: 41 },
        { name: 'USD_FUNDING', portfolio: 'RATES.USD', description: 'Short-term funding and liquidity.',
          ccy: 'USD', gl: 'GL-10160-FND', cost: 'CC-121', unit: 'USD Rates Desk', status: 'Active',
          regulatory: 'Banking', purpose: 'Funding', ledger: 'Manual', sweepable: false,
          centre: 'USNY', openActivity: 0 },
        { name: 'RESERVE_01', portfolio: 'RATES.USD', description: 'Reserve deals held apart from trading.',
          ccy: 'USD', gl: 'GL-10170-RSV', cost: 'CC-122', unit: 'USD Rates Desk', status: 'Frozen',
          regulatory: 'Banking', purpose: 'Reserve', ledger: 'None', sweepable: false,
          centre: 'USNY', openActivity: 0 },
        { name: 'CN_CREDIT_01', portfolio: 'CREDIT.CN', description: 'China credit sales book.',
          ccy: 'CNY', gl: 'GL-10200-CN', cost: 'CC-210', unit: 'China Credit Desk', status: 'Active',
          regulatory: 'Banking', purpose: 'Sales', ledger: 'Automatic', sweepable: false,
          centre: 'CNBJ', openActivity: 7 },
        { name: 'SWEEP_TARGET', portfolio: 'GLOBAL', description: 'The single central spot-sweep target.',
          ccy: 'USD', gl: 'GL-10000-SWP', cost: 'CC-001', unit: 'Group Treasury', status: 'Active',
          regulatory: 'Banking', purpose: 'SweepTarget', ledger: 'Manual', sweepable: true,
          centre: 'WRLD', openActivity: 0 },
        { name: 'REMITTANCE_TARGET', portfolio: 'GLOBAL', description: 'The central remittance target.',
          ccy: 'USD', gl: 'GL-10000-REM', cost: 'CC-002', unit: 'Group Treasury', status: 'Active',
          regulatory: 'Banking', purpose: 'RemittanceTarget', ledger: 'Manual', sweepable: false,
          centre: 'WRLD', openActivity: 0 }
    ];

    /* ores_refdata_portfolio_rights_tbl. A right at a node applies to every
     * node below it, so a right on a desk covers its sub-desks, not its
     * siblings. Two rights exist: read and open_sandbox. */
    var RIGHTS_BASE = [
        { account: 'tenant_admin', portfolio: 'GLOBAL', right: 'read' },
        { account: 'tenant_admin', portfolio: 'GLOBAL', right: 'open_sandbox' },
        { account: 'j.smith', portfolio: 'GLOBAL', right: 'read' },
        { account: 'a.tanaka', portfolio: 'RATES', right: 'open_sandbox' },
        { account: 'a.tanaka', portfolio: 'RATES.EUR', right: 'read' },
        { account: 'm.okafor', portfolio: 'CREDIT', right: 'read' }
    ];

    /* ores_refdata_business_units_tbl, the units a book's owner_unit_id may
     * name. The model says it must be a unit named in the portfolio ancestry. */
    var UNITS = [
        { code: 'GRP-TREASURY', name: 'Group Treasury', centre: 'WRLD' },
        { code: 'RATES-DESK', name: 'Rates Desk', centre: 'GBLO' },
        { code: 'EUR-RATES', name: 'EUR Rates Desk', centre: 'GBLO' },
        { code: 'USD-RATES', name: 'USD Rates Desk', centre: 'USNY' },
        { code: 'APAC-CREDIT', name: 'APAC Credit Desk', centre: 'HKHH' },
        { code: 'CHINA-CREDIT', name: 'China Credit Desk', centre: 'CNBJ' }
    ];

    /* The shared taxonomy. Each list is a system-tenant lookup with its own
     * subject; a book only names a code from it. The values are the seeded
     * ones the refdata screens paint. */
    var LISTS = {
        book_statuses: {
            label: 'Book statuses', subject: 'refdata.v1.book_statuses.list',
            model: 'ores.refdata.book_status', key: 'book_status',
            why: 'The lifecycle states a book may carry: Active, Closed, Frozen.',
            rows: [
                { code: 'Active', desc: 'Book is open and accepting new trades.' },
                { code: 'Closed', desc: 'Book is closed. Existing trades remain but no new trades are allowed.' },
                { code: 'Frozen', desc: 'Book is frozen. No modifications allowed, including new trades or amendments.' }
            ]
        },
        book_purpose_types: {
            label: 'Book purpose types', subject: 'refdata.v1.book_purpose_types.list',
            model: 'ores.refdata.book_purpose_type', key: 'book_purpose_type',
            why: 'The risk role a book plays. Mutually exclusive: a book carries exactly one.',
            rows: [
                { code: 'Trading', desc: 'Ordinary trading book: market-making, arbitrage, client-facing risk-taking.' },
                { code: 'Reserve', desc: 'Holds risk retained by the desk rather than passed on.' },
                { code: 'Funding', desc: 'Books the short-term funding and liquidity trades.' },
                { code: 'Wash', desc: 'Routes offsetting back-to-back trades to a designated risk book.' },
                { code: 'WriteOff', desc: 'Holds written-off positions, kept for the audit trail.' },
                { code: 'Test', desc: 'Non-production book for testing and training.' },
                { code: 'Sales', desc: 'Books sales-credit entries that are not risk-bearing trading.' },
                { code: 'SweepTarget', desc: 'The single central book that receives spot-sweep transfers.' },
                { code: 'RemittanceTarget', desc: 'The single central book that receives central-remittance transfers.' }
            ]
        },
        ledger_feed_types: {
            label: 'Ledger feed types', subject: 'refdata.v1.ledger_feed_types.list',
            model: 'ores.refdata.ledger_feed_type', key: 'ledger_feed_type',
            why: 'How a book’s ledger balance is fed. One value at a time.',
            rows: [
                { code: 'None', desc: 'Not fed from any source book.' },
                { code: 'Automatic', desc: 'Fed by an automated ledger process.' },
                { code: 'Manual', desc: 'Fed by manual entry.' }
            ]
        },
        regulatory_book_types: {
            label: 'Regulatory book types', subject: 'refdata.v1.regulatory_book_types.list',
            model: 'ores.refdata.regulatory_book_type', key: 'regulatory_book_type',
            why: 'The Basel III/IV FRTB trading book / banking book boundary.',
            rows: [
                { code: 'Trading', desc: 'Held with trading intent; FRTB market risk capital.' },
                { code: 'Banking', desc: 'Held to maturity or for balance-sheet management; credit risk capital.' }
            ]
        },
        purpose_types: {
            label: 'Purpose types', subject: 'refdata.v1.purpose_types.list',
            model: 'ores.refdata.purpose_type', key: 'purpose_type',
            why: 'The intent a portfolio is classified with. A portfolio names one; a book does not.',
            rows: [
                { code: 'Risk', desc: 'Portfolio used for risk aggregation and risk management reporting.' },
                { code: 'Regulatory', desc: 'Portfolio used for regulatory capital and compliance reporting.' },
                { code: 'ClientReporting', desc: 'Portfolio used for client-facing reporting and statements.' },
                { code: 'Internal', desc: 'Portfolio used for internal management reporting and P&L attribution.' }
            ]
        }
    };

    var CENTRES = [
        { code: 'WRLD', name: 'Worldwide (global sentinel)' },
        { code: 'GBLO', name: 'London' },
        { code: 'USNY', name: 'New York' },
        { code: 'CNBJ', name: 'Beijing' },
        { code: 'HKHH', name: 'Hong Kong' }
    ];

    var CURRENCIES = ['EUR', 'USD', 'CNY', 'GBP', 'JPY', 'CHF'];

    /* The transitions the journey draws a rule for. The store has no such
     * rule today: ores_refdata_validate_book_status_fn only checks that the
     * code exists. Drawn here as the rule the refusal state argues for. */
    var TRANSITIONS = { Active: ['Frozen', 'Closed'], Frozen: ['Active'], Closed: [] };

    var CHANGE_REASONS = [
        { code: 'system.new_record', name: 'New record' },
        { code: 'refdata.book.created', name: 'Book created' },
        { code: 'refdata.book.classification_changed', name: 'Book classification changed' },
        { code: 'refdata.book.status_changed', name: 'Book status changed' },
        { code: 'refdata.portfolio_right.granted', name: 'Portfolio right granted' }
    ];

    var STEPS = [
        { id: 'tree', title: 'Book tree', lead: 'The tenant’s portfolios and the books under them, with search.' },
        { id: 'portfolio', title: 'Choose the portfolio', lead: 'A book belongs to exactly one portfolio. Everything it inherits comes from here.' },
        { id: 'book', title: 'Create or open the book', lead: 'Name it, and give it the fields finance and the ledger use.' },
        { id: 'classify', title: 'Set its classification', lead: 'The book’s own status and the three independent classification axes.' },
        { id: 'rights', title: 'Set the rights', lead: 'Who may see this node, and what lies below it.' },
        { id: 'review', title: 'Review', lead: 'Nothing is written until you confirm.' },
        { id: 'outcome', title: 'Outcome', lead: '', final: true }
    ];

    var SIDES = [
        { id: 'refused', title: 'Refused' },
        { id: 'history', title: 'History' }
    ];

    var ALL_STATES = STEPS.map(function (s) { return s.id; }).concat(SIDES.map(function (s) { return s.id; }));

    /* History, per book. Version 1 is the oldest and carries no diff. */
    var HISTORIES = {
        EUR_SWAPS_01: [
            { version: 1, by: 'ores_refdata_service', performed: 'ores_refdata_service',
              at: '2026-06-01 09:14', reason: 'system.new_record',
              note: 'Created by the nightly book sync from the ledger.', fields: {
                  name: 'EUR_SWAPS_01', description: '', functional_currency: 'EUR',
                  gl_account_ref: '', cost_center: '', book_status: 'Active',
                  regulatory_book_type: 'Trading', book_purpose_type: 'Trading',
                  ledger_feed_type: 'None', is_sweepable: 'false', rates_centre_code: 'WRLD' } },
            { version: 2, by: 'j.smith', performed: 'ores_refdata_service',
              at: '2026-07-18 11:02', reason: 'refdata.book.classification_changed',
              note: 'Mapped the book to the ledger and its cost centre.', fields: {
                  name: 'EUR_SWAPS_01', description: '', functional_currency: 'EUR',
                  gl_account_ref: 'GL-10150-IRS', cost_center: 'CC-110', book_status: 'Active',
                  regulatory_book_type: 'Trading', book_purpose_type: 'Trading',
                  ledger_feed_type: 'Automatic', is_sweepable: 'false', rates_centre_code: 'GBLO' } },
            { version: 3, by: 'a.tanaka', performed: 'ores_refdata_service',
              at: '2026-09-02 16:41', reason: 'refdata.book.classification_changed',
              note: 'Described the book for the desk report.', fields: {
                  name: 'EUR_SWAPS_01', description: 'Vanilla EUR interest rate swaps.',
                  functional_currency: 'EUR', gl_account_ref: 'GL-10150-IRS', cost_center: 'CC-110',
                  book_status: 'Active', regulatory_book_type: 'Trading',
                  book_purpose_type: 'Trading', ledger_feed_type: 'Automatic',
                  is_sweepable: 'false', rates_centre_code: 'GBLO' } }
        ]
    };

    /* ----------------------------------------------------------- state */

    var S = {
        screen: 'tree',
        query: '',
        portfolio: 'RATES.EUR',
        book: 'EUR_SWAPS_01',
        isNew: false,
        open: { GLOBAL: true, RATES: true, 'RATES.EUR': true },
        fields: null,
        rights: null,
        account: 'm.okafor',
        right: 'read',
        fail: null,
        version: -1,
        pfForm: false
    };

    /* ------------------------------------------------------- lookups */

    function pf(id) {
        return PORTFOLIOS.filter(function (p) { return p.id === id; })[0];
    }

    function book(name) {
        return BOOKS.filter(function (b) { return b.name === name; })[0];
    }

    function storedBook() {
        return S.isNew ? undefined : book(S.book);
    }

    function childPfs(id) {
        return PORTFOLIOS.filter(function (p) { return p.parent === id; });
    }

    function booksUnder(id) {
        return BOOKS.filter(function (b) { return b.portfolio === id; });
    }

    function ancestry(id) {
        var out = [];
        var cur = pf(id);
        while (cur) { out.push(cur); cur = cur.parent ? pf(cur.parent) : null; }
        return out;
    }

    function pathOf(id) {
        return ancestry(id).reverse().map(function (p) { return p.name; }).join(' / ');
    }

    function unitOf(code) {
        return UNITS.filter(function (u) { return u.code === code; })[0];
    }

    function hasDirect(account, node, right) {
        return S.rights.some(function (r) {
            return r.account === account && r.portfolio === node && r.right === right;
        });
    }

    function effective(account, node, right) {
        var an = ancestry(node);
        for (var i = 0; i < an.length; i += 1) {
            if (hasDirect(account, an[i].id, right)) return an[i];
        }
        return null;
    }

    function readersOf(node) {
        var names = ACCOUNTS.filter(function (a) { return effective(a.id, node, 'read'); })
            .map(function (a) { return a.id; });
        return names.length ? names.join(', ') : 'no account holds a read right here';
    }

    /* --------------------------------------------------- working copies */

    var FIELD_KEYS = ['name', 'description', 'functional_currency', 'gl_account_ref',
        'cost_center', 'owner_unit_id', 'book_status', 'regulatory_book_type',
        'book_purpose_type', 'ledger_feed_type', 'is_sweepable', 'rates_centre_code'];

    function resetWorking() {
        var b = storedBook();
        var p = pf(S.portfolio) || PORTFOLIOS[0];
        if (b) {
            S.fields = {
                name: b.name, description: b.description || '',
                functional_currency: b.ccy, gl_account_ref: b.gl, cost_center: b.cost,
                owner_unit_id: b.unit, book_status: b.status,
                regulatory_book_type: b.regulatory, book_purpose_type: b.purpose,
                ledger_feed_type: b.ledger, is_sweepable: b.sweepable,
                rates_centre_code: b.centre
            };
        } else {
            S.fields = {
                name: '', description: '', functional_currency: p.ccy,
                gl_account_ref: '', cost_center: '', owner_unit_id: p.unit,
                book_status: 'Active', regulatory_book_type: 'Trading',
                book_purpose_type: 'Trading', ledger_feed_type: 'None',
                is_sweepable: false, rates_centre_code: 'WRLD'
            };
        }
        S.rights = RIGHTS_BASE.map(function (r) {
            return { account: r.account, portfolio: r.portfolio, right: r.right };
        });
    }

    /* --------------------------------------------------------- params */

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (p.get('query') !== null) S.query = p.get('query');

        var port = p.get('portfolio');
        if (port !== null && pf(port)) S.portfolio = port;

        if (p.get('new') === '1') {
            S.isNew = true;
            S.book = '';
        } else {
            var bk = p.get('book');
            if (bk !== null && book(bk)) {
                S.book = bk;
                S.portfolio = book(bk).portfolio;
            }
        }

        if (p.get('account') !== null) S.account = p.get('account');
        if (['read', 'open_sandbox', 'none'].indexOf(p.get('right')) >= 0) S.right = p.get('right');
        var failParam = p.get('fail');
        if (failParam === '1') S.fail = 'open_activity';
        else if (['open_activity', 'transition'].indexOf(failParam) >= 0) S.fail = failParam;

        resetWorking();

        if (['Active', 'Closed', 'Frozen'].indexOf(p.get('status')) >= 0) S.fields.book_status = p.get('status');
        if (['Trading', 'Banking'].indexOf(p.get('regulatory')) >= 0) S.fields.regulatory_book_type = p.get('regulatory');
        if (['None', 'Automatic', 'Manual'].indexOf(p.get('ledger')) >= 0) S.fields.ledger_feed_type = p.get('ledger');
        if (p.get('sweepable') === '1') S.fields.is_sweepable = true;
        if (p.get('purpose') !== null) S.fields.book_purpose_type = p.get('purpose');

        var state = p.get('state') || p.get('step');
        if (ALL_STATES.indexOf(state) >= 0) S.screen = state;

        var v = parseInt(p.get('version'), 10);
        if (!isNaN(v) && v > 0) S.version = v - 1;
    }

    /* ------------------------------------------------------ utilities */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function srcChip(kind, text) {
        var glyph = kind === 'shared' ? '\u25a0' : kind === 'locked' ? '\u25a0' : '\u25cf';
        return '<span class="src ' + kind + '">' + glyph + ' ' + esc(text) + '</span>';
    }

    function sharedChip(listKey) {
        return srcChip('shared', 'Shared list \u00b7 system tenant');
    }

    function tenantChip() {
        return srcChip('tenant', 'This tenant’s data');
    }

    function badge(text, kind) {
        return '<span class="badge ' + (kind || '') + '">' + esc(text) + '</span>';
    }

    function statusBadge(status) {
        var kind = status === 'Active' ? 'ok' : status === 'Frozen' ? 'warn' : status === 'Closed' ? 'bad' : '';
        return badge(status, kind);
    }

    function boolBadge(value, yes, no) {
        return badge(value ? (yes || 'Yes') : (no || 'No'), value ? 'ok' : '');
    }

    function listOf(key) { return LISTS[key]; }

    function optionRows(rows, selected) {
        return rows.map(function (r) {
            return '<option value="' + esc(r.code) + '"' + (r.code === selected ? ' selected' : '') + '>' +
                esc(r.code) + (r.name && r.name !== r.code ? ' \u2014 ' + esc(r.name) : '') + '</option>';
        }).join('');
    }

    /* -------------------------------------------------------- searching */

    function q() { return S.query.trim().toLowerCase(); }

    function pfMatches(p) {
        return p.name.toLowerCase().indexOf(q()) >= 0 || p.id.toLowerCase().indexOf(q()) >= 0;
    }

    function bkMatches(b) {
        return b.name.toLowerCase().indexOf(q()) >= 0 ||
            (b.description || '').toLowerCase().indexOf(q()) >= 0;
    }

    function subtreeMatches(id) {
        if (q() === '') return true;
        if (pfMatches(pf(id))) return true;
        if (booksUnder(id).some(bkMatches)) return true;
        return childPfs(id).some(function (c) { return subtreeMatches(c.id); });
    }

    /* ------------------------------------------------------ the tree */

    function treeRows() {
        var html = '';
        var searching = q() !== '';

        function walk(id, depth) {
            var p = pf(id);
            if (!subtreeMatches(id) && !searching) return;
            if (searching && !subtreeMatches(id)) return;
            var kids = childPfs(id);
            var bs = booksUnder(id).filter(function (b) { return !searching || bkMatches(b) || pfMatches(p); });
            var open = searching || S.open[id] === true;
            var pad = 8 + depth * 16;
            html += '<div class="trow' + (S.book === '' && S.portfolio === id ? ' on' : '') + '" style="padding-left:' + pad + 'px" data-act="pickpf" data-node="' + esc(id) + '">' +
                '<button type="button" class="twisty' + (kids.length ? '' : ' leaf') + '" data-act="toggle" data-node="' + esc(id) + '">' +
                (open ? '\u25be' : '\u25b8') + '</button>' +
                '<span class="tname">' + esc(p.name) + '</span>' +
                '<span class="count">' + booksUnder(id).length + ' books</span>' +
                '<span class="tmeta">' + esc(p.purpose) + ' \u00b7 ' + esc(p.ccy) + ' \u00b7 ' + esc(p.status) + '</span>' +
                '</div>';
            if (!open) return;
            kids.forEach(function (c) { walk(c.id, depth + 1); });
            bs.forEach(function (b) {
                var bpad = 8 + (depth + 1) * 16;
                html += '<div class="trow bk' + (S.book === b.name ? ' on' : '') + '" style="padding-left:' + bpad + 'px" data-act="pickbk" data-name="' + esc(b.name) + '">' +
                    '<span class="twisty leaf">\u25be</span>' +
                    '<span class="tname">' + esc(b.name) + '</span>' +
                    '<span class="tmeta">' + statusBadge(b.status) + ' ' + badge(b.regulatory) + ' ' + badge(b.purpose) + '</span>' +
                    '</div>';
            });
        }

        PORTFOLIOS.filter(function (p) { return p.parent === null; }).forEach(function (p) { walk(p.id, 0); });
        return html === '' ? '<div class="tempty">No portfolio or book matches \u201c' + esc(S.query) + '\u201d.</div>' : html;
    }

    /* What a book reads from its portfolio, and what constrains it. */
    function inheritedPanel(node, compact) {
        var p = pf(node);
        if (!p) return '';
        var rows = [
            ['Portfolio', esc(p.name) + ' <span class="mono faint">' + esc(p.id) + '</span>'],
            ['Aggregation currency', esc(p.ccy) + ' <span class="faint">\u2014 read from the portfolio; the book is rolled up in it.</span>'],
            ['Owner business unit', esc(p.unit) + ' <span class="faint">\u2014 the book’s owner unit must be one named in the ancestry chain ' + esc(ancestry(node).map(function (x) { return x.unit; }).reverse().join(' \u2192 ')) + '.</span>'],
            ['Portfolio purpose', esc(p.purpose) + ' <span class="faint">\u2014 the portfolio’s own purpose; a book carries a purpose of its own.</span>'],
            ['Sandbox', p.sandbox ? esc(p.sandbox) : '<span class="faint">None \u2014 an official book. A book and its portfolio share one sandbox.</span>'],
            ['Who may see it', '<span class="faint">' + esc(readersOf(node)) + '</span>']
        ];
        var body = rows.map(function (r) {
            return '<tr><td>' + r[0] + '</td><td>' + r[1] + '</td></tr>';
        }).join('');
        return '<div class="panel-soft">' +
            '<div class="sectionhead"><h3>Inherited from the portfolio</h3>' + srcChip('inherited', 'Read-through, not copied') + '</div>' +
            (compact ? '' : '<p class="hint" style="margin-top:0">A book copies none of these. It reads them from its portfolio, and the portfolio’s rights cover the book.</p>') +
            '<table class="kv">' + body + '</table></div>';
    }

    function ownFieldsPanel(b) {
        var rows = [
            ['Name', '<span class="mono">' + esc(b.name) + '</span>'],
            ['Description', b.description ? esc(b.description) : '<span class="faint">None</span>'],
            ['Functional currency', esc(b.ccy)],
            ['GL account ref', b.gl ? '<span class="mono">' + esc(b.gl) + '</span>' : '<span class="faint">None</span>'],
            ['Cost center', b.cost ? '<span class="mono">' + esc(b.cost) + '</span>' : '<span class="faint">None</span>'],
            ['Owner business unit', esc(b.unit)],
            ['Status', statusBadge(b.status)],
            ['Regulatory book type', badge(b.regulatory)],
            ['Book purpose type', badge(b.purpose)],
            ['Ledger feed type', badge(b.ledger)],
            ['Sweepable', boolBadge(b.sweepable)],
            ['Rates centre', esc(b.centre)],
            ['Open activity', b.openActivity > 0 ? badge(b.openActivity + ' open trades', 'warn') : badge('None', 'ok')]
        ];
        return '<div class="panel-soft">' +
            '<div class="sectionhead"><h3>This book</h3>' + tenantChip() + '</div>' +
            '<table class="kv">' + rows.map(function (r) { return '<tr><td>' + r[0] + '</td><td>' + r[1] + '</td></tr>'; }).join('') + '</table></div>';
    }

    function bookPeek(b) {
        var rights = effective(S.account, b.portfolio, 'read');
        return '<div class="panel"><div class="sectionhead"><h2>' + esc(b.name) + '</h2>' +
            '<button type="button" class="btn small" data-act="openbook" data-name="' + esc(b.name) + '">Open this book</button></div>' +
            ownFieldsPanel(b) + inheritedPanel(b.portfolio, true) +
            '<div class="notice info" style="margin-top:14px"><b>' + esc(S.account) + '</b> reaches this book through ' +
            (rights ? '<b>' + esc(rights.name) + '</b> (right <span class="mono">read</span>)' : '<b>no right</b>') + '. ' +
            'A book carries no rights of its own; the portfolio’s right covers every node below it.</div>' +
            '<div class="stepfoot" style="margin-top:12px">' +
            '<button type="button" class="btn ghost" data-act="goclass">Classification</button>' +
            '<button type="button" class="btn ghost" data-act="gorights">Rights</button>' +
            '<button type="button" class="btn ghost" data-act="gohistory">History</button>' +
            '</div></div>';
    }

    function portfolioPeek(p) {
        var bs = booksUnder(p.id);
        var rows = bs.map(function (b) {
            return '<tr><td><button type="button" class="btn ghost small" style="padding-left:0" data-act="pickbk" data-name="' + esc(b.name) + '"><span class="mono">' + esc(b.name) + '</span></button></td>' +
                '<td>' + statusBadge(b.status) + '</td><td>' + badge(b.regulatory) + '</td><td>' + badge(b.purpose) + '</td>' +
                '<td class="sub">' + (b.openActivity ? b.openActivity + ' open' : 'none') + '</td></tr>';
        }).join('');
        return '<div class="panel"><h2>' + esc(p.name) + '</h2>' +
            '<p class="lead">' + esc(p.description || '') + '</p>' +
            '<table class="kv">' +
            '<tr><td>Path</td><td class="mono">' + esc(p.id) + '</td></tr>' +
            '<tr><td>Purpose type ' + sharedChip('purpose_types') + '</td><td>' + badge(p.purpose) + '</td></tr>' +
            '<tr><td>Aggregation currency</td><td>' + esc(p.ccy) + '</td></tr>' +
            '<tr><td>Owner business unit</td><td>' + esc(p.unit) + '</td></tr>' +
            '<tr><td>Status</td><td>' + statusBadge(p.status) + '</td></tr>' +
            '<tr><td>Virtual</td><td>' + boolBadge(p.virtual, 'Virtual', 'Not virtual') + '</td></tr>' +
            '</table>' +
            (bs.length ? '<h3 style="font-size:14px;margin:18px 0 8px">Books in this portfolio</h3>' +
                '<table class="grid"><thead><tr><th>Book</th><th>Status</th><th>Regulatory</th><th>Purpose</th><th>Open activity</th></tr></thead><tbody>' + rows + '</tbody></table>'
                : '<p class="hint">No book sits here. A portfolio is a folder: it holds books and other portfolios, never a deal.</p>') +
            inheritedPanel(p.id, true) +
            '<div class="stepfoot" style="margin-top:12px">' +
            '<button type="button" class="btn primary small" data-act="newbookhere">Create a book here</button>' +
            '<button type="button" class="btn ghost" data-act="gorights">Rights</button>' +
            '</div></div>';
    }

    /* ------------------------------------------------------ tree screen */

    function treeScreen() {
        var sel = storedBook();
        var detail = sel ? bookPeek(sel) : (pf(S.portfolio) ? portfolioPeek(pf(S.portfolio)) : '');
        return '<div class="searchbar">' +
            '<input data-focus="query" data-q="1" placeholder="Search portfolios and books \u2014 EUR, Rates, RESERVE_01\u2026" value="' + esc(S.query) + '">' +
            '<button type="button" class="btn small" data-act="newbookhere">New book</button>' +
            '</div>' +
            '<p class="legend">' +
            srcChip('tenant', 'This tenant’s data') + ' portfolios and books are yours to shape. ' +
            srcChip('shared', 'Shared list') + ' the taxonomy they name is the system tenant’s, and is edited under Reference data \u2192 Classifications, not here.' +
            '</p>' +
            '<div class="split"><div class="tree">' + treeRows() + '</div>' + detail + '</div>';
    }

    /* ------------------------------------------------- portfolio screen */

    function portfolioScreen() {
        var rows = PORTFOLIOS.filter(function (p) { return q() === '' || pfMatches(p) || subtreeMatches(p.id); });
        var list = rows.map(function (p) {
            return '<button type="button" class="trow' + (S.portfolio === p.id ? ' on' : '') + '" data-act="pickpf" data-node="' + esc(p.id) + '">' +
                '<span class="twisty leaf">\u25be</span>' +
                '<span class="tname">' + esc(p.name) + '</span>' +
                '<span class="count">' + booksUnder(p.id).length + ' books</span>' +
                '<span class="tmeta">' + esc(p.purpose) + ' \u00b7 ' + esc(p.ccy) + '</span></button>';
        }).join('') || '<div class="tempty">No portfolio matches.</div>';

        var p = pf(S.portfolio) || PORTFOLIOS[0];
        var form = '';
        if (S.pfForm) {
            form = '<div class="panel-soft" style="margin-top:14px">' +
                '<div class="sectionhead"><h3>New portfolio in ' + esc(p.name) + '</h3>' + tenantChip() + '</div>' +
                '<div class="grid2">' +
                '<label class="field"><span class="lbl">Name</span><input placeholder="APAC Rates" data-f="pfname"></label>' +
                '<label class="field"><span class="lbl">Aggregation currency</span><select data-f="pfccy">' +
                optionRows(CURRENCIES.map(function (c) { return { code: c }; }), p.ccy) + '</select></label>' +
                '<label class="field"><span class="lbl">Purpose type ' + sharedChip('purpose_types') + '</span>' +
                '<select data-f="pfpurpose">' + optionRows(LISTS.purpose_types.rows, 'Risk') + '</select>' +
                '<div class="hint">The values come from a shared list. To add one, open Purpose types in Classifications.</div></label>' +
                '<label class="field"><span class="lbl">Status</span><select data-f="pfstatus">' +
                optionRows([{ code: 'Active' }, { code: 'Inactive' }, { code: 'Closed' }], 'Active') + '</select></label>' +
                '<label class="field span2"><span class="lbl">Description</span><input placeholder="Optional" data-f="pfdesc"></label>' +
                '</div>' +
                '<p class="hint">Writes <span class="mono">refdata.v1.portfolios.put</span> with ' +
                '<span class="mono">intent = must_not_exist</span>. The server sets the party from the session.</p>' +
                '<div class="stepfoot"><button type="button" class="btn ghost" data-act="pf-cancel">Cancel</button>' +
                '<button type="button" class="btn primary ml-auto" data-act="pf-write">Create portfolio</button></div></div>';
        } else {
            form = '<button type="button" class="btn small" data-act="pf-form" style="margin-top:14px">New portfolio here</button>';
        }

        return '<div class="searchbar"><input data-focus="query" data-q="1" placeholder="Search portfolios" value="' + esc(S.query) + '"></div>' +
            '<div class="split"><div class="tree">' + list + '</div>' +
            '<div class="panel"><h2>What a book inherits</h2>' +
            '<p class="lead">Choose the portfolio the book will sit in. The book copies none of this: it reads it from the portfolio, and the portfolio’s rights cover the book.</p>' +
            inheritedPanel(p.id) + form + '</div></div>';
    }

    /* ------------------------------------------------------ book screen */

    function unitOptions() {
        var chain = ancestry(S.portfolio).map(function (p) { return p.unit; });
        return UNITS.map(function (u) {
            var inChain = chain.indexOf(u.name) >= 0;
            return '<option value="' + esc(u.code) + '"' + (u.code === S.fields.owner_unit_id ? ' selected' : '') + '>' +
                esc(u.name) + (inChain ? '' : ' (outside the ancestry chain)') + '</option>';
        }).join('');
    }

    function bookScreen() {
        var p = pf(S.portfolio) || PORTFOLIOS[0];
        var b = storedBook();
        var title = S.isNew ? 'New book in ' + p.name : 'Book ' + esc(S.book);
        return '<div class="notice info" style="margin-top:0">' +
            '<span class="title">' + (S.isNew ? 'A new book' : 'An open book') + '</span>' +
            'A book is a ledger leaf: the only record that holds trades. It belongs to exactly one portfolio, and that link does not change. ' +
            'The server writes <span class="mono">refdata.v1.books.put</span>.</div>' +
            '<div class="sectionhead"><h3>' + title + '</h3>' + tenantChip() + '</div>' +
            '<div class="grid2">' +
            '<label class="field"><span class="lbl">Name &middot; natural key</span>' +
            '<input data-focus="name" data-f="name" value="' + esc(S.fields.name) + '" placeholder="EUR_SWAPS_01"' + (S.isNew ? '' : ' disabled') + '>' +
            '<div class="hint">Unique within the party. The key does not change after the write.</div></label>' +
            '<label class="field"><span class="lbl">Parent portfolio &middot; fixed</span>' +
            '<input value="' + esc(p.name + ' (' + p.id + ')') + '" disabled>' +
            '<div class="hint">A book must belong to a portfolio, and it stays there.</div></label>' +
            '<label class="field"><span class="lbl">Functional currency ' + sharedChip('purpose_types') + '</span>' +
            '<select data-f="functional_currency">' + optionRows(CURRENCIES.map(function (c) { return { code: c }; }), S.fields.functional_currency) + '</select>' +
            '<div class="hint">Designated by the ledger. The code must be an active currency for this tenant.</div></label>' +
            '<label class="field"><span class="lbl">Owner business unit</span>' +
            '<select data-f="owner_unit_id">' + unitOptions() + '</select>' +
            '<div class="hint">The model says it must name a unit in the ancestry chain: ' +
            esc(ancestry(S.portfolio).map(function (x) { return x.unit; }).reverse().join(' \u2192 ')) + '.</div></label>' +
            '<label class="field"><span class="lbl">GL account reference</span>' +
            '<input data-f="gl_account_ref" value="' + esc(S.fields.gl_account_ref) + '" placeholder="GL-10150-FXO">' +
            '<div class="hint">Reference to the external General Ledger. Nullable when the book is not integrated.</div></label>' +
            '<label class="field"><span class="lbl">Cost center</span>' +
            '<input data-f="cost_center" value="' + esc(S.fields.cost_center) + '" placeholder="CC-001">' +
            '<div class="hint">Internal finance code for P&amp;L attribution.</div></label>' +
            '<label class="field span2"><span class="lbl">Description</span>' +
            '<input data-f="description" value="' + esc(S.fields.description) + '" placeholder="Optional free text"></label>' +
            '</div>' +
            inheritedPanel(p.id, true) +
            '<div class="stepfoot" style="margin-top:14px">' +
            '<button type="button" class="btn danger" data-act="refuse-close">Close this book</button>' +
            '<button type="button" class="btn ghost" data-act="gohistory">History</button>' +
            '</div>';
    }

    /* -------------------------------------------------- classify screen */

    function pick(label, listKey, name, selected) {
        return '<label class="field"><span class="lbl">' + label + ' ' + sharedChip(listKey) + '</span>' +
            '<select data-f="' + name + '">' + optionRows(listOf(listKey).rows, selected) + '</select>' +
            '<div class="hint">' + esc(listOf(listKey).why) + ' Its values come from ' +
            '<span class="mono">' + esc(listOf(listKey).subject) + '</span>, a shared system-tenant list. ' +
            'To change the list itself, open <a href="#" data-act="nolink">' + esc(listOf(listKey).label) + '</a> in Classifications; this screen never edits it.</div></label>';
    }

    function transitionPanel() {
        var cur = storedBook() ? storedBook().status : 'Active';
        var next = S.fields.book_status;
        var allowed = TRANSITIONS[cur] || [];
        var r = refusalOf();
        var rows = ['Active', 'Closed', 'Frozen'].map(function (s) {
            var isNext = s === next;
            var ok = s === cur || allowed.indexOf(s) >= 0;
            return '<tr' + (isNext ? ' style="background:#16161a"' : '') + '><td>' + statusBadge(cur) + ' \u2192 ' + statusBadge(s) + '</td>' +
                '<td>' + (s === cur ? '<span class="faint">no change</span>' : ok ? badge('Allowed', 'ok') : badge('Refused', 'bad')) + '</td>' +
                '<td class="sub">' + (s === cur ? '' : ok ? 'Writes a new version with a change reason.' : s === 'Closed' && storedBook() && storedBook().openActivity > 0 ? 'Refused while the book holds open activity.' : 'Refused by the status rule; return it to Active first.') + '</td></tr>';
        }).join('');
        return '<div class="panel-soft">' +
            '<div class="sectionhead"><h3>Status transition check</h3>' + tenantChip() + '</div>' +
            '<table class="grid"><thead><tr><th>Transition</th><th>Rule</th><th></th></tr></thead><tbody>' + rows + '</tbody></table>' +
            (r ? '<div class="notice ' + (r.kind === 'open_activity' ? 'error' : 'warn') + '" style="margin:14px 0 0">' +
                '<span class="title">' + (r.kind === 'open_activity' ? 'This write will be refused' : 'This transition is not allowed') + '</span>' +
                refusalText(r) + '</div>' : '') +
            '<p class="proto-hint">Gap: no server-side rule refuses a status change today. ' +
            '<span class="mono">ores_refdata_validate_book_status_fn</span> only checks that the code exists, and no operation checks open activity. ' +
            'The rule drawn here is what the refusal state argues for.</p></div>';
    }

    function classifyScreen() {
        var gap = '<div class="notice warn" style="margin-top:0">' +
            '<span class="title">Gap: two of these fields have no wire field yet</span>' +
            'The book record carries <span class="mono">book_status</span> and <span class="mono">regulatory_book_type</span>, ' +
            'but <span class="mono">book_purpose_type</span> and <span class="mono">ledger_feed_type</span> are absent from the model and from ' +
            '<span class="mono">refdata.v1.books.put</span>. Both lookup tables exist and both are drawn here, because book classification says every book carries them. ' +
            'Until the field is added, the screen can show the value but cannot save it.</div>';

        var taxo = Object.keys(LISTS).map(function (key) {
            var l = listOf(key);
            return '<div class="list"><div class="top"><span class="nm">' + esc(l.label) + '</span>' +
                srcChip('locked', 'Read-only here') + '</div>' +
                '<div class="codes">' + l.rows.map(function (r) { return badge(r.code); }).join('') + '</div>' +
                '<p class="why">' + esc(l.why) + ' <span class="mono">' + esc(l.subject) + '</span> \u00b7 model <span class="mono">' + esc(l.model) + '</span></p>' +
                '<a href="#" data-act="nolink">Open ' + esc(l.label) + ' in Classifications \u2192</a></div>';
        }).join('');

        return gap +
            '<div class="split">' +
            '<div><div class="sectionhead"><h3>This book’s classification</h3>' + tenantChip() + '</div>' +
            '<p class="hint" style="margin-top:0">The book is yours. Each picker below names one row of a shared list; the list itself is not yours.</p>' +
            pick('Status', 'book_statuses', 'book_status', S.fields.book_status) +
            pick('Regulatory book type', 'regulatory_book_types', 'regulatory_book_type', S.fields.regulatory_book_type) +
            '<div class="notice info"><b>The three axes are independent.</b> ' +
            'Regulatory type (trading or banking), ledger feed (none, automatic or manual) and purpose (the risk role) combine freely. ' +
            'A Basel rule never forces a reserve book to be a banking book; that correlation lives in process and defaults, not in a check constraint. ' +
            'Within the purpose axis a book carries exactly one value.</div>' +
            pick('Book purpose type', 'book_purpose_types', 'book_purpose_type', S.fields.book_purpose_type) +
            pick('Ledger feed type', 'ledger_feed_types', 'ledger_feed_type', S.fields.ledger_feed_type) +
            '<div class="grid2">' +
            '<label class="field"><span class="lbl">Rates centre ' + sharedChip('purpose_types') + '</span>' +
            '<select data-f="rates_centre_code">' + optionRows(CENTRES, S.fields.rates_centre_code) + '</select>' +
            '<div class="hint">Determines the revaluation market data snapshot at end of day. A soft reference to the business centres scheme.</div></label>' +
            '<label class="field"><span class="lbl">Sweepable ' + tenantChip() + '</span>' +
            '<span class="checkline"><input type="checkbox" data-f="is_sweepable"' + (S.fields.is_sweepable ? ' checked' : '') + '> Eligible for spot-sweep transfers</span>' +
            '<div class="hint">A boolean, independent of the three axes. It governs spot-sweep eligibility only.</div></label>' +
            '</div>' +
            transitionPanel() +
            '</div>' +
            '<div><div class="sectionhead"><h3>The shared taxonomy</h3>' + srcChip('shared', 'Not editable from this journey') + '</div>' +
            '<p class="hint" style="margin-top:0">These five lists belong to the system tenant. They are the same for every tenant, so this journey shows them and never writes them. ' +
            'The only way to change one is the Classifications journey, which carries the permissions and the change reason.</p>' +
            '<div class="taxo">' + taxo + '</div>' +
            '<button type="button" class="btn small" data-act="nolink" style="margin-top:12px">Open Classifications</button></div>' +
            '</div>';
    }

    /* ---------------------------------------------------- rights screen */

    function rightMeaning(code) {
        if (code === 'read') return 'See what the node holds, and every node below it. A right on a desk covers its sub-desks and not its sibling desks.';
        if (code === 'open_sandbox') return 'Open a sandbox anchored at the node. The sandbox’s virtual books never reach the ledger and nothing official reads them.';
        return 'No right. The account cannot see the node or anything below it.';
    }

    function rightsScreen() {
        var node = S.portfolio;
        var editor = ACCOUNTS.map(function (a) {
            var directRead = hasDirect(a.id, node, 'read');
            var directOpen = hasDirect(a.id, node, 'open_sandbox');
            return '<div class="acct"><span class="who"><span class="mono">' + esc(a.id) + '</span></span>' +
                '<span class="checkline"><input type="checkbox" data-act="right" data-account="' + esc(a.id) + '" data-right="read"' + (directRead ? ' checked' : '') + '> <span class="mono">read</span></span>' +
                '<span class="checkline"><input type="checkbox" data-act="right" data-account="' + esc(a.id) + '" data-right="open_sandbox"' + (directOpen ? ' checked' : '') + '> <span class="mono">open_sandbox</span></span>' +
                '</div>';
        }).join('');

        var rows = ACCOUNTS.map(function (a) {
            var r = effective(a.id, node, 'read');
            var o = effective(a.id, node, 'open_sandbox');
            var none = !r && !o;
            function cell(src, right) {
                if (!src) return '<span class="faint">\u2014</span>';
                var how = src.id === node ? 'direct at this node' : 'inherited from ' + src.name;
                return badge(right, 'ok') + ' <span class="sub">' + esc(how) + '</span>';
            }
            return '<tr class="' + (none ? 'noright' : '') + '"' + (S.account === a.id ? ' style="box-shadow:inset 2px 0 0 var(--accent)"' : '') + '>' +
                '<td><span class="mono">' + esc(a.id) + '</span></td>' +
                '<td>' + cell(r, 'read') + '</td>' +
                '<td>' + cell(o, 'open_sandbox') + '</td>' +
                '<td class="sub">' + (none ? 'No right at this node or below it. The account cannot see it.' : 'Covers every node below this one.') + '</td></tr>';
        }).join('');

        var chosen = ACCOUNTS.filter(function (a) { return a.id === S.account; })[0];
        var eff = chosen ? effective(chosen.id, node, S.right) : null;
        var explain = S.right === 'none' ? '' :
            '<div class="panel-soft"><h3 style="font-size:13px;margin-top:0">What <span class="mono">' + esc(S.right) + '</span> grants</h3>' +
            '<p class="hint" style="margin-top:0">' + esc(rightMeaning(S.right)) + '</p>' +
            (S.right === 'read' ?
                '<p class="hint">Effective for <span class="mono">' + esc(S.account) + '</span>: ' +
                (eff ? '<b>' + esc(eff.name) + '</b> \u2014 ' + (eff.id === node ? 'granted directly at this node.' : 'inherited from an ancestor.') : '<b>not held.</b>') + '</p>'
                : '') + '</div>';

        return '<div class="notice info" style="margin-top:0">' +
            '<span class="title">Rights are held at a portfolio node, never on a book</span>' +
            'A right at <span class="mono">' + esc(node) + '</span> applies to every node below it, so it covers this node’s books and sub-portfolios, and not its siblings. ' +
            'The server answers the question with <span class="mono">ores_refdata_account_holds_portfolio_right_fn</span>. ' +
            'Writes go to <span class="mono">refdata.v1.portfolio_rights.put_many</span>.</div>' +
            '<div class="split">' +
            '<div class="panel"><h2>Direct rights at ' + esc(pf(node).name) + '</h2>' +
            '<p class="lead">Tick a right to grant it here. Untick it to take it away. A right granted to a closed account stays on record; the function stops honouring it.</p>' +
            '<div class="rightbox">' + editor + '</div>' +
            '<div class="stepfoot" style="margin-top:12px"><button type="button" class="btn primary small" data-act="goreview">Review the write</button></div></div>' +
            '<div class="panel"><h2>Effective rights</h2>' +
            '<p class="lead">What each account actually holds at this node, directly or through an ancestor.</p>' +
            '<table class="grid"><thead><tr><th>Account</th><th>read</th><th>open_sandbox</th><th></th></tr></thead><tbody>' + rows + '</tbody></table>' +
            explain +
            '<p class="hint">' + srcChip('tenant', 'This tenant’s data') +
            ' The grant is this tenant’s. The two right codes are fixed by a check constraint: <span class="mono">read</span>, <span class="mono">open_sandbox</span>.</p>' +
            '</div></div>';
    }

    /* ---------------------------------------------------- review screen */

    function changesList() {
        var out = [];
        var b = storedBook();
        if (S.isNew) {
            out.push({ what: 'Create the book', who: S.fields.name,
                sub: 'refdata.v1.books.put \u00b7 intent = must_not_exist, in portfolio ' + S.portfolio, from: null, to: null });
        } else if (b) {
            var labels = {
                name: 'Name', description: 'Description', functional_currency: 'Functional currency',
                gl_account_ref: 'GL account ref', cost_center: 'Cost center',
                owner_unit_id: 'Owner business unit', book_status: 'Status',
                regulatory_book_type: 'Regulatory book type', book_purpose_type: 'Book purpose type',
                ledger_feed_type: 'Ledger feed type', is_sweepable: 'Sweepable',
                rates_centre_code: 'Rates centre'
            };
            var current = {
                name: b.name, description: b.description || '', functional_currency: b.ccy,
                gl_account_ref: b.gl, cost_center: b.cost, owner_unit_id: b.unit,
                book_status: b.status, regulatory_book_type: b.regulatory,
                book_purpose_type: b.purpose, ledger_feed_type: b.ledger,
                is_sweepable: b.sweepable, rates_centre_code: b.centre
            };
            FIELD_KEYS.forEach(function (k) {
                var was = String(current[k]);
                var now = String(S.fields[k]);
                if (was !== now) {
                    out.push({ what: 'Set ' + labels[k], who: b.name,
                        sub: 'refdata.v1.books.put \u00b7 intent = must_match_version',
                        from: was === 'true' ? 'Yes' : was === 'false' ? 'No' : was,
                        to: now === 'true' ? 'Yes' : now === 'false' ? 'No' : now });
                }
            });
        }
        /* Rights: what the working copy adds or removes against the baseline. */
        var keyed = {};
        RIGHTS_BASE.forEach(function (r) { keyed[r.account + '|' + r.portfolio + '|' + r.right] = true; });
        S.rights.forEach(function (r) {
            var k = r.account + '|' + r.portfolio + '|' + r.right;
            if (!keyed[k]) {
                out.push({ what: 'Grant the right ' + r.right, who: r.account,
                    sub: 'refdata.v1.portfolio_rights.put_many \u00b7 at ' + r.portfolio,
                    from: null, to: 'granted at ' + r.portfolio });
            }
            keyed[k] = 'kept';
        });
        RIGHTS_BASE.forEach(function (r) {
            var k = r.account + '|' + r.portfolio + '|' + r.right;
            if (keyed[k] === true) {
                out.push({ what: 'Remove the right ' + r.right, who: r.account,
                    sub: 'refdata.v1.portfolio_rights.delete_many \u00b7 at ' + r.portfolio,
                    from: 'held at ' + r.portfolio, to: null });
            }
        });
        return out;
    }

    function reviewScreen() {
        var changes = changesList();
        var list = changes.length === 0 ?
            '<div class="notice info" style="margin-top:0">Nothing has changed. Go back and change a field, or grant a right.</div>' :
            '<ul class="changes">' + changes.map(function (c) {
                var fromto = c.from === null && c.to === null ? '' :
                    '<div class="fromto">' +
                    '<span class="was">' + (c.from === null ? '\u2014' : esc(c.from)) + '</span>' +
                    '<span class="arrow">\u2192</span>' +
                    '<span class="now">' + (c.to === null ? '\u2014' : esc(c.to)) + '</span></div>';
                return '<li><div class="what">' + esc(c.what) + '</div><div class="who">' + esc(c.who) + '</div>' +
                    fromto + '<div class="sub">' + esc(c.sub) + '</div></li>';
            }).join('') + '</ul>';

        var r = refusalOf();
        var refusal = r ?
            '<div class="notice error"><span class="title">The server will refuse this write</span>' + refusalText(r) + '</div>' :
            '<div class="notice success"><span class="title">The write is allowed</span>Every change is recorded as a new version, with the actor and the change reason.</div>';

        return list + refusal +
            '<div class="panel-soft" style="margin-top:14px"><h3 style="font-size:13px;margin-top:0">The change reason</h3>' +
            '<label class="field" style="margin-bottom:0"><span class="lbl">Reason code \u00b7 from <span class="mono">dq.v1.change_reasons.list</span></span>' +
            '<select>' + optionRows(CHANGE_REASONS, 'refdata.book.classification_changed') + '</select></label>' +
            '<p class="hint">Every refdata write carries a reason and keeps its versions.</p></div>';
    }

    /* --------------------------------------------------- outcome screen */

    function outcomeScreen() {
        var r = refusalOf();
        if (r) return refusedScreen();
        var b = storedBook();
        var version = b ? historyFor(b).length + 1 : 1;
        var cards = [
            ['Open the book’s history', 'Every version, with the field diff.'],
            ['Set the rights', 'Who may see this node and below it.'],
            ['Return to the tree', 'Choose another portfolio or book.']
        ].map(function (c, i) {
            return '<button type="button" class="nextcard" data-act="outcome" data-to="' +
                (i === 0 ? 'history' : i === 1 ? 'rights' : 'tree') + '">' +
                '<span class="nm">' + esc(c[0]) + '</span><p>' + esc(c[1]) + '</p></button>';
        }).join('');
        return '<div class="outcome"><div class="mark ok">\u2713</div>' +
            '<h2>' + (S.isNew ? 'The book is created' : 'The book is written') + '</h2>' +
            '<p>' + (S.isNew ? esc(S.fields.name) + ' now sits in ' + esc(pf(S.portfolio).name) + '.' : esc(S.book) + ' is at version ' + version + '.') +
            ' The rights you changed are written at ' + esc(S.portfolio) + '. Every write is a new version with an actor and a reason.</p>' +
            '<div class="nextcards">' + cards + '</div></div>';
    }

    /* --------------------------------------------------- refused screen */

    function refusalOf() {
        var b = storedBook();
        if (!b || S.isNew) return null;
        var next = S.fields.book_status;
        var cur = b.status;
        if (next === cur) return null;
        if (next === 'Closed' && b.openActivity > 0) {
            return { kind: 'open_activity', bookName: b.name, count: b.openActivity, from: cur, to: next };
        }
        var allowed = TRANSITIONS[cur] || [];
        if (allowed.indexOf(next) < 0) {
            return { kind: 'transition', bookName: b.name, from: cur, to: next, allowed: allowed };
        }
        return null;
    }

    function refusalText(r) {
        if (r.kind === 'open_activity') {
            return 'Book <span class="mono">' + esc(r.bookName) + '</span> cannot move from <b>' + esc(r.from) +
                '</b> to <b>Closed</b>: the ledger still shows <b>' + r.count + ' open trades</b> against it. ' +
                'Close or reassign them first.';
        }
        return 'Book <span class="mono">' + esc(r.bookName) + '</span> cannot move from <b>' + esc(r.from) +
            '</b> to <b>' + esc(r.to) + '</b>. From ' + esc(r.from) + ' the only allowed target is ' +
            (r.allowed.length ? r.allowed.join(', ') : 'none; ' + esc(r.from) + ' is terminal') + '.';
    }

    function refusedScreen() {
        var r = refusalOf();
        if (!r) {
            if (S.fail === 'open_activity') {
                r = { kind: 'open_activity', bookName: S.book || 'EUR_SWAPS_01', count: 14, from: 'Active', to: 'Closed' };
            } else if (S.fail === 'transition') {
                r = { kind: 'transition', bookName: S.book || 'RESERVE_01', from: 'Frozen', to: 'Closed', allowed: ['Active'] };
            } else {
                r = { kind: 'open_activity', bookName: S.book || 'EUR_SWAPS_01', count: 14, from: 'Active', to: 'Closed' };
            }
        }
        var subject = 'refdata.v1.books.put';
        var code = '23514';
        var b = storedBook();
        var versionRow = b ?
            'Stays at version ' + historyFor(b).length + '. No new version is written.' :
            'No version is written, because no record exists yet.';
        var detail = r.kind === 'open_activity' ?
            '<table class="grid" style="margin-top:12px"><thead><tr><th>What the book still holds</th><th>Count</th></tr></thead><tbody>' +
            '<tr><td>Open trades</td><td>' + r.count + '</td></tr>' +
            '<tr><td>Unsettled cash flows</td><td>6</td></tr>' +
            '<tr><td>Open positions</td><td>' + (r.count - 4 > 0 ? r.count - 4 : 0) + '</td></tr>' +
            '</tbody></table>' +
            '<p class="hint" style="margin-top:12px">A closed book keeps its trades and its history. It accepts no new trades, and it is not deleted. ' +
            'Move the activity to another leaf book, or wait for it to settle.</p>' :
            '<table class="grid" style="margin-top:12px"><thead><tr><th>From</th><th>Allowed targets</th></tr></thead><tbody>' +
            '<tr><td>' + statusBadge(r.from) + '</td><td>' + (r.allowed.length ? r.allowed.map(function (s) { return statusBadge(s); }).join(' ') : '<span class="faint">none \u2014 terminal</span>') + '</td></tr>' +
            '</tbody></table>' +
            '<p class="hint" style="margin-top:12px">A frozen book must be returned to Active before it can be closed. The rule exists so that a freeze is always reversible and a close is always deliberate.</p>';

        return '<div class="outcome"><div class="mark bad">\u2715</div>' +
            '<h2>Refused</h2><p>' + refusalText(r) + '</p></div>' +
            '<div class="notice error" style="margin-top:22px">' +
            '<span class="title">The server returned an error</span>' +
            '<table class="kv">' +
            '<tr><td>What was refused</td><td>' + refusalText(r) + '</td></tr>' +
            '<tr><td>Subject</td><td class="mono">' + subject + '</td></tr>' +
            '<tr><td>Intent</td><td class="mono">must_match_version</td></tr>' +
            '<tr><td>Error code</td><td class="mono">' + code + '</td></tr>' +
            '<tr><td>Message</td><td>' + refusalText(r) + '</td></tr>' +
            '<tr><td>Record version</td><td>' + versionRow + '</td></tr>' +
            '<tr><td>Raised at</td><td>Classification \u2192 Write. Go back to that step to change the value that raised it.</td></tr>' +
            '</table></div>' +
            detail +
            '<div class="notice warn">' +
            '<span class="title">Gap: the server does not refuse either case today</span>' +
            'No operation checks a book’s open activity, and <span class="mono">ores_refdata_validate_book_status_fn</span> only checks that the status code exists. ' +
            'The store holds no transition rule. The screen must either check before it writes or the server must refuse; the refusal is drawn here as the state the journey must handle.</div>' +
            '<div class="stepfoot">' +
            '<button type="button" class="btn ghost" data-act="goback">Back to the classification</button>' +
            '<button type="button" class="btn ghost ml-auto" data-act="gohistory">History</button>' +
            '</div>';
    }

    /* --------------------------------------------------- history screen */

    function historyFor(b) {
        if (!b) return [];
        if (HISTORIES[b.name]) return HISTORIES[b.name];
        return [{
            version: 1, by: 'ores_refdata_service', performed: 'ores_refdata_service',
            at: '2026-06-01 09:14', reason: 'system.new_record',
            note: 'Created by the nightly book sync from the ledger.', fields: {
                name: b.name, description: b.description || '', functional_currency: b.ccy,
                gl_account_ref: b.gl, cost_center: b.cost, book_status: b.status,
                regulatory_book_type: b.regulatory, book_purpose_type: b.purpose,
                ledger_feed_type: b.ledger, is_sweepable: String(b.sweepable),
                rates_centre_code: b.centre
            }
        }];
    }

    function markPair(a, b) {
        a = String(a === undefined ? '' : a);
        b = String(b === undefined ? '' : b);
        var p = 0;
        while (p < a.length && p < b.length && a.charAt(p) === b.charAt(p)) p += 1;
        var s = 0;
        while (s < a.length - p && s < b.length - p && a.charAt(a.length - 1 - s) === b.charAt(b.length - 1 - s)) s += 1;
        var aMid = a.slice(p, a.length - s);
        var bMid = b.slice(p, b.length - s);
        return {
            old: esc(a.slice(0, p)) + (aMid ? '<mark>' + esc(aMid) + '</mark>' : '') + esc(s ? a.slice(a.length - s) : ''),
            new: esc(b.slice(0, p)) + (bMid ? '<mark>' + esc(bMid) + '</mark>' : '') + esc(s ? b.slice(b.length - s) : '')
        };
    }

    function historyScreen() {
        var b = storedBook();
        if (!b) {
            return '<div class="notice warn">History reads one book’s versions. Choose a book in the tree first.</div>';
        }
        var h = historyFor(b);
        if (S.version < 0 || S.version >= h.length) S.version = h.length - 1;
        var v = h[S.version];
        var prev = S.version > 0 ? h[S.version - 1] : null;

        var timeline = h.map(function (item, i) {
            return '<li><button type="button" class="' + (i === S.version ? 'on' : '') + '" data-act="version" data-v="' + i + '">' +
                '<span class="v">Version ' + item.version + '</span>' +
                '<span class="m">' + esc(item.by) + ' \u00b7 ' + esc(item.at) + '</span>' +
                '<span class="m mono">' + esc(item.reason) + '</span></button></li>';
        }).join('');

        var diff;
        if (!prev) {
            diff = '<tr><td class="f">\u2014</td><td class="sub">The oldest version. There is nothing before it to compare.</td></tr>';
        } else {
            var rows = FIELD_KEYS.filter(function (k) { return String(prev.fields[k]) !== String(v.fields[k]); });
            rows = rows.map(function (k) {
                var m = markPair(prev.fields[k], v.fields[k]);
                return '<tr><td class="f">' + esc(k) + '</td><td><div class="diff">' +
                    '<div class="line old">\u2212 ' + m.old + '</div>' +
                    '<div class="line new">+ ' + m.new + '</div></div></td></tr>';
            });
            diff = rows.length ? rows.join('') :
                '<tr><td class="f">\u2014</td><td class="sub">No field changed between these versions.</td></tr>';
        }

        var fields = FIELD_KEYS.map(function (k) {
            var changed = prev && String(prev.fields[k]) !== String(v.fields[k]);
            return '<tr' + (changed ? ' style="background:#16161a"' : '') + '><td class="f">' + esc(k) + '</td><td>' + esc(v.fields[k]) + '</td></tr>';
        }).join('');

        return '<div class="notice info" style="margin-top:0">' +
            'History is one request for every refdata entity: <span class="mono">refdata.v1.history.get</span> with ' +
            '<span class="mono">entity_type = ores.refdata.book</span> and the book’s id. ' +
            'A version is never changed and never removed; a correction is a new version.</div>' +
            '<div class="history">' +
            '<div><h3 style="font-size:13px;margin:0 0 8px">Versions of <span class="mono">' + esc(b.name) + '</span></h3>' +
            '<ul class="versions">' + timeline + '</ul>' +
            '<button type="button" class="btn small" data-act="revert" style="margin-top:10px">Revert this version</button>' +
            '<p class="hint">A revert writes the old values as a new version. Nothing in the history is lost.</p></div>' +
            '<div><div class="sectionhead"><h3>Version ' + v.version + '</h3>' + tenantChip() + '</div>' +
            '<p class="lede" style="margin-bottom:14px">' + esc(v.by) + ' \u00b7 ' + esc(v.at) + ' \u00b7 performed by ' + esc(v.performed) + '</p>' +
            (v.note ? '<div class="notice info" style="margin-bottom:14px">' + esc(v.note) + '</div>' : '') +
            '<table class="difftable"><thead><tr><th>Field</th><th>Change from version ' + (prev ? prev.version : '\u2014') + '</th></tr></thead><tbody>' + diff + '</tbody></table>' +
            '<h3 style="font-size:13px;margin:20px 0 8px">The full version</h3>' +
            '<table class="difftable"><tbody>' + fields + '</tbody></table></div>' +
            '</div>';
    }

    /* ------------------------------------------------------- the shell */

    function renderRail() {
        var currentIndex = STEPS.map(function (s) { return s.id; }).indexOf(S.screen);
        var sideParent = S.screen === 'refused' ? 'review' : S.screen === 'history' ? 'tree' : null;
        return '<nav class="railnav" aria-label="Journey steps"><ol>' +
            STEPS.map(function (s, i) {
                var cls;
                if (currentIndex >= 0) cls = i === currentIndex ? 'current' : (i < currentIndex ? 'done' : 'ahead');
                else cls = s.id === sideParent ? 'current' : (STEPS.map(function (x) { return x.id; }).indexOf(sideParent) > i ? 'done' : 'ahead');
                return '<li class="railentry ' + cls + '"' + (cls === 'current' ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (cls === 'done' ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(s.title) + '</li>';
            }).join('') + '</ol></nav>';
    }

    function stepHeader() {
        var p = pf(S.portfolio);
        var sub = S.isNew ? 'New book' : (S.book ? 'Book ' + S.book : 'No book chosen');
        return '<div class="stepheader"><div>' +
            '<div class="nm">' + esc(TENANT) + '</div>' +
            '<div class="sub">Portfolio ' + esc(p ? p.name : '\u2014') + ' \u00b7 <span class="mono">' + esc(S.portfolio) + '</span> \u00b7 ' +
            (S.book ? '<span class="mono">' + esc(S.book) + '</span>' : '<span class="mono">' + esc(sub) + '</span>') +
            '</div></div></div>';
    }

    function stepBody() {
        if (S.screen === 'tree') return treeScreen();
        if (S.screen === 'portfolio') return portfolioScreen();
        if (S.screen === 'book') return bookScreen();
        if (S.screen === 'classify') return classifyScreen();
        if (S.screen === 'rights') return rightsScreen();
        if (S.screen === 'review') return reviewScreen();
        if (S.screen === 'outcome') return outcomeScreen();
        if (S.screen === 'refused') return refusedScreen();
        return historyScreen();
    }

    function currentStep() {
        return STEPS.filter(function (s) { return s.id === S.screen; })[0];
    }

    function nextOf() {
        var id = S.screen;
        if (id === 'tree') return { label: 'Choose the portfolio', enabled: true, to: 'portfolio' };
        if (id === 'portfolio') return { label: 'Continue', enabled: true, to: 'book' };
        if (id === 'book') return { label: 'Continue', enabled: S.fields.name.trim() !== '', to: 'classify' };
        if (id === 'classify') return { label: 'Continue', enabled: true, to: 'rights' };
        if (id === 'rights') return { label: 'Continue', enabled: true, to: 'review' };
        if (id === 'review') return { label: 'Write', enabled: true, to: 'write' };
        return null;
    }

    function render() {
        var step = currentStep();
        var side = SIDES.filter(function (s) { return s.id === S.screen; })[0];
        var title = step ? step.title : (side ? side.title : S.screen);
        var lead = step ? step.lead : (S.screen === 'refused' ? 'What the server says when the write is not allowed.' : 'Every version, with the field diff.');
        var next = nextOf();
        var prevId = STEPS[STEPS.map(function (s) { return s.id; }).indexOf(S.screen) - 1];
        var foot = next === null ? '' :
            '<div class="stepfoot">' +
            '<button type="button" class="btn ghost" data-act="back"' + (prevId ? '' : ' disabled') + '>Back</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="next"' + (next.enabled ? '' : ' disabled') + '>' +
            esc(next.label) + '</button></div>';

        document.getElementById('app').innerHTML =
            '<div class="page"><h1>Shape the book structure</h1>' +
            '<div class="journey">' + renderRail() +
            '<section class="card">' + stepHeader() +
            '<h2>' + esc(title) + '</h2><p class="lead">' + esc(lead) + '</p>' +
            stepBody() + foot + '</section></div></div>';

        renderNote();
        renderBar();
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey Shape the book structure \u00b7 tenant ' + TENANT +
            ' \u00b7 portfolio ' + S.portfolio + ' \u00b7 book ' + (S.isNew ? '(new)' : (S.book || 'none')) +
            ' \u00b7 state ' + S.screen;
    }

    function renderBar() {
        var states = STEPS.map(function (s) {
            return '<button data-act="state" data-state="' + s.id + '"' + (S.screen === s.id ? ' class="on"' : '') + '>' + esc(s.id) + '</button>';
        }).join('');
        var sides = SIDES.map(function (s) {
            return '<button data-act="state" data-state="' + s.id + '"' + (S.screen === s.id ? ' class="on"' : '') + '>' + esc(s.id) + '</button>';
        }).join('');
        var fails = '<span class="label">refuse</span>' +
            '<button data-act="fail" data-fail="open_activity"' + (S.fail === 'open_activity' ? ' class="on"' : '') + '>open activity</button>' +
            '<button data-act="fail" data-fail="transition"' + (S.fail === 'transition' ? ' class="on"' : '') + '>transition</button>';
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + states + '<span class="sep">|</span>' + sides +
            '<span class="sep">|</span>' + fails;
    }

    /* -------------------------------------------------------- behaviour */

    function rerender() {
        var ae = document.activeElement;
        var key = ae && ae.getAttribute ?
            (ae.getAttribute('data-focus') || ae.getAttribute('data-f')) : null;
        var start = ae && typeof ae.selectionStart === 'number' ? ae.selectionStart : null;
        var end = ae && typeof ae.selectionEnd === 'number' ? ae.selectionEnd : null;
        render();
        if (key) {
            var el = document.querySelector('[data-focus="' + key + '"]') ||
                document.querySelector('[data-f="' + key + '"]');
            if (el) {
                el.focus();
                if (start !== null && el.setSelectionRange) {
                    try { el.setSelectionRange(start, end); } catch (e) { /* not a text input */ }
                }
            }
        }
    }

    function go(screen) {
        S.screen = screen;
    }

    function pickBook(name) {
        var b = book(name);
        if (!b) return;
        S.isNew = false;
        S.book = b.name;
        S.portfolio = b.portfolio;
        S.open[b.portfolio] = true;
        resetWorking();
    }

    function pickPortfolio(id) {
        S.portfolio = id;
        var b = S.book ? book(S.book) : undefined;
        if (!S.isNew && b && b.portfolio !== id) {
            /* A book belongs to exactly one portfolio, so choosing another
             * portfolio starts a new book rather than moving one. */
            S.isNew = true;
            S.book = '';
            resetWorking();
        } else if (S.isNew) {
            resetWorking();
        }
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'nolink') { ev.preventDefault(); return; }
        ev.preventDefault();

        if (act === 'state') {
            go(el.getAttribute('data-state'));
        } else if (act === 'toggle') {
            var node = el.getAttribute('data-node');
            S.open[node] = !S.open[node];
        } else if (act === 'pickpf') {
            pickPortfolio(el.getAttribute('data-node'));
        } else if (act === 'pickbk') {
            pickBook(el.getAttribute('data-name'));
        } else if (act === 'openbook') {
            pickBook(el.getAttribute('data-name'));
            go('book');
        } else if (act === 'newbookhere') {
            S.isNew = true;
            S.book = '';
            resetWorking();
            go('book');
        } else if (act === 'goclass') {
            go('classify');
        } else if (act === 'gorights') {
            go('rights');
        } else if (act === 'gohistory') {
            go('history');
        } else if (act === 'goreview') {
            go('review');
        } else if (act === 'goback') {
            go('classify');
        } else if (act === 'outcome') {
            go(el.getAttribute('data-to'));
        } else if (act === 'pf-form') {
            S.pfForm = true;
        } else if (act === 'pf-cancel') {
            S.pfForm = false;
        } else if (act === 'pf-write') {
            S.pfForm = false;
        } else if (act === 'right') {
            var account = el.getAttribute('data-account');
            var right = el.getAttribute('data-right');
            var i = -1;
            S.rights.forEach(function (r, j) {
                if (r.account === account && r.portfolio === S.portfolio && r.right === right) i = j;
            });
            if (el.checked && i < 0) S.rights.push({ account: account, portfolio: S.portfolio, right: right });
            else if (!el.checked && i >= 0) S.rights.splice(i, 1);
        } else if (act === 'version') {
            S.version = parseInt(el.getAttribute('data-v'), 10);
        } else if (act === 'fail') {
            S.fail = el.getAttribute('data-fail');
            go('refused');
        } else if (act === 'refuse-close') {
            S.fields.book_status = 'Closed';
            S.fail = storedBook() && storedBook().openActivity > 0 ? 'open_activity' : 'transition';
            go('refused');
        } else if (act === 'back') {
            if (!el.disabled) {
                var ids = STEPS.map(function (s) { return s.id; });
                var idx = ids.indexOf(S.screen);
                if (S.screen === 'refused' || S.screen === 'history') go('review');
                else if (idx > 0) go(ids[idx - 1]);
            }
        } else if (act === 'next') {
            if (!el.disabled) {
                var n = nextOf();
                if (!n) return;
                if (n.to === 'write') {
                    var r = refusalOf();
                    if (r) { S.fail = r.kind; go('refused'); } else { S.fail = null; go('outcome'); }
                } else {
                    go(n.to);
                }
            }
        } else {
            return;
        }
        rerender();
    });

    function inputChanged(el) {
        if (el.getAttribute('data-act') !== null) return false;
        if (el.getAttribute('data-q') !== null) { S.query = el.value; return true; }
        var f = el.getAttribute('data-f');
        if (f === null) return false;
        if (f === 'is_sweepable') S.fields.is_sweepable = el.checked;
        else if (f.indexOf('pf') === 0) return false;
        else S.fields[f] = el.value;
        return true;
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

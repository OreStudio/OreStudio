/* Trace a published value.
 *
 * The journey walks backwards first: the row, its provenance, the dataset behind
 * it. The forward question — which trades reference this value — has no path in
 * the tree, because a trade names a convention by a text column and no subject
 * searches for one. The screen states that rather than drawing an empty list,
 * which would read as "no trade uses this". */

(function () {
    'use strict';

    var M = window.Mgmt;
    var Q = M.params();

    M.NOW = Date.parse('2026-11-22T12:00:00Z');
    M.LOADED = '2026-11-22T11:58:00+00:00';

    var KINDS = [
        ['conventions', 'Convention'],
        ['currencies', 'Currency'],
        ['countries', 'Country'],
        ['calendars', 'Calendar']
    ];

    var VALUES = {
        'AED-EIBOR-6M-SWAP': {
            kind: 'conventions',
            key: 'AED-EIBOR-6M-SWAP',
            values: [['Business day', 'modfollow'], ['Calendars', 'AED, AEB'], ['Fixed frequency', 'Semiannual']],
            provenance: [
                ['Dataset', 'refdata.conventions'],
                ['Reason', 'system.external_data_import'],
                ['Commentary', 'Imported from DQ dataset: refdata.conventions'],
                ['Version', '3'],
                ['Recorded', '2026-11-22T10:10:00Z']
            ],
            dataset: [['Name', 'ORE Instrument Conventions'], ['Source', 'Vendor conventions list'], ['As of', '2026-11-20']]
        }
    };

    var KNOWN_TRADES = [
        { key: 'ACME-2026-000481', party: 'ACME', convention: 'AED-EIBOR-6M-SWAP' },
        { key: 'ACME-2026-000902', party: 'ACME', convention: 'AED-EIBOR-6M-SWAP' }
    ];

    var S = {
        kind: Q.get('kind') || 'conventions',
        key: Q.get('key') || 'AED-EIBOR-6M-SWAP',
        person: M.PEOPLE[Q.get('actor')] || M.PEOPLE['m.okafor']
    };
    if (Q.get('gaps') === '0') { document.body.classList.add('nogaps'); }

    function value() { return VALUES[S.key]; }

    function go(changes) {
        var q = new URLSearchParams(window.location.search);
        Object.keys(changes).forEach(function (k) {
            if (changes[k] === null) { q.delete(k); } else { q.set(k, changes[k]); }
        });
        window.location.search = q.toString();
    }

    function cards(v) {
        if (!v) {
            return '<div class="card"><p class="tblstate">The mock holds one value: ' +
                '<span class="key">AED-EIBOR-6M-SWAP</span>.</p></div>';
        }
        var back = '<div class="card cardgap"><div class="cardhead"><h2>Where it came from</h2>' +
            '<span class="dim">The half the tree answers</span></div>' +
            '<h3>The row</h3><dl class="fields">' +
            '<div><dt>Kind</dt><dd class="mono">' + M.esc(S.kind) + '</dd></div>' +
            '<div><dt>Key</dt><dd class="mono">' + M.esc(v.key) + '</dd></div>' +
            v.values.map(function (row) {
                return '<div><dt>' + M.esc(row[0]) + '</dt><dd class="mono">' + M.esc(row[1]) + '</dd></div>';
            }).join('') +
            '</dl>' +
            '<h3>Its provenance</h3><dl class="fields">' +
            v.provenance.map(function (row) {
                return '<div><dt>' + M.esc(row[0]) + '</dt><dd class="mono">' +
                    (row[0] === 'Recorded' ? M.relTime(row[1]) : M.esc(row[1])) + '</dd></div>';
            }).join('') +
            '</dl>' +
            '<h3>The dataset behind it</h3><dl class="fields">' +
            v.dataset.map(function (row) {
                return '<div><dt>' + M.esc(row[0]) + '</dt><dd class="mono">' + M.esc(row[1]) + '</dd></div>';
            }).join('') +
            '</dl>' +
            '<p class="hint">The provenance names the dataset and not the run, because a run leaves no row. ' +
            'From the dataset the person reaches its artefact type and its staging table.</p></div>';

        var trades = KNOWN_TRADES.map(function (t) {
            return '<tr><td class="key">' + M.esc(t.key) + '</td><td>' + M.esc(t.party) + '</td>' +
                '<td class="mono">' + M.esc(t.convention) + '</td></tr>';
        }).join('');

        var forward = '<div class="card cardgap"><div class="cardhead"><h2>What references it</h2>' +
            '<span class="dim">The half it cannot answer</span></div>' +
            '<h3>The question</h3><p class="lede">Which trades use ' + M.esc(v.key) + '?</p>' +
            M.notice('warn', '<b>No path from a value to a trade.</b> A trade names a convention by a text ' +
                'column on its instrument tables, not by an identifier, and no subject searches the trading ' +
                'tree for a value. Drawing an empty list here would read as "no trade uses this", which is a ' +
                'different and false claim.') +
            '<h3>What the tree can answer instead</h3>' +
            '<p class="lede">A person who already knows a trade can open it and read the convention it names.</p>' +
            '<table class="rows"><thead><tr><th>Trade</th><th>Party</th><th>Convention named</th></tr></thead>' +
            '<tbody>' + trades + '</tbody></table>' +
            '<p class="hint">These two are mock rows a person found by trying keys, not rows the screen ' +
            'searched for. Until the reference is an identifier, or something indexes the text, that is the ' +
            'only way to answer.</p></div>';

        return back + forward;
    }

    function render() {
        var v = value();
        document.getElementById('app').innerHTML =
            M.shell('Reference data', S.person) +
            '<main class="page">' +
            M.crumbs([
                { t: 'Home', href: '#' },
                { t: 'Reference data', href: '#' },
                { t: 'Reference data coverage', href: '../reference-data-coverage/index.html' },
                { t: 'Trace a value' }
            ]) +
            M.pagehead('Trace a published value',
                'Where the value came from, and which trades reference it',
                M.refresh(false)) +
            '<div class="card cardgap"><div class="cardhead"><h2>The value I hold</h2>' +
            '<span class="dim">A kind and a key, because one string names rows in more than one kind</span></div>' +
            '<div class="grid2">' +
            '<div class="field"><span class="lbl">Kind</span>' +
            M.select('kind', KINDS.map(function (k) { return { v: k[0], t: k[1] }; }), S.kind) + '</div>' +
            '<div class="field"><span class="lbl">Key</span>' +
            M.select('key', Object.keys(VALUES).map(function (k) { return { v: k, t: k }; }), S.key) + '</div>' +
            '</div></div>' +
            cards(v) + '</main>';

        M.wireSelects(document.getElementById('app'), function (id, value) {
            var change = {};
            change[id] = value;
            go(change);
        });

        document.getElementById('proto-bar').innerHTML = M.bar([
            { v: 'value', t: 'The value' },
            { v: 'provenance', t: 'Its provenance' },
            { v: 'forward', t: 'What references it' }
        ], 'value');
        var bar = document.getElementById('proto-bar');
        bar.querySelectorAll('[data-act="bar"]').forEach(function (btn) {
            btn.addEventListener('click', function () { go({}); });
        });
        bar.querySelector('[data-act="gaps"]').addEventListener('click', function () {
            var q = new URLSearchParams(window.location.search);
            if (document.body.classList.contains('nogaps')) { q.delete('gaps'); } else { q.set('gaps', '0'); }
            window.location.search = q.toString();
        });
    }

    render();
})();

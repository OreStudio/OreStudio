(function () {
    'use strict';

    var RUN = {
        id: '4b7e1a02-9c3d-4f61-8a75-2e0d9b4c1f83',
        definition: 'Cross-Entity Counterparty Concentration',
        kind: 'risk',
        party: { code: 'ACMCOR', name: 'ACME CORPORATION' },
        state: 'completed',
        startedAt: '17 Oct 2026 08:12:44',
        completedAt: '17 Oct 2026 08:15:02',
        duration: '2 m 18 s',
        analytics: ['npv', 'saccr'],
        errorRows: 11,
        unattributedErrorRows: 2
    };

    var CP = [
        {
            id: 'c-acmegrp', code: 'ACMEGRP', name: 'ACME FINANCIAL GROUP', parent: null,
            pop: { npv: 0, notional: 0, count: 0, exposure: 0 }, trades: []
        },
        {
            id: 'c-acmebk', code: 'ACMEBK', name: 'ACME BANK PLC', parent: 'c-acmegrp',
            pop: { npv: 21904772, notional: 3902000000, count: 2318, exposure: 1486300000 },
            trades: [
                { id: '2df8fa31-5dc6-4b79-a1d7-7c4e9f3b6a05', type: 'Swap', notional: 40000000, npv: 1284402, ns: 'ACME-CSA-1', error: null },
                { id: '7c1a9d44-2f6b-4c8e-9a10-3b5d2e6f8c71', type: 'Swap', notional: 120000000, npv: 3902118, ns: 'ACME-CSA-1', error: null },
                { id: '4e8b2c05-9d17-4a6f-b3c2-6e1f8a7d5b20', type: 'FX Forward', notional: 85000000, npv: -412905, ns: 'ACME-CSA-1', error: null },
                {
                    id: '2d0f8a31-5e6c-4b79-a1d2-7c4e9f3b6a05', type: 'Bond', notional: 40000000, npv: null, ns: 'ACME-CSA-2',
                    error: { severity: 'error', scope: 'Analytics', message: 'Trade failed to price: index curve SONIA-3M has no point at 2031-06-15.' }
                },
                { id: '9a3e7b12-6c04-4d58-8f21-1b7c5e2a9d63', type: 'FX Option', notional: 25000000, npv: 188240, ns: 'ACME-CSA-2', error: null }
            ]
        },
        {
            id: 'c-acmeint', code: 'ACMEINT', name: 'ACME BANK INTERNATIONAL', parent: 'c-acmegrp',
            pop: { npv: 6402118, notional: 1120000000, count: 604, exposure: 428900000 },
            trades: [
                { id: '5b2d9e77-8a41-4c03-b6f9-2d7e1c4a8f35', type: 'Cross Currency Swap', notional: 150000000, npv: 2104552, ns: 'ACME-INT-1', error: null },
                { id: '1f7a3c88-4e2b-49d1-a05c-8b3d6f9e2c47', type: 'Swap', notional: 90000000, npv: 1402331, ns: 'ACME-INT-1', error: null },
                {
                    id: '6c9b1d20-3f58-4a7e-9c14-5e2b8d0f7a36', type: 'Bond', notional: 60000000, npv: 894120, ns: 'ACME-INT-1',
                    error: { severity: 'warning', scope: 'Curve', message: 'Curve EUR-6M extrapolated beyond 30Y for maturity 2058-03-01.' }
                }
            ]
        },
        {
            id: 'c-meridgrp', code: 'MERIDGRP', name: 'MERIDIAN CAPITAL GROUP', parent: null,
            pop: { npv: 3112004, notional: 620000000, count: 388, exposure: 121400000 },
            trades: [
                { id: '9b3d7a24-1e68-4c95-8f07-2a5e9c1b4d38', type: 'Swap', notional: 180000000, npv: 2204118, ns: 'MERIDGRP-TREAS', error: null },
                { id: '5f1c8e60-4a27-4b93-a6d1-8e3b5f2c7a09', type: 'Swap', notional: 95000000, npv: 907886, ns: 'MERIDGRP-TREAS', error: null }
            ]
        },
        {
            id: 'c-merid', code: 'MERID', name: 'MERIDIAN CAPITAL', parent: 'c-meridgrp',
            pop: { npv: 8204118, notional: 1610000000, count: 1204, exposure: 604250000 },
            trades: [
                { id: '3a8e5b41-7d29-4c16-8f02-9b6e4d1a3c58', type: 'Swap', notional: 200000000, npv: 2808441, ns: 'MERID-CSA-1', error: null },
                { id: '8d4c2f19-5b63-47a8-a1e0-2c9f7b5d4e82', type: 'FX Forward', notional: 75000000, npv: -204118, ns: 'MERID-CSA-1', error: null },
                { id: '0e6f9a35-1c47-4b82-bd93-7a2e5c8f1d04', type: 'Swap', notional: 140000000, npv: 1640905, ns: 'MERID-CSA-2', error: null },
                { id: '4b1d7e90-8a26-4f53-9c71-6d3b2a9e5c18', type: 'CapFloor', notional: 50000000, npv: 412604, ns: 'MERID-CSA-2', error: null }
            ]
        },
        {
            id: 'c-meriduk', code: 'MERID-UK', name: 'MERIDIAN CAPITAL UK', parent: 'c-meridgrp',
            pop: { npv: 4278433, notional: 808000000, count: 538, exposure: 314150000 },
            trades: [
                { id: '7e2a5c81-9f34-4d06-b7a8-3c1e6d9b2f47', type: 'Swap', notional: 110000000, npv: 1804220, ns: 'MERIDUK-CSA-1', error: null },
                {
                    id: '2c8f4b17-6d59-4a30-8e1c-5b7a3f0d9e26', type: 'FX Forward', notional: 65000000, npv: null, ns: 'MERIDUK-CSA-1',
                    error: { severity: 'error', scope: 'Trade', message: 'Trade failed to price: settlement falls on a holiday in the trade calendars.' }
                }
            ]
        },
        {
            id: 'c-nrthclr', code: 'NRTHCLR', name: 'NORTHGATE CLEARING', parent: null,
            pop: { npv: 9118004, notional: 1744000000, count: 906, exposure: 512900000 },
            trades: [
                { id: '6a9e2c47-8b15-4f30-9d82-1c4b7e6a3f59', type: 'Swap', notional: 160000000, npv: 2940118, ns: 'NRT-CLEAR-1', error: null },
                { id: '1d5b8f73-2c49-4a61-b8e0-7f3a9d2c5b14', type: 'Bond', notional: 95000000, npv: 1704662, ns: 'NRT-CLEAR-1', error: null },
                { id: '3f7a1d58-9e24-4c70-a3b1-6d8e2f5c0a97', type: 'FX Forward', notional: 120000000, npv: -312904, ns: 'NRT-CLEAR-2', error: null }
            ]
        },
        {
            id: 'c-hansapf', code: 'HANSAPF', name: 'HANSA PFANDBRIEF', parent: null,
            pop: { npv: 2940188, notional: 622000000, count: 214, exposure: 88700000 },
            trades: [
                { id: '8c2f6b19-4d73-4a08-9e15-3b7c1d9a5f42', type: 'Bond', notional: 140000000, npv: 1882004, ns: 'HANSA-CSA-1', error: null },
                { id: '4a9d3e75-6b18-4c29-8f30-2e5a7c1b9d64', type: 'Swap', notional: 90000000, npv: 1058184, ns: 'HANSA-CSA-1', error: null }
            ]
        },
        {
            id: 'c-pactrst', code: 'PACTRST', name: 'PACIFIC TRUST', parent: null,
            pop: { npv: 1704662, notional: 418000000, count: 168, exposure: 41200000 },
            trades: [
                { id: '2b6e9a41-3c85-4d17-8a92-5f1b7c4d2e08', type: 'Swap', notional: 100000000, npv: 1204118, ns: 'PAC-CSA-1', error: null },
                { id: '7d1c4f82-5e39-4b60-9a27-8c3e1f6b5a90', type: 'Bond', notional: 70000000, npv: 500544, ns: 'PAC-CSA-1', error: null }
            ]
        },
        {
            id: 'c-kstlasr', code: 'KSTLASR', name: 'KESTREL ASSURANCE', parent: null,
            pop: { npv: 884310, notional: 212000000, count: 96, exposure: 18400000 },
            trades: [
                { id: '5e8a2d16-7f40-4c93-b1e8-9d2c6a4f3b75', type: 'Swap', notional: 60000000, npv: 604118, ns: 'KESTREL-CSA-1', error: null },
                { id: '9c4b7e23-1a58-4d06-8f39-2b6d5e8a1c47', type: 'FX Forward', notional: 40000000, npv: 280192, ns: 'KESTREL-CSA-1', error: null }
            ]
        },
        {
            id: 'c-blueriv', code: 'BLUERIV', name: 'BLUE RIVER PENSION', parent: null,
            pop: { npv: 0, notional: 0, count: 0, exposure: 0 }, trades: [], noResult: true
        }
    ];

    var ORPHAN = {
        id: 'c-none', code: '--', name: 'No counterparty', parent: null, orphan: true,
        pop: { npv: 1205000, notional: 980000000, count: 412, exposure: null },
        trades: [
            {
                id: '1a7c4e92-6d35-4b08-9f21-3e8a5c2d7b64', type: 'Swap', notional: 500000000, npv: 804118, ns: 'INTRAGRP-1',
                error: { severity: 'warning', scope: 'Analytics', message: 'Position skipped by the sensitivity analytic; no Delta recorded.' }
            },
            { id: '6d3f9b51-2e84-4a27-8c06-7b1e4d9a3f58', type: 'Bond', notional: 380000000, npv: 400882, ns: 'INTRAGRP-1', error: null }
        ]
    };

    var VARIANTS = {
        A: 'ranked list',
        B: 'tree',
        C: 'map'
    };

    var params = new URLSearchParams(window.location.search);
    var state = {
        variant: VARIANTS[params.get('variant')] ? params.get('variant') : 'A',
        analytics: params.get('analytics') === 'npv' ? 'npv' : 'npv-saccr',
        selected: params.get('counterparty') || null,
        collapsed: {}
    };

    var byId = {};
    CP.forEach(function (cp) { byId[cp.id] = cp; });

    function childrenOf(id) {
        return CP.filter(function (cp) { return cp.parent === id; });
    }

    function isGroup(cp) { return childrenOf(cp.id).length > 0; }

    function hasSaccr() { return state.analytics === 'npv-saccr'; }

    var aggMemo = {};
    function aggregate(cp) {
        if (aggMemo[cp.id]) { return aggMemo[cp.id]; }
        var total = {
            npv: cp.pop.npv, notional: cp.pop.notional,
            count: cp.pop.count, exposure: hasSaccr() ? cp.pop.exposure : null
        };
        childrenOf(cp.id).forEach(function (kid) {
            var k = aggregate(kid);
            total.npv += k.npv;
            total.notional += k.notional;
            total.count += k.count;
            total.exposure = (total.exposure === null || k.exposure === null)
                ? null : total.exposure + k.exposure;
        });
        aggMemo[cp.id] = total;
        return total;
    }

    function rankMetric(cp) {
        var a = aggregate(cp);
        return hasSaccr() && a.exposure !== null ? a.exposure : a.npv;
    }

    function topLevel() {
        return CP.filter(function (cp) { return cp.parent === null && !cp.noResult; })
            .sort(function (x, y) { return rankMetric(y) - rankMetric(x); });
    }

    function noResultCPs() {
        return CP.filter(function (cp) { return cp.noResult; });
    }

    function runTotals() {
        var t = { npv: 0, notional: 0, count: 0, exposure: 0, exposureKnown: hasSaccr() };
        topLevel().forEach(function (cp) {
            var a = aggregate(cp);
            t.npv += a.npv;
            t.notional += a.notional;
            t.count += a.count;
            if (a.exposure === null) { t.exposureKnown = false; } else { t.exposure += a.exposure; }
        });
        t.cpNpv = t.npv;
        t.cpNotional = t.notional;
        t.cpTrades = t.count;
        t.cpExposure = t.exposure;
        t.npv += ORPHAN.pop.npv;
        t.notional += ORPHAN.pop.notional;
        t.count += ORPHAN.pop.count;
        return t;
    }

    function sumMembers(cp) {
        var out = [];
        childrenOf(cp.id).forEach(function (kid) {
            out.push(kid);
            out = out.concat(sumMembers(kid));
        });
        return out;
    }

    function findSelectable(key) {
        if (!key) { return null; }
        var all = CP.concat([ORPHAN]);
        var hit = all.filter(function (cp) {
            return cp.id === key || cp.code.toLowerCase() === String(key).toLowerCase();
        })[0];
        return hit || null;
    }

    if (!findSelectable(state.selected)) { state.selected = 'c-acmebk'; }
    var selected = findSelectable(state.selected);

    /* ---------------------------------------------------------- format */

    function n0(v) { return String(Math.round(v)).replace(/\B(?=(\d{3})+(?!\d))/g, ','); }
    function money(v) { return v === null || v === undefined ? '-' : n0(v); }
    function pct(v, total) { return total ? (v / total * 100).toFixed(1) + '%' : '-'; }
    function plural(n, word) { return n0(n) + ' ' + word + (n === 1 ? '' : 's'); }

    function errorsOf(cp) {
        var rows = [];
        cp.trades.forEach(function (t) {
            if (t.error) { rows.push({ trade: t, error: t.error }); }
        });
        if (isGroup(cp)) {
            sumMembers(cp).forEach(function (kid) {
                kid.trades.forEach(function (t) {
                    if (t.error) { rows.push({ trade: t, error: t.error, member: kid }); }
                });
            });
        }
        return rows;
    }

    function drawnTrades(cp) {
        var rows = cp.trades.map(function (t) { return { trade: t, member: null }; });
        if (isGroup(cp)) {
            sumMembers(cp).forEach(function (kid) {
                kid.trades.forEach(function (t) { rows.push({ trade: t, member: kid }); });
            });
        }
        return rows;
    }

    function nettingSetsOf(cp) {
        var sets = [];
        drawnTrades(cp).forEach(function (row) {
            var name = row.trade.ns;
            var found = sets.filter(function (s) { return s.name === name; })[0];
            if (!found) { sets.push({ name: name, drawn: 0 }); found = sets[sets.length - 1]; }
            found.drawn += 1;
        });
        return sets;
    }

    /* ------------------------------------------------------------ markup */

    function esc(s) {
        return String(s).replace(/[&<>"]/g, function (c) {
            return ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c];
        });
    }

    function chrome() {
        return '' +
            '<div class="shell">' +
            '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav">' +
            '<a href="#">Home</a><a href="#">Trading</a><a href="#">Reference data</a>' +
            '<a class="here" href="#">Reporting</a><a href="#">Operations</a>' +
            '</nav>' +
            '</div></header>' +
            '<main class="main" id="main"></main>' +
            '</div>';
    }

    function runHead() {
        var t = runTotals();
        return '' +
            '<div class="crumbs"><span>Reporting</span><span>/</span><span>' + esc(RUN.party.code) + '</span>' +
            '<span>/</span><span>Runs</span><span>/</span><span>' + esc(RUN.id.slice(0, 8)) + '</span>' +
            '<span>/</span><span>Results by counterparty</span></div>' +
            '<section class="card runhead">' +
            '<div class="top"><div>' +
            '<h1>' + esc(RUN.definition) + '</h1>' +
            '<p class="lede">The same run read by counterparty rather than by book: who it faces, ' +
            'what each name is worth to it, and which name carries the concentration. ' +
            'Every figure below belongs to this run and to no other.</p>' +
            '</div></div>' +
            '<div class="metaline">' +
            '<span>Party <b>' + esc(RUN.party.code) + '</b></span>' +
            '<span>Run <b>' + esc(RUN.id) + '</b></span>' +
            '<span>Kind <b>' + esc(RUN.kind) + '</b></span>' +
            '<span>State <b>' + esc(RUN.state) + '</b></span>' +
            '<span>Started <b>' + esc(RUN.startedAt) + '</b></span>' +
            '<span>Completed <b>' + esc(RUN.completedAt) + '</b></span>' +
            '<span>Took <b>' + esc(RUN.duration) + '</b></span>' +
            '</div>' +
            '<div class="analytics-line">' +
            '<span>This run\u2019s report configuration runs</span>' +
            '<span class="ana-chip">npv</span>' +
            (hasSaccr() ? '<span class="ana-chip">saccr</span>' : '') +
            '<span>' + (hasSaccr()
                ? 'so every counterparty states an exposure beside its NPV, its notional and its trade count.'
                : 'and not saccr, so every counterparty states an NPV, a notional and a trade count, and no exposure exists for it.') +
            '</span>' +
            '<span>\u00b7</span>' +
            '<span>' + plural(t.count, 'trade') + ' in ' + plural(topLevel().length, 'counterparty') +
            ' \u00b7 ' + money(t.npv) + ' USD of NPV</span>' +
            '</div>' +
            '</section>';
    }

    function fixtureNotice() {
        return '' +
            '<div class="fixture-note"><b>Fixture.</b> Every counterparty, trade, netting set, error and figure ' +
            'on this page is drawn from the file, not read from a server. No operation returns results ' +
            'grouped by counterparty, and nothing parses the run\u2019s archive. ' +
            '<span class="mono">npv.csv</span> names the counterparty as a name and not by id, so a built screen ' +
            'joins each <span class="mono">TradeId</span> to the trade\u2019s ' +
            '<span class="mono">counterparty_id</span>.</div>';
    }

    /* ---------------------------------------------------------- variant A */

    function rowCells(cp, depth) {
        var a = aggregate(cp);
        var t = runTotals();
        var cells = '' +
            '<td class="code">' + esc(cp.code) + '</td>' +
            '<td class="fullname">' +
            (depth ? '<span class="indent">\u2937</span>' : '') + esc(cp.name) +
            (isGroup(cp) ? ' <span class="tag accent">group</span>' : '') +
            '</td>' +
            '<td class="r num">' + money(a.npv) + '</td>' +
            '<td class="r num">' + money(a.notional) + '</td>' +
            '<td class="r num">' + money(a.count) + '</td>';
        if (hasSaccr()) {
            cells += '<td class="r num">' + money(a.exposure) + '</td>';
        }
        cells += '<td class="r share num">' + pct(a.npv, t.npv) + '</td>';
        return cells;
    }

    function variantA() {
        var t = runTotals();
        var head = '<tr><th>Code</th><th>Counterparty</th><th class="r">Agg NPV (USD)</th>' +
            '<th class="r">Notional (USD)</th><th class="r">Trades</th>' +
            (hasSaccr() ? '<th class="r">Exposure (USD)</th>' : '') +
            '<th class="r">Share of run</th></tr>';

        var rows = '';
        topLevel().forEach(function (cp) {
            rows += '<tr class="row group' + (selected.id === cp.id ? ' sel' : '') + '" data-select="' + cp.id + '">' +
                rowCells(cp, 0) + '</tr>';
            if (isGroup(cp)) {
                childrenOf(cp.id)
                    .sort(function (x, y) { return rankMetric(y) - rankMetric(x); })
                    .forEach(function (kid) {
                        rows += '<tr class="row member' + (selected.id === kid.id ? ' sel' : '') +
                            '" data-select="' + kid.id + '">' + rowCells(kid, 1) + '</tr>';
                    });
            }
        });

        var totalRow = '<tr class="total"><td></td><td>Counterparties, total</td>' +
            '<td class="r num">' + money(t.cpNpv) + '</td>' +
            '<td class="r num">' + money(t.cpNotional) + '</td>' +
            '<td class="r num">' + money(t.cpTrades) + '</td>' +
            (hasSaccr() ? '<td class="r num">' + money(t.cpExposure) + '</td>' : '') +
            '<td class="r num">' + pct(t.cpNpv, t.npv) + '</td></tr>';

        var zeroRows = '';
        noResultCPs().forEach(function (cp) {
            zeroRows += '<tr class="row zero' + (selected.id === cp.id ? ' sel' : '') + '" data-select="' + cp.id + '">' +
                '<td class="code">' + esc(cp.code) + '</td>' +
                '<td class="fullname">' + esc(cp.name) + '</td>' +
                '<td class="r">no result</td><td class="r">-</td><td class="r num">0</td>' +
                (hasSaccr() ? '<td class="r">-</td>' : '') +
                '<td class="r">-</td></tr>';
        });

        var orphanRow = '<tr class="row orphan' + (selected.id === ORPHAN.id ? ' sel' : '') +
            '" data-select="' + ORPHAN.id + '">' +
            '<td class="code">' + esc(ORPHAN.code) + '</td>' +
            '<td class="fullname">No counterparty <span class="tag warn">counterparty_scope = intra_entity</span></td>' +
            '<td class="r num">' + money(ORPHAN.pop.npv) + '</td>' +
            '<td class="r num">' + money(ORPHAN.pop.notional) + '</td>' +
            '<td class="r num">' + money(ORPHAN.pop.count) + '</td>' +
            (hasSaccr() ? '<td class="r">-</td>' : '') +
            '<td class="r num">' + pct(ORPHAN.pop.npv, t.npv) + '</td></tr>';

        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>Counterparties this run is exposed to</h2>' +
            '<span class="meta">largest first, by ' + (hasSaccr() ? 'exposure' : 'aggregate NPV') +
            ' \u00b7 a group row carries the sum of its members \u00b7 ' +
            '<span class="tag warn">every figure is a fixture</span></span></div>' +
            fixtureNotice() +
            '<div class="table-wrap" style="margin-top:12px"><table class="grid ranklist">' +
            '<thead>' + head + '</thead><tbody>' + rows +
            zeroRows + orphanRow + totalRow +
            '</tbody></table></div>' +
            '<div class="rowfoot">' +
            '<span><b>Ordering.</b> Largest first, by exposure where the report runs ' +
            '<span class="mono">saccr</span> and by aggregate NPV where it does not. ' +
            'A group sits above its members; a member keeps its own figure because the desk faces the member ' +
            'and credit faces the group.</span>' +
            '<span><b>Share of run.</b> Of ' + money(t.npv) + ' USD including the trades no counterparty names, ' +
            'so the counterparty rows add up to less than 100% and the line below them says why.</span>' +
            '<span><b>Two stated cases.</b> ' + esc(noResultCPs().map(function (c) { return c.name; }).join(', ')) +
            ' is a counterparty the party can see with no result in this run: ' + plural(0, 'trade') +
            ', no figures, and not a measure of zero. The ' + plural(ORPHAN.pop.count, 'trade') +
            ' whose <span class="mono">counterparty_scope</span> is <span class="mono">intra_entity</span> ' +
            'carry no <span class="mono">counterparty_id</span>, so they sit outside every name and the run\u2019s ' +
            'counterparty totals are smaller than the run\u2019s total by ' + money(ORPHAN.pop.npv) + ' USD.</span>' +
            '</div></section>';
    }

    /* ---------------------------------------------------------- variant B */

    function treeNode(cp, depth) {
        var a = aggregate(cp);
        var kids = isGroup(cp) ? childrenOf(cp.id).sort(function (x, y) { return rankMetric(y) - rankMetric(x); }) : [];
        var collapsed = state.collapsed[cp.id];
        var agg = '' +
            '<div class="agg">' +
            '<div><span class="lbl">Agg NPV</span><span class="v">' + money(a.npv) + '</span></div>' +
            (hasSaccr() ? '<div><span class="lbl">Exposure</span><span class="v">' + money(a.exposure) + '</span></div>' : '') +
            '<div><span class="lbl">Notional</span><span class="v">' + money(a.notional) + '</span></div>' +
            '<div><span class="lbl">Trades</span><span class="v">' + money(a.count) + '</span></div>' +
            '</div>';

        var head = '<div class="nodehead" data-select="' + cp.id + '"' +
            (kids.length ? ' data-twisty="' + cp.id + '"' : '') + '>' +
            '<span class="twisty">' + (kids.length ? (collapsed ? '\u25b8' : '\u25be') : '\u00b7') + '</span>' +
            '<span class="who"><span class="nm">' + esc(cp.name) +
            (kids.length ? ' <span class="tag accent">group</span>' : '') + '</span>' +
            '<span class="cd">' + esc(cp.code) + ' \u00b7 ' + pct(a.npv, runTotals().npv) + ' of the run</span></span>' +
            agg + '</div>';

        var inner = '';
        if (kids.length && !collapsed) {
            inner = '<div class="kids">' + kids.map(function (kid) { return treeNode(kid, depth + 1); }).join('') +
                (depth === 0
                    ? '<div class="rollnote">This node\u2019s aggregate is the sum of the nodes below it plus the ' +
                      'trades booked to it directly. Every figure is a fixture.</div>'
                    : '') + '</div>';
        }

        return '<div class="node' + (kids.length ? ' groupnode' : '') +
            (selected.id === cp.id ? ' sel' : '') + '">' + head + inner + '</div>';
    }

    function variantB() {
        var t = runTotals();
        var orphanNode = '<div class="node' + (selected.id === ORPHAN.id ? ' sel' : '') + '">' +
            '<div class="nodehead" data-select="' + ORPHAN.id + '">' +
            '<span class="twisty">\u00b7</span>' +
            '<span class="who"><span class="nm">No counterparty <span class="tag warn">intra_entity</span></span>' +
            '<span class="cd">-- \u00b7 ' + pct(ORPHAN.pop.npv, t.npv) + ' of the run</span></span>' +
            '<div class="agg">' +
            '<div><span class="lbl">Agg NPV</span><span class="v">' + money(ORPHAN.pop.npv) + '</span></div>' +
            '<div><span class="lbl">Notional</span><span class="v">' + money(ORPHAN.pop.notional) + '</span></div>' +
            '<div><span class="lbl">Trades</span><span class="v">' + money(ORPHAN.pop.count) + '</span></div>' +
            '</div></div></div>';

        var zeroNode = noResultCPs().map(function (cp) {
            return '<div class="node' + (selected.id === cp.id ? ' sel' : '') + '">' +
                '<div class="nodehead" data-select="' + cp.id + '">' +
                '<span class="twisty">\u00b7</span>' +
                '<span class="who"><span class="nm">' + esc(cp.name) + ' <span class="tag muted">no result</span></span>' +
                '<span class="cd">' + esc(cp.code) + ' \u00b7 visible to the party, absent from this run</span></span>' +
                '<div class="agg">' +
                '<div><span class="lbl">Agg NPV</span><span class="v">-</span></div>' +
                (hasSaccr() ? '<div><span class="lbl">Exposure</span><span class="v">-</span></div>' : '') +
                '<div><span class="lbl">Notional</span><span class="v">-</span></div>' +
                '<div><span class="lbl">Trades</span><span class="v">0</span></div>' +
                '</div></div></div>';
        }).join('');

        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>The run as a group tree</h2>' +
            '<span class="meta">a parent counterparty above its members \u00b7 every node carries its ' +
            'rolled-up aggregate \u00b7 <span class="tag warn">every figure is a fixture</span></span></div>' +
            fixtureNotice() +
            '<div class="tree" style="margin-top:12px">' + topLevel().map(function (cp) { return treeNode(cp, 0); }).join('') +
            orphanNode + zeroNode + '</div>' +
            '<div class="rowfoot">' +
            '<span><b>The roll-up is arithmetic.</b> A parent counterparty holds its members through ' +
            '<span class="mono">parent_counterparty_id</span>, and its aggregate is the sum of its members plus ' +
            'the trades booked to the parent itself. The page adds it every time it draws, so a parent that ' +
            'disagrees with its parts cannot appear. ACME FINANCIAL GROUP owns no trades and its figure is ' +
            'exactly its two members; MERIDIAN CAPITAL GROUP carries trades of its own beside them.</span>' +
            '<span><b>The two stated cases.</b> BLUE RIVER PENSION is a counterparty the party can see with no ' +
            'result in this run. The intra-entity trades carry no <span class="mono">counterparty_id</span> ' +
            'by the schema\u2019s own allowance, and they are ' + plural(ORPHAN.pop.count, 'trade') +
            ' worth ' + money(ORPHAN.pop.npv) + ' USD of NPV.</span>' +
            '</div></section>';
    }

    /* ---------------------------------------------------------- variant C */

    function segment(cp, cls) {
        var a = aggregate(cp);
        var t = runTotals();
        var kids = isGroup(cp) ? childrenOf(cp.id).sort(function (x, y) { return rankMetric(y) - rankMetric(x); }) : [];
        var label = '<span class="sl">' + esc(cp.orphan ? '\u2014' : cp.code) + '</span>';
        var share = a.npv / t.npv * 100;
        var figures = share >= 5
            ? '<span class="sv">' + money(a.npv) + ' \u00b7 ' + pct(a.npv, t.npv) + '</span>'
            : '';
        var title = esc(cp.name + ' \u00b7 ' + money(a.npv) + ' USD \u00b7 ' + pct(a.npv, t.npv) + ' of the run');
        if (kids.length) {
            return '<div class="seg group' + (selected.id === cp.id ? ' sel' : '') + '" data-select="' + cp.id +
                '" title="' + title + '" style="flex:' + Math.max(1, a.npv) + ' 1 0; min-width:0">' + label +
                '<div class="subsegs">' + kids.map(function (kid) {
                    return segment(kid, 'sub');
                }).join('') + '</div>' + figures + '</div>';
        }
        return '<div class="seg ' + (cls || '') + (selected.id === cp.id ? ' sel' : '') + '" data-select="' + cp.id +
            '" title="' + title + '" style="flex:' + Math.max(1, a.npv) + ' 1 0; min-width:0">' + label +
            figures + '</div>';
    }

    function legendRow(cp, cls) {
        var a = cp.orphan ? cp.pop : aggregate(cp);
        var t = runTotals();
        var swatch = cp.orphan ? 'background:#3a2d13' : (isGroup(cp) ? 'background:#26313f' : 'background:#233042');
        return '<div class="lrow ' + (cls || '') + (selected.id === cp.id ? ' sel' : '') +
            '" data-select="' + cp.id + '">' +
            '<span class="sw" style="' + swatch + '"></span>' +
            '<span class="cd">' + esc(cp.code) + '</span>' +
            '<span class="nm">' + esc(cp.orphan ? 'No counterparty' : cp.name) +
            (isGroup(cp) ? ' <span class="tag accent">group</span>' : '') +
            (cp.noResult ? ' <span class="tag muted">no result</span>' : '') + '</span>' +
            '<span class="v">' + (cp.noResult ? 'no result' : money(a.npv)) + '</span>' +
            '<span class="v">' + (cp.noResult ? '-' : money(a.notional)) + '</span>' +
            '<span class="v">' + money(a.count) + '</span>' +
            '<span class="v">' + (cp.noResult ? '-' : (hasSaccr() ? money(a.exposure) : 'no saccr')) + '</span>' +
            '<span class="v share">' + (cp.noResult ? '-' : pct(a.npv, t.npv)) + '</span>' +
            '</div>';
    }

    function variantC() {
        var t = runTotals();
        var legendHead = '<div class="lrow head"><span></span><span>Code</span><span>Counterparty</span>' +
            '<span class="v">Agg NPV</span><span class="v">Notional</span><span class="v">Trades</span>' +
            '<span class="v">Exposure</span><span class="v share">Share</span></div>';

        var rows = '';
        topLevel().forEach(function (cp) {
            rows += legendRow(cp, '');
            if (isGroup(cp)) {
                childrenOf(cp.id)
                    .sort(function (x, y) { return rankMetric(y) - rankMetric(x); })
                    .forEach(function (kid) { rows += legendRow(kid, 'sub'); });
            }
        });
        noResultCPs().forEach(function (cp) { rows += legendRow(cp, 'zero'); });
        rows += legendRow(ORPHAN, 'orphan');

        var bar = topLevel().map(function (cp) { return segment(cp, ''); }).join('') +
            segment(ORPHAN, 'orphan');

        return '' +
            '<section class="card">' +
            '<div class="sectionhead"><h2>How concentrated the run is, by counterparty</h2>' +
            '<span class="meta">bar width is NPV \u00b7 a group\u2019s members sit inside it \u00b7 ' +
            '<span class="tag warn">every figure is a fixture</span></span></div>' +
            fixtureNotice() +
            '<div class="mapwrap" style="margin-top:12px">' +
            '<div>' +
            '<div class="stackbar">' + bar + '</div>' +
            '<div class="maplegend">' +
            '<span><span class="sw" style="background:#26313f"></span>group, divided into its members</span>' +
            '<span><span class="sw" style="background:#1d2a38"></span>member</span>' +
            '<span><span class="sw" style="background:#3a2d13"></span>trades naming no counterparty</span>' +
            '<span><span class="sw" style="border:1px dashed #4a5568"></span>counterparty with no result</span>' +
            '</div>' +
            '<div class="legend">' + legendHead + rows + '</div>' +
            '</div>' +
            '<div id="detail-slot"></div>' +
            '</div>' +
            '<div class="rowfoot">' +
            '<span><b>Width, not height.</b> Each segment is as wide as its share of the run\u2019s NPV, so the ' +
            'eye answers "how concentrated" before it reads a number. A group\u2019s members are drawn inside the ' +
            'group, and the amber segment is the population no counterparty names: ' +
            money(ORPHAN.pop.npv) + ' USD, ' + pct(ORPHAN.pop.npv, t.npv) + ' of the run.</span>' +
            '<span><b>Area answers one question, and the legend answers the rest.</b> A name with no result has ' +
            'no area to draw, so BLUE RIVER PENSION is a dashed legend row rather than a zero-width segment. ' +
            (hasSaccr()
                ? 'The bar is sized by NPV while the ranked list and the tree rank by exposure: the same run read ' +
                'by a different measure gives a different order, and neither is wrong.'
                : 'This report does not run <span class="mono">saccr</span>, so NPV is the only measure and every ' +
                'exposure column is absent rather than empty.') +
            '</span>' +
            '</div></section>';
    }

    /* ----------------------------------------------------------- detail */

    function detailPanel(cp) {
        if (!cp) { return '<section class="card"><p class="empty">Select a counterparty.</p></section>'; }

        if (cp.orphan) {
            var rowsO = cp.trades.map(function (t) {
                return '<tr><td class="num">' + esc(t.id.slice(0, 8)) + '\u2026</td><td>' + esc(t.type) + '</td>' +
                    '<td class="r num">' + money(t.notional) + '</td>' +
                    '<td class="r num">' + money(t.npv) + '</td>' +
                    '<td class="mono">' + esc(t.ns) + '</td>' +
                    '<td>' + (t.error ? '<span class="tag ' + (t.error.severity === 'error' ? 'bad' : 'warn') + '">' +
                        esc(t.error.scope) + '</span>' : '-') + '</td></tr>';
            }).join('');
            return '<section class="card detail">' +
                '<div class="dhead"><h2>Trades that name no counterparty</h2>' +
                '<span class="sub">' + plural(cp.pop.count, 'trade') + ' \u00b7 ' + money(cp.pop.npv) + ' USD</span></div>' +
                '<div class="notice warn"><strong>The run holds these trades and no counterparty claims them.</strong> ' +
                '<span class="mono">ores_trading_trades_tbl</span> allows <span class="mono">counterparty_id</span> ' +
                'to be null when <span class="mono">counterparty_scope</span> is <span class="mono">intra_entity</span>, ' +
                'so this is a real state and not a broken row. Their netting sets are named by the booking; ' +
                'no exposure is attributed to them here because a <span class="mono">saccr</span> exposure is ' +
                'reported against a counterparty\u2019s netting set and these trades have none.</div>' +
                '<div class="table-wrap"><table class="grid"><thead><tr>' +
                '<th>Trade</th><th>Type</th><th class="r">Notional (USD)</th><th class="r">NPV (USD)</th>' +
                '<th>Netting set</th><th>Error</th></tr></thead><tbody>' + rowsO + '</tbody></table></div>' +
                '<p class="footnote">Two of the ' + plural(cp.pop.count, 'trade') +
                ' are drawn here; the rest are the population the aggregate is summed over.</p>' +
                '</section>';
        }

        if (cp.noResult) {
            return '<section class="card detail">' +
                '<div class="dhead"><h2>' + esc(cp.name) + '</h2><span class="sub">' + esc(cp.code) + '</span></div>' +
                '<div class="notice"><strong>No result in this run.</strong> The party can see this counterparty ' +
                'through <span class="mono">refdata.v1.party_counterparties.list_by_party_id</span>, and this run ' +
                'produced no row for it: no trades, no NPV, no notional, no exposure. It is listed so that a reader ' +
                'who expected the name finds it, and it is not drawn as a measure of zero. ' +
                'A zero trade count is a fact about the run; a zero NPV would be a figure it never produced.</div>' +
                '</section>';
        }

        var a = aggregate(cp);
        var t = runTotals();
        var drawn = drawnTrades(cp);
        var errs = errorsOf(cp);
        var sets = nettingSetsOf(cp);

        var tradeRows = drawn.map(function (row) {
            var tr = row.trade;
            return '<tr><td class="num">' + esc(tr.id.slice(0, 8)) + '\u2026</td>' +
                (isGroup(cp) ? '<td class="code">' + esc(row.member ? row.member.code : cp.code) + '</td>' : '') +
                '<td>' + esc(tr.type) + '</td>' +
                '<td class="r num">' + money(tr.notional) + '</td>' +
                '<td class="r num">' + money(tr.npv) + '</td>' +
                '<td class="mono">' + esc(tr.ns) + '</td>' +
                '<td>' + (tr.error ? '<span class="tag ' + (tr.error.severity === 'error' ? 'bad' : 'warn') + '">' +
                    esc(tr.error.scope) + '</span>' : '-') + '</td></tr>';
        }).join('');

        var errRows = errs.map(function (row) {
            return '<div class="errrow"><span class="tag ' + (row.error.severity === 'error' ? 'bad' : 'warn') + '">' +
                esc(row.error.severity) + '</span>' +
                '<span class="tag muted">' + esc(row.error.scope) + '</span>' +
                '<span class="msg">' + esc(row.error.message) +
                '<span class="trade"> \u00b7 ' + esc(row.trade.id.slice(0, 8)) + '\u2026' +
                (row.member ? ' \u00b7 ' + esc(row.member.code) : '') + '</span></span></div>';
        }).join('');

        return '<section class="card detail">' +
            '<div class="dhead"><h2>' + esc(cp.name) + '</h2>' +
            '<span class="sub">' + esc(cp.code) + ' \u00b7 ' +
            (isGroup(cp) ? 'group, ' + plural(childrenOf(cp.id).length, 'member') + ' \u00b7 ' : '') +
            pct(a.npv, t.npv) + ' of the run</span>' +
            (isGroup(cp) ? '<span class="tag accent">group total</span>' : '') + '</div>' +

            '<div class="statgrid">' +
            '<div><span class="lbl">Aggregate NPV (USD)</span><span class="val">' + money(a.npv) +
            ' <span class="fx">fixture</span></span></div>' +
            (hasSaccr() ? '<div><span class="lbl">Exposure (USD)</span><span class="val">' + money(a.exposure) +
                ' <span class="fx">fixture</span></span></div>' : '') +
            '<div><span class="lbl">Notional (USD)</span><span class="val">' + money(a.notional) + '</span></div>' +
            '<div><span class="lbl">Trades</span><span class="val">' + money(a.count) + '</span></div>' +
            '</div>' +

            (isGroup(cp)
                ? '<div class="notice">This group\u2019s figure is the sum of its members plus the trades booked to ' +
                'the group itself: ' + childrenOf(cp.id).map(function (kid) {
                    return esc(kid.code) + ' ' + money(aggregate(kid).npv);
                }).join(', ') + ', and ' + money(cp.pop.npv) + ' booked to ' + esc(cp.code) +
                ' directly. The trades below are drawn from the group and from every member under it.</div>'
                : '') +

            '<div><div class="sectionhead"><h3>The trades behind the number</h3>' +
            '<span class="meta">' + drawn.length + ' of ' + plural(a.count, 'trade') + ' drawn</span></div>' +
            '<div class="table-wrap"><table class="grid"><thead><tr>' +
            '<th>Trade</th>' + (isGroup(cp) ? '<th>Member</th>' : '') +
            '<th>Type</th><th class="r">Notional (USD)</th><th class="r">NPV (USD)</th>' +
            '<th>Netting set</th><th>Error</th></tr></thead><tbody>' + tradeRows + '</tbody></table></div></div>' +

            '<div><div class="sectionhead"><h3>The netting sets they sit in</h3>' +
            '<span class="meta">from each trade\u2019s booking</span></div>' +
            '<div class="maplegend" style="margin-top:0">' + sets.map(function (s) {
                return '<span class="tag">' + esc(s.name) + ' \u00b7 ' + s.drawn + ' drawn</span>';
            }).join('') + '</div>' +
            '<p class="footnote" style="margin-top:8px">A trade\u2019s booking carries ' +
            '<span class="mono">trade_id</span> and <span class="mono">netting_set_id</span>, so the sets come from ' +
            '<span class="mono">trading.v1.trade_bookings.list</span>; the run\u2019s own result row names the set in ' +
            '<span class="mono">NettingSet</span>. ' + (hasSaccr()
                ? 'A <span class="mono">saccr</span> exposure is reported by netting set, and this page carries only ' +
                'the counterparty\u2019s total.'
                : 'This run does not run <span class="mono">saccr</span>, so there is no exposure by set.') +
            '</p></div>' +

            '<div><div class="sectionhead"><h3>Errors recorded against these trades</h3>' +
            '<span class="meta">' + (errs.length ? plural(errs.length, 'error') + ' in the trades drawn' : 'none') +
            '</span></div>' +
            (errs.length ? '<div class="errlist">' + errRows + '</div>'
                : '<p class="empty">The run recorded no error against the trades drawn here.</p>') +
            '<p class="footnote" style="margin-top:8px">The error scopes are exactly ' +
            '<span class="mono">Trade</span>, <span class="mono">Analytics</span>, <span class="mono">Curve</span>, ' +
            '<span class="mono">Configuration</span> and <span class="mono">Report</span>. None of them is a ' +
            'counterparty, so this list is reached through the trades above and never directly. ' +
            'This run records ' + plural(RUN.errorRows, 'error') + ' in all; ' + RUN.unattributedErrorRows +
            ' of them are recorded against the analysis and name no trade, so they are not attributed to any ' +
            'counterparty here.</p></div>' +
            '</section>';
    }

    /* ------------------------------------------------------------ gaps */

    function gapsPanel() {
        return '<section class="card gaps">' +
            '<div class="sectionhead"><h2>What this page cannot show, and why</h2></div>' +
            '<dl>' +
            '<dt>Nothing parses the archive.</dt>' +
            '<dd>The run\u2019s result is <span class="mono">npv.csv</span> inside ' +
            '<span class="mono">reporting/runs/&lt;instance-id&gt;/output.tar.gz</span>. Its columns are exactly ' +
            '<span class="mono">TradeId, TradeType, Maturity, MaturityTime, NPV, NpvCurrency, NPV(Base), ' +
            'BaseCurrency, Notional, NotionalCurrency, Notional(Base), NettingSet, CounterParty</span>. No ' +
            'operation reads it, so no figure on this page has a read behind it.</dd>' +
            '<dt>The counterparty is named, not identified.</dt>' +
            '<dd><span class="mono">npv.csv</span>\u2019s <span class="mono">CounterParty</span> column carries a ' +
            'name. Attributing a result row to a counterparty record needs the trade\u2019s ' +
            '<span class="mono">counterparty_id</span> and the netting set on its booking, or a name match, and a ' +
            'name match is not safe: full names are not unique in the tenant and the file spells them the ' +
            'engine\u2019s way.</dd>' +
            '<dt>No operation returns results grouped by counterparty.</dt>' +
            '<dd>Every aggregate, every share and every roll-up here is arithmetic this page does from its own ' +
            'fixture, and the screen a real build needs must do the same arithmetic from a parsed result.</dd>' +
            '<dt>The error model has no counterparty subject.</dt>' +
            '<dd>Its scopes are <span class="mono">Trade</span>, <span class="mono">Analytics</span>, ' +
            '<span class="mono">Curve</span>, <span class="mono">Configuration</span> and ' +
            '<span class="mono">Report</span>. A counterparty\u2019s errors are reached only through the trades that ' +
            'name it.</dd>' +
            '<dt>No BFF route serves reporting.</dt>' +
            '<dd>Nothing in the web application shows a report today, so this screen is a new route and a new ' +
            'read as well as a new page.</dd>' +
            '</dl></section>';
    }

    /* ------------------------------------------------------------- render */

    function bar() {
        var variantButtons = ['A', 'B', 'C'].map(function (v) {
            return '<button data-variant="' + v + '"' + (state.variant === v ? ' class="on"' : '') + '>' +
                v + ' \u00b7 ' + VARIANTS[v] + '</button>';
        }).join('');
        var anaButtons =
            '<button data-analytics="npv-saccr"' + (hasSaccr() ? ' class="on"' : '') + '>npv, saccr</button>' +
            '<button data-analytics="npv"' + (!hasSaccr() ? ' class="on"' : '') + '>npv only</button>';
        return '' +
            '<div class="bar-line">' +
            '<span class="tag-prototype">prototype \u00b7 fixture</span>' +
            '<span class="sep">|</span><span class="label">Variant</span>' + variantButtons +
            '<span class="sep">|</span><span class="label">Report runs</span>' + anaButtons +
            '<span class="gist">Which counterparties this run is exposed to, and how much of the run is each one.</span>' +
            '</div>' +
            '<div class="bar-state">' +
            'variant <b>' + state.variant + ' \u00b7 ' + VARIANTS[state.variant] + '</b>' +
            '<span class="dot">|</span>runs <b>' + (hasSaccr() ? 'npv, saccr' : 'npv') + '</b>' +
            '<span class="dot">|</span>selected <b>' + esc(selected.name) + ' (' + esc(selected.code) + ')</b>' +
            '<span class="dot">|</span>run <b>' + esc(RUN.id.slice(0, 8)) + '</b>' +
            '<span class="dot">|</span>' + (hasSaccr() ? 'exposure shown' : 'no exposure on this report') +
            '</div>';
    }

    function render() {
        document.getElementById('proto-note').textContent = 'Prototype. Throwaway. Mock data, no backend, no build step. Every figure is a fixture.';
        var app = document.getElementById('app');
        app.innerHTML = chrome();
        var main = document.getElementById('main');
        var body = state.variant === 'A' ? variantA() : state.variant === 'B' ? variantB() : variantC();
        main.innerHTML = runHead() + body;

        if (state.variant === 'C') {
            var slot = document.getElementById('detail-slot');
            if (slot) { slot.innerHTML = detailPanel(selected); }
        } else {
            main.insertAdjacentHTML('beforeend', detailPanel(selected));
        }
        main.insertAdjacentHTML('beforeend', gapsPanel());
        document.getElementById('proto-bar').innerHTML = bar();
    }

    document.addEventListener('click', function (ev) {
        var target = ev.target.closest('[data-variant], [data-analytics], [data-select], [data-twisty]');
        if (!target) { return; }

        if (target.hasAttribute('data-variant')) {
            state.variant = target.getAttribute('data-variant');
            render();
            return;
        }
        if (target.hasAttribute('data-analytics')) {
            state.analytics = target.getAttribute('data-analytics');
            aggMemo = {};
            render();
            return;
        }
        if (target.hasAttribute('data-twisty') && ev.target.classList.contains('twisty')) {
            var id = target.getAttribute('data-twisty');
            state.collapsed[id] = !state.collapsed[id];
            render();
            return;
        }
        if (target.hasAttribute('data-select')) {
            var cp = findSelectable(target.getAttribute('data-select'));
            if (cp) {
                selected = cp;
                state.selected = cp.id;
                render();
            }
        }
    });

    render();
}());

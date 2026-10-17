/* Take data out: the export workspace. Self-contained: plain JavaScript, mock
 * data, no framework, no build step.
 *
 * The argument: the support export and the archival export are one screen. They
 * differ in scope, in what completeness means, and in where the file goes — not
 * in the act, which is "choose a scope, read the parts, check the manifest,
 * take the files away". A promotion bundle is the same act with a target named.
 * The reading side is the import prototype; this is the writing side.
 *
 * Query string:
 *   ?purpose=share|archive|promote
 *   ?state=ready|written|refused|partial
 *   ?instant=one|apart       one read instant, or today's parts read apart
 *   ?actor=reader|ops        any reader, or an operations user
 *   ?sandbox=official|<sandbox id>   the official tree, or a sandbox to export
 *
 * There is no export screen in the product today, and no bundle, manifest,
 * redaction, as-of read or export record.
 */

(function () {
    'use strict';

    var TENANT = 'Acme Corporation';
    var PARTY = 'Acme Trading';

    var ACTORS = {
        reader: { id: 'm.okafor', name: 'M. Okafor', part: 'Any signed-in reader', can: 'reads only what the session may read' },
        ops: { id: 'k.mori', name: 'K. Mori', part: 'Operations user', can: 'reads the party\u2019s data and stores the archive' }
    };

    var PURPOSES = {
        share: {
            title: 'Share with support',
            gist: 'The smallest set that shows the problem, and no row the person cannot read.',
            defaultScope: 'book',
            scopeLabel: 'RATES.EUR / EUR_SWAPS_01',
            instant: 'one',
            archive: false
        },
        archive: {
            title: 'Archive the party',
            gist: 'A complete, dated copy of the party as it stands at one instant, that can be read back later.',
            defaultScope: 'party',
            scopeLabel: 'Acme Trading, every portfolio and report definition',
            instant: 'one',
            archive: true
        },
        promote: {
            title: 'Move to another installation',
            gist: 'The accepted set, in dependency order, with a manifest the target can check before it imports.',
            defaultScope: 'party',
            scopeLabel: 'Acme Trading \u2192 staging',
            instant: 'one',
            archive: false
        }
    };

    /* Sandboxes that may be exported from. A sandbox hangs from an official
     * portfolio and owns a subtree; it is not a node of the official tree.
     * Exporting one is how a defect is reproduced elsewhere, so it sits beside
     * the official scope rather than instead of it. */
    var SANDBOXES = [
        { id: 'sb-import-repro', name: 'Reproduce 2026-10-16', anchor: 'Global Trading \u203a Global Rates \u203a EUR Rates',
            path: 'Global Trading \u203a Global Rates \u203a EUR Rates \u203a Reproduce 2026-10-16',
            purpose: 'import', status: 'open', portfolios: 2, books: 2 },
        { id: 'sb-import-onboard', name: 'Onboard Acme 2026-10-17', anchor: 'Global Trading \u203a Global Rates',
            path: 'Global Trading \u203a Global Rates \u203a Onboard Acme 2026-10-17',
            purpose: 'import', status: 'open', portfolios: 1, books: 1 },
        { id: 'sb-sample-seed', name: 'Seed samples 2026-10', anchor: 'Global Trading',
            path: 'Global Trading \u203a Seed samples 2026-10',
            purpose: 'sample', status: 'open', portfolios: 1, books: 2 },
        { id: 'sb-import-feed', name: 'Vendor feed 2026-10-17', anchor: 'Global Trading \u203a Global Rates \u203a USD Rates',
            path: 'Global Trading \u203a Global Rates \u203a USD Rates \u203a Vendor feed 2026-10-17',
            purpose: 'import', status: 'open', portfolios: 1, books: 1 }
    ];

    var PARTS = [
        { key: 'trades', name: 'Portfolio and trades', op: 'trading.v1.ops.export_portfolio', rows: '47 portfolios \u00b7 1,642 trades', bytes: '12.4 MB', read: '10:31:02', grants: '3 books excluded: rights', include: true },
        { key: 'conventions', name: 'Conventions document', op: 'ore.v1.ops.export_run_configuration', rows: '12 families \u00b7 344 conventions', bytes: '48 KB', read: '10:31:04', include: true },
        { key: 'curves', name: 'Curve configurations', op: 'ore.v1.ops.export_run_configuration', rows: '84 curves', bytes: '1.1 MB', read: '10:31:06', include: true },
        { key: 'market', name: 'Quotes and fixings', op: 'marketdata.v1.ops.export_market_data', rows: '312 series \u00b7 18,442 quotes \u00b7 1,204 fixings', bytes: '2.1 MB', read: '10:31:08', include: true },
        { key: 'run', name: 'Run documents', op: 'ore.v1.ops.export_run_configuration', rows: '5 bindings', bytes: '12 KB', read: '10:31:09', include: true },
        { key: 'job', name: 'Compute job input', op: 'the compute storage download', rows: 'the run\u2019s input bundle', bytes: '2.1 GB', read: '\u2014', include: false, why: 'not party data: the defect journey collects it separately' }
    ];

    var S = readQuery();

    function readQuery() {
        var q = {};
        window.location.search.replace(/^\?/, '').split('&').forEach(function (pair) {
            if (!pair) return;
            var i = pair.indexOf('=');
            var k = decodeURIComponent(i < 0 ? pair : pair.slice(0, i));
            q[k] = decodeURIComponent(i < 0 ? '' : pair.slice(i + 1));
        });
        return {
            purpose: PURPOSES[q.purpose] ? q.purpose : 'share',
            state: ['ready', 'written', 'refused'].indexOf(q.state) >= 0 ? q.state : 'ready',
            instant: q.instant === 'apart' ? 'apart' : 'one',
            actor: q.actor === 'ops' ? 'ops' : 'reader',
            sandbox: q.sandbox === 'official' || SANDBOXES.some(function (s) { return s.id === q.sandbox; }) ? q.sandbox : 'official'
        };
    }

    function go(patch) {
        Object.keys(patch).forEach(function (k) { S[k] = patch[k]; });
        var p = [];
        if (S.purpose !== 'share') p.push('purpose=' + S.purpose);
        if (S.state !== 'ready') p.push('state=' + S.state);
        if (S.instant !== 'one') p.push('instant=' + S.instant);
        if (S.actor !== 'reader') p.push('actor=' + S.actor);
        if (S.sandbox !== 'official') p.push('sandbox=' + S.sandbox);
        window.history.replaceState(null, '', window.location.pathname + (p.length ? '?' + p.join('&') : ''));
        render();
    }

    function esc(v) {
        return String(v === undefined || v === null ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    }
    function pur() { return PURPOSES[S.purpose]; }
    function actor() { return ACTORS[S.actor]; }
    function sandboxSource() {
        return S.sandbox === 'official' ? null : SANDBOXES.filter(function (s) { return s.id === S.sandbox; })[0];
    }
    function scopeLabel() { var sb = sandboxSource(); return sb ? sb.path : pur().scopeLabel; }

    function included() {
        return PARTS.filter(function (p) { return p.include && !(S.purpose === 'share' && p.key === 'run'); });
    }

    /* ------------------------------------------------------------- render */

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype \u00b7 Data exchange \u00b7 Take data out \u00b7 mock data, no backend, no file written';
        document.getElementById('app').innerHTML = '<div class="page wide">' + head() + body() + '</div>';
        document.getElementById('proto-bar').innerHTML = bar();
    }

    function head() {
        return '<div class="brand">' + esc(TENANT) + '</div>' +
            '<div class="tenant">Signed in as <b>' + esc(actor().name) + '</b> \u00b7 ' + esc(actor().part) + ' \u00b7 party <b>' + esc(PARTY) + '</b></div>' +
            '<h1>Take data out</h1>' +
            '<p class="lede">' + esc(pur().gist) + ' The export reads only what the session may read, at one instant, and says what it left out.</p>' +
            gapbanner() + purposeRow() + strip();
    }

    function gapbanner() {
        return '<div class="gapbanner"><b>Gap</b>There is no export screen and no export route. The entry points are ' +
            '<span class="mono">ore export</span>, <span class="mono">ore export-run</span> and <span class="mono">marketdata export</span>. ' +
            'No command writes a bundle, a manifest, a date, a checksum or an omission. The person packs the files by hand and writes the note by hand.</div>';
    }

    function purposeRow() {
        return '<div class="toolbar"><label class="faint">Purpose</label>' +
            Object.keys(PURPOSES).map(function (k) {
                return '<button class="btn small' + (k === S.purpose ? '' : ' ghost') + '" data-act="purpose" data-purpose="' + k + '">' +
                    esc(PURPOSES[k].title) + '</button>';
            }).join('') +
            '<span class="grow"></span><span class="pill ' + (pur().archive ? 'warn' : 'info') + '">' +
            (pur().archive ? 'archive, not a backup' : S.purpose === 'promote' ? 'bundle for a target' : 'smallest readable set') + '</span></div>';
    }

    function strip() {
        var rows = 0;
        included().forEach(function (p) { rows += 1; });
        return '<div class="strip">' +
            '<div class="cell"><div class="k">Scope</div><div class="v" style="font-size:14px">' + esc(scopeLabel()) + '</div><div class="s">' +
            (sandboxSource() ? 'sandbox ' + esc(S.sandbox) + ' \u00b7 party ' + esc(PARTY) : 'the official tree \u00b7 party ' + esc(PARTY)) + '</div></div>' +
            '<div class="cell"><div class="k">Parts</div><div class="v">' + rows + '</div><div class="s">' + (S.instant === 'one' ? 'read at one instant' : 'read apart, as today') + '</div></div>' +
            '<div class="cell"><div class="k">Rows the session cannot read</div><div class="v">0</div><div class="s">3 books excluded and named</div></div>' +
            '<div class="cell"><div class="k">Manifest</div><div class="v" style="font-size:14px">man_01J9Z7\u2026</div><div class="s">' + (S.state === 'written' ? 'written with the bundle' : 'not written yet') + '</div></div>' +
            '</div>';
    }

    function body() {
        if (S.state === 'refused') return refusedState();
        if (S.state === 'written') return writtenState();
        return '<div class="split"><div>' + scopeCard() + partsCard() + '</div><div>' + manifestCard() + completenessCard() + '</div></div>' +
            foot();
    }

    function scopeCard() {
        var p = pur();
        return '<div class="card"><h2>The scope</h2>' +
            '<p class="lead">The export reads what the session may read. A book whose rights exclude the person is refused, not returned empty.</p>' +
            '<div class="grid2">' +
            '<div class="field ro"><span class="lbl">Party</span><input value="' + esc(PARTY) + '" readonly></div>' +
            '<div class="field"><span class="lbl">Export from</span><select data-act="sandbox">' +
            '<option value="official"' + (S.sandbox === 'official' ? ' selected' : '') + '>Official tree \u2014 the party</option>' +
            SANDBOXES.map(function (s) {
                return '<option value="' + esc(s.id) + '"' + (S.sandbox === s.id ? ' selected' : '') + '>Sandbox \u2014 ' +
                    esc(s.name) + ' \u00b7 ' + esc(s.anchor) + '</option>';
            }).join('') + '</select><span class="hint">A sandbox export names its source sandbox and reads its subtree. The official tree is ' +
            'exportable too: unlike an import, an export may read it.</span></div>' +
            '<div class="field"><span class="lbl">What to export</span><select data-act="scope">' +
            ['One book', 'One portfolio', 'One report definition', 'One series', 'The whole party', 'One sandbox']
                .map(function (o) {
                    var sel = sandboxSource() ? o === 'One sandbox'
                        : (p.defaultScope === 'party' && o === 'The whole party') || (p.defaultScope === 'book' && o === 'One book') || (p.defaultScope === 'report' && o === 'One report definition');
                    return '<option' + (sel ? ' selected' : '') + '>' + o + '</option>';
                }).join('') + '</select></div>' +
            '<div class="field span2"><span class="lbl">Node or definition</span><input value="' + esc(scopeLabel()) + '"' +
            (S.purpose === 'share' ? '' : ' readonly') + '></div>' +
            '<div class="field"><span class="lbl">As of</span><input value="2026-10-17 10:31:00Z" readonly></div>' +
            '<div class="field"><span class="lbl">Read at</span><input value="' + (S.instant === 'one' ? 'one snapshot (snapshot_id 8f21\u2026)' : '10:31:02 \u2026 10:38:41, four independent reads') + '" readonly></div>' +
            '</div>' +
            '<div class="notice ' + (p.archive ? 'warn' : 'info') + ' mb0">' +
            (p.archive ? 'An archive is not a backup. The parts carry no audit columns, no row versions, no history and no book rights. ' +
                'A PostgreSQL backup is the only complete copy, and it is taken outside the shell.' :
                S.purpose === 'promote' ? 'A promotion bundle moves through files only. Production never reads development\u2019s database, message bus or object storage.' :
                    'The smallest set that shows the problem. The ticket rarely needs the whole party.') +
            '</div>' +
            (sandboxSource() ? '<div class="notice info mt"><span class="title">This export reads a sandbox, and that is how a defect travels</span>' +
                'The bundle names <b>' + esc(sandboxSource().name) + '</b> and its anchor. The receiving import creates a new sandbox and writes the virtual ' +
                'books there; a sandbox id is fixed for life, so it is never reused. The sandbox\u2019s root portfolios have no parent, and every virtual book ' +
                'in it carries no ledger reference and is not sweepable.</div>' : '') +
            '</div>';
    }

    function plural(count, one, many) { return count + ' ' + (count === 1 ? one : many); }

    function partWhere(p) {
        var sb = sandboxSource();
        if (!sb) return '<span class="pill">official tree</span>';
        if (p.key === 'trades') {
            return '<span class="pill info">sandbox</span> <span class="sub">' + plural(sb.portfolios, 'portfolio', 'portfolios') +
                ' \u00b7 ' + plural(sb.books, 'virtual book', 'virtual books') + '</span>';
        }
        return '<span class="pill">party-wide</span> <span class="sub">no sandbox column</span>';
    }

    function partRows(p) {
        var sb = sandboxSource();
        return sb && p.key === 'trades' ? plural(sb.portfolios, 'portfolio', 'portfolios') + ' \u00b7 ' +
            plural(sb.books, 'virtual book', 'virtual books') : p.rows;
    }

    function partsCard() {
        return '<div class="card mt"><div class="row between"><h2>What goes in</h2>' +
            '<span class="pill info">' + included().length + ' parts</span></div>' +
            '<p class="lead">Every part names the read that produced it, and where it sits. A part left out is named in the manifest, never silently absent.</p>' +
            '<table class="grid"><thead><tr><th></th><th>Part</th><th>Where</th><th>Rows</th><th>Size</th><th>Read at</th></tr></thead><tbody>' +
            PARTS.map(function (p) {
                var on = included().indexOf(p) >= 0;
                return '<tr' + (on ? '' : ' class="off"') + '><td class="chk"><input type="checkbox" data-act="part" data-key="' + p.key + '"' +
                    (on ? ' checked' : '') + (p.key === 'job' ? ' disabled' : '') + '></td>' +
                    '<td class="nm">' + esc(p.name) + '<span class="ent mono" style="font-size:10px">' + esc(p.op) + '</span>' +
                    (p.why ? '<span class="ent">' + esc(p.why) + '</span>' : '') + (p.grants ? '<span class="ent">' + esc(p.grants) + '</span>' : '') + '</td>' +
                    '<td>' + partWhere(p) + '</td>' +
                    '<td class="n">' + esc(partRows(p)) + '</td><td class="n">' + esc(p.bytes) + '</td><td class="mono" style="font-size:11px">' + esc(p.read) + '</td></tr>';
            }).join('') + '</tbody></table>' +
            (sandboxSource() ? '<div class="notice info mt"><span class="title">The two trees cannot mix, and this export proves it</span>' +
                'The sandbox part holds only rows that carry the sandbox id: ' + plural(sandboxSource().portfolios, 'portfolio', 'portfolios') + ' and ' +
                plural(sandboxSource().books, 'virtual book', 'virtual books') + '. It never holds an official book, because a book and its portfolio share one sandbox or both to none. ' +
                'The other parts are party-wide reference data and carry no sandbox column; the export names that rather than pretending otherwise.</div>' : '') +
            '<div class="notice warn mt"><span class="title">3 books are excluded by rights, and the export refuses rather than returns nothing</span>' +
            'RATES.USD / USD_CROSS_01, RATES.USD / USD_CORP_02 and CREDIT / CDS_HY_03 carry rights that exclude this session. ' +
            'The export names them; a support bundle with no rows and no explanation is worse than a refusal.' +
            (S.actor === 'reader' ? ' A reader may take away only what they may read; an operations user sees the same three.' : '') + '</div>' +
            '</div>';
    }

    function manifestCard() {
        return '<div class="card"><div class="row between"><h2>The manifest</h2><span class="pill warn">the person writes this by hand today</span></div>' +
            '<p class="lead">One page that says what each file is, which party and instant it came from, and what was left out.</p>' +
            '<table class="kv">' +
            kv('Manifest', 'man_01J9Z7K2Q4RT (would be minted by the export)') +
            kv('Party', PARTY) +
            kv('Sandbox', sandboxSource() ? sandboxSource().name + ' (' + sandboxSource().id + ')' : 'none \u2014 the official tree') +
            kv('Sandbox anchor', sandboxSource() ? sandboxSource().anchor : 'not applicable') +
            kv('Source installation', 'development \u00b7 ores.build 0.28.4 \u00b7 schema 2026-10-16') +
            kv('As of', '2026-10-17 10:31:00Z, snapshot 8f21\u2026') +
            kv('Parts', included().map(function (p) { return p.name; }).join(', ')) +
            kv('Left out', 'compute job input (not party data); 3 books by rights; ' + (S.purpose === 'share' ? 'run documents (not needed by the ticket)' : 'nothing else')) +
            kv('Consistency', S.instant === 'one' ? 'every part read at the same instant' : 'the parts were read 7 minutes apart; the set never existed together') +
            '</table>' +
            '<div class="proto-hint">Gap: no export writes a manifest, a checksum or an instant. This is the one artefact every export journey says is missing.</div>' +
            '</div>';
    }

    function kv(k, v) {
        return '<tr><td>' + esc(k) + '</td><td>' + (v ? esc(v) : '<span class="none">none</span>') + '</td></tr>';
    }

    function completenessCard() {
        var items = S.purpose === 'archive' ? archiveChecks() : S.purpose === 'promote' ? promoteChecks() : supportChecks();
        return '<div class="card mt"><h2>Can this leave the building?</h2>' +
            '<p class="lead">' + (S.purpose === 'archive' ? 'A dated copy is signed off only when every one of these is true.' : 'The checks the person runs before the file leaves.') + '</p>' +
            '<ul class="checklist">' + items.map(function (c) {
                return '<li><span class="mk ' + (c.ok === true ? 'ok' : c.ok === false ? 'bad' : 'warn') + '">' +
                    (c.ok === true ? '\u2713' : c.ok === false ? '\u2715' : '!') + '</span>' +
                    '<span><b>' + esc(c.nm) + '</b><span class="faint"> \u2014 ' + esc(c.why) + '</span></span></li>';
            }).join('') + '</ul>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="refused">Refuse a read</button>' +
            '<span class="ml-auto"><button class="btn primary" data-act="state" data-state="written">' +
            (S.purpose === 'archive' ? 'Write the archive' : S.purpose === 'promote' ? 'Write the bundle' : 'Write the files') + '</button></span></div>' +
            '</div>';
    }

    function supportChecks() {
        return [
            { ok: true, nm: 'Only rows the session may read', why: 'The read filters by book rights; the three excluded books are named.' },
            { ok: true, nm: 'The smallest set that shows the problem', why: 'One book, its report definition and the quotes it reads.' },
            { ok: true, nm: 'The parts open in the platform\u2019s importer', why: 'ORE-shaped XML and text; the receiving side reads them.' },
            { ok: 'warn', nm: 'A short note names each part', why: 'No command writes it. The person writes the party, node and time by hand.' },
            { ok: false, nm: 'Attached to the ticket by the product', why: 'There is no ticket integration. The person attaches the files by hand.' }
        ];
    }

    function archiveChecks() {
        return [
            { ok: true, nm: 'Dated and self-describing', why: 'The manifest carries the party, the instant, the build and each file\u2019s checksum.' },
            { ok: S.instant === 'one', nm: 'Read at one instant', why: S.instant === 'one' ? 'Every part reads the same snapshot.' : 'No export takes an as_of. The parts were read minutes apart and the set never existed together.' },
            { ok: false, nm: 'Carries the audit trail', why: 'No audit columns, row versions or history are in any ORE-shaped file.' },
            { ok: false, nm: 'Carries book rights', why: 'Rights are not exported, so an import cannot reconstruct who may see what.' },
            { ok: false, nm: 'A complete copy', why: 'It is not a database backup. A PostgreSQL backup is the only complete copy, and it is taken outside the shell.' },
            { ok: 'warn', nm: 'Names what was not exported', why: 'The manifest lists the omissions only because a person typed them.' },
            { ok: true, nm: 'Reads back into a scratch environment', why: 'The receiving import reproduces the trades, the conventions and the market data.' }
        ];
    }

    function promoteChecks() {
        return [
            { ok: true, nm: 'Target environment named', why: 'staging, with the source development and the hop time in the manifest.' },
            { ok: true, nm: 'Dependency order written down', why: 'Currencies, portfolio, conventions, curves, market, run documents.' },
            { ok: S.instant === 'one', nm: 'The parts existed together', why: 'A change between two exports produces a set that never existed.' },
            { ok: false, nm: 'A promotion record at the target', why: 'Nothing stores which environment received what, when, or by whom. The ticket is the record.' },
            { ok: false, nm: 'An ordering guard', why: 'The person must know that conventions precede curves. A wrong order fails late.' }
        ];
    }

    function foot() {
        return '<div class="stepfoot"><span class="hint">The receiving side is the import prototype: the same bundle is its source. ' +
            'Nothing is written until the person takes it away.</span>' +
            '<span class="ml-auto"><a class="btn ghost" href="../bring-data-in/index.html?case=' +
            (S.purpose === 'promote' ? 'promote' : 'defect') + (sandboxSource() ? '&amp;target=' + esc(S.sandbox) : '') +
            '">Open the receiving screen</a> ' +
            '<button class="btn primary" data-act="state" data-state="written">Take it away</button></span></div>';
    }

    function writtenState() {
        return '<div class="card"><div class="outcome"><div class="mark ok">\u2713</div><h2>' +
            (sandboxSource() ? 'Sandbox bundle written' : S.purpose === 'archive' ? 'Archive written' : S.purpose === 'promote' ? 'Bundle written' : 'Files written') + '</h2>' +
            '<p>' + included().length + ' parts, ' + (S.instant === 'one' ? 'read at one instant, ' : 'read apart, ') +
            'with a manifest, written to the person\u2019s own file system. ' +
            (sandboxSource() ? 'The bundle names sandbox ' + esc(sandboxSource().name) + ' and its anchor; the receiving import creates a new sandbox. ' : '') +
            'The platform keeps no export record.</p></div>' +
            '<table class="grid mt"><thead><tr><th>File</th><th>Rows</th><th>Size</th><th>sha256</th></tr></thead><tbody>' +
            included().map(function (p, i) {
                return '<tr><td class="mono">' + esc(fileName(p.key)) + '</td><td class="n">' + esc(partRows(p)) + '</td><td class="n">' + esc(p.bytes) +
                    '</td><td class="mono" style="font-size:11px">' + ('8d41c2' + (i + 1) + 'f9\u2026') + '</td></tr>';
            }).join('') +
            '<tr><td class="mono">manifest.json</td><td class="n">1</td><td class="n">6 KB</td><td class="mono" style="font-size:11px">4b71e0\u2026</td></tr>' +
            '</tbody></table>' +
            '<div class="notice warn mt"><span class="title">The record the product does not keep</span>' +
            'Nothing lists what the person exported and when. For an archive, the retention register is the only record. ' +
            'For a support ticket, the attachment is. For a promotion, the ticket and the target\u2019s import register are.' +
            '</div>' +
            (sandboxSource() ? '<div class="notice info mt"><span class="title">A sandbox id is fixed for its life</span>' +
                'The bundle names the source sandbox, not a copy of its id. The receiving import opens a new sandbox at its own anchor and writes the ' +
                'portfolios and virtual books there, so two installments never share one sandbox id. The official tree is untouched on both sides.</div>' : '') +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="ready">Back to the checks</button>' +
            '<span class="ml-auto"><a class="btn primary" href="../import-register/index.html?variant=' +
            (S.purpose === 'promote' ? 'promotion' : 'list') + '">Open the register</a></span></div></div>';
    }

    function fileName(key) {
        return { trades: 'portfolio.xml', conventions: 'conventions.xml', curves: 'curveconfig.xml',
            market: 'market.txt + fixings.txt', run: 'ore.xml', job: 'run_4f2c-input.tar.gz' }[key];
    }

    function refusedState() {
        return '<div class="card"><div class="outcome"><div class="mark bad">\u2715</div><h2>The read is refused</h2>' +
            '<p>RATES.USD / USD_CROSS_01 carries rights that exclude this session. The export refuses and names the book; it does not return an empty file.</p></div>' +
            '<div class="notice error mt"><span class="title">A refusal is not an empty success</span>' +
            'An export must never include a row the person cannot read, and it must never present an incomplete set as complete. ' +
            'The person can still attach this failure message to the ticket, which is the honest thing to send.</div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="ready">Back to the scope</button>' +
            '<span class="ml-auto"><button class="btn primary" data-act="state" data-state="written">Export only what I may read</button></span></div></div>';
    }

    /* ---------------------------------------------------------------- bar */

    function bar() {
        var out = '<span class="label">Purpose</span>';
        Object.keys(PURPOSES).forEach(function (k) {
            out += '<button data-act="purpose" data-purpose="' + k + '"' + (k === S.purpose ? ' class="on"' : '') + '>' + esc(PURPOSES[k].title) + '</button>';
        });
        out += '<span class="sep">|</span><span class="label">Actor</span>';
        out += '<button data-act="actor" data-actor="reader"' + (S.actor === 'reader' ? ' class="on"' : '') + '>Any reader</button>';
        out += '<button data-act="actor" data-actor="ops"' + (S.actor === 'ops' ? ' class="on"' : '') + '>Operations user</button>';
        out += '<span class="sep">|</span>';
        out += '<button data-act="instant" data-instant="one"' + (S.instant === 'one' ? ' class="on"' : '') + '>One instant</button>';
        out += '<button data-act="instant" data-instant="apart"' + (S.instant === 'apart' ? ' class="on"' : '') + '>Parts apart (today)</button>';
        out += '<span class="sep">|</span>';
        out += '<button data-act="state" data-state="ready"' + (S.state === 'ready' ? ' class="on"' : '') + '>Ready</button>';
        out += '<button data-act="state" data-state="written"' + (S.state === 'written' ? ' class="on"' : '') + '>Written</button>';
        return out;
    }

    /* -------------------------------------------------------------- events */

    document.addEventListener('change', function (e) {
        var t = e.target;
        if (!t.dataset) return;
        if (t.dataset.act === 'part') {
            var p = PARTS.filter(function (x) { return x.key === t.dataset.key; })[0];
            if (p) p.include = t.checked;
            render();
        }
        if (t.dataset.act === 'scope') render();
        if (t.dataset.act === 'sandbox') go({ sandbox: t.value, state: 'ready' });
    });

    document.addEventListener('click', function (e) {
        var t = e.target.closest('[data-act]');
        if (!t || t.disabled) return;
        var act = t.dataset.act;
        if (act === 'purpose') go({ purpose: t.dataset.purpose, state: 'ready' });
        else if (act === 'actor') go({ actor: t.dataset.actor });
        else if (act === 'instant') go({ instant: t.dataset.instant });
        else if (act === 'state') go({ state: t.dataset.state });
    });

    render();
}());

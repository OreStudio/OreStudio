/*
 * The curve configuration record screen in the pricing engines products shape:
 * one Curves tab, a searchable rail of curves grouped by their section, and the
 * chosen curve's parameters in a workbench beside it.
 *
 * The question: a document declares nineteen sections and populates between one
 * and sixteen of them, so one tab per section neither fits a screen nor earns
 * the space. Fake data: the sections and their order are curveconfig.xsd's; the
 * counts are fixtures for a one-, four- and sixteen-section document; the leaves
 * are the yield curve's, with one undocumented leaf so the gap shows.
 */

const SECTIONS = [
    ['FXSpots', 'FXSpot', 'FX spot rates, each named by its currency pair.'],
    ['FXVolatilities', 'FXVolatility', 'FX option volatility surfaces.'],
    ['SwaptionVolatilities', 'SwaptionVolatility', 'Interest rate swaption volatility surfaces.'],
    ['YieldVolatilities', 'YieldVolatility', 'Bond yield volatility surfaces.'],
    ['CapFloorVolatilities', 'CapFloorVolatility', 'Interest rate cap and floor volatility surfaces.'],
    ['CDSVolatilities', 'CDSVolatility', 'Credit default swap option volatility surfaces.'],
    ['DefaultCurves', 'DefaultCurve', 'Credit default probability curves.'],
    ['YieldCurves', 'YieldCurve', 'Interest rate curves, each bootstrapped from a list of quotes.'],
    ['InflationCurves', 'InflationCurve', 'Zero coupon and year on year inflation curves.'],
    ['InflationCapFloorVolatilities', 'InflationCapFloorVolatility', 'Inflation cap and floor volatility surfaces.'],
    ['EquityCurves', 'EquityCurve', 'Equity forward curves and dividend yields.'],
    ['EquityVolatilities', 'EquityVolatility', 'Equity option volatility surfaces.'],
    ['Securities', 'Security', 'Bond securities with their spread, recovery and price quotes.'],
    ['BaseCorrelations', 'BaseCorrelation', 'Base correlation curves for credit index tranches.'],
    ['CommodityCurves', 'CommodityCurve', 'Commodity price curves.'],
    ['CommodityVolatilities', 'CommodityVolatility', 'Commodity option volatility surfaces.'],
    ['Correlations', 'Correlation', 'Correlation curves between two indices.'],
    ['BondFutureVolatilities', 'BondFutureVolatility', 'Bond future option volatility surfaces.'],
    ['IntradayPowerCurves', 'IntradayPowerCurve', 'Intraday power price curves.'],
].map(([code, entry, description]) => ({ code, entry, description }));

const FIXTURES = {
    1: { YieldCurves: 4 },
    4: { YieldCurves: 17, DefaultCurves: 5, FXVolatilities: 3, SwaptionVolatilities: 20 },
    16: {
        YieldCurves: 17, DefaultCurves: 5, FXVolatilities: 3, SwaptionVolatilities: 20,
        CapFloorVolatilities: 6, CDSVolatilities: 2, InflationCurves: 4,
        InflationCapFloorVolatilities: 3, EquityCurves: 8, EquityVolatilities: 5,
        Securities: 12, BaseCorrelations: 2, CommodityCurves: 4, CommodityVolatilities: 3,
        Correlations: 9, BondFutureVolatilities: 2,
    },
};

const NAMES = { 1: 'Group curve configuration', 4: 'UK curve configuration', 16: 'ACME global curve configuration' };
const CURRENCIES = ['GBP', 'USD', 'EUR', 'JPY', 'CHF'];
const SUFFIX = ['1D', '1W', '1M', '3M', '6M', '1Y', '2Y', '5Y', '10Y', '30Y'];

/** The curves of a section, named as ORE's example documents name them. */
function curvesOf(section, count) {
    const kind = ['YieldCurves', 'DefaultCurves', 'InflationCurves'].includes(section)
        ? section === 'DefaultCurves' ? 'credit' : section === 'InflationCurves' ? 'inflation' : 'discount'
        : '';
    return Array.from({ length: count }, (_, index) => {
        const currency = CURRENCIES[index % CURRENCIES.length];
        return kind === ''
            ? { id: `${currency}-${section}-${index + 1}`, description: `One entry of ${section}`, section }
            : { id: `${currency}-${SUFFIX[index % SUFFIX.length]}`, description: `${currency} ${kind} curve`, section };
    });
}

const LEAVES = [
    ['CurveId', 'GBP-10Y', true, 'The name today’s market uses for this curve.'],
    ['CurveDescription', 'GBP discount curve', true, 'A description of the curve, for a reader who does not know ORE.'],
    ['Currency', 'GBP', true, 'The currency the curve is built in.'],
    ['DiscountCurve', 'GBP-10Y', true, 'The curve that discounts the cash flows of the instruments it is built from.'],
    ['InterpolationVariable', 'Discount', true, 'The quantity the interpolation works on.'],
    ['InterpolationMethod', 'LogLinear', true, 'How ORE interpolates between the pillars it bootstraps.'],
    ['MixedInterpolationCutoff', null, false, ''],
    ['YieldCurveDayCounter', 'A365', true, 'The day counter the curve uses for its year fractions.'],
    ['Tolerance', '1e-12', true, 'The accuracy the bootstrap stops at.'],
    ['Extrapolation', 'false', true, 'Whether the curve extrapolates beyond its last pillar.'],
    ['ExtrapolationMethod', null, true, 'How it extrapolates: a continuous or a discrete forward.'],
    ['ExcludeT0FromInterpolation', 'false', true, 'Whether today’s pillar is left out of the interpolation.'],
];

const params = new URLSearchParams(location.search);
const STATE = {
    sections: Number(params.get('sections') ?? '4'),
    tab: params.get('tab') ?? 'curves',
    curve: params.get('curve') ?? '',
    search: '',
    mode: 'simple',
    open: new Set(['YieldCurves']),
};

const fixture = () => FIXTURES[STATE.sections] ?? FIXTURES[4];
const filled = () => SECTIONS.filter((section) => fixture()[section.code] !== undefined);
const blank = () => SECTIONS.filter((section) => fixture()[section.code] === undefined);
const countOf = (code) => fixture()[code] ?? 0;
const total = () => filled().reduce((sum, section) => sum + countOf(section.code), 0);
const everyCurve = () => filled().flatMap((section) => curvesOf(section.code, countOf(section.code)));

const escape = (text) => String(text).replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
const words = (name) => name.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/^./, (c) => c.toUpperCase());

function set(patch) {
    Object.assign(STATE, patch);
    history.replaceState(null, '', `?${new URLSearchParams({ sections: STATE.sections, tab: STATE.tab, curve: STATE.curve })}`);
    render();
}

function workbench(curve) {
    const section = SECTIONS.find((candidate) => candidate.code === curve.section) ?? SECTIONS[7];
    const leaves = LEAVES.map(([name, value, documented, help]) => {
        const shown = value === null
            ? `<span class="faint">${documented ? 'Default: None' : '—'}</span>`
            : ['Extrapolation', 'ExcludeT0FromInterpolation'].includes(name)
              ? `<span>${value === 'false' ? 'No' : 'Yes'}</span>`
              : `<span class="mono" style="font-size:.82rem">${escape(value)}</span>`;
        return `<li class="leaf">
            <div class="leafhead"><span class="leaflabel">${escape(words(name))}</span>
                ${documented ? '' : '<span class="tag warn">no documentation</span>'}
                <span class="grow"></span>${shown}</div>
            ${help === '' ? '' : `<div class="leafhelp faint">${escape(help)}</div>`}
            ${STATE.mode === 'advanced' ? `<div class="leafadv faint mono">/CurveConfiguration/${escape(section.code)}/${escape(section.entry)}/${escape(name)} · 1.8.0.7 or earlier</div>` : ''}
            <button class="leafdoc">Documentation</button>
        </li>`;
    }).join('');
    return `<section class="panel entrycard">
        <div class="entryhead">
            <div><h2 class="entrytitle">${escape(curve.id)}</h2>
                <div class="muted">${escape(curve.description)}</div>
                <div class="faint" style="font-size:.8rem">${escape(section.code)} · one ${escape(section.entry)} entry</div></div>
            <div class="row" style="gap:8px"><span class="tag">${LEAVES.length} parameters</span><span class="tag warn">1 without documentation</span></div>
        </div>
        <ul class="leaves">${leaves}</ul>
        <div class="blocks"><span class="faint">Held as its own element:</span>
            <span class="tag">Report</span><span class="faint">not stated</span>
            <span class="tag">BootstrapConfig</span><span class="faint">not recorded here</span>
            <span class="tag">Segments</span><span class="faint">not recorded here</span></div>
    </section>`;
}

function rail(needle) {
    const groups = filled()
        .map((section) => ({
            section,
            curves: curvesOf(section.code, countOf(section.code)).filter((curve) =>
                needle === '' ||
                curve.id.toLowerCase().includes(needle) ||
                section.code.toLowerCase().includes(needle) ||
                curve.description.toLowerCase().includes(needle),
            ),
        }))
        .filter((group) => group.curves.length > 0);
    const openAll = filled().length > 0 && STATE.open.size >= filled().length;
    const inView = groups.reduce((sum, group) => sum + group.curves.length, 0);
    return `<aside class="card rail">
        <div class="railhead"><h2>Curves</h2>
            <p class="faint">${total()} in ${filled().length} of ${SECTIONS.length} sections</p></div>
        <input class="search" id="search" value="${escape(STATE.search)}" placeholder="Filter the curves" aria-label="Filter the curves">
        <button class="collapse" id="collapse">${openAll ? 'Collapse all' : 'Expand all'}</button>
        <div class="groups">${
            groups.length === 0
                ? '<p class="muted" style="font-size:.86rem">No curve matches the filter.</p>'
                : groups.map((group) => `<details class="group" data-section="${group.section.code}" ${STATE.open.has(group.section.code) || needle !== '' ? 'open' : ''}>
                    <summary><span class="groupname">${escape(group.section.code)}</span><span class="groupcount">${group.curves.length}</span></summary>
                    <div class="groupbody">${group.curves.map((curve) => `<button class="curve ${curve.id === STATE.curve ? 'on' : ''}" data-curve="${escape(curve.id)}">
                        <span class="curvename">${escape(curve.id)}</span>
                        <span class="curvedesc">${escape(curve.description)}</span></button>`).join('')}</div>
                </details>`).join('')
        }</div>
        <details class="emptysec"><summary>${blank().length} sections hold no curve here</summary>
            <div class="emptylist">${blank().map((section) => `<div class="emptyrow"><span>${escape(section.code)}</span><span class="faint">empty</span></div>`).join('')}</div>
        </details>
        <p class="faint railfoot">${inView} of ${total()} curves in view</p>
    </aside>`;
}

function details() {
    return `<section class="panel"><h2>Details</h2>
        <p class="muted">It decides how each curve of a run is defined and bootstrapped: its quotes, its segments and its interpolation. It does not decide the market a run reads, nor the trade data.</p>
        <div class="row" style="gap:24px">
            <div class="dl"><div class="k">Name</div><div class="v">${escape(NAMES[STATE.sections])}</div></div>
            <div class="dl"><div class="k">Party</div><div class="v">Acme Corporation Plc</div></div>
            <div class="dl"><div class="k">Version</div><div class="v">1</div></div>
            <div class="dl"><div class="k">Curves</div><div class="v">${total()} in ${filled().length} of ${SECTIONS.length} sections</div></div>
            <div class="dl"><div class="k">Documented</div><div class="v">172 of 250 parameters</div></div>
            <div class="dl"><div class="k">Used by</div><div class="v">2 reports</div></div>
            <div class="dl wide"><div class="k">Related configurations</div><div class="v"><a class="link" href="?tab=details">UK curve configuration</a></div></div>
        </div></section>`;
}

function render() {
    document.querySelectorAll('#documents button').forEach((button) => button.classList.toggle('on', Number(button.dataset.sections) === STATE.sections));
    document.querySelectorAll('#mode button').forEach((button) => button.classList.toggle('on', button.dataset.mode === STATE.mode));
    document.getElementById('title').textContent = NAMES[STATE.sections];
    document.getElementById('sub').textContent = `Version 1 · ${filled().length} of ${SECTIONS.length} sections hold curves · ${total()} curves · 172 of 250 parameters documented`;
    document.querySelector('.tabrow').innerHTML = [['details', 'Details', ''], ['curves', 'Curves', String(total())], ['usedBy', 'Used by', '2'], ['history', 'History', '']]
        .map(([key, label, count]) => `<button class="tab ${STATE.tab === key ? 'on' : ''}" data-tab="${key}">${label}${count === '' ? '' : ` <span class="tabcount">${count}</span>`}</button>`)
        .join('');

    const stage = document.getElementById('stage');
    if (STATE.tab !== 'curves') {
        stage.innerHTML = STATE.tab === 'details'
            ? details()
            : `<section class="panel"><h2>${STATE.tab === 'usedBy' ? 'Used by' : 'History'}</h2><p class="muted">${STATE.tab === 'usedBy' ? 'The reports that bind the configuration this document fills.' : 'Every version of the configuration, newest first.'}</p></section>`;
    } else {
        const chosen = everyCurve().find((curve) => curve.id === STATE.curve);
        stage.innerHTML = `<div class="two">${rail(STATE.search.trim().toLowerCase())}<div class="pane">${
            chosen === undefined
                ? '<section class="panel"><h2>No curve chosen</h2><p class="muted">Choose a curve in the rail to read its parameters, its documentation and the elements it holds.</p></section>'
                : workbench(chosen)
        }</div></div>`;
        wireRail();
    }
    document.querySelectorAll('.tabrow .tab').forEach((button) => button.addEventListener('click', () => set({ tab: button.dataset.tab })));
}

function wireRail() {
    const search = document.getElementById('search');
    if (search !== null) {
        search.addEventListener('input', (event) => {
            STATE.search = event.target.value;
            const at = event.target.selectionStart;
            render();
            const again = document.getElementById('search');
            if (again !== null) { again.focus(); again.setSelectionRange(at, at); }
        });
    }
    const collapse = document.getElementById('collapse');
    if (collapse !== null) {
        collapse.addEventListener('click', () => {
            STATE.open = STATE.open.size >= filled().length ? new Set() : new Set(filled().map((s) => s.code));
            render();
        });
    }
    document.querySelectorAll('.group').forEach((group) => {
        group.addEventListener('toggle', () => {
            if (group.open) STATE.open.add(group.dataset.section);
            else STATE.open.delete(group.dataset.section);
        });
    });
    document.querySelectorAll('.curve').forEach((button) => button.addEventListener('click', () => set({ curve: button.dataset.curve })));
    document.querySelectorAll('.leafdoc').forEach((button) => {
        button.addEventListener('click', () => {
            const leaf = button.closest('.leaf');
            leaf.classList.toggle('open');
            button.textContent = leaf.classList.contains('open') ? 'Hide documentation' : 'Documentation';
            if (leaf.classList.contains('open') && leaf.querySelector('.docpanel') === null) {
                const panel = document.createElement('div');
                panel.className = 'docpanel notice';
                panel.textContent = 'What the parameter means, from the ORE parameter contract: the description, the default, and in Advanced the ORE XML path and the release that introduced it.';
                leaf.appendChild(panel);
            }
        });
    });
}

function openFixture(sections) {
    const first = everyCurveOf(sections)[0];
    set({ sections, curve: first === undefined ? '' : first.id, open: new Set(['YieldCurves']) });
}
function everyCurveOf(sections) {
    return Object.keys(FIXTURES[sections]).flatMap((code) => curvesOf(code, FIXTURES[sections][code]));
}

document.getElementById('documents').addEventListener('click', (event) => {
    const button = event.target.closest('button');
    if (button !== null) openFixture(Number(button.dataset.sections));
});
document.getElementById('mode').addEventListener('click', (event) => {
    const button = event.target.closest('button');
    if (button !== null) set({ mode: button.dataset.mode });
});
document.getElementById('bar').addEventListener('click', (event) => {
    const button = event.target.closest('button');
    if (button !== null) openFixture(Number(button.dataset.sections));
});
document.getElementById('bar').innerHTML = `<span class="label">Curve configuration · curves tab</span>
    <span class="sep">|</span>
    <button data-sections="1">1 section</button><button data-sections="4">4 sections</button><button data-sections="16">16 sections</button>`;

if (STATE.curve === '' && everyCurve()[0] !== undefined) {
    STATE.curve = everyCurve()[0].id;
}
render();

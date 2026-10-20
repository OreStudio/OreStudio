/* The structure editor.
 *
 * It is a workbench: a person edits a working copy of one structure, and Save writes it
 * as one version with a kind of change, a reason and a commentary (see draft.js).
 * Authorisation is a state of the deal, and it does not move the version. */

let structure = null;
let selected = null;          // { kind: 'structure' | 'group' | 'component', id }
let pendingComponents = [];   // candidates ticked in the "form from trades" dialog
let activeTab = 'terms';      // which economics section the component pane shows
let stale = false;

/* --- Utilities ------------------------------------------------------ */

function esc(value) {
    return String(value === undefined || value === null ? '' : value)
        .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

function clone(value) {
    return JSON.parse(JSON.stringify(value));
}

function shortTime(iso) {
    return String(iso).slice(11, 16);
}

function allComponents() {
    return structure.groups.flatMap(g => g.components);
}

function findComponent(id) {
    return allComponents().find(c => c.id === id) || null;
}

function findGroup(id) {
    return structure.groups.find(g => g.id === id) || null;
}

function nextId(prefix, list) {
    let n = list.length + 1;
    const taken = new Set(list.map(x => x.id));
    while (taken.has(`${prefix}-${n}`)) n += 1;
    return `${prefix}-${n}`;
}

/* The selected row and the tab are in the address, so a reload and Back return to them. */
function addressOf() {
    const sel = selected && selected.kind !== 'structure' ? selected.id : null;
    return qs({ node: sel, tab: sel && selected.kind === 'component' && activeTab !== 'terms' ? activeTab : null });
}

function go(next, tab) {
    selected = next;
    activeTab = tab || 'terms';
    history.pushState(null, '', addressOf());
    render();
}

function readAddress() {
    const q = new URLSearchParams(location.search);
    const id = q.get('node');
    if (id && findComponent(id)) selected = { kind: 'component', id };
    else if (id && findGroup(id)) selected = { kind: 'group', id };
    else selected = { kind: 'structure', id: structure.id };
    activeTab = q.get('tab') || 'terms';
    render();
}

/* --- Changes ------------------------------------------------------------ */

/* An edit changes the working copy and names itself in the list of changes. Save writes
 * the list as one version. */
function mutate(text, redraw) {
    stage(structure, text);
    if (redraw === false) renderLight();
    else render();
}

/* The control to change the state of the deal asks for a reason first. */
function authorise() {
    const next = structure.authorisation === 'Draft' ? 'Authorised' : 'Draft';
    const state = { reason: 'trade.client_request', note: '' };
    const d = openDialog({
        title: next === 'Authorised' ? 'Authorise this structure' : 'Withdraw the authorisation', loadedAt: LOADED,
        body: [h('p', null, next === 'Authorised' ? 'The structure becomes Authorised. The version does not move.' : 'The structure goes back to Draft. The version does not move.'),
            changeFields(Object.assign(state, { activity: 'amendment_reference' }), 'PUT ' + structure.id + ' · claims version ' + structure.version)
                .filter((_, i) => i > 0)],
        buttons: [h('button', { class: 'btn ghost', onclick: () => d.close() }, 'Cancel'),
            h('button', { class: 'btn primary', onclick: () => { setAuthorisation(structure, next, state); d.close(); render(); } }, next === 'Authorised' ? 'Authorise' : 'Withdraw')]
    });
}

/* Save is the Edit pattern: the kind of change, the reason, the commentary, and a claim on the version read. */
function saveChanges() {
    const state = { activity: 'amendment_economic', reason: 'trade.client_request', note: '' };
    const d = openDialog({
        title: 'Save the structure', loadedAt: LOADED,
        body: [h('p', null, 'Save writes ' + structure.pending.length + ' change' + (structure.pending.length === 1 ? '' : 's') + ' as version ' + (structure.version + 1) + '. Authorisation does not change.'),
            h('ul', { class: 'pending' }, structure.pending.map(t => h('li', null, t))),
            changeFields(state, 'PUT ' + structure.id + ' · claims version ' + structure.version)],
        buttons: [h('button', { class: 'btn ghost', onclick: () => { if (!state.note || confirm('You have unsaved changes. Close anyway?')) d.close(); } }, 'Cancel'),
            h('button', { class: 'btn primary', onclick: () => { saveStructure(structure, state); d.close(); stale = true; render(); } }, 'Save')]
    });
    d.addEventListener('cancel', e => { if (state.note && !confirm('You have unsaved changes. Close anyway?')) e.preventDefault(); });
}

function discard() {
    const n = structure.pending.length;
    if (!confirm('Discard ' + n + ' change' + (n === 1 ? '' : 's') + '?')) return;
    discardChanges(structure);
    selected = { kind: 'structure', id: structure.id };
    history.replaceState(null, '', addressOf());
    render();
}

/* --- Strategy invariants --------------------------------------------- */

function currentStrategy() {
    return STRATEGIES.find(s => s.code === structure.strategy) || null;
}

/* The strategy's own legs. A hedge group sits outside the strategy: its
 * trades offset the deal, they do not make it. */
function strategyLegs() {
    return structure.groups
        .filter(g => !g.hedge)
        .flatMap(g => g.components)
        .filter(c => c.productType !== 'composite');
}

/* Returns null when the structure is valid, or the reason it is not. */
function validate() {
    if (structure.mode !== 'strategy') return null;

    const strategy = currentStrategy();
    if (!strategy) return { what: 'No strategy chosen', why: 'Pick the strategy this structure follows.' };

    const legs = strategyLegs();
    if (legs.length !== strategy.legs) {
        return {
            what: `${strategy.name} needs ${strategy.legs} legs`,
            why: `The structure has ${legs.length}. Strategy mode fixes the leg set.`
        };
    }

    const broken = strategy.invariant(legs.map(c => c.fields));
    if (broken) return { what: `${strategy.name} invariant broken`, why: broken };

    if (legs.some(c => !c.fields.notional)) {
        return { what: 'Incomplete leg', why: 'Every leg needs a notional before the booking is valid.' };
    }

    return null;
}

/* --- Rendering -------------------------------------------------------- */

function render() {
    renderHeader();
    renderValidation();
    renderTree();
    renderEditor();
    renderNpv();
    renderAudit();
}

/* A field typed into changes the copy without drawing the form again, which would end the typing. */
function renderLight() {
    renderHeader();
    renderValidation();
    renderTree();
    renderNpv();
}

const REFKINDS = { Counterparty: 'counterparty', Book: 'book', Portfolio: 'portfolio', 'Netting set': 'netting_set', 'Trade type': 'trade_type', Successor: 'trade' };

function refHtml(kind, key, flag) {
    return `<a class="ref" href="#" data-ref="${esc(kind)}:${esc(key)}">${flag ? `<span class="flag">${flag}</span>` : ''}${esc(key)}</a>`;
}

function ccyHtml(code) {
    return code ? refHtml('currency', code, FLAGS[code]) : esc('—');
}

function renderHeader() {
    const dirty = structure.pending.length;
    const meta = [
        chip('v' + structure.version, 'v', 'Version of this structure'),
        h('span', { class: 'chip auth-' + structure.authorisation.toLowerCase() }, structure.authorisation),
        chip(structure.mode === 'strategy' ? 'Strategy' : 'Package'),
        dirty ? chip(dirty + ' unsaved', 'warn', structure.pending.join('; ')) : null
    ];
    const actions = [
        refreshButton({ stale, onClick: () => { stale = false; LOADED = new Date(NOW.getTime()); render(); toast('Read again at ' + iso(LOADED)); } }),
        h('div', { class: 'mode-switch', role: 'group', 'aria-label': 'Composition rung' },
            ['strategy', 'package'].map(m => h('button', { class: structure.mode === m ? 'on' : '', onclick: () => setMode(m) }, m === 'strategy' ? 'Strategy' : 'Package'))),
        structure.mode === 'strategy' ? h('select', { class: 'field-select', title: 'Strategy this structure follows', onchange: e => setStrategy(e.target.value) },
            STRATEGIES.map(s => h('option', { value: s.code, selected: s.code === structure.strategy }, s.name))) : null,
        h('button', { class: 'btn', onclick: authorise }, structure.authorisation === 'Draft' ? 'Authorise' : 'Withdraw'),
        h('a', { href: 'pricing.html', class: 'btn' }, 'Price this structure'),
        dirty ? h('button', { class: 'btn ghost', onclick: discard }, 'Discard changes') : null,
        h('button', { class: 'btn primary', disabled: !dirty, title: dirty ? 'Save ' + dirty + ' change' + (dirty === 1 ? '' : 's') : 'Nothing has changed.', onclick: saveChanges }, 'Save')
    ];
    const id = h('span', null, structure.id, ' · ', ref('counterparty', structure.counterparty), ' · ', ref('book', structure.book), ' · ', ref('portfolio', structure.portfolio), ' · ', ref('netting_set', structure.nettingSet), ' · ', structure.tradeDate);
    fill($('#head'), pageHead({ title: structure.name, id, meta, actions }));
}

function renderValidation() {
    const banner = document.getElementById('validation');
    const problem = validate();
    banner.hidden = !problem;
    if (!problem) return;
    banner.className = 'notice warn';
    banner.replaceChildren(h('b', null, problem.what + '. '), problem.why);
}

/* The side chip is coloured by its class, and the generic form lets the
 * reader type anything into longShort, so the class comes from a known pair
 * rather than from the field. */
function sideClass(value) {
    const side = String(value || '').toLowerCase();
    if (side === 'long') return 'side-buy';
    if (side === 'short') return 'side-sell';
    return '';
}

function componentMeta(component) {
    const f = component.fields;
    const side = f.longShort ? `<span class="${sideClass(f.longShort)}">${esc(f.longShort)}</span>` : '';
    const num = f.notional ? Number(f.notional).toLocaleString() : '';
    const ccy = f.currency || f.boughtCurrency || '';
    return [side, num && `${num} ${esc(ccy)}`].filter(Boolean).join(' · ');
}

function renderTree() {
    const tree = document.getElementById('tree');
    const rows = [];

    rows.push(`
        <div class="node depth-0">
            <div class="node-row${selected && selected.kind === 'structure' ? ' selected' : ''}" data-kind="structure">
                <span class="twisty">▾</span>
                <span class="label">
                    <span class="name">${esc(structure.name)}</span>
                    <span class="meta">${esc(structure.id)} · ${allComponents().length} components</span>
                </span>
                <span class="chip version">v${structure.version}</span>
            </div>
        </div>`);

    structure.groups.forEach(group => {
        rows.push(`
            <div class="node depth-1">
                <div class="node-row${selected && selected.kind === 'group' && selected.id === group.id ? ' selected' : ''}"
                     data-kind="group" data-id="${esc(group.id)}">
                    <span class="twisty">▾</span>
                    <span class="type-tag">${group.hedge ? 'Hedge group' : 'Group'}</span>
                    <span class="label"><span class="name">${esc(group.name)}</span></span>
                    <span class="num">${group.components.length}</span>
                </div>
                <div class="group-children">
                    ${group.components.map(c => `
                        <div class="node depth-2">
                            <div class="node-row${selected && selected.kind === 'component' && selected.id === c.id ? ' selected' : ''}"
                                 data-kind="component" data-id="${esc(c.id)}">
                                <span class="twisty"></span>
                                <span class="type-tag">${esc(productTypeName(c.productType))}</span>
                                <span class="label">
                                    <span class="name">${esc(c.label)}</span>
                                    <span class="meta">${esc(c.id)}${c.tradeTypeCode ? ' · ' + esc(c.tradeTypeCode) : ''}</span>
                                </span>
                                <span class="num">${componentMeta(c)}</span>
                            </div>
                        </div>`).join('')}
                </div>
            </div>`);
    });

    tree.innerHTML = rows.join('');

    tree.querySelectorAll('.node-row').forEach(row => {
        row.addEventListener('click', () => go({ kind: row.dataset.kind, id: row.dataset.id || structure.id }));
    });
}

/* The pane has two regions, and they never swap places. The trade header
 * is reference data: the same fields, in the same order, whatever the
 * product type. The economics below it is the instrument, and only that
 * part changes with the product type. */
function renderEditor() {
    SLOTS = {};
    const editor = document.getElementById('editor');
    const removeBtn = document.getElementById('remove-component');
    const envelope = document.getElementById('envelope');
    const tabs = document.getElementById('tabs');

    const component = selected && selected.kind === 'component' ? findComponent(selected.id) : null;
    removeBtn.hidden = !component;
    envelope.hidden = !component;
    tabs.hidden = !component;

    if (!component) {
        envelope.innerHTML = '';
        tabs.innerHTML = '';
        editor.innerHTML = structureOverview();
        return;
    }

    renderEnvelope(component);

    const widget = widgetFor(component.productType, component.tradeTypeCode);
    const sections = sectionsFor(widget, component);
    const shown = sections.find(s => s.id === activeTab) || sections[0];
    activeTab = shown.id;

    tabs.innerHTML = sections.map(s => `
        <button role="tab" data-tab="${esc(s.id)}" aria-selected="${s.id === shown.id}"
                class="${s.id === shown.id ? 'on' : ''}">${esc(s.label)}</button>`).join('');

    editor.innerHTML = `
        <div class="widget-head">
            <div>
                <h3>${esc(component.label)}</h3>
                <div class="widget-kind">${esc(productTypeName(component.productType))} ·
                    ${esc(component.tradeTypeCode || 'no type code')} · ${esc(WIDGET_NAMES[widget])}</div>
            </div>
            ${groupField(component)}
        </div>
        <div class="tab-panel" role="tabpanel">${shown.html}</div>`;

    bindTabs();
    bindWidget(component);
    bindGroupField(component);
}

function envValue(key, value) {
    if (value === undefined || value === null || value === '') return esc('—');
    if (REFKINDS[key]) return refHtml(REFKINDS[key], value);
    if (key === 'Modified by') return esc(PEOPLE[value] ? PEOPLE[value].name : value);
    return esc(value);
}

function renderEnvelope(component) {
    document.getElementById('envelope').innerHTML = `
        <div class="envelope-title">Envelope</div>
        <div class="env-board">
            ${envelopeOf(component).map(g => `
                <div class="env-group">
                    <h4>${esc(g.group)}</h4>
                    ${g.fields.map(([k, v]) => `
                        <div class="env-pair">
                            <span class="env-key">${esc(k)}</span>
                            <span class="env-val">${envValue(k, v)}</span>
                        </div>`).join('')}
                </div>`).join('')}
        </div>`;
}

/* What the structure pane shows when the reader has the root selected
 * rather than a component. */
function structureOverview() {
    return `
        <div class="widget">
            <h3>${esc(structure.name)}</h3>
            <div class="widget-kind">Structure · ${esc(structure.mode)} mode</div>
            <div class="subhead">Components by type</div>
            <table class="legs">
                <thead><tr><th>ID</th><th>Type</th><th>Label</th><th>Widget</th></tr></thead>
                <tbody>
                    ${allComponents().map(c => `
                        <tr>
                            <td>${refHtml('trade', c.id)}</td>
                            <td>${esc(productTypeName(c.productType))}</td>
                            <td>${esc(c.label)}</td>
                            <td>${esc(WIDGET_NAMES[widgetFor(c.productType, c.tradeTypeCode)])}</td>
                        </tr>`).join('')}
                </tbody>
            </table>
        </div>`;
}

function bindTabs() {
    document.querySelectorAll('#tabs button[data-tab]').forEach(btn => {
        btn.addEventListener('click', () => go(selected, btn.dataset.tab));
    });
}

/* The last audit line that names this component. The trade header reads
 * its audit group from here, so the header and the drawer cannot drift
 * apart. */
function lastActor(component) {
    const hit = structure.audit.filter(a => String(a.what || a.text).includes(component.id)).pop();
    return hit ? hit.actor : ME;
}

function lastTouchOf(component) {
    const hit = structure.audit.filter(a => String(a.what || a.text).includes(component.id)).pop();
    return hit ? `${hit.at.slice(0, 10)} ${shortTime(hit.at)}` : structure.tradeDate;
}

/* The trade header of one component: the five field groups ores.trading
 * composes a trade from. None of them is the instrument. The parties and
 * the dates are inherited from the structure, because that is the deal
 * the component is booked under. */
function envelopeOf(component) {
    const e = component.envelope || {};
    const family = PRODUCT_TYPES.find(p => p.code === component.productType);
    const f = component.fields;

    return [
        {
            group: 'Identity',
            fields: [
                ['Trade ID', component.id],
                ['External ID', e.externalId || `ORES-${structure.id}-${component.id}`]
            ]
        },
        {
            group: 'Classification',
            fields: [
                ['Trade type', component.tradeTypeCode || '—'],
                ['Product type', family ? family.name : component.productType],
                ['Asset class', e.assetClass || assetClassName(family && family.assetClass)],
                ['Trade status', e.status || 'Booked'],
                ['Activity', e.activityType || 'New'],
                ['Netting set', structure.nettingSet]
            ]
        },
        {
            group: 'Parties',
            fields: [
                ['Book', structure.book],
                ['Portfolio', structure.portfolio],
                ['Counterparty', structure.counterparty],
                ['Successor', e.successorTradeId]
            ]
        },
        {
            group: 'Lifecycle',
            fields: [
                ['Trade date', structure.tradeDate],
                ['Effective date', f.startDate || structure.tradeDate],
                ['Termination date', f.expiry || f.endDate],
                ['Executed at', `${structure.tradeDate}T09:04:00Z`]
            ]
        },
        {
            group: 'Audit',
            fields: [
                ['Modified by', lastActor(component)],
                ['Change reason', e.changeReasonCode || 'NEW'],
                ['Recorded at', lastTouchOf(component)]
            ]
        }
    ];
}

/* A component sits in exactly one group, and the group it sits in decides
 * whether the strategy invariant covers it. Moving one is a change to the
 * structure, so it takes the version with it. */
function groupOf(component) {
    const group = structure.groups.find(g => g.components.some(c => c.id === component.id));
    return group ? group.id : '';
}

/* The group is where the component sits in the tree. It is a display
 * concept, not a term of the trade, so it is no field on the instrument:
 * it sits beside the component's name, not in the form. */
function groupField(component) {
    const here = groupOf(component);
    return `
        <div class="head-group">
            <label for="f-group">Group</label>
            <select id="f-group">
                ${structure.groups.map(g => `
                    <option value="${esc(g.id)}"${g.id === here ? ' selected' : ''}>
                        ${esc(g.name)}${g.hedge ? ' — hedge, outside the strategy' : ''}
                    </option>`).join('')}
            </select>
        </div>`;
}

function bindGroupField(component) {
    const select = document.getElementById('f-group');
    if (!select) return;
    select.addEventListener('change', () => {
        const target = findGroup(select.value);
        if (!target || target.id === groupOf(component)) return;
        structure.groups.forEach(g => {
            g.components = g.components.filter(c => c.id !== component.id);
        });
        target.components.push(component);
        mutate(`${component.id} moved to ${target.name}`);
    });
}

/* Each widget is the specialised instrument view for a product family,
 * and the fallback is a real path, not dead code: most trade type codes
 * have no dedicated form.
 *
 * A widget returns named sections, not one block, so the pane shows one
 * at a time instead of a single long scroll. Which sections exist is the
 * product type's business; the trade header above them is not. */
function sectionsFor(widget, component) {
    if (widget === 'vanilla') {
        return [
            { id: 'terms', label: 'Terms', html: vanillaTerms(component) },
            { id: 'schedule', label: 'Schedule', html: scheduleSection(component) },
            { id: 'premium', label: 'Premium', html: premiumSection(component) }
        ];
    }

    if (widget === 'barrier') {
        return [
            { id: 'terms', label: 'Terms', html: vanillaTerms(component) },
            { id: 'barriers', label: 'Barriers', html: barriersSection(component) },
            { id: 'schedule', label: 'Schedule', html: scheduleSection(component) },
            { id: 'premium', label: 'Premium', html: premiumSection(component) }
        ];
    }

    if (widget === 'schedule') {
        return [
            { id: 'terms', label: 'Terms', html: swapTerms(component) },
            { id: 'schedule', label: 'Schedule', html: swapScheduleSection(component) }
        ];
    }

    if (widget === 'composite') {
        return [
            { id: 'terms', label: 'Terms', html: compositeTerms(component) },
            { id: 'legs', label: 'Legs', html: legsSection(component) }
        ];
    }

    return [{ id: 'terms', label: 'Terms', html: genericTerms(component) }];
}

/* Each field is a slot. bindWidget fills a slot with valueInput(), the input every
 * screen uses: a combo box for a value that exists, a date input for a date, and Clear
 * on an optional field. */
let SLOTS = {};
const BUSINESS_DAY_CONVENTIONS = ['Following', 'ModifiedFollowing', 'Preceding', 'Unadjusted'];

function slot(key, label, value, fd) {
    SLOTS[key] = { fd: Object.assign({}, fd), value: value === undefined || value === null ? '' : value };
    return `
        <div class="field">
            <label data-for="${esc(key)}">${esc(label)}</label>
            <div data-slot="${esc(key)}"></div>
        </div>`;
}

const termField = (key, label, value, kind, extra) => slot(key, label, value, { type: kind || 'text', optional: !!(extra && extra.optional) });
const ccyField = (key, label, value, optional) => slot(key, label, value, { type: 'ccy', optional });
const selectField = (key, label, value, options) => slot(key, label, value, { options });

function vanillaTerms(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${selectField('longShort', 'Long / short', f.longShort, ['Long', 'Short'])}
            ${selectField('optionType', 'Option type', f.optionType, ['Call', 'Put'])}
            ${selectField('style', 'Style', f.style, ['European', 'American', 'Bermudan'])}
            ${selectField('settlement', 'Settlement', f.settlement, ['Cash', 'Physical'])}
            ${ccyField('boughtCurrency', 'Bought currency', f.boughtCurrency)}
            ${termField('boughtAmount', 'Bought amount', f.boughtAmount, 'number')}
            ${ccyField('soldCurrency', 'Sold currency', f.soldCurrency)}
            ${termField('soldAmount', 'Sold amount', f.soldAmount, 'number')}
            ${termField('strike', 'Strike', f.strike, 'number', 'step="0.0001"')}
            ${termField('notional', 'Notional', f.notional, 'number')}
        </div>`;
}

function scheduleSection(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${termField('expiry', 'Expiry', f.expiry, 'date')}
            ${selectField('payOffAtExpiry', 'Pay off at expiry', f.payOffAtExpiry, ['false', 'true'])}
        </div>
        <div class="subhead">Exercise dates</div>
        <table class="schedule">
            <thead><tr><th>#</th><th>Date</th><th>Type</th></tr></thead>
            <tbody><tr><td>1</td><td>${esc(f.expiry || '—')}</td><td>Expiry</td></tr></tbody>
        </table>
`;
}

/* The premium is a sub-leg: a trade in its own right with its own
 * identifier, currency and payment date. It is built here, not described
 * here, and the option does not own it. */
function premiumSection(c) {
    if (!c.premium) {
        return `
            <button class="primary" id="add-premium">Add premium sub-leg</button>`;
    }

    const p = c.premium;
    return `
        <div class="subleg">
            <div class="subleg-head">
                <span class="type-tag">Sub-leg</span>
                <strong>${esc(p.id)}</strong>
                <span class="grow">FX option premium</span>
                <button class="danger ghost" id="remove-premium">Remove</button>
            </div>
            <div class="field-grid">
                ${slot('premium.amount', 'Amount', p.amount, { type: 'number' })}
                ${ccyField('premium.currency', 'Currency', p.currency)}
                ${slot('premium.payDate', 'Pay date', p.payDate, { type: 'date', optional: true })}
            </div>
        </div>`;
}

function barriersSection(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${selectField('barrierType', 'Barrier type', f.barrierType, ['UpAndIn', 'UpAndOut', 'DownAndIn', 'DownAndOut'])}
            ${termField('barrierLevel', 'Level 1', f.barrierLevel, 'number', 'step="0.0001"')}
            ${slot('barrierLevel2', 'Level 2 (double barrier)', f.barrierLevel2 || '', { type: 'number', optional: true })}
            ${slot('rebate', 'Rebate', f.rebate, { type: 'number', optional: true })}
            ${slot('fxIndex', 'FX index', f.fxIndex, { optional: true })}
        </div>
`;
}

function swapTerms(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${selectField('legType', 'Leg type', f.legType, ['Fixed', 'Floating'])}
            ${selectField('payer', 'Payer', f.payer, ['true', 'false'])}
            ${ccyField('currency', 'Currency', f.currency)}
            ${termField('notional', 'Notional', f.notional, 'number')}
            ${termField('rate', 'Rate', f.rate, 'number', 'step="0.0001"')}
            ${selectField('dayCounter', 'Day counter', f.dayCounter, DAY_COUNTS)}
        </div>`;
}

function swapScheduleSection(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${termField('startDate', 'Start date', f.startDate, 'date')}
            ${termField('endDate', 'End date', f.endDate, 'date')}
            ${selectField('tenor', 'Tenor', f.tenor, ['1M', '3M', '6M', '1Y'])}
            ${slot('calendar', 'Calendar', f.calendar, { type: 'calendar', optional: true })}
            ${selectField('convention', 'Business day convention', f.convention, BUSINESS_DAY_CONVENTIONS)}
            ${selectField('rule', 'Rule', f.rule, ['Forward', 'Backward'])}
            ${selectField('paymentConvention', 'Payment convention', f.paymentConvention, BUSINESS_DAY_CONVENTIONS)}
        </div>

        <div class="subhead">Generated schedule</div>
        <table class="schedule">
            <thead><tr><th>#</th><th>Accrual start</th><th>Accrual end</th></tr></thead>
            <tbody>${scheduleRows(f)}</tbody>
        </table>
`;
}

/* The schedule is rebuilt from the rules above rather than stored, which
 * is what the leg is priced from. The period arithmetic lives in the
 * catalogue, where the self-check can reach it. */
function scheduleRows(f) {
    const periods = schedulePeriods(f);
    if (!periods.length) {
        return '<tr><td colspan="3">Set a start date, an end date and a tenor.</td></tr>';
    }
    return periods.map((p, i) =>
        `<tr><td>${i + 1}</td><td>${p.start}</td><td>${p.end}</td></tr>`).join('');
}

function compositeTerms(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${ccyField('currency', 'Currency', f.currency)}
            ${selectField('notionalCalculation', 'Notional calculation', f.notionalCalculation, ['Mean', 'Sum', 'Min', 'Max'])}
            ${termField('underlying', 'Underlying', f.underlying)}
            ${termField('strike', 'Strike', f.strike, 'number', 'step="0.0001"')}
            ${termField('expiry', 'Expiry', f.expiry, 'date')}
            ${termField('quantity', 'Quantity', f.quantity, 'number')}
        </div>`;
}

function legsSection(c) {
    const legs = c.components || [];
    return `
        <table class="legs">
            <thead><tr><th>Leg</th><th>Type</th><th>Side</th><th>Strike</th><th class="num">Notional</th></tr></thead>
            <tbody>
                ${legs.map(l => `
                    <tr>
                        <td>${esc(l.label)}</td>
                        <td>${esc(l.fields.optionType || '—')}</td>
                        <td><span class="${sideClass(l.fields.longShort)}">${esc(l.fields.longShort || '—')}</span></td>
                        <td>${esc(l.fields.strike || '—')}</td>
                        <td class="num">${esc(l.fields.notional || '—')}</td>
                    </tr>`).join('')}
            </tbody>
        </table>
`;
}

/* The fallback. It captures the instrument and nothing else: the
 * counterparty and the book are already in the trade header above, and a
 * form that asks for them twice is the split failing. */
function genericTerms(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${selectField('longShort', 'Long / short', f.longShort || 'Long', ['Long', 'Short'])}
            ${termField('notional', 'Notional', f.notional || '', 'number')}
            ${ccyField('currency', 'Currency', f.currency || '')}
            ${termField('expiry', 'Expiry', f.expiry || '', 'date')}
        </div>`;
}

function bindWidget(component) {
    const editor = document.getElementById('editor');

    editor.querySelectorAll('[data-slot]').forEach(host => {
        const key = host.dataset.slot;
        const { fd, value } = SLOTS[key];
        const premium = key.startsWith('premium.');
        const name = premium ? key.slice(8) : key;
        const target = premium ? component.premium : component.fields;
        const typed = !fd.options && !['ccy', 'calendar', 'date'].includes(fd.type);
        const set = (v, redraw) => {
            if (target[name] === v) return;
            target[name] = v;
            if (!premium) component.label = relabel(component);
            mutate(`${component.id} ${premium ? 'premium ' : ''}${name} changed`, !typed || !!redraw);
            if (redraw || !typed) return;
            const clear = host.querySelector('button');
            if (clear) clear.disabled = v === '';
        };
        host.append(valueInput(fd, value, set, false));
        const input = host.querySelector('input, select');
        const label = editor.querySelector(`label[data-for="${key}"]`);
        if (input && label) { input.id = 'f-' + key; label.htmlFor = input.id; }
        if (input && fd.type === 'number') input.inputMode = 'decimal';
    });

    const addPremium = document.getElementById('add-premium');
    if (addPremium) {
        addPremium.addEventListener('click', () => {
            component.premium = {
                id: nextTradeId(),
                amount: '0',
                currency: component.fields.boughtCurrency || component.fields.currency || 'USD',
                payDate: ''
            };
            mutate(`${component.id} premium sub-leg ${component.premium.id} added`);
            go(selected, 'premium');
        });
    }

    const removePremium = document.getElementById('remove-premium');
    if (removePremium) {
        removePremium.addEventListener('click', () => {
            const id = component.premium.id;
            component.premium = null;
            mutate(`${component.id} premium sub-leg ${id} removed`);
        });
    }
}

/* The label follows the economics, so the tree stays readable while the
 * widget is edited. The field that identifies a component depends on what
 * its widget shows: a swap has a leg type, an option has an option type. */
function relabel(component) {
    const f = component.fields;
    const widget = widgetFor(component.productType, component.tradeTypeCode);
    const identifying = {
        schedule: f.legType,
        generic: f.longShort,
        vanilla: f.optionType,
        barrier: f.barrierType,
        composite: f.notionalCalculation
    }[widget];
    const bits = [f.currency || f.boughtCurrency, identifying, f.expiry].filter(Boolean);
    return bits.length ? bits.join(' ') : component.label;
}

function renderNpv() {
    const npv = structure.initialNpv;
    const value = document.getElementById('npv-value');

    if (!npv) {
        value.textContent = 'Not valued';
        document.getElementById('provenance').innerHTML = '';
        return;
    }

    value.textContent = `${Number(npv.amount).toLocaleString()} ${npv.currency}`;
    document.getElementById('provenance').innerHTML = `
        <div><strong>As of</strong>${esc(npv.asOf)}</div>
        <div><strong>Market data cut</strong>${esc(npv.snapshot)}</div>
        <div><strong>Model</strong>${esc(npv.model)}</div>
        <div><strong>Run</strong>${esc(npv.runId)}</div>
        <div><strong>Source</strong>${esc(npv.source || 'Pricing screen')}</div>`;
}

function renderAudit() {
    fill($('#audit'), timeline(historyEntries(structure), { isService: false }));
}

/* --- Actions ---------------------------------------------------------- */

function openPicker() {
    const body = document.getElementById('picker-body');
    body.innerHTML = ASSET_CLASSES.map(ac => {
        const families = PRODUCT_TYPES.filter(p => p.assetClass === ac.code);
        return `
            <div class="picker-class">
                <h4>${esc(ac.name)}</h4>
                <div class="picker-grid">
                    ${families.map(p => `
                        <button class="pick" data-product="${esc(p.code)}" data-code="">
                            <span class="pick-name">${esc(p.name)}</span>
                            <span class="pick-meta">${esc(p.qtForm)}</span>
                        </button>`).join('')}
                    ${ac.code === 'fx' ? TRADE_TYPE_CODES.map(t => `
                        <button class="pick" data-product="fx" data-code="${esc(t.code)}">
                            <span class="pick-name">${esc(t.code)}</span>
                            <span class="pick-meta${t.widget ? ' specialised' : ''}">
                                ${esc(WIDGET_NAMES[widgetFor('fx', t.code)])}
                            </span>
                        </button>`).join('') : ''}
                </div>
            </div>`;
    }).join('');

    body.querySelectorAll('.pick').forEach(btn => {
        btn.addEventListener('click', () => {
            addComponent(btn.dataset.product, btn.dataset.code);
            document.getElementById('picker-dialog').close();
        });
    });

    openStatic('picker-dialog');
}

/* A dialog drawn in the page says when the record it started from was loaded. */
function openStatic(id) {
    const d = document.getElementById(id);
    d.querySelectorAll('[data-loaded]').forEach(el => { el.textContent = 'Loaded ' + iso(LOADED); el.title = 'Loaded'; });
    d.showModal();
}

function addComponent(productType, tradeTypeCode) {
    if (!structure.groups.length) structure.groups.push({ id: 'G-1', name: 'Components', components: [] });

    const targetGroup = selected && selected.kind === 'group'
        ? findGroup(selected.id)
        : structure.groups[structure.groups.length - 1];

    const component = blankComponent(productType, tradeTypeCode, allComponents());
    targetGroup.components.push(component);
    mutate(`${component.id} added to ${targetGroup.name}`, false);
    go({ kind: 'component', id: component.id });
}

/* A premium sub-leg is a trade, so it takes an identifier from the same
 * space as the components. Nothing reuses one. */
function nextTradeId() {
    const taken = allComponents()
        .flatMap(c => [c.id].concat(c.premium && c.premium.id ? [c.premium.id] : []))
        .map(id => ({ id }));
    return nextId('T', taken);
}

function blankComponent(productType, tradeTypeCode, existing) {
    const family = PRODUCT_TYPES.find(p => p.code === productType);
    return {
        id: nextId('T', existing),
        productType,
        tradeTypeCode,
        label: `New ${family ? family.name : productType} component`,
        fields: {
            longShort: 'Long', optionType: 'Call', style: 'European', settlement: 'Cash',
            payOffAtExpiry: 'false', expiry: '', strike: '', notional: '',
            currency: 'USD', boughtCurrency: '', soldCurrency: '',
            ...((family && family.defaults) || {})
        },
        envelope: { activityType: 'New', status: 'Draft' },
        premium: null
    };
}

function openFormFromTrades() {
    pendingComponents = [];
    const body = document.getElementById('trades-body');

    body.innerHTML = `
        <div class="candidate-list">
            ${existingTrades().map(t => `
                <div class="candidate" data-id="${esc(t.id)}">
                    <input type="checkbox" data-tick="${esc(t.id)}">
                    <span class="type-tag">${esc(productTypeName(t.productType))}</span>
                    <span class="grow">
                        <strong>${esc(t.label)}</strong>
                        <span class="meta">${refHtml('trade', t.id)} · ${Number(t.notional).toLocaleString()} ${ccyHtml(t.ccy)}</span>
                    </span>
                </div>`).join('')}
        </div>`;

    body.querySelectorAll('.candidate').forEach(row => {
        row.addEventListener('click', event => {
            if (event.target.closest('a')) { event.preventDefault(); return; }
            if (event.target.tagName !== 'INPUT') {
                const box = row.querySelector('input');
                box.checked = !box.checked;
            }
            row.classList.toggle('selected', row.querySelector('input').checked);
            syncPending(body);
        });
    });

    openStatic('trades-dialog');
}

function syncPending(body) {
    pendingComponents = Array.from(body.querySelectorAll('input[data-tick]:checked'))
        .map(box => box.dataset.tick);
}

function formFromTrades() {
    const chosen = existingTrades().filter(t => pendingComponents.includes(t.id));
    if (!chosen.length) return;

    const group = { id: `G-${structure.groups.length + 1}`, name: 'Formed group', components: [] };
    chosen.forEach((t, index) => {
        group.components.push({
            ...blankComponent(t.productType, t.tradeTypeCode, group.components),
            id: t.id,
            label: t.label,
            fields: {
                longShort: 'Long', optionType: 'Call', style: 'European', settlement: 'Cash',
                payOffAtExpiry: 'false', expiry: '2025-08-10',
                strike: '', notional: t.notional, currency: t.ccy,
                boughtCurrency: t.ccy, soldCurrency: ''
            },
            premium: null
        });
    });

    structure.groups.push(group);
    structure.mode = 'package';
    structure.strategy = '';
    mutate(`Structure formed from ${chosen.length} existing trades`, false);
    pendingComponents = [];
    document.getElementById('trades-dialog').close();
    go({ kind: 'group', id: group.id });
}

function removeComponent() {
    if (!selected || selected.kind !== 'component') return;
    const id = selected.id;
    structure.groups.forEach(g => {
        g.components = g.components.filter(c => c.id !== id);
    });
    mutate(`${id} removed`, false);
    go({ kind: 'structure', id: structure.id });
}

function setMode(mode) {
    if (structure.mode === mode) return;
    structure.mode = mode;
    if (mode === 'strategy' && !structure.strategy) structure.strategy = STRATEGIES[0].code;
    mutate(`Mode changed to ${mode}`, false);
    go({ kind: 'structure', id: structure.id });
}

function setStrategy(code) {
    if (structure.strategy === code) return;
    structure.strategy = code;
    mutate(`Strategy changed to ${STRATEGIES.find(s => s.code === code).name}`);
}

/* --- Wiring ----------------------------------------------------------- */

function addGroup() {
    const state = { name: `Group ${structure.groups.length + 1}`, touched: false };
    const d = openDialog({
        title: 'Add a group', loadedAt: LOADED,
        body: field('Name', h('input', { class: 'field-input', value: state.name, oninput: e => { state.name = e.target.value; state.touched = true; } })),
        buttons: [h('button', { class: 'btn ghost', onclick: () => { if (!state.touched || confirm('You have unsaved changes. Close anyway?')) d.close(); } }, 'Cancel'),
            h('button', { class: 'btn primary', onclick: () => {
                if (!state.name.trim()) return;
                const group = { id: `G-${structure.groups.length + 1}`, name: state.name.trim(), components: [] };
                structure.groups.push(group);
                mutate(`Group ${group.name} added`, false);
                d.close();
                go({ kind: 'group', id: group.id });
            } }, 'Add')]
    });
    d.addEventListener('cancel', e => { if (state.touched && !confirm('You have unsaved changes. Close anyway?')) e.preventDefault(); });
}

function wire() {
    document.getElementById('add-component').addEventListener('click', openPicker);
    document.getElementById('remove-component').addEventListener('click', removeComponent);
    document.getElementById('form-from-trades').addEventListener('click', openFormFromTrades);
    document.getElementById('trades-confirm').addEventListener('click', formFromTrades);
    document.getElementById('add-group').addEventListener('click', addGroup);

    document.querySelectorAll('dialog [data-close]').forEach(btn => {
        const dialog = btn.closest('dialog');
        const close = dialog.id === 'trades-dialog' ? guardDialog(dialog, () => pendingComponents.length > 0) : () => dialog.close();
        btn.addEventListener('click', close);
    });
    window.addEventListener('popstate', readAddress);
}

/* A structure stored by an earlier version of the page carries no trade
 * header on its components, and its premium carries no identifier of its
 * own. Derive both rather than discard the booking. */
function backfill() {
    allComponents().forEach(c => {
        if (!c.envelope) c.envelope = { activityType: 'New', status: 'Booked' };
        if (c.premium && !c.premium.id) c.premium.id = nextTradeId();
    });
}

function boot() {
    if (!CAN.structure) {
        document.querySelector('.app').replaceChildren(pageHead({ title: 'Structure editor' }),
            needsRole('the structure editor', 'change structures', STRUCTURE_ROLES));
        return;
    }
    structure = normaliseStructure(loadStructure() || openingStructure());
    backfill();
    stale = P.get('state') === 'stale';
    wire();
    readAddress();
}

/* The guard lets the widget renderers be exercised outside a browser, the
 * same way catalogue.js guards its self-check. */
if (typeof document !== 'undefined') boot();

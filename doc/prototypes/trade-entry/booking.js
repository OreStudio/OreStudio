/* The booking screen.
 *
 * The screen is deliberately not interactive in the pricing sense: no
 * live market data, no model inputs. It concentrates on the correctness
 * of the booking.
 *
 * The load-bearing rule is the version rule, so it is modelled literally
 * rather than asserted. Every mutating action routes through mutate(),
 * which bumps the version and appends to the audit log. authorise() does
 * not route through it, because authorisation is metadata about the deal,
 * not a term of it. See the Trade Structures and Deal Composition page.
 */

const STORAGE_KEY = 'ores.trade-entry.structure';

let structure = null;
let selected = null;          // { kind: 'structure' | 'group' | 'component', id }
let pendingComponents = [];   // candidates ticked in the "form from trades" dialog

/* --- Utilities ------------------------------------------------------ */

function esc(value) {
    return String(value === undefined || value === null ? '' : value)
        .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

function clone(value) {
    return JSON.parse(JSON.stringify(value));
}

function nowIso() {
    return new Date().toISOString().replace(/\.\d+Z$/, 'Z');
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

function persist() {
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(structure));
    } catch (err) {
        /* A private window or blocked site data. The prototype still runs;
         * the Book handoff simply does not survive the page change. */
        console.warn('prototype: could not persist structure', err);
    }
}

function load() {
    try {
        const raw = localStorage.getItem(STORAGE_KEY);
        if (raw) return JSON.parse(raw);
    } catch (err) {
        console.warn('prototype: could not read stored structure', err);
    }
    return null;
}

/* --- The version rule ----------------------------------------------- */

/* The one path that changes the deal. It bumps the version and records
 * why, so the header and the audit trail can never disagree. */
function mutate(text) {
    structure.version += 1;
    structure.audit.push({ at: nowIso(), actor: 'marco', text });
    persist();
    render();
    flashVersion();
}

/* Authorisation is metadata about the deal, not a term of it, so it does
 * not touch the version. The audit trail still records it. */
function authorise() {
    const next = structure.authorisation === 'Draft' ? 'Authorised' : 'Draft';
    structure.authorisation = next;
    structure.audit.push({ at: nowIso(), actor: 'marco', text: next });
    persist();
    render();
}

function flashVersion() {
    const chip = document.getElementById('version-chip');
    chip.classList.remove('bump');
    void chip.offsetWidth;
    chip.classList.add('bump');
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

function renderHeader() {
    document.getElementById('struct-name').textContent = structure.name;
    document.getElementById('struct-sub').textContent =
        `${structure.id} · ${structure.counterparty} · ${structure.book} · traded ${structure.tradeDate}`;

    document.getElementById('version-chip').textContent = `v${structure.version}`;

    const authChip = document.getElementById('auth-chip');
    authChip.textContent = structure.authorisation;
    authChip.className = `chip auth-${structure.authorisation.toLowerCase()}`;

    const authBtn = document.getElementById('authorise-btn');
    authBtn.textContent = structure.authorisation === 'Draft' ? 'Authorise' : 'Withdraw';

    document.querySelectorAll('#mode-switch button').forEach(btn => {
        btn.classList.toggle('on', btn.dataset.mode === structure.mode);
    });

    const select = document.getElementById('strategy-select');
    select.hidden = structure.mode !== 'strategy';
    select.innerHTML = STRATEGIES.map(s =>
        `<option value="${esc(s.code)}"${s.code === structure.strategy ? ' selected' : ''}>${esc(s.name)}</option>`
    ).join('');

    document.getElementById('topology-note').textContent =
        'Structure → group → component. Two levels, as the composition rule allows.';
}

/* Why the structure passes. It names the hedge exclusion, because a reader
 * counting components would otherwise expect the invariant to cover them. */
function validNote() {
    if (structure.mode !== 'strategy') return 'Package mode enforces nothing structural.';
    const hedged = structure.groups.some(g => g.hedge);
    return hedged
        ? `${currentStrategy().name} invariant holds on the strategy legs. Hedge legs sit outside it.`
        : `${currentStrategy().name} invariant holds.`;
}

function renderValidation() {
    const banner = document.getElementById('validation');
    const problem = validate();

    if (!problem) {
        banner.className = 'validation ok';
        document.getElementById('validation-what').textContent = 'Valid';
        document.getElementById('validation-why').textContent = validNote();
        return;
    }

    banner.className = 'validation bad';
    document.getElementById('validation-what').textContent = problem.what;
    document.getElementById('validation-why').textContent = problem.why;
}

function componentMeta(component) {
    const f = component.fields;
    const side = f.longShort ? `<span class="side-${f.longShort.toLowerCase()}">${esc(f.longShort)}</span>` : '';
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
        row.addEventListener('click', () => {
            selected = { kind: row.dataset.kind, id: row.dataset.id || structure.id };
            render();
        });
    });
}

function renderEditor() {
    const editor = document.getElementById('editor');
    const removeBtn = document.getElementById('remove-component');

    const component = selected && selected.kind === 'component' ? findComponent(selected.id) : null;
    removeBtn.hidden = !component;

    if (!component) {
        editor.innerHTML = `
            <div class="widget">
                <h3>${esc(structure.name)}</h3>
                <div class="widget-kind">Structure · ${esc(structure.mode)} mode</div>
                <p class="field-note">
                    Select a component to edit it. A change to any component
                    increments the structure version.
                </p>
                <div class="subhead">Components by type</div>
                <table class="legs">
                    <thead><tr><th>ID</th><th>Type</th><th>Label</th><th>Widget</th></tr></thead>
                    <tbody>
                        ${allComponents().map(c => `
                            <tr>
                                <td>${esc(c.id)}</td>
                                <td>${esc(productTypeName(c.productType))}</td>
                                <td>${esc(c.label)}</td>
                                <td>${esc(WIDGET_NAMES[widgetFor(c.productType, c.tradeTypeCode)])}</td>
                            </tr>`).join('')}
                    </tbody>
                </table>
            </div>`;
        return;
    }

    const widget = widgetFor(component.productType, component.tradeTypeCode);
    editor.innerHTML = groupField(component) + renderWidget(widget, component);
    bindWidget(component, widget);
    bindGroupField(component);
}

/* A component sits in exactly one group, and the group it sits in decides
 * whether the strategy invariant covers it. Moving one is a change to the
 * structure, so it takes the version with it. */
function groupOf(component) {
    const group = structure.groups.find(g => g.components.some(c => c.id === component.id));
    return group ? group.id : '';
}

function groupField(component) {
    const here = groupOf(component);
    return `
        <div class="field">
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

/* Each widget is the specialised view for a product family, and the
 * fallback is a real path, not dead code: most trade type codes have no
 * dedicated form. */
function renderWidget(widget, component) {
    const head = `
        <h3>${esc(component.label)}</h3>
        <div class="widget-kind">${esc(productTypeName(component.productType))} ·
            ${esc(component.tradeTypeCode || 'no type code')} · ${esc(WIDGET_NAMES[widget])}</div>`;

    if (widget === 'vanilla') return head + vanillaFields(component);
    if (widget === 'barrier') return head + barrierFields(component);
    if (widget === 'schedule') return head + scheduleFields(component);
    if (widget === 'composite') return head + compositeFields(component);
    return head + genericFields(component);
}

function field(key, label, value, kind, extra) {
    const type = kind || 'text';
    const attrs = extra || '';
    if (type === 'select') return '';
    return `
        <div class="field">
            <label for="f-${esc(key)}">${esc(label)}</label>
            <input id="f-${esc(key)}" data-field="${esc(key)}" type="${type}" value="${esc(value)}" ${attrs}>
        </div>`;
}

function selectField(key, label, value, options) {
    return `
        <div class="field">
            <label for="f-${esc(key)}">${esc(label)}</label>
            <select id="f-${esc(key)}" data-field="${esc(key)}">
                ${options.map(o => `<option${o === value ? ' selected' : ''}>${esc(o)}</option>`).join('')}
            </select>
        </div>`;
}

function vanillaFields(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${selectField('longShort', 'Long / short', f.longShort, ['Long', 'Short'])}
            ${selectField('optionType', 'Option type', f.optionType, ['Call', 'Put'])}
            ${selectField('style', 'Style', f.style, ['European', 'American', 'Bermudan'])}
            ${selectField('settlement', 'Settlement', f.settlement, ['Cash', 'Physical'])}
            ${field('boughtCurrency', 'Bought currency', f.boughtCurrency)}
            ${field('boughtAmount', 'Bought amount', f.boughtAmount, 'number')}
            ${field('soldCurrency', 'Sold currency', f.soldCurrency)}
            ${field('soldAmount', 'Sold amount', f.soldAmount, 'number')}
            ${field('strike', 'Strike', f.strike, 'number', 'step="0.0001"')}
            ${field('notional', 'Notional', f.notional, 'number')}
        </div>

        <div class="subhead">Schedule and expiry</div>
        <div class="field-grid">
            ${field('expiry', 'Expiry', f.expiry, 'date')}
            ${selectField('payOffAtExpiry', 'Pay off at expiry', f.payOffAtExpiry, ['false', 'true'])}
        </div>
        <div class="field full" style="margin-top:10px">
            <label>Exercise dates</label>
            <table class="schedule">
                <thead><tr><th>#</th><th>Date</th><th>Type</th></tr></thead>
                <tbody><tr><td>1</td><td>${esc(f.expiry || '—')}</td><td>Expiry</td></tr></tbody>
            </table>
        </div>
        ${premiumFields(c)}`;
}

function premiumFields(c) {
    const on = c.premium ? '' : 'disabled';
    const p = c.premium || { amount: '', currency: '', payDate: '' };
    return `
        <div class="subhead">Premium — a trade in its own right, not a field on the option</div>
        <div class="legend">The prototype records it here only because the product-type list has
            no premium type yet. The target model gives it its own identifier.</div>
        <div class="field-grid">
            <div class="field"><label>Carries a premium</label>
                <select data-premium="on">
                    <option value="no"${c.premium ? '' : ' selected'}>No</option>
                    <option value="yes"${c.premium ? ' selected' : ''}>Yes</option>
                </select>
            </div>
            ${field('premiumAmount', 'Amount', p.amount, 'number', on)}
            ${field('premiumCurrency', 'Currency', p.currency, '', on)}
            ${field('premiumPayDate', 'Pay date', p.payDate, 'date', on)}
        </div>`;
}

function barrierFields(c) {
    const f = c.fields;
    const levels = [f.barrierLevel, f.barrierLevel2].filter(Boolean);
    return `
        ${vanillaFields(c)}
        <div class="subhead">Barrier management</div>
        <div class="barrier-grid">
            <label for="f-barrierType">Type</label>
            <div class="field">
                <select id="f-barrierType" data-field="barrierType">
                    ${['UpAndIn', 'UpAndOut', 'DownAndIn', 'DownAndOut'].map(t =>
                        `<option${t === f.barrierType ? ' selected' : ''}>${t}</option>`).join('')}
                </select>
            </div>
            <label>Levels</label>
            <div class="level-row">
                ${field('barrierLevel', 'Level 1', f.barrierLevel, 'number', 'step="0.0001"')}
                ${field('barrierLevel2', 'Level 2 (double barrier)', f.barrierLevel2 || '', 'number', 'step="0.0001"')}
            </div>
            <label for="f-rebate">Rebate</label>
            <div>${field('rebate', '', f.rebate, 'number', 'step="0.0001"')}</div>
            <label for="f-fxIndex">FX index</label>
            <div>${field('fxIndex', '', f.fxIndex)}</div>
        </div>
        <div class="legend">${levels.length} level${levels.length === 1 ? '' : 's'} set.</div>`;
}

function scheduleFields(c) {
    const f = c.fields;
    return `
        <div class="field-grid">
            ${selectField('legType', 'Leg type', f.legType, ['Fixed', 'Floating'])}
            ${selectField('payer', 'Payer', f.payer, ['true', 'false'])}
            ${field('currency', 'Currency', f.currency)}
            ${field('notional', 'Notional', f.notional, 'number')}
            ${field('rate', 'Rate', f.rate, 'number', 'step="0.0001"')}
            ${field('dayCounter', 'Day counter', f.dayCounter)}
        </div>

        <div class="subhead">Two-leg schedule</div>
        <div class="field-grid">
            ${field('startDate', 'Start date', f.startDate, 'date')}
            ${field('endDate', 'End date', f.endDate, 'date')}
            ${field('tenor', 'Tenor', f.tenor)}
            ${field('calendar', 'Calendar', f.calendar)}
            ${field('convention', 'Business day convention', f.convention)}
            ${selectField('rule', 'Rule', f.rule, ['Forward', 'Backward'])}
            ${field('paymentConvention', 'Payment convention', f.paymentConvention)}
        </div>

        <div class="subhead">Generated schedule</div>
        <table class="schedule">
            <thead><tr><th>#</th><th>Accrual start</th><th>Accrual end</th></tr></thead>
            <tbody>${scheduleRows(f)}</tbody>
        </table>
        <div class="legend">Rebuilt from the rules above. The leg is priced from this schedule.</div>`;
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

function compositeFields(c) {
    const f = c.fields;
    const legs = c.components || [];
    return `
        <div class="field-grid">
            ${field('currency', 'Currency', f.currency)}
            ${selectField('notionalCalculation', 'Notional calculation', f.notionalCalculation, ['Mean', 'Sum', 'Min', 'Max'])}
            ${field('underlying', 'Underlying', f.underlying)}
            ${field('strike', 'Strike', f.strike, 'number', 'step="0.0001"')}
            ${field('expiry', 'Expiry', f.expiry, 'date')}
            ${field('quantity', 'Quantity', f.quantity, 'number')}
        </div>

        <div class="subhead">Leg list</div>
        <table class="legs">
            <thead><tr><th>Leg</th><th>Type</th><th>Side</th><th>Strike</th><th class="num">Notional</th></tr></thead>
            <tbody>
                ${legs.map(l => `
                    <tr>
                        <td>${esc(l.label)}</td>
                        <td>${esc(l.fields.optionType || '—')}</td>
                        <td><span class="side-${String(l.fields.longShort || '').toLowerCase()}">${esc(l.fields.longShort || '—')}</span></td>
                        <td>${esc(l.fields.strike || '—')}</td>
                        <td class="num">${esc(l.fields.notional || '—')}</td>
                    </tr>`).join('')}
            </tbody>
        </table>
        <div class="legend">A composite is our untyped structure: it bundles legs and reports one NPV.
            The legs keep their own identifiers.</div>`;
}

function genericFields(c) {
    const f = c.fields;
    return `
        <div class="generic-note">
            No dedicated form exists for <strong>${esc(c.tradeTypeCode || 'this type code')}</strong>.
            The generic form captures the reference data by hand. This is the path most
            product types take, and it is the one that proves the picker is extensible.
        </div>

        <div class="subhead">Reference data</div>
        <div class="field-grid">
            ${field('longShort', 'Long / short', f.longShort || '')}
            ${field('notional', 'Notional', f.notional || '', 'number')}
            ${field('currency', 'Currency', f.currency || '')}
            ${field('expiry', 'Expiry', f.expiry || '', 'date')}
            ${field('counterparty', 'Counterparty', f.counterparty || structure.counterparty)}
            ${field('book', 'Book', f.book || structure.book)}
        </div>
        <div class="legend">Fields are free text on this path. The booking still validates
            the notional and the expiry.</div>`;
}

function bindWidget(component, widget) {
    const editor = document.getElementById('editor');

    editor.querySelectorAll('[data-field]').forEach(input => {
        const key = input.dataset.field;
        input.addEventListener('change', () => {
            const before = JSON.stringify(component.fields[key]);
            const value = input.value;
            if (before === JSON.stringify(value)) return;
            component.fields[key] = value;
            component.label = relabel(component);
            mutate(`${component.id} ${key} changed`);
        });
    });

    const premiumToggle = editor.querySelector('[data-premium="on"]');
    if (premiumToggle) {
        premiumToggle.addEventListener('change', () => {
            component.premium = premiumToggle.value === 'yes'
                ? { amount: '0', currency: component.fields.boughtCurrency || '', payDate: '' }
                : null;
            mutate(`${component.id} premium ${premiumToggle.value === 'yes' ? 'added' : 'removed'}`);
        });

        if (component.premium) {
            [['premiumAmount', 'amount'], ['premiumCurrency', 'currency'], ['premiumPayDate', 'payDate']]
                .forEach(([id, key]) => {
                    const el = editor.querySelector(`[data-field="${id}"]`);
                    if (!el) return;
                    el.addEventListener('change', () => {
                        component.premium[key] = el.value;
                        mutate(`${component.id} premium ${key} changed`);
                    });
                });
        }
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
        document.getElementById('provenance').innerHTML =
            `<div><strong>Provenance</strong>Price this structure to capture an initial NPV.</div>`;
        return;
    }

    value.textContent = `${Number(npv.amount).toLocaleString()} ${npv.currency}`;
    document.getElementById('provenance').innerHTML = `
        <div><strong>As of</strong>${esc(npv.asOf)}</div>
        <div><strong>Market data cut</strong>${esc(npv.snapshot)}</div>
        <div><strong>Model</strong>${esc(npv.model)}</div>
        <div><strong>Run</strong>${esc(npv.runId)}</div>
        <div><strong>Source</strong>${esc(npv.source || 'Pricing screen handoff')}</div>`;
}

function renderAudit() {
    document.getElementById('audit').innerHTML = structure.audit.slice().reverse().map(a => `
        <li>
            <span class="when">${esc(shortTime(a.at))}</span>
            <span>${esc(a.text)}</span>
        </li>`).join('');
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

    document.getElementById('picker-note').textContent =
        'Specialised widgets are named. Every other choice opens the generic form.';

    body.querySelectorAll('.pick').forEach(btn => {
        btn.addEventListener('click', () => {
            addComponent(btn.dataset.product, btn.dataset.code);
            document.getElementById('picker-dialog').close();
        });
    });

    document.getElementById('picker-dialog').showModal();
}

function addComponent(productType, tradeTypeCode) {
    if (!structure.groups.length) structure.groups.push({ id: 'G-1', name: 'Components', components: [] });

    const targetGroup = selected && selected.kind === 'group'
        ? findGroup(selected.id)
        : structure.groups[structure.groups.length - 1];

    const component = blankComponent(productType, tradeTypeCode, allComponents());
    targetGroup.components.push(component);
    selected = { kind: 'component', id: component.id };
    mutate(`${component.id} added to ${targetGroup.name}`);
}

function blankComponent(productType, tradeTypeCode, existing) {
    const family = PRODUCT_TYPES.find(p => p.code === productType);
    return {
        id: nextId('T-9', existing),
        productType,
        tradeTypeCode,
        label: `New ${family ? family.name : productType} component`,
        fields: {
            longShort: 'Long', optionType: 'Call', style: 'European', settlement: 'Cash',
            payOffAtExpiry: 'false', expiry: '', strike: '', notional: '',
            currency: 'USD', boughtCurrency: '', soldCurrency: '',
            ...((family && family.defaults) || {})
        },
        premium: null
    };
}

function openFormFromTrades() {
    pendingComponents = [];
    const body = document.getElementById('trades-body');

    body.innerHTML = `
        <p class="legend" style="margin-top:0">
            A structure can be formed from trades already booked. Forming it changes no
            economics until the structure is confirmed; the links are keys, not ownership.
        </p>
        <div class="candidate-list">
            ${existingTrades().map(t => `
                <div class="candidate" data-id="${esc(t.id)}">
                    <input type="checkbox" data-tick="${esc(t.id)}">
                    <span class="type-tag">${esc(productTypeName(t.productType))}</span>
                    <span class="grow">
                        <strong>${esc(t.label)}</strong>
                        <span class="meta">${esc(t.id)} · ${Number(t.notional).toLocaleString()} ${esc(t.ccy)}</span>
                    </span>
                </div>`).join('')}
        </div>`;

    body.querySelectorAll('.candidate').forEach(row => {
        row.addEventListener('click', event => {
            if (event.target.tagName !== 'INPUT') {
                const box = row.querySelector('input');
                box.checked = !box.checked;
            }
            row.classList.toggle('selected', row.querySelector('input').checked);
            syncPending(body);
        });
    });

    document.getElementById('trades-dialog').showModal();
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
    selected = { kind: 'group', id: group.id };
    mutate(`Structure formed from ${chosen.length} existing trades`);

    document.getElementById('trades-dialog').close();
}

function removeComponent() {
    if (!selected || selected.kind !== 'component') return;
    const id = selected.id;
    structure.groups.forEach(g => {
        g.components = g.components.filter(c => c.id !== id);
    });
    selected = { kind: 'structure', id: structure.id };
    mutate(`${id} removed`);
}

function setMode(mode) {
    if (structure.mode === mode) return;
    structure.mode = mode;
    if (mode === 'strategy' && !structure.strategy) structure.strategy = STRATEGIES[0].code;
    selected = { kind: 'structure', id: structure.id };
    mutate(`Mode changed to ${mode}`);
}

function setStrategy(code) {
    if (structure.strategy === code) return;
    structure.strategy = code;
    mutate(`Strategy changed to ${STRATEGIES.find(s => s.code === code).name}`);
}

/* --- Wiring ----------------------------------------------------------- */

function wire() {
    document.getElementById('add-component').addEventListener('click', openPicker);
    document.getElementById('remove-component').addEventListener('click', removeComponent);
    document.getElementById('authorise-btn').addEventListener('click', authorise);
    document.getElementById('form-from-trades').addEventListener('click', openFormFromTrades);
    document.getElementById('trades-confirm').addEventListener('click', formFromTrades);

    document.getElementById('mode-switch').addEventListener('click', event => {
        const btn = event.target.closest('button[data-mode]');
        if (btn) setMode(btn.dataset.mode);
    });

    document.getElementById('strategy-select').addEventListener('change', event => {
        setStrategy(event.target.value);
    });

    document.getElementById('add-group').addEventListener('click', () => {
        const name = prompt('Group name', `Group ${structure.groups.length + 1}`);
        if (!name) return;
        const group = { id: `G-${structure.groups.length + 1}`, name, components: [] };
        structure.groups.push(group);
        selected = { kind: 'group', id: group.id };
        mutate(`Group ${name} added`);
    });

    document.querySelectorAll('dialog [data-close]').forEach(btn => {
        btn.addEventListener('click', () => btn.closest('dialog').close());
    });
}

function boot() {
    structure = load() || openingStructure();
    if (!structure.mode) structure.mode = 'package';
    if (!structure.audit) structure.audit = [];
    if (!structure.groups) structure.groups = [];
    selected = { kind: 'structure', id: structure.id };
    wire();
    render();
}

/* The guard lets the widget renderers be exercised outside a browser, the
 * same way catalogue.js guards its self-check. */
if (typeof document !== 'undefined') boot();

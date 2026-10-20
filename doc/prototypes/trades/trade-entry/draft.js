'use strict';
/* The working copy of a structure, shared by the structure editor and the pricing screen.
 *
 * Both screens change a copy of the structure. Nothing is written until Save, and Save
 * is the Edit pattern: a kind of change, a reason, a commentary, and a claim on the
 * version that was read. A save moves the structure version once, whatever the number
 * of changes in it. Authorisation is a state, and it does not move the version. */

const STORAGE_KEY = 'ores.trade-entry.structure';

/* The editor and the pricing screen are for a person who may change a structure. */
const STRUCTURE_ROLES = ['Trader'];

function nowIso() {
    return new Date().toISOString().replace(/\.\d+Z$/, 'Z');
}

function loadStructure() {
    try {
        const raw = localStorage.getItem(STORAGE_KEY);
        if (raw) return JSON.parse(raw);
    } catch (err) {
        console.warn('prototype: could not read stored structure', err);
    }
    return null;
}

function persistStructure(structure) {
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(structure));
    } catch (err) {
        console.warn('prototype: could not persist structure', err);
    }
}

/* A structure stored by an earlier version of the page has no saved copy, no list of
 * changes and an actor that is not a person of the prototype. */
function normaliseStructure(structure) {
    if (!structure.mode) structure.mode = 'package';
    if (!structure.groups) structure.groups = [];
    if (!structure.audit) structure.audit = [];
    let v = 0;
    structure.audit.forEach((a, i) => {
        if (!a.id) a.id = 'a' + i;
        if (!PEOPLE[a.actor]) a.actor = 'ada.lovelace';
        if (a.kind) return;
        const state = /^(Authorised|Draft)$/.test(a.text);
        a.kind = i === 0 ? 'booked' : state ? 'event' : 'amended';
        if (state) a.noVersion = true;
        else a.version = Math.min(++v, structure.version);
    });
    if (!structure.pending) structure.pending = [];
    if (!structure.saved) structure.saved = JSON.stringify(savedState(structure));
    return structure;
}

function savedState(structure) {
    const copy = JSON.parse(JSON.stringify(structure));
    delete copy.saved;
    delete copy.pending;
    return copy;
}

/* Records a change in the working copy. The same change is listed once. */
function stage(structure, text) {
    if (!structure.pending.includes(text)) structure.pending.push(text);
    persistStructure(structure);
}

/* Save writes the working copy as the next version of the structure. */
function saveStructure(structure, { activity, reason, note }) {
    const from = structure.version;
    const text = structure.pending.join('; ');
    structure.version = from + 1;
    structure.audit.push({
        id: 'a' + structure.audit.length, at: nowIso(), actor: ME, kind: 'amended', what: text,
        version: structure.version, activity, reason, note
    });
    structure.pending = [];
    structure.saved = JSON.stringify(savedState(structure));
    persistStructure(structure);
}

/* Discard puts the saved copy back. */
function discardChanges(structure) {
    const back = JSON.parse(structure.saved);
    Object.keys(structure).forEach(k => delete structure[k]);
    Object.assign(structure, back, { pending: [], saved: JSON.stringify(back) });
    persistStructure(structure);
}

/* The authorisation changes the state and not the version. The history records who and why. */
function setAuthorisation(structure, next, { reason, note }) {
    structure.authorisation = next;
    structure.audit.push({
        id: 'a' + structure.audit.length, at: nowIso(), actor: ME, kind: 'event', what: next === 'Authorised' ? 'Authorised' : 'Authorisation withdrawn',
        noVersion: true, reason, note
    });
    const back = JSON.parse(structure.saved);
    back.authorisation = next;
    back.audit = structure.audit.slice();
    structure.saved = JSON.stringify(back);
    persistStructure(structure);
}

/* The history entries as the timeline draws them. */
function historyEntries(structure) {
    return structure.audit.map(a => Object.assign({ kind: 'event', version: structure.version, reason: '', note: '' }, a, {
        what: a.what || a.text
    }));
}

/* A dialog is closed with its changes only after the person agrees. */
function guardDialog(dialog, isDirty) {
    dialog.addEventListener('cancel', e => {
        if (isDirty() && !confirm('You have unsaved changes. Close anyway?')) e.preventDefault();
    });
    return () => {
        if (isDirty() && !confirm('You have unsaved changes. Close anyway?')) return;
        dialog.close();
    };
}

/* The reason and the commentary of a change, with the claim it makes on the version. */
function changeFields(state, claim) {
    const reasons = REASON_CODES.filter(r => r[0] !== 'trade.no_change');
    const kinds = ['amendment_economic', 'amendment_reference', 'misbooking_correction'];
    return [
        field('What kind of change is this?', select(kinds.map(k => [k, activityType(k).label]), state.activity, v => { state.activity = v; })),
        field('Reason', select(reasons.map(r => [r[0], r[1]]), state.reason, v => { state.reason = v; })),
        field('Commentary', h('textarea', { class: 'field-input', rows: 2, oninput: e => { state.note = e.target.value; } })),
        h('div', { class: 'mono small faint' }, claim)
    ];
}

/* Scratch harness: runs the profile prototype's render() against a minimal
 * DOM shim, so the state matrix can be checked without a browser. It asserts
 * the things the prototype promises a reviewer: every variant, actor and state
 * renders, the read-only panels say why, the picker appears, and the partial
 * write names the record that failed.
 *
 * Run it from the repository root: node <this file> */

const fs = require('fs');
const vm = require('vm');

const src = fs.readFileSync('doc/prototypes/profile/prototype.js', 'utf8');

function run(search) {
    const nodes = {};
    const document = {
        addEventListener() {},
        getElementById(id) {
            if (!nodes[id]) nodes[id] = { innerHTML: '', textContent: '' };
            return nodes[id];
        }
    };
    const sandbox = {
        window: { location: { search } },
        document,
        URLSearchParams,
        console,
        encodeURIComponent
    };
    sandbox.globalThis = sandbox;
    vm.createContext(sandbox);
    vm.runInContext(src, sandbox, { filename: 'prototype.js' });
    return { app: nodes.app.innerHTML, note: nodes['proto-note'].textContent, bar: nodes['proto-bar'].innerHTML };
}

const variants = ['A', 'B', 'C'];
const actors = ['member', 'admin'];
const states = ['view', 'photo', 'saved', 'partial', 'proposed', 'refused'];

let failures = 0;
let checked = 0;

for (const variant of variants) {
    for (const actor of actors) {
        for (const state of states) {
            const search = `?variant=${variant}&actor=${actor}&state=${state}`;
            const out = run(search);
            checked++;
            const where = `${variant}/${actor}/${state}`;
            if (!out.app || out.app.length < 200) {
                console.log(`FAIL ${where}: nothing rendered`);
                failures++;
                continue;
            }
            if (!out.note.includes(`variant ${variant}`)) {
                console.log(`FAIL ${where}: note does not name the variant`);
                failures++;
            }
            if (actor === 'admin' && !['saved', 'partial'].includes(state) && !out.app.includes('Find the person')) {
                console.log(`FAIL ${where}: the administrator gets no person picker`);
                failures++;
            }
            if (actor === 'member' && out.app.includes('Find the person')) {
                console.log(`FAIL ${where}: the member gets the administrator's picker`);
                failures++;
            }
        }
    }
}

/* The promises each state makes, on one variant that renders all of them. */
const checks = [
    ['?variant=A&actor=member&state=view', 'Read-only here', 'the access panel says why it is read-only'],
    ['?variant=A&actor=member&state=view', 'update-self', 'the member is told which subject makes the panel theirs'],
    ['?variant=A&actor=member&state=photo', 'PNG, JPEG or WebP', 'the picker states the rule'],
    ['?variant=A&actor=member&state=photo', 'ores.assets', 'the picker says where the upload goes'],
    ['?variant=A&actor=member&state=saved', 'Contact record', 'the saved state names both records'],
    ['?variant=A&actor=admin&state=partial', 'Contact record', 'the partial state names the records'],
    ['?variant=A&actor=admin&state=partial', 'write_failed', 'the partial state carries the refusal code'],
    ['?variant=A&actor=member&state=proposed', 'Waiting for approval', 'the proposal waits'],
    ['?variant=A&actor=member&state=proposed', 'tenant administrator', 'the proposal names its approvers'],
    ['?variant=A&actor=member&state=refused', 'field_not_self_writable', 'the refusal carries a code'],
    ['?variant=B&actor=member&state=view', 'Save identity', 'variant B saves the identity panel on its own'],
    ['?variant=B&actor=member&state=view', 'Save contact details', 'variant B saves the contact panel on its own'],
    ['?variant=C&actor=member&state=view', 'Contact details', 'variant C draws the rail'],
    ['?variant=C&actor=member&state=photo', 'Photo', 'variant C starts at the photo step']
];

for (const [search, needle, what] of checks) {
    const out = run(search);
    if (!out.app.includes(needle)) {
        console.log(`FAIL ${search}: missing "${needle}" -- ${what}`);
        failures++;
    }
}

console.log(`rendered ${checked} variant/actor/state combinations and ${checks.length} promises`);
console.log(failures === 0 ? 'ALL CHECKS PASSED' : `${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);

/* Harness: runs the account menu prototype's render() against a minimal DOM
 * shim, so the state matrix can be checked without a browser. It asserts what
 * the prototype promises a reviewer: every variant, actor and state renders,
 * each variant puts the administrator's screens where it says it does, the
 * trigger states what it can and cannot show before the account is read, and
 * the current screen is marked.
 *
 * Run it from the repository root: node <this file> */

const fs = require('fs');
const vm = require('vm');

const src = fs.readFileSync('doc/prototypes/account-menu/prototype.js', 'utf8');

function run(search) {
    const nodes = {};
    const document = {
        addEventListener() {},
        getElementById(id) {
            if (!nodes[id]) nodes[id] = { innerHTML: '', textContent: '', className: '' };
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
const states = ['closed', 'open', 'current', 'updated', 'pending', 'narrow'];

let failures = 0;
let checked = 0;

for (const variant of variants) {
    for (const actor of actors) {
        for (const state of states) {
            const where = `${variant}/${actor}/${state}`;
            const out = run(`?variant=${variant}&actor=${actor}&state=${state}`);
            checked++;
            if (!out.app || out.app.length < 200) {
                console.log(`FAIL ${where}: nothing rendered`);
                failures++;
                continue;
            }
            if (!out.note.includes(`variant ${variant}`)) {
                console.log(`FAIL ${where}: the note does not name the variant`);
                failures++;
            }
            const menuShown = out.app.includes('class="menu"');
            const shouldShow = state !== 'closed';
            if (menuShown !== shouldShow) {
                console.log(`FAIL ${where}: the menu is ${menuShown ? 'shown' : 'hidden'} and should be the other way`);
                failures++;
            }
            if (out.app.includes('undefined') || out.app.includes('NaN')) {
                console.log(`FAIL ${where}: the render leaks undefined`);
                failures++;
            }
        }
    }
}

/* Each variant's own claim about where an administrator's screens go. */
const checks = [
    ['?variant=A&actor=admin&state=open', 'nowhere to go', 'variant A admits it has no home for the administrator'],
    ['?variant=A&actor=member&state=open', 'My profile', 'variant A still reaches the member\'s screens'],
    ['?variant=B&actor=admin&state=open', 'This tenant', 'variant B groups the administrator\'s screens'],
    ['?variant=B&actor=admin&state=open', 'Reporting lines', 'variant B reaches the reporting lines'],
    ['?variant=C&actor=admin&state=open', 'class="appnav"', 'variant C puts the tenant\'s surfaces in the header'],
    ['?variant=B&actor=member&state=pending', 'reading the account', 'the trigger says the account is being read'],
    ['?variant=B&actor=member&state=updated', 'Jane Doe', 'the trigger shows the name after the account is read'],
    ['?variant=B&actor=member&state=updated', 'photos/jane_doe.jpeg', 'the trigger shows the photo after the account is read'],
    ['?variant=B&actor=member&state=current', 'you are here', 'the screen in view is marked'],
    ['?variant=B&actor=member&state=narrow', 'class="shell narrow"', 'the narrow state is the narrow shell'],
    ['?variant=B&actor=member&state=open', 'Sign out', 'the menu keeps the way out'],
    ['?variant=B&actor=member&state=open', 'Credentials', 'a screen whose group has not landed is marked, not promised']
];

for (const [search, needle, what] of checks) {
    const out = run(search);
    if (!out.app.includes(needle)) {
        console.log(`FAIL ${search}: missing "${needle}" -- ${what}`);
        failures++;
    }
}

/* Three of the promises are about absence, so state them plainly. */
for (const [search, needle, what] of [
    ['?variant=B&actor=member&state=open', 'This tenant', 'the member must not see the tenant group'],
    ['?variant=C&actor=member&state=open', 'class="appnav"', 'the member must not see a top-level navigation'],
    ['?variant=B&actor=member&state=pending', 'Jane Doe', 'the name must be absent before the account is read']
]) {
    if (run(search).app.includes(needle)) {
        console.log(`FAIL ${search}: found "${needle}" -- ${what}`);
        failures++;
    }
}

console.log(`rendered ${checked} variant/actor/state combinations and ${checks.length} promises`);
console.log(failures === 0 ? 'ALL CHECKS PASSED' : `${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);

/* Harness: runs the shell prototype's render() against a minimal DOM shim, so
 * the four sign-ins can be checked without a browser.
 *
 * It asserts what the prototype promises a reviewer: every sign-in and state
 * renders; the sign-in decides the mode and therefore the whole menu; a menu
 * never offers a journey the person may not run; the avatar carries the
 * journeys about the person; and a journey that owns the screen draws no shell
 * at all.
 *
 * Run it from the repository root: node <this file> */

const fs = require('fs');
const vm = require('vm');

const src = fs.readFileSync('doc/prototypes/navigation/prototype.js', 'utf8');

function run(search) {
    const nodes = {};
    const document = {
        addEventListener() {},
        getElementById(id) {
            if (!nodes[id]) nodes[id] = { innerHTML: '', textContent: '' };
            return nodes[id];
        }
    };
    const sandbox = { window: { location: { search } }, document, URLSearchParams, console, encodeURIComponent };
    sandbox.globalThis = sandbox;
    vm.createContext(sandbox);
    vm.runInContext(src, sandbox, { filename: 'prototype.js' });
    return { app: nodes.app.innerHTML, note: nodes['proto-note'].textContent, bar: nodes['proto-bar'].innerHTML };
}

const whos = ['super', 'tenant', 'privileged', 'regular'];
const states = ['home', 'journey', 'fullscreen'];
const modes = { super: 'System administration', tenant: 'Tenant administration', privileged: 'Application', regular: 'Application' };

let failures = 0;
let checked = 0;

for (const who of whos) {
    for (const state of states) {
        const where = `${who}/${state}`;
        const out = run(`?as=${who}&state=${state}`);
        checked++;
        if (!out.app || out.app.length < 200) {
            console.log(`FAIL ${where}: nothing rendered`);
            failures++;
            continue;
        }
        if (!out.note.includes(modes[who])) {
            console.log(`FAIL ${where}: the note does not name the mode`);
            failures++;
        }
        if (out.app.includes('undefined') || out.app.includes('NaN')) {
            console.log(`FAIL ${where}: the render leaks undefined`);
            failures++;
        }
        const shell = out.app.includes('class="appheader"');
        if (state === 'fullscreen' && shell) {
            console.log(`FAIL ${where}: the shell is drawn around a journey that owns the screen`);
            failures++;
        }
        if (state !== 'fullscreen' && !shell) {
            console.log(`FAIL ${where}: the shell is missing`);
            failures++;
        }
    }
}

/* What each sign-in is, and what it is not. */
const checks = [
    ['?as=super&state=home', 'System administration', 'the super administrator lands in the system mode'],
    ['?as=super&state=home', 'New tenant', 'the system mode holds creating a tenant'],
    ['?as=super&state=home', 'Retire or reset a tenant', 'the system mode holds retiring a tenant'],
    ['?as=tenant&state=home', 'Tenant administration', 'the tenant administrator lands in the tenant mode'],
    ['?as=tenant&state=home', 'New party', 'the tenant mode holds standing up a party'],
    ['?as=tenant&state=home', 'Shape the role catalogue', 'the tenant mode holds the role catalogue'],
    ['?as=privileged&state=home', 'Application', 'a privileged party user lands in the application mode'],
    ['?as=privileged&state=home', 'See who has access', 'the privileged user gets the people of the party'],
    ['?as=privileged&state=home', 'Draw the reporting line', 'the privileged user gets the reporting lines'],
    ['?as=regular&state=home', 'Application', 'a regular party user lands in the application mode'],
    ['?as=regular&state=home&open=1', 'Present myself', 'the avatar carries the journey about the person'],
    ['?as=regular&state=home&open=1', 'Choose where I work', 'the avatar carries where a party user works'],
    ['?as=regular&state=home', 'Reference Data', 'a party user is told an area exists even before it is built'],
    ['?as=super&state=home&open=1', 'Protect my account', 'the avatar carries the person even in the system mode'],
    ['?as=privileged&state=home&area=People', 'Directory', 'an area opens to its cards'],
    ['?as=privileged&state=journey&screen=%2Fpeople', 'See who has access', 'a journey opens under its mode']
];

for (const [search, needle, what] of checks) {
    const out = run(search);
    if (!out.app.includes(needle)) {
        console.log(`FAIL ${search}: missing "${needle}" -- ${what}`);
        failures++;
    }
}

/* The promises that are about absence: the mode decides the menu, and a menu
   never offers what the person may not run. */
const absences = [
    ['?as=super&state=home', 'Reference Data', 'the super administrator is not offered a party area'],
    ['?as=super&state=home', 'People', 'the super administrator is not offered the people of a party'],
    ['?as=super&state=home', 'First run', 'the bootstrap journey is a door, not an area'],
    ['?as=super&state=home', 'Bootstrap', 'the bootstrap area is gone'],
    ['?as=tenant&state=home', 'Bootstrap', 'the tenant administrator is not offered the bootstrap'],
    ['?as=tenant&state=home', 'Reference Data', 'the tenant administrator is not offered a party area'],
    ['?as=regular&state=home', '>People<', 'a regular party user is not offered the people of the party'],
    ['?as=regular&state=home&open=1', 'Audit sign-ins', 'a regular party user is not offered the sign-in audit'],
    ['?as=super&state=home&open=1', 'Choose where I work', 'the system mode has no party to choose'],
    ['?as=regular&state=home', 'class="side"', 'there is no sidebar']
];

for (const [search, needle, what] of absences) {
    if (run(search).app.includes(needle)) {
        console.log(`FAIL ${search}: found "${needle}" -- ${what}`);
        failures++;
    }
}

console.log(`rendered ${checked} sign-in/state combinations and ${checks.length + absences.length} promises`);
console.log(failures === 0 ? 'ALL CHECKS PASSED' : `${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);

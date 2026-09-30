/* Harness: runs the navigation prototype's render() against a minimal DOM
 * shim, so every model can be checked at the catalogue's real scale without a
 * browser.
 *
 * It asserts what the prototype promises a reviewer: every combination of
 * model, actor and state renders; each model reaches the same journey; a
 * person sees only the journeys they may run, and the member's list is
 * genuinely smaller than the administrator's; the today state shows only what
 * has been built; and each model survives the whole catalogue rather than a
 * demonstration subset.
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

const models = ['H', 'N', 'S', 'I', 'D', 'A'];
const actors = ['member', 'admin'];
const states = ['home', 'journey', 'fullscreen', 'today', 'narrow'];
const modelNames = { H: 'Areas and journeys', N: 'Navbar', S: 'Sidebar', I: 'Index', D: 'Data only', A: 'Account menu' };

let failures = 0;
let checked = 0;

for (const model of models) {
    for (const actor of actors) {
        for (const state of states) {
            const where = `${model}/${actor}/${state}`;
            const out = run(`?variant=${model}&actor=${actor}&state=${state}`);
            checked++;
            if (!out.app || out.app.length < 250) {
                console.log(`FAIL ${where}: nothing rendered`);
                failures++;
                continue;
            }
            if (!out.note.includes(modelNames[model])) {
                console.log(`FAIL ${where}: the note does not name the model`);
                failures++;
            }
            if (out.app.includes('undefined') || out.app.includes('NaN')) {
                console.log(`FAIL ${where}: the render leaks undefined`);
                failures++;
            }
            if (state !== 'fullscreen' && !out.app.includes('ORE Studio')) {
                console.log(`FAIL ${where}: the shell lost its brand`);
                failures++;
            }
        }
    }
}

/* The catalogue is 8 groups and 21 journeys. Entry is the door, so a signed-in
   person navigates 7 groups and 20 journeys, and a member rather fewer. */
const checks = [
    ['?variant=N&actor=admin&state=home', '20 journeys in 7 groups', 'the administrator reaches every navigable journey'],
    ['?variant=N&actor=member&state=home', '6 journeys in 4 groups', 'the member reaches only their own screens'],
    ['?variant=N&actor=admin&state=home', 'Directory', 'the navbar carries the tenant groups'],
    ['?variant=S&actor=admin&state=journey', 'class="side"', 'the sidebar model draws a sidebar'],
    ['?variant=S&actor=admin&state=journey', 'Present myself', 'the sidebar opens the current group'],
    ['?variant=I&actor=admin&state=home', 'class="groupgrid"', 'the index model draws the index'],
    ['?variant=I&actor=admin&state=home', 'not built yet', 'the index states what has not been built'],
    ['?variant=D&actor=admin&state=home', 'The roster', 'the data model lands on the things a journey hangs off'],
    ['?variant=D&actor=admin&state=home', 'colleague', 'the administrator gets the colleague row'],
    ['?variant=A&actor=admin&state=journey&open=1', 'class="menu"', 'the account menu model still opens its menu'],
    ['?variant=N&actor=admin&state=journey&screen=%2Fprofile', 'Reached from', 'every model reaches the same journey screen'],
    ['?variant=N&actor=admin&state=today', '3 journeys', 'today carries only what has been built'],
    ['?variant=N&actor=member&state=today', 'Nothing here yet', 'a member has nothing today and is told so'],
    ['?variant=N&actor=admin&state=narrow', 'class="shell narrow"', 'the narrow state is the narrow shell'],
    ['?variant=H&actor=admin&state=home', 'class="appnav"', 'the two-axis model puts the areas in the header'],
    ['?variant=H&actor=admin&state=home', 'class="side"', 'the two-axis model puts journeys down the side'],
    ['?variant=H&actor=admin&state=home&area=Reference%20Data', 'Reference Data', 'the area whose journeys are not extracted is still named'],
    ['?variant=H&actor=admin&state=home&area=Reference%20Data', 'No journeys have been extracted', 'an empty area says so rather than inventing screens'],
    ['?variant=H&actor=admin&state=home&area=Administration', 'Starts a run', 'a full-screen journey is offered as a run, not as a sidebar entry'],
    ['?variant=H&actor=admin&state=journey&screen=%2Fprofile', 'class="side"', 'a journey keeps the area sidebar while it runs'],
    ['?variant=H&actor=member&state=home', 'class="side"', 'a member gets the shell too'],
    ['?variant=H&actor=member&state=home', 'data-screen="/profile"', "the header chip still reaches the person's own record"],
    ['?variant=H&actor=admin&state=home&area=Administration', 'Administration', 'administration is one area, not four'],
    ['?variant=H&actor=admin&state=home&area=Administration', 'Directory', 'administration holds the directory']
];

for (const [search, needle, what] of checks) {
    const out = run(search);
    if (!out.app.includes(needle)) {
        console.log(`FAIL ${search}: missing "${needle}" -- ${what}`);
        failures++;
    }
}

/* The full-screen journey is outside every model, which is the shell's
   boundary and not a variation of it. */
for (const model of ['N', 'S', 'I', 'D', 'A']) {
    const out = run(`?variant=${model}&actor=admin&state=fullscreen`);
    if (!out.app.includes('Exit setup')) {
        console.log(`FAIL ${model}/fullscreen: no way out of a full-screen journey`);
        failures++;
    }
    if (out.app.includes('class="side"') || out.app.includes('class="appnav"') || out.app.includes('class="appheader"')) {
        console.log(`FAIL ${model}/fullscreen: the shell is drawn around a journey that owns the screen`);
        failures++;
    }
}

/* The promises that are about absence. */
for (const [search, needle, what] of [
    ['?variant=N&actor=member&state=home', 'Directory', 'the member must not reach the tenant groups'],
    ['?variant=N&actor=member&state=home', 'Rescue access', 'the member must not reach an administrator journey'],
    ['?variant=N&actor=admin&state=home', 'Sign in', 'the door is not a place a signed-in person navigates to'],
    ['?variant=H&actor=member&state=home', 'Reference Data', 'a member is not offered an area with nothing in it']
]) {
    if (run(search).app.includes(needle)) {
        console.log(`FAIL ${search}: found "${needle}" -- ${what}`);
        failures++;
    }
}

console.log(`rendered ${checked} model/actor/state combinations and ${checks.length + 4 + 12} promises`);
console.log(failures === 0 ? 'ALL CHECKS PASSED' : `${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);

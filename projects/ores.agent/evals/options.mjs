// SPDX-License-Identifier: GPL-3.0-or-later
const switches = new Set(['strict', 'require-handling', 'live-read', 'help']);
const valued = new Set(['profile', 'repeat', 'cases', 'corpus', 'out', 'config', 'credentials', 'party', 'other-party', 'inference-root', 'min-pass', 'timeout-ms']);

export function parseEvaluationOptions(argv) {
    const options = { profile: 'replay', repeat: 1 };
    const seen = new Set();
    for (let index = 0; index < argv.length; index++) {
        const argument = argv[index];
        const name = argument.replace(/^--/, '');
        if (!argument.startsWith('--') || !switches.has(name) && !valued.has(name) || seen.has(name)) {
            throw Error('Unknown or duplicate evaluation option. Use --help.');
        }
        seen.add(name);
        if (switches.has(name)) options[name] = true;
        else {
            const value = argv[++index];
            if (!value || value.startsWith('--')) throw Error('Missing evaluation option value.');
            options[name] = value;
        }
    }
    return options;
}

export const evaluationHelp = `npm run eval -- --profile replay|model|live [--cases id,id] [--corpus PATH] [--repeat 1..5]
    [--timeout-ms 100..120000] [--out NEW_DIRECTORY] [--strict --min-pass 0..1 --require-handling]
model: --config AGENT_CONFIG [--inference-root ROOT]
live: --live-read --config AGENT_CONFIG --credentials PRIVATE_JSON --party UUID
    [--other-party UUID] [--inference-root ROOT]`;

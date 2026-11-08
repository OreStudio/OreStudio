// Explicit local integration check: stops/restarts only the two supplied AI services.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
const exec = promisify(execFile);
const [appRoot, inferenceRoot, credentialsPath, partyId] = process.argv.slice(2);
if (!appRoot || !inferenceRoot || !credentialsPath || !partyId) throw Error('Pass app root, inference root, private credentials JSON and party UUID.');
const appSpec = JSON.parse(await readFile(join(appRoot, 'service.json'), 'utf8'));
const inferenceSpec = JSON.parse(await readFile(join(inferenceRoot, 'service.json'), 'utf8'));
assert.equal(appSpec.component, 'ores.agent'); assert.equal(inferenceSpec.component, 'llama.cpp');
const controller = join(appSpec.cwd, 'releases', appSpec.manifestPath.split(/[\\/]/).at(-2), 'service.mjs');
async function control(action, root) { await exec(process.execPath, [controller, action, join(root, 'service.json')], { timeout: 40000 }); }
const base = new URL(appSpec.healthUrl).origin;
const backendUrl = 'http://127.0.0.1:20002/'; // Existing development reference deployment.
const credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
let appStopped = false, inferenceStopped = false;
try {
    assert.equal((await fetch(backendUrl)).status, 200);
    await control('stop', appRoot); appStopped = true;
    await assert.rejects(() => fetch(appSpec.healthUrl));
    assert.equal((await fetch(inferenceSpec.healthUrl)).status, 200);
    assert.equal((await fetch(backendUrl)).status, 200);
    await control('start', appRoot); appStopped = false;
    await control('stop', inferenceRoot); inferenceStopped = true;
    assert.equal((await fetch(appSpec.healthUrl)).status, 200);
    assert.equal((await fetch(base + '/readyz')).status, 200);
    const loginResponse = await fetch(base + '/auth/login', { method: 'POST', body: JSON.stringify({ principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId }) });
    assert.equal(loginResponse.status, 200);
    const login = await loginResponse.json(), headers = { authorization: 'Bearer ' + login.accessToken };
    try {
        const read = await fetch(base + '/tools/list_currencies', { method: 'POST', headers, body: '{"limit":1}' });
        assert.equal(read.status, 200); assert.equal((await read.json()).rows.length, 1);
        const chat = await fetch(base + '/chat', { method: 'POST', headers, body: '{"message":"List one currency."}' });
        assert.equal(chat.status, 503);
    } finally { await fetch(base + '/auth/logout', { method: 'POST', headers }); }
    assert.equal((await fetch(backendUrl)).status, 200);
    console.log(JSON.stringify({ independentAppStop: true, independentInferenceStop: true, toolsWithoutInference: true, chatFailureWithoutInference: 503, backendHealthy: true }));
} finally {
    if (inferenceStopped) await control('start', inferenceRoot);
    if (appStopped) await control('start', appRoot);
}

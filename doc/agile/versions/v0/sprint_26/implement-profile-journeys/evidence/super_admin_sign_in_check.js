/* Harness: signs the super administrator in through the running BFF and
 * photographs the landing the browser draws from that session.
 *
 * It is a live check, not a unit test. It drives Chrome over the DevTools
 * protocol against a started deployment, so what it proves is what a person
 * sees: the login answer states the mode, the shell states it back, the
 * Tenants area is offered, and the bootstrap journey is not.
 *
 * Expectations: the services are up and the system is provisioned. The
 * defaults are this environment's own; override them in the environment.
 *
 * Run it from the repository root:
 *   node <this file> */

const { spawn } = require('child_process');
const fs = require('fs');

const APP = process.env.ORES_WEB_URL ?? 'http://127.0.0.1:21402';
const PORT = Number(process.env.ORES_CHROME_DEBUG_PORT ?? 9333);
const OUT = process.env.ORES_SHOT ?? 'build/evidence/super_admin_landing.png';
const CREDENTIALS = {
    username: process.env.ORES_SUPER_ADMIN ?? 'super_admin',
    password: process.env.ORES_SUPER_ADMIN_PASSWORD ?? 'Secure-Password-123',
};

function sleep(ms) { return new Promise((r) => setTimeout(r, ms)); }

async function targetUrl() {
    for (let attempt = 0; attempt < 60; attempt++) {
        try {
            const answer = await fetch(`http://127.0.0.1:${PORT}/json/list`);
            const pages = (await answer.json()).filter((t) => t.type === 'page');
            if (pages.length > 0) return pages[0].webSocketDebuggerUrl;
        } catch {
            /* Chrome is not listening yet. */
        }
        await sleep(250);
    }
    throw new Error('Chrome never opened its debugging port');
}

function connect(url) {
    const ws = new WebSocket(url);
    let next = 0;
    const pending = new Map();
    ws.addEventListener('message', (event) => {
        const message = JSON.parse(event.data);
        const waiter = pending.get(message.id);
        if (waiter !== undefined) {
            pending.delete(message.id);
            message.error
                ? waiter.reject(new Error(JSON.stringify(message.error)))
                : waiter.resolve(message.result);
        }
    });
    const ready = new Promise((resolve, reject) => {
        ws.addEventListener('open', resolve);
        ws.addEventListener('error', reject);
    });
    const send = (method, params = {}) =>
        new Promise((resolve, reject) => {
            const id = ++next;
            pending.set(id, { resolve, reject });
            ws.send(JSON.stringify({ id, method, params }));
        });
    return { ready, send, close: () => ws.close() };
}

(async () => {
    const chrome = spawn('google-chrome', [
        '--headless=new',
        '--disable-gpu',
        '--no-sandbox',
        '--hide-scrollbars',
        `--remote-debugging-port=${PORT}`,
        '--user-data-dir=/tmp/ev_chrome',
        '--window-size=1440,1000',
        'about:blank',
    ], { stdio: 'ignore' });

    let failures = 0;
    const check = (ok, what) => {
        console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`);
        if (!ok) failures++;
    };

    try {
        const client = connect(await targetUrl());
        await client.ready;
        await client.send('Page.enable');
        await client.send('Runtime.enable');

        await client.send('Page.navigate', { url: `${APP}/login` });
        await sleep(1500);

        const login = await client.send('Runtime.evaluate', {
            expression: `(async () => {
                const answer = await fetch('/api/session', {
                    method: 'POST',
                    headers: { 'content-type': 'application/json' },
                    body: JSON.stringify(${JSON.stringify(CREDENTIALS)}),
                });
                return { status: answer.status, body: await answer.json() };
            })()`,
            awaitPromise: true,
            returnByValue: true,
        });
        const outcome = login.result.value;
        check(outcome.status === 200, `the login answers 200 (got ${outcome.status})`);
        check(
            outcome.body?.session?.mode === 'system-administration',
            `the login answer states the mode (${outcome.body?.session?.mode})`,
        );

        await client.send('Page.navigate', { url: `${APP}/` });
        await sleep(2000);

        const draw = await client.send('Runtime.evaluate', {
            expression: 'document.body.innerText',
            returnByValue: true,
        });
        const text = draw.result.value ?? '';
        check(text.includes('System administration'), 'the page states the mode');
        check(text.includes('Tenants'), 'the page offers the Tenants area');
        check(text.includes('New tenant'), 'the page offers New tenant');
        check(text.includes('Retire or reset a tenant'), 'the page offers the tenant retirement');
        check(!text.includes('First run'), 'the page does not offer First run');
        check(!text.includes('Bootstrap'), 'the page does not offer a Bootstrap area');

        const shot = await client.send('Page.captureScreenshot', { format: 'png' });
        fs.writeFileSync(OUT, Buffer.from(shot.data, 'base64'));
        console.log(`wrote ${OUT}`);
        client.close();
    } finally {
        chrome.kill();
    }

    console.log(failures === 0 ? 'ALL CHECKS PASSED' : `${failures} FAILURES`);
    process.exit(failures === 0 ? 0 : 1);
})();

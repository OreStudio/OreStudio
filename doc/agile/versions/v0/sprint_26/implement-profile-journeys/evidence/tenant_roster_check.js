/* Harness: reads the tenant roster through the running BFF, and photographs the
 * screen the browser draws from it.
 *
 * It is a live check, not a unit test. It signs in as the super administrator
 * against a started deployment, so what it proves is what a person sees: the
 * roster answers, the system tenant is not in it, the columns are the ones the
 * journey document names, and a caller with no session is refused.
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
const OUT = process.env.ORES_SHOT ?? 'build/evidence/tenant_roster.png';
const CREDENTIALS = {
    username: process.env.ORES_SUPER_ADMIN ?? 'super_admin',
    password: process.env.ORES_SUPER_ADMIN_PASSWORD ?? 'Secure-Password-123',
};
const SYSTEM_TENANT = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

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
    let failures = 0;
    const check = (ok, what) => {
        console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`);
        if (!ok) failures++;
    };

    /* The refusal is asserted outside the browser, because it is about a caller
       that carries no session at all. */
    const anonymous = await fetch(`${APP}/api/tenants`);
    check(anonymous.status === 401, `a caller with no session is refused (${anonymous.status})`);

    const chrome = spawn('google-chrome', [
        '--headless=new',
        '--disable-gpu',
        '--no-sandbox',
        '--hide-scrollbars',
        `--remote-debugging-port=${PORT}`,
        '--user-data-dir=/tmp/ev_chrome_tenants',
        '--window-size=1920,1080',
        'about:blank',
    ], { stdio: 'ignore' });

    try {
        const client = connect(await targetUrl());
        await client.ready;
        await client.send('Page.enable');
        await client.send('Runtime.enable');

        await client.send('Page.navigate', { url: `${APP}/login` });
        await sleep(1500);

        const read = await client.send('Runtime.evaluate', {
            expression: `(async () => {
                const login = await fetch('/api/session', {
                    method: 'POST',
                    headers: { 'content-type': 'application/json' },
                    body: JSON.stringify(${JSON.stringify(CREDENTIALS)}),
                });
                if (login.status !== 200) return { login: login.status };
                const answer = await fetch('/api/tenants');
                return {
                    login: login.status,
                    roster: answer.status,
                    body: await answer.json(),
                };
            })()`,
            awaitPromise: true,
            returnByValue: true,
        });
        const value = read.result.value ?? {};
        check(value.login === 200, `the login answers 200 (got ${value.login})`);
        check(value.roster === 200, `the roster answers 200 (got ${value.roster})`);
        const tenants = value.body?.tenants ?? [];
        check(tenants.length > 0, 'the roster names at least one tenant');
        check(
            tenants.every((tenant) => tenant.id !== SYSTEM_TENANT),
            'the system tenant is not in the roster',
        );
        check(
            tenants.every(
                (tenant) =>
                    typeof tenant.code === 'string' &&
                    typeof tenant.name === 'string' &&
                    typeof tenant.hostname === 'string' &&
                    typeof tenant.type === 'string' &&
                    typeof tenant.status === 'string',
            ),
            'every row carries the five columns the journey names',
        );

        await client.send('Page.navigate', { url: `${APP}/tenants` });
        await sleep(2000);

        const draw = await client.send('Runtime.evaluate', {
            expression: `({
                text: document.body.innerText,
                rows: [...document.querySelectorAll('table tbody tr')].map((row) => row.innerText),
            })`,
            returnByValue: true,
        });
        const text = draw.result.value?.text ?? '';
        const rows = draw.result.value?.rows ?? [];
        /* The headers are drawn in upper case, so the check is case-folded. */
        const folded = text.toLowerCase();
        check(text.includes('Tenants'), 'the page names the area');
        check(folded.includes('hostname'), 'the page names the hostname column');
        check(folded.includes('status'), 'the page names the status column');
        check(
            rows.length === tenants.length,
            `the table holds one row per tenant (${rows.length} of ${tenants.length})`,
        );
        check(
            tenants.some((tenant) => text.includes(tenant.name)),
            'the page names the tenant the read answered with',
        );
        /*
         * The shell's own session line names the tenant the session acts in, and
         * for a super administrator that is the system tenant. What must not
         * happen is a system tenant row in the roster itself.
         */
        check(
            rows.every((row) => !row.includes('Root Tenant')),
            'the roster holds no row for the system tenant',
        );
        check(text.includes('New tenant'), 'the page offers the journey that creates a tenant');

        /*
         * The table is the page. A card around it makes it narrower than the
         * screen the shell gave it, which is the whole of what a list-shaped
         * screen asks for.
         */
        const boxed = await client.send('Runtime.evaluate', {
            expression: `(() => {
                const table = document.querySelector('table');
                return table === null ? null : table.closest('.card') !== null;
            })()`,
            returnByValue: true,
        });
        check(boxed.result.value === false, 'the table is not wrapped in a card');

        const width = await client.send('Runtime.evaluate', {
            expression: `(() => {
                const table = document.querySelector('table');
                return table === null ? null : Math.round(table.getBoundingClientRect().width);
            })()`,
            returnByValue: true,
        });
        console.log(`      the table draws ${width.result.value}px wide`);

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

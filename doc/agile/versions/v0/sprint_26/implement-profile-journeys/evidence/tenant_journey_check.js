/* Harness: walks the New tenant journey in a real browser against the running
 * BFF, and photographs the details step it reaches.
 *
 * It is a live check, not a unit test. Two defects are what it exists for: the
 * banner drew at the full width of a wide display on a signed-in journey,
 * because only the public shell bounded its column; and a starting point that
 * offers to hand the creating administrator's password on left the details step
 * with a disabled Continue, because this journey holds no such password.
 *
 * Expectations: the services are up and the system is provisioned. The defaults
 * are this environment's own; override them in the environment.
 *
 * Run it from the repository root:
 *   node <this file> */

const { spawn } = require('child_process');
const fs = require('fs');

const APP = process.env.ORES_WEB_URL ?? 'http://127.0.0.1:21402';
const PORT = Number(process.env.ORES_CHROME_DEBUG_PORT ?? 9333);
const OUT = process.env.ORES_SHOT ?? 'build/evidence/tenant_details.png';
const CREDENTIALS = {
    username: process.env.ORES_SUPER_ADMIN ?? 'super_admin',
    password: process.env.ORES_SUPER_ADMIN_PASSWORD ?? 'Secure-Password-123',
};
const TYPED_PASSWORD = 'Typed-Password-123';
const WIDTH_BOUND = 1100;

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
        '--user-data-dir=/tmp/ev_chrome_tenant',
        '--window-size=1600,1000',
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
        const evaluate = async (expression) => {
            const result = await client.send('Runtime.evaluate', {
                expression,
                awaitPromise: true,
                returnByValue: true,
            });
            return result.result.value;
        };

        await client.send('Page.navigate', { url: `${APP}/login` });
        await sleep(1500);
        const login = await evaluate(`(async () => {
            const answer = await fetch('/api/session', {
                method: 'POST',
                headers: { 'content-type': 'application/json' },
                body: JSON.stringify(${JSON.stringify(CREDENTIALS)}),
            });
            return answer.status;
        })()`);
        check(login === 200, `the login answers 200 (got ${login})`);

        /* The profile this harness is about is the one that offers to hand the
           creating administrator's password on. */
        const profile = await evaluate(`(async () => {
            const answer = await fetch('/api/seed-profiles');
            const body = await answer.json();
            return body.profiles.find((p) => p.inheritsAdminPassword) ?? null;
        })()`);
        check(profile !== null, 'a starting point offers to inherit the creating password');

        await client.send('Page.navigate', { url: `${APP}/tenants/new` });
        await sleep(2500);

        const banner = await evaluate(`(() => {
            const splash = [...document.querySelectorAll('img')]
                .find((image) => (image.currentSrc || image.src).includes('splash'));
            if (splash === undefined) return null;
            return {
                width: splash.getBoundingClientRect().width,
                viewport: window.innerWidth,
            };
        })()`);
        check(banner !== null, 'the journey draws the banner');
        check(
            banner !== null && banner.width <= WIDTH_BOUND,
            `the banner is bounded (${banner?.width}px of ${banner?.viewport}px)`,
        );

        const chosen = await evaluate(`(() => {
            const wanted = ${JSON.stringify(profile?.name ?? '')};
            const card = [...document.querySelectorAll('button')]
                .find((button) => button.textContent.includes(wanted));
            if (card === undefined) return false;
            card.click();
            return true;
        })()`);
        check(chosen, `the starting point "${profile?.name}" is offered and chosen`);

        await sleep(600);
        const advanced = await evaluate(`(() => {
            const next = [...document.querySelectorAll('button')]
                .find((button) => button.textContent.trim() === 'Continue');
            if (next === undefined || next.disabled) return false;
            next.click();
            return true;
        })()`);
        check(advanced, 'the first step moves on to the details step');

        await sleep(700);
        const field = await evaluate(`(() => {
            const inputs = document.querySelectorAll('input[type="password"]');
            return inputs.length;
        })()`);
        check(field === 2, `the details step asks for a password and its confirmation (${field})`);

        /* The field reports a password as usable only when the confirmation
           matches it, so both inputs are typed. */
        const typed = await evaluate(`(() => {
            const inputs = [...document.querySelectorAll('input[type="password"]')];
            if (inputs.length !== 2) return false;
            const setter = Object.getOwnPropertyDescriptor(
                window.HTMLInputElement.prototype, 'value').set;
            for (const input of inputs) {
                setter.call(input, ${JSON.stringify(TYPED_PASSWORD)});
                input.dispatchEvent(new Event('input', { bubbles: true }));
            }
            return true;
        })()`);
        check(typed, 'the password and its confirmation are typed');

        await sleep(700);
        const footer = await evaluate(`(() => {
            const buttons = [...document.querySelectorAll('button')].map((button) => ({
                text: button.textContent.trim(),
                disabled: button.disabled,
            }));
            const next = [...document.querySelectorAll('button')]
                .find((button) => button.textContent.trim() === 'Continue');
            return { ready: next === undefined ? null : !next.disabled, buttons };
        })()`);
        if (footer.ready !== true) {
            console.log('      buttons: ' + JSON.stringify(footer.buttons));
        }
        check(footer.ready === true, 'the details step moves on once the password is typed');

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

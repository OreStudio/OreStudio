// SPDX-License-Identifier: GPL-3.0-or-later
import { randomBytes } from 'node:crypto';
import type { ServerResponse } from 'node:http';

export function serveUi(res: ServerResponse) {
    const nonce = randomBytes(18).toString('base64');
    res.writeHead(200, {
        'content-type': 'text/html; charset=utf-8',
        'cache-control': 'no-store',
        'content-security-policy': `default-src 'none'; script-src 'nonce-${nonce}'; style-src 'nonce-${nonce}'; connect-src 'self'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'`,
        'x-content-type-options': 'nosniff',
        'referrer-policy': 'no-referrer',
    });
    res.end(`<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>OreStudio assistant</title>
<style nonce="${nonce}">
:root {
    color-scheme: dark;
    --page: #101a26;
    --panel: #172536;
    --border: #40546b;
    --text: #e5edf7;
    --muted: #afbed1;
    --accent: #91d7c5;
    font: 16px/1.55 system-ui, sans-serif;
}
* { box-sizing: border-box; }
[hidden] { display: none !important; }
body { margin: 0; background: var(--page); color: var(--text); }
h1, h2, p { margin: 0; }
h1 { font-size: 1.125rem; }
h2 { font-size: clamp(1.5rem, 4vw, 2rem); }
small, .muted { color: var(--muted); }
button, input, textarea { font: inherit; }
button {
    border: 1px solid transparent; border-radius: 8px; padding: 7px 14px;
    background: var(--accent); color: #10232b; font-weight: 600; cursor: pointer;
}
button.secondary { background: var(--panel); color: var(--text); border-color: var(--border); }
button:disabled { opacity: .55; cursor: default; }
:focus-visible { outline: 2px solid var(--accent); outline-offset: 3px; }
input, textarea { width: 100%; background: var(--panel); color: inherit; }
#signin { max-width: 440px; margin: 10vh auto 0; padding: 0 20px 24px; }
#signin h1 { font-size: 1.5rem; }
#login { display: grid; gap: 14px; margin: 24px 0 12px; }
#login input { display: block; border: 1px solid var(--border); border-radius: 8px; padding: 8px 12px; margin-top: 4px; }
#workspace { height: 100vh; height: 100dvh; display: grid; grid-template-rows: auto minmax(0, 1fr) auto; }
.topbar {
    display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between;
    gap: 8px 20px; border-bottom: 1px solid var(--border); padding: 12px clamp(12px, 4vw, 36px);
}
#identity { display: block; overflow-wrap: anywhere; }
.actions { display: flex; gap: 8px; }
#transcript { overflow-y: auto; overscroll-behavior: contain; }
.thread, .compose { width: min(100%, 960px); margin: 0 auto; padding-inline: clamp(12px, 4vw, 28px); }
.thread { padding-block: 24px; }
#messages:empty { padding-block: 0; }
#empty { display: grid; min-height: 100%; align-content: center; text-align: center; gap: 8px; padding-block: 24px; }
.turn { margin-bottom: 24px; }
.speaker { font-size: .8rem; font-weight: 700; margin-bottom: 6px; color: var(--muted); }
.user { display: grid; justify-items: end; }
.user-message {
    max-width: min(88%, 680px); padding: 10px 16px; background: #243c4a;
    border: 1px solid #385668; border-radius: 16px 16px 4px 16px;
    white-space: pre-wrap; overflow-wrap: anywhere;
}
.assistant { min-width: 0; }
.assistant.pending .markdown { color: var(--muted); }
.error { color: #ffb7ab; }
.error:empty { display: none; }
details { margin-top: 10px; color: var(--muted); font-size: .875rem; }
summary { cursor: pointer; width: fit-content; max-width: 100%; }
pre { overflow: auto; background: var(--panel); padding: 14px; border-radius: 8px; }
.reasoning pre { max-height: 18rem; white-space: pre-wrap; overflow-wrap: anywhere; }
.compose { padding-block: 8px 12px; }
#chat {
    display: flex; align-items: flex-end; gap: 10px; padding: 8px;
    border: 1px solid var(--border); border-radius: 14px; background: var(--panel);
}
#chat:focus-within { border-color: var(--accent); }
#message { display: block; flex: 1; min-width: 0; max-height: 28vh; resize: vertical; border: 0; padding: 6px; outline: 0; }
#message::placeholder { color: var(--muted); }
#chat-status { margin-bottom: 6px; }
#composer-hint { display: block; margin-top: 6px; text-align: center; font-size: .75rem; }
.markdown { overflow-x: auto; overflow-wrap: anywhere; }
.markdown > :first-child { margin-top: 0; }
.markdown > :last-child { margin-bottom: 0; }
.markdown h1, .markdown h2, .markdown h3, .markdown h4, .markdown h5, .markdown h6 { line-height: 1.3; margin: 1.2em 0 .5em; color: #bcebdd; }
.markdown h1 { font-size: 1.6em; }
.markdown h2 { font-size: 1.35em; }
.markdown h3 { font-size: 1.15em; }
.markdown p { margin: .75em 0; }
.markdown table { border-collapse: collapse; width: 100%; font-size: .9em; margin: 16px 0; font-variant-numeric: tabular-nums; }
.markdown th, .markdown td { border: 1px solid #52677e; padding: 8px 12px; text-align: left; vertical-align: top; white-space: nowrap; overflow-wrap: normal; }
.markdown th { background: #263c50; color: #c5f2e6; }
.markdown tbody tr:nth-child(even) { background: #1c2e41; }
.markdown code { font: .9em ui-monospace, monospace; background: #25394e; padding: 2px 5px; border-radius: 3px; }
.markdown pre code { padding: 0; background: none; }
.markdown blockquote { border-left: 3px solid var(--accent); margin: 16px 0; padding: 0 16px; color: #c4d2e3; }
.markdown a { color: #a7d7ff; }
.markdown li + li { margin-top: 4px; }
.markdown hr { border: 0; border-top: 1px solid #52677e; margin: 24px 0; }
.draft-editor { margin-top: 16px; }
.draft-editor fieldset { border: 1px solid var(--border); border-radius: 8px; padding: 12px; min-width: 0; }
.draft-fields { display: grid; grid-template-columns: repeat(auto-fit, minmax(min(100%, 220px), 1fr)); gap: 12px; }
.draft-fields label { font-size: .9em; }
.draft-fields input { display: block; border: 1px solid var(--border); border-radius: 6px; padding: 6px 8px; margin-top: 4px; }
.draft-actions { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 12px; }
.report-progress { margin-top: 16px; padding-top: 12px; border-top: 1px solid var(--border); }
.report-progress button { margin-top: 8px; }
</style>
</head>
<body>
<main id="signin">
    <h1>OreStudio assistant</h1>
    <p class="muted">Local access to your selected party’s data and reviewed NPV report runs.</p>
    <form id="login">
        <label>Account<input id="principal" autocomplete="username" required></label>
        <label>Password<input id="password" type="password" autocomplete="current-password" required></label>
        <label>Party ID <small>(optional)</small><input id="party" placeholder="Leave blank to use your default"></label>
        <button id="login-button">Sign in</button>
    </form>
    <p id="login-status" class="error" role="status"></p>
</main>
<main id="workspace" hidden>
    <header class="topbar">
        <div><h1>OreStudio assistant</h1><small id="identity"></small></div>
        <div class="actions">
            <button id="reset" class="secondary" type="button" disabled>New chat</button>
            <button id="logout" class="secondary" type="button">Sign out</button>
        </div>
    </header>
    <div id="transcript" tabindex="0" aria-label="Conversation">
        <div id="empty" class="thread">
            <h2>What would you like to look up?</h2>
            <p class="muted">Ask about trades, books, report runs, or stored market data.</p>
        </div>
        <div id="messages" class="thread" role="log" aria-label="Chat messages" aria-relevant="additions text"></div>
    </div>
    <footer class="compose">
        <p id="chat-status" class="error" role="status"></p>
        <form id="chat">
            <textarea id="message" aria-label="Message OreStudio" aria-describedby="composer-hint" maxlength="16000" rows="2" placeholder="Message OreStudio…" required></textarea>
            <button id="send" type="submit">Send</button>
            <button id="stop" class="secondary" type="button" hidden>Stop</button>
        </form>
        <small id="composer-hint">Enter to send · Shift+Enter for a new line</small>
    </footer>
</main>
<script nonce="${nonce}">
const el = id => document.getElementById(id);
const composer = el('message');
const messages = el('messages');
const transcript = el('transcript');
let token = '';
let request;
let draftEditor;
let resetting = false;
let signingOut = false;
const reportWatches = new Map();

async function api(path, data, signal, onReasoning) {
    const response = await fetch(path, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            ...(onReasoning ? { Accept: 'application/x-ndjson' } : {}),
            ...(token ? { Authorization: 'Bearer ' + token } : {}),
        },
        body: JSON.stringify(data),
        signal,
    });
    if (onReasoning && response.headers.get('content-type')?.includes('application/x-ndjson'))
        return readChatStream(response, onReasoning);
    const result = await response.json();
    if (!response.ok) {
        throw Object.assign(new Error(result.error || 'Request failed.'), { status: response.status });
    }
    return result;
}

async function readChatStream(response, onReasoning) {
    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let pending = '';
    try {
        while (true) {
            const { value, done } = await reader.read();
            if (done) throw new Error('The chat connection ended before a reply arrived.');
            const lines = (pending + decoder.decode(value, { stream: true })).split('\\n');
            pending = lines.pop();
            for (const line of lines) {
                if (!line) continue;
                const event = JSON.parse(line);
                if (event.type === 'reasoning') onReasoning(event.text);
                else if (event.type === 'result') return event.result;
                else if (event.type === 'error')
                    throw Object.assign(new Error(event.error), { status: event.status });
            }
        }
    } finally {
        await reader.cancel().catch(() => {});
        reader.releaseLock();
    }
}

function syncControls() {
    el('send').disabled = Boolean(request || resetting || signingOut);
    el('send').hidden = Boolean(request);
    el('stop').hidden = !request;
    el('stop').disabled = Boolean(request?.signal.aborted || signingOut);
    el('reset').disabled = Boolean(request || resetting || signingOut || !messages.childElementCount);
    if (draftEditor) draftEditor.elements[0].disabled = Boolean(request || resetting || signingOut);
}

function scrollToLatest() {
    transcript.scrollTop = transcript.scrollHeight;
}

function clearChat() {
    for (const stop of reportWatches.values()) stop();
    messages.replaceChildren();
    draftEditor = undefined;
    el('empty').hidden = false;
    composer.value = '';
    el('chat-status').textContent = '';
    syncControls();
}

function renderEvidence(parent, rows) {
    if (!rows?.length) return;
    const details = document.createElement('details');
    const summary = document.createElement('summary');
    summary.textContent = 'OreStudio tool evidence';
    const evidence = document.createElement('pre');
    evidence.textContent = JSON.stringify(rows, null, 2);
    details.append(summary, evidence);
    parent.append(details);
}

function watchReport(turn, runId) {
    reportWatches.get(runId)?.('Watching moved to the latest reply.');
    const controller = new AbortController();
    const section = document.createElement('section');
    section.className = 'report-progress';
    const status = document.createElement('div');
    status.className = 'markdown';
    status.setAttribute('aria-live', 'polite');
    status.textContent = 'Following report progress…';
    const evidence = document.createElement('div');
    const note = document.createElement('div');
    const stopButton = document.createElement('button');
    stopButton.type = 'button';
    stopButton.className = 'secondary';
    stopButton.textContent = 'Stop watching';
    section.append(status, evidence, note, stopButton);
    turn.append(section);
    let timer;
    let lastHtml;
    const until = Date.now() + 300000;
    const stop = message => {
        clearTimeout(timer);
        controller.abort();
        if (reportWatches.get(runId) === stop) reportWatches.delete(runId);
        stopButton.remove();
        if (message) note.textContent = message;
    };
    const schedule = () => {
        if (controller.signal.aborted) return;
        if (Date.now() >= until) stop('Automatic status checks ended. Say “Follow run ' + runId + '” to resume.');
        else timer = setTimeout(poll, 5000);
    };
    const poll = async () => {
        try {
            const result = await api('/chat/report-progress', { runId }, controller.signal);
            if (controller.signal.aborted) return;
            if (result.answerHtml !== lastHtml) {
                status.innerHTML = result.answerHtml;
                lastHtml = result.answerHtml;
                evidence.replaceChildren();
                renderEvidence(evidence, result.evidence);
            }
            note.textContent = '';
            if (!result.watching) return stop();
        } catch (error) {
            if (controller.signal.aborted) return;
            if (error.status === 401) return showSignIn(error.message);
            if (error.status === 404) return stop(error.message);
            note.textContent = 'Status check is temporarily unavailable. Trying again…';
        }
        schedule();
    };
    stopButton.onclick = () => stop('Stopped watching. Check OreStudio for the latest run status.');
    reportWatches.set(runId, stop);
    schedule();
}

function showSignIn(message = '') {
    token = '';
    el('workspace').hidden = true;
    el('identity').textContent = '';
    clearChat();
    el('signin').hidden = false;
    el('login-status').textContent = message;
    el('principal').focus();
}

function appendMessage(author, className, text) {
    const turn = document.createElement('article');
    turn.className = 'turn ' + className;
    const speaker = document.createElement('div');
    speaker.className = 'speaker';
    speaker.textContent = author;
    const content = document.createElement('div');
    content.className = className === 'user' ? 'user-message' : 'markdown';
    content.textContent = text;
    turn.append(speaker, content);
    messages.append(turn);
    return { turn, content };
}

function renderDraft(turn, draft) {
    draftEditor?.remove();
    draftEditor = undefined;
    if (!draft) return;
    const form = document.createElement('form');
    form.className = 'draft-editor';
    const fieldset = document.createElement('fieldset');
    const legend = document.createElement('legend');
    legend.textContent = draft.productName ? 'Edit ' + draft.productName + ' draft' : 'Edit FX-forward draft';
    const fields = document.createElement('div');
    fields.className = 'draft-fields';
    for (const [key, title] of Object.entries(draft.labels)) {
        const label = document.createElement('label');
        label.textContent = title;
        const input = document.createElement('input');
        input.name = key;
        input.value = draft.fields[key] ?? '';
        input.maxLength = 200;
        input.autocomplete = 'off';
        label.append(input);
        fields.append(label);
    }
    const actions = document.createElement('div');
    actions.className = 'draft-actions';
    const update = document.createElement('button');
    update.textContent = 'Update draft';
    const discard = document.createElement('button');
    discard.type = 'button';
    discard.className = 'secondary';
    discard.textContent = 'Discard draft';
    actions.append(update, discard);
    if (draft.confirmation) {
        const book = document.createElement('button');
        book.type = 'button';
        book.textContent = 'Confirm booking';
        form.addEventListener('input', () => { book.disabled = true; });
        book.onclick = () => void sendChat({ message: draft.confirmation }, 'Confirm booking');
        actions.append(book);
    }
    fieldset.append(legend, fields, actions);
    form.append(fieldset);
    form.onsubmit = event => {
        event.preventDefault();
        const values = Object.fromEntries(new FormData(form));
        void sendChat({ bookingDraft: { action: 'edit', id: draft.id, revision: draft.revision, fields: values } }, 'Update draft');
    };
    discard.onclick = () => void sendChat({ bookingDraft: { action: 'discard', id: draft.id, revision: draft.revision } }, 'Discard draft');
    draftEditor = form;
    turn.append(form);
}

el('login').onsubmit = async event => {
    event.preventDefault();
    const button = el('login-button');
    if (button.disabled) return;
    button.disabled = true;
    el('login-status').textContent = '';
    try {
        const partyId = el('party').value.trim();
        const result = await api('/auth/login', {
            principal: el('principal').value,
            password: el('password').value,
            ...(partyId ? { partyId } : {}),
        });
        token = result.accessToken;
        el('password').value = '';
        el('signin').hidden = true;
        el('identity').textContent = result.username + ' · party ' + result.partyId;
        el('workspace').hidden = false;
        composer.focus();
    } catch (error) {
        el('login-status').textContent = error.message;
    } finally {
        button.disabled = false;
    }
};

el('logout').onclick = async () => {
    signingOut = true;
    el('logout').disabled = true;
    request?.abort();
    syncControls();
    try {
        await api('/auth/logout', {});
    } catch {
        // Clear the local session even if the server is unreachable.
    } finally {
        signingOut = false;
        el('logout').disabled = false;
        showSignIn();
    }
};

el('reset').onclick = async () => {
    if (request || resetting || signingOut) return;
    const sessionToken = token;
    resetting = true;
    el('chat-status').textContent = '';
    syncControls();
    try {
        await api('/chat/reset', {});
        if (token === sessionToken && !signingOut) {
            clearChat();
            composer.focus();
        }
    } catch (error) {
        if (token !== sessionToken || signingOut) return;
        if (error.status === 401) showSignIn(error.message);
        else el('chat-status').textContent = error.message;
    } finally {
        resetting = false;
        syncControls();
    }
};

composer.onkeydown = event => {
    if (event.key !== 'Enter' || event.shiftKey || event.isComposing) return;
    event.preventDefault();
    el('chat').requestSubmit();
};

el('chat').onsubmit = event => {
    event.preventDefault();
    const message = composer.value.trim();
    if (message) void sendChat({ message }, message);
};

async function sendChat(payload, label) {
    if (request || resetting || signingOut) return;
    const sessionToken = token;
    const controller = new AbortController();
    request = controller;
    if (payload.message) composer.value = '';
    el('empty').hidden = true;
    el('chat-status').textContent = '';
    appendMessage('You', 'user', label);
    const { turn, content } = appendMessage('OreStudio', 'assistant', 'Working…');
    turn.classList.add('pending');
    syncControls();
    composer.focus();
    scrollToLatest();

    let reasoningText;
    const showReasoning = text => {
        if (token !== sessionToken || signingOut || controller.signal.aborted) return;
        const following = transcript.scrollHeight - transcript.scrollTop - transcript.clientHeight < 48;
        if (!reasoningText) {
            const panel = document.createElement('details');
            panel.className = 'reasoning';
            panel.open = true;
            const label = document.createElement('summary');
            label.textContent = 'Model reasoning';
            const body = document.createElement('pre');
            reasoningText = document.createTextNode('');
            body.append(reasoningText);
            panel.append(label, body);
            content.before(panel);
        }
        const body = reasoningText.parentElement;
        const atBottom = body.scrollHeight - body.scrollTop - body.clientHeight < 24;
        reasoningText.appendData(text);
        if (atBottom) body.scrollTop = body.scrollHeight;
        if (following) scrollToLatest();
    };
    try {
        const result = await api('/chat', payload, controller.signal, showReasoning);
        if (token !== sessionToken || signingOut) return;
        const following = transcript.scrollHeight - transcript.scrollTop - transcript.clientHeight < 48;
        if (typeof result.answerHtml === 'string') content.innerHTML = result.answerHtml;
        else content.textContent = result.answer;
        if (Object.hasOwn(result, 'bookingDraft')) renderDraft(turn, result.bookingDraft);
        renderEvidence(turn, result.evidence);
        if (result.reportFollow) watchReport(turn, result.reportFollow);
        if (following) scrollToLatest();
    } catch (error) {
        if (token !== sessionToken || signingOut) return;
        if (error.status === 401) {
            showSignIn(error.message);
            return;
        }
        content.classList.add('error');
        content.textContent = error.name === 'AbortError' ? 'Stopped.' : error.message;
        if (error.status === 409 && payload.bookingDraft) {
            draftEditor?.remove();
            draftEditor = undefined;
        }
        if (payload.message && !composer.value.trim()) composer.value = payload.message;
    } finally {
        turn.classList.remove('pending');
        request = undefined;
        syncControls();
    }
}

el('stop').onclick = () => {
    request?.abort();
    composer.focus();
    syncControls();
};
</script>
</body>
</html>`);
}

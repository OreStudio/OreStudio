// SPDX-License-Identifier: GPL-3.0-or-later
import { randomBytes } from 'node:crypto';
import type { ServerResponse } from 'node:http';
export function serveUi(res: ServerResponse) {
    const nonce = randomBytes(18).toString('base64');
    res.writeHead(200, { 'content-type': 'text/html; charset=utf-8', 'cache-control': 'no-store',
        'content-security-policy': `default-src 'none'; script-src 'nonce-${nonce}'; style-src 'nonce-${nonce}'; connect-src 'self'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'`,
        'x-content-type-options': 'nosniff', 'referrer-policy': 'no-referrer' });
    res.end(`<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>OreStudio assistant</title><style nonce="${nonce}">
body{font:16px/1.6 system-ui,sans-serif;background:#101a26;color:#e5edf7;max-width:850px;margin:40px auto;padding:0 24px}h1{font-size:28px}label{display:block;margin-top:12px}input,textarea,select{box-sizing:border-box;width:100%;padding:10px;background:#1c2b3c;color:inherit;border:1px solid #607185;border-radius:6px}button{padding:10px 18px;margin:12px 8px 12px 0;background:#91d7c5;color:#10232b;border:0;border-radius:6px;cursor:pointer}button:disabled{opacity:.5}pre{white-space:pre-wrap;overflow-wrap:anywhere;font:14px/1.6 ui-monospace,monospace;background:#1c2b3c;padding:16px;border-radius:6px}small{color:#afbed1}.error{color:#ffb7ab}[hidden]{display:none!important}
</style><h1>OreStudio assistant</h1><p>Local, read-only access to your selected party’s data.</p>
<form id="login"><label>Account<input id="principal" autocomplete="username" required></label>
<label>Password<input id="password" type="password" autocomplete="current-password" required></label>
<label>Party ID <small>(leave empty to use your default; sign-in lists available parties if needed)</small><input id="party" placeholder="Optional party UUID"></label><button>Sign in</button></form>
<section id="workspace" hidden><p id="identity"></p><button id="logout">Sign out</button>
<form id="chat"><label>Ask OreStudio<textarea id="message" maxlength="2000" rows="4" required placeholder="List the first three configured currencies."></textarea></label><button id="send">Ask</button><button id="cancel" type="button" disabled>Cancel</button></form>
<small>Model answers can be wrong. Check the OreStudio evidence below. Quotes and booking are unavailable when the backend has no supporting data or operation.</small>
<pre id="answer" aria-live="polite"></pre><details><summary>OreStudio tool evidence</summary><pre id="evidence"></pre></details></section>
<p id="status" class="error" role="status"></p><script nonce="${nonce}">
const el=id=>document.getElementById(id);let token='',request;
async function api(path,data,signal){const r=await fetch(path,{method:'POST',headers:{'Content-Type':'application/json',...(token?{Authorization:'Bearer '+token}:{})},body:JSON.stringify(data),signal});const result=await r.json();if(!r.ok)throw Error(result.error||'Request failed');return result}
el('login').onsubmit=async e=>{e.preventDefault();el('status').textContent='';try{const partyId=el('party').value.trim();const r=await api('/auth/login',{principal:el('principal').value,password:el('password').value,...(partyId?{partyId}:{})});token=r.accessToken;el('password').value='';el('login').hidden=true;el('workspace').hidden=false;el('identity').textContent=r.username+' · party '+r.partyId}catch(e){el('status').textContent=e.message}};
el('logout').onclick=async()=>{request?.abort();try{await api('/auth/logout',{})}finally{token='';el('workspace').hidden=true;el('login').hidden=false;el('answer').textContent='';el('evidence').textContent=''}};
el('chat').onsubmit=async e=>{e.preventDefault();request=new AbortController();el('send').disabled=true;el('cancel').disabled=false;el('status').textContent='';el('answer').textContent='Working…';el('evidence').textContent='';try{const r=await api('/chat',{message:el('message').value},request.signal);el('answer').textContent=r.answer;el('evidence').textContent=JSON.stringify(r.evidence,null,2)}catch(e){el('answer').textContent='';el('status').textContent=e.name==='AbortError'?'Cancelled.':e.message}finally{el('send').disabled=false;el('cancel').disabled=true}};
el('cancel').onclick=()=>request?.abort();
</script></html>`);
}

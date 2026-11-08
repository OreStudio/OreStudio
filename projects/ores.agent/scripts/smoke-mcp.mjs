import { readFile } from 'node:fs/promises';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
const [base, credentialsPath, partyId] = process.argv.slice(2);
if (!base || !credentialsPath || !partyId) throw Error('Pass agent base URL, private credentials JSON and party UUID.');
const c = JSON.parse(await readFile(credentialsPath, 'utf8'));
const response = await fetch(base + '/auth/login', { method: 'POST', body: JSON.stringify({ principal: c.tenantUsername, password: c.tenantPassword, partyId }) });
const login = await response.json();
if (!response.ok) throw Error(JSON.stringify(login));
const headers = { Authorization: 'Bearer ' + login.accessToken };
const client = new Client({ name: 'ores-independent-smoke', version: '1.0.0' });
try {
    await client.connect(new StreamableHTTPClientTransport(new URL(base + '/mcp'), { requestInit: { headers } }));
    console.log(JSON.stringify({ tools: (await client.listTools()).tools.map(t => t.name) }));
    console.log(JSON.stringify(await client.callTool({ name: 'list_currencies', arguments: { limit: 3 } }), null, 2));
} finally { await client.close(); await fetch(base + '/auth/logout', { method: 'POST', headers }); }

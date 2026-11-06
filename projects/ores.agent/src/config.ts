import { readFile } from 'node:fs/promises';

export interface Config {
    host: string;
    port: number;
    allowedOrigins: string[];
    nats: null | { server: string; prefix: string; ca: string; cert: string; key: string };
    inference: { baseUrl: string; model: string; timeoutMs: number };
}

export function validateConfig(raw: unknown): Config {
    const c = raw as Config;
    if (!c || !['127.0.0.1', '::1'].includes(c.host) || !Number.isInteger(c.port) || c.port < 0 || c.port > 65535) {
        throw new Error('Configure a loopback host and a valid port; use a TLS reverse proxy for remote access.');
    }
    if (!Array.isArray(c.allowedOrigins) || c.allowedOrigins.some(x => typeof x !== 'string')) throw new Error('allowedOrigins must be an array of origins.');
    if (c.nats !== null && (!c.nats || ['server', 'prefix', 'ca', 'cert', 'key'].some(k => typeof (c.nats as any)[k] !== 'string' || !(c.nats as any)[k]))) throw new Error('Incomplete NATS configuration.');
    if (!c.inference || !/^https?:\/\//.test(c.inference.baseUrl) || !c.inference.model || !Number.isInteger(c.inference.timeoutMs) || c.inference.timeoutMs < 100) throw new Error('Invalid inference configuration.');
    return c;
}

export async function loadConfig(path: string): Promise<Config> {
    return validateConfig(JSON.parse(await readFile(path, 'utf8')));
}

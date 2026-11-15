// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from 'node:fs/promises';
import { z } from 'zod';
import { pythonSandboxConfig } from './python-sandbox.js';

function httpUrl(value: string): URL | undefined {
    try {
        const url = new URL(value);
        if (
            !['http:', 'https:'].includes(url.protocol) ||
            url.username ||
            url.password ||
            url.search ||
            url.hash
        )
            return;
        return url;
    } catch {
        return;
    }
}
const origin = z.string().refine((value) => {
    const url = httpUrl(value);
    return url !== undefined && value === url.origin;
}, 'Use an HTTP(S) origin with no path, credentials, query or fragment.');
const loopback = ['127.0.0.1', 'localhost', '[::1]'];
const configSchema = z
    .object({
        deployment: z.literal('container').optional(),
        host: z.enum(['127.0.0.1', '::1', '0.0.0.0']),
        port: z.number().int().min(0).max(65535),
        allowedOrigins: z.array(origin),
        pythonSandbox: pythonSandboxConfig.optional(),
        nats: z
            .object({
                server: z.string().min(1),
                prefix: z.string().min(1),
                ca: z.string().min(1),
                cert: z.string().min(1),
                key: z.string().min(1),
                tlsServerName: z.hostname().optional(),
            })
            .strict()
            .nullable(),
        inference: z
            .object({
                baseUrl: z.string(),
                model: z.string().min(1),
                timeoutMs: z.number().int().min(100).max(120000),
                apiKeyFile: z.string().min(1).optional(),
                maxOutputTokens: z.number().int().min(128).max(8192).optional(),
            })
            .strict(),
    })
    .strict()
    .superRefine((config, ctx) => {
        const container = config.deployment === 'container';
        if (config.host === '0.0.0.0' && !container)
            ctx.addIssue({
                code: 'custom',
                path: ['host'],
                message: 'Use a loopback bind address.',
            });
        const url = httpUrl(config.inference.baseUrl);
        if (
            !url ||
            (url.protocol !== 'https:' &&
                !loopback.includes(url.hostname) &&
                !(container && url.hostname === 'llama'))
        )
            ctx.addIssue({
                code: 'custom',
                path: ['inference', 'baseUrl'],
                message:
                    'Use HTTPS, loopback HTTP, or the private llama service in container mode.',
            });
    });
export type Config = z.infer<typeof configSchema>;

export function validateConfig(raw: unknown): Config {
    return configSchema.parse(raw);
}

export async function loadConfig(path: string): Promise<Config> {
    return validateConfig(JSON.parse(await readFile(path, 'utf8')));
}

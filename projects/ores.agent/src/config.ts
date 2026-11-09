// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from 'node:fs/promises';
import { z } from 'zod';

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
const inferenceUrl = z.string().refine((value) => {
    const url = httpUrl(value);
    return (
        url !== undefined &&
        (url.protocol === 'https:' || ['127.0.0.1', 'localhost', '[::1]'].includes(url.hostname))
    );
}, 'Use loopback HTTP or HTTPS with no credentials, query or fragment.');
const configSchema = z
    .object({
        host: z.enum(['127.0.0.1', '::1']),
        port: z.number().int().min(0).max(65535),
        allowedOrigins: z.array(origin),
        nats: z
            .object({
                server: z.string().min(1),
                prefix: z.string().min(1),
                ca: z.string().min(1),
                cert: z.string().min(1),
                key: z.string().min(1),
            })
            .strict()
            .nullable(),
        inference: z
            .object({
                baseUrl: inferenceUrl,
                model: z.string().min(1),
                timeoutMs: z.number().int().min(100).max(120000),
                apiKeyFile: z.string().min(1).optional(),
                maxOutputTokens: z.number().int().min(128).max(8192).optional(),
            })
            .strict(),
    })
    .strict();
export type Config = z.infer<typeof configSchema>;

export function validateConfig(raw: unknown): Config {
    return configSchema.parse(raw);
}

export async function loadConfig(path: string): Promise<Config> {
    return validateConfig(JSON.parse(await readFile(path, 'utf8')));
}

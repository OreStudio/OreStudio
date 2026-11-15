// SPDX-License-Identifier: GPL-3.0-or-later
import { spawn } from 'node:child_process';
import { isAbsolute } from 'node:path';
import { z } from 'zod';
import { AppError } from './backend.js';

const absolutePath = z.string().max(1024).refine(isAbsolute, 'Use an absolute path.');
export const pythonSandboxConfig = z
    .object({ executable: absolutePath, module: absolutePath })
    .strict();
export const pythonTool = {
    description:
        'Use after read tools to compute custom statistics, group rows by currency, or transform retrieved fields. For example, pass trade economics amounts and currencies as data, group by currency in Python and return the requested statistics. This does not require a valuation report. Use calculate for simple exact decimal arithmetic. Python has only the standard library, with no access to OreStudio, host files or the network. Keep exact decimals and large integers as strings.',
    schema: z
        .object({
            code: z
                .string()
                .min(1)
                .max(4096)
                .describe('Read the data variable and assign a JSON-compatible value to result.'),
            data: z
                .unknown()
                .describe(
                    'JSON from the user or an authorized tool result. Pass null if the snippet needs no input.',
                ),
        })
        .strict(),
};

// Only this fixed program is passed as an argument to Wasmtime. Model code and
// data travel over stdin. The WASI guest receives neither mounts nor host env.
const bootstrap = `
import contextlib, json, sys

request = json.load(sys.stdin)
scope = {'data': request['data']}
with contextlib.redirect_stdout(sys.stderr):
    exec(compile(request['code'], '<analysis>', 'exec'), scope)
print(json.dumps(scope['result'], allow_nan=False, separators=(',', ':')))
`;
const outputLimit = 8192;

export class PythonSandbox {
    private busy = false;

    constructor(private config: z.infer<typeof pythonSandboxConfig>) {}

    async run(input: z.infer<typeof pythonTool.schema>, signal?: AbortSignal) {
        signal?.throwIfAborted();
        const payload = JSON.stringify({ code: input.code, data: input.data ?? null });
        if (Buffer.byteLength(payload) > 16384)
            throw new AppError('python_input_limit', 'Python input exceeds 16 KiB.', 413);
        if (this.busy) throw new AppError('python_busy', 'The Python worker is busy.', 429);
        this.busy = true;
        try {
            const output = await this.runWorker(payload, signal);
            signal?.throwIfAborted();
            const value: unknown = JSON.parse(output, (_key, value: unknown) => {
                if (
                    typeof value === 'number' &&
                    (!Number.isFinite(value) ||
                        (Number.isInteger(value) && !Number.isSafeInteger(value)))
                )
                    throw new AppError(
                        'python_result_precision',
                        'Return large numbers as strings.',
                        422,
                    );
                return value;
            });
            return { tool: 'run_python', status: 'ok', value };
        } catch (error) {
            signal?.throwIfAborted();
            if (error instanceof SyntaxError || error instanceof RangeError)
                throw new AppError(
                    'invalid_python_result',
                    'Python must return a JSON result.',
                    422,
                );
            throw error;
        } finally {
            this.busy = false;
        }
    }

    private runWorker(payload: string, signal?: AbortSignal): Promise<string> {
        return new Promise((resolve, reject) => {
            const child = spawn(
                this.config.executable,
                [
                    'run',
                    '-C',
                    'cache=n',
                    '-W',
                    'fuel=1000000000,max-memory-size=67108864',
                    '-S',
                    'inherit-network=n,allow-ip-name-lookup=n,tcp=n,udp=n,inherit-env=n',
                    '--',
                    this.config.module,
                    '-I',
                    '-c',
                    bootstrap,
                ],
                { env: {}, windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'] },
            );
            const stdout: Buffer[] = [];
            let size = 0;
            let failure: unknown;
            const stop = (error: unknown) => {
                failure ??= error;
                child.kill('SIGKILL');
            };
            const abort = () => stop(signal?.reason);
            const timer = setTimeout(
                () => stop(new AppError('python_timeout', 'Python ran out of time.', 422)),
                5000,
            );
            signal?.addEventListener('abort', abort, { once: true });
            if (signal?.aborted) abort();
            const consume = (chunk: Buffer, retain: boolean) => {
                size += chunk.length;
                if (size > outputLimit) {
                    if (!failure)
                        stop(
                            new AppError(
                                'python_output_limit',
                                'Python produced too much output.',
                                422,
                            ),
                        );
                } else if (retain) stdout.push(chunk);
            };
            child.stdout.on('data', (chunk: Buffer) => consume(chunk, true));
            child.stderr.on('data', (chunk: Buffer) => consume(chunk, false));
            child.stdin.on('error', () => {
                /* The process exit determines the result. */
            });
            child.on('error', () => {
                failure ??= new AppError(
                    'python_unavailable',
                    'The Python worker could not start.',
                    503,
                );
            });
            child.once('close', (code) => {
                clearTimeout(timer);
                signal?.removeEventListener('abort', abort);
                if (failure) reject(failure);
                else if (code !== 0)
                    reject(
                        new AppError(
                            'python_failed',
                            'Python failed or exceeded its execution limits.',
                            422,
                        ),
                    );
                else resolve(Buffer.concat(stdout).toString('utf8'));
            });
            child.stdin.end(payload);
        });
    }
}

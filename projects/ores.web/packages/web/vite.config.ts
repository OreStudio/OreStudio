/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { defineConfig, loadEnv } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

/**
 * The browser bundle.
 *
 * The BFF serves the built bundle in production, so `/api` is proxied for the
 * development server only: one origin and a first-party session cookie, and no
 * CORS configuration in the build. The BFF port is read from the same variable
 * the BFF reads, so there is one number rather than two that can disagree.
 */
/**
 * The build this bundle came from, stated the way the services state theirs:
 * the release, then the commit, then whether the tree was clean.
 *
 * It is stamped in at build time rather than asked for at run time, because
 * the point of a client version is to say what the *browser* is running: a
 * person comparing it with the version the server answers with is looking for
 * exactly the case this deployment hit once already, where a cached bundle
 * served a screen the server no longer matched.
 *
 * The release is read from the project's own declaration, so there is one
 * place the number is written. A checkout without git, or a shallow one, still
 * builds: the commit is then simply absent.
 */
interface BuildStamp {
    readonly release: string;
    readonly commit: string;
    readonly dirty: boolean;
}

function buildStamp(checkout: string): BuildStamp {
    const cmake = readFileSync(resolve(checkout, 'CMakeLists.txt'), 'utf8');
    const release = /project\(\s*\w+\s+VERSION\s+([0-9.]+)/.exec(cmake)?.[1] ?? 'unknown';
    const version = `v${release}`;
    try {
        const commit = execFileSync('git', ['rev-parse', '--short', 'HEAD'], {
            cwd: checkout,
            encoding: 'utf8',
        }).trim();
        const dirty =
            execFileSync('git', ['status', '--porcelain'], {
                cwd: checkout,
                encoding: 'utf8',
            }).trim().length > 0;
        return { release: version, commit, dirty };
    } catch {
        return { release: version, commit: '', dirty: false };
    }
}

/** The stamp as the one line the footer states. */
function buildVersion(stamp: BuildStamp): string {
    if (stamp.commit === '') {
        return stamp.release;
    }
    return `${stamp.release} (${stamp.commit}${stamp.dirty ? '-dirty' : ''})`;
}

export default defineConfig(({ mode }) => {
    /*
     * The checkout's `.env` is the one place those ports are declared. Vite loads
     * only `VITE_`-prefixed variables by itself, and these belong to the service,
     * so they are read from the file directly. The directory is derived from this
     * file rather than from the working directory, because npm runs the workspace
     * script from the package and the file is four levels up.
     *
     * An empty prefix also makes `loadEnv` merge the whole of `process.env` into
     * its result, and merge it last, so a variable exported by the caller wins
     * over the file. Reading `process.env` again here would be dead code.
     */
    const checkout = resolve(import.meta.dirname, '../../../..');
    const env = loadEnv(mode, checkout, '');
    const BFF_PORT = env['ORES_WEB_PORT'] ?? '8080';
    const WEB_PORT = Number(env['ORES_WEB_DEV_PORT'] ?? '5173');
    const stamp = buildStamp(checkout);

    return {
        plugins: [react(), tailwindcss()],
        define: {
            __BUILD_VERSION__: JSON.stringify(buildVersion(stamp)),
            __BUILD_RELEASE__: JSON.stringify(stamp.release),
            __BUILD_COMMIT__: JSON.stringify(stamp.commit),
            __BUILD_DIRTY__: JSON.stringify(stamp.dirty),
        },
        server: {
            port: WEB_PORT,
            // Fail rather than slide to another port: a dev server that quietly moves
            // is a dev server whose URL somebody writes down and then cannot reach.
            strictPort: true,
            proxy: {
                '/api': {
                    target: `http://127.0.0.1:${BFF_PORT}`,
                    changeOrigin: false,
                },
            },
        },
        build: {
            outDir: 'dist',
            sourcemap: true,
        },
    };
});

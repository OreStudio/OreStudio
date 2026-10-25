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

import { defineConfig, devices } from '@playwright/test';
import { webUrl } from './src/env.js';

/**
 * The harness drives the environment's web, not a server it starts: the
 * checkout's .env names the port, as it does for compass open-as, and the
 * services must be up. Traces and screenshots of a failing test land under
 * the component's runtime folder, which git ignores.
 */
export default defineConfig({
    testDir: './tests',
    outputDir: '../../.runtime/harness/results',
    fullyParallel: false,
    retries: 0,
    workers: 1,
    reporter: [
        ['list'],
        ['html', { outputFolder: '../../.runtime/harness/report', open: 'never' }],
    ],
    use: {
        baseURL: webUrl(),
        trace: 'retain-on-failure',
        screenshot: 'only-on-failure',
        locale: 'en-GB',
    },
    projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});

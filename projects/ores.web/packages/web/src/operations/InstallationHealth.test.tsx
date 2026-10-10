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
 */

import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { ServiceRosterRow } from '@ores/wire-protocol/browser';
import type { LogsRange } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import {
    InstallationFigures,
    SERVICES_QUERY_KEY,
    logCountKey,
    releaseLabel,
    summariseRoster,
} from './InstallationHealth.js';

/**
 * The installation figures: the roster in counts, the logs in totals, and what
 * the figures say when one of the reads is missing.
 */

function row(overrides: Partial<ServiceRosterRow> = {}): ServiceRosterRow {
    return {
        service_name: 'ores.iam.service',
        display_name: 'IAM Service',
        description: '',
        service_account: null,
        slot: 1,
        state: 'running',
        instance_id: '91b0f33d-4b32-4f65-8809-2d3e4f506172',
        host_id: null,
        version: '0.0.27',
        sampled_at: '2026-10-10 11:00:00Z',
        age_seconds: 6,
        ...overrides,
    };
}

const roster: readonly ServiceRosterRow[] = [
    row({ service_name: 'ores.iam.service' }),
    row({ service_name: 'ores.dq.service' }),
    row({ service_name: 'ores.web.service', version: '0.0.27' }),
    row({ service_name: 'ores.telemetry.service', state: 'lost', age_seconds: 900 }),
    row({
        service_name: 'ores.inbox.service',
        state: 'missing',
        instance_id: null,
        version: null,
        age_seconds: null,
    }),
    row({ service_name: 'ores.compute.wrapper', slot: 1 }),
    row({ service_name: 'ores.compute.wrapper', slot: 2, state: 'missing', version: null }),
];

function render(options: {
    readonly roster?: readonly ServiceRosterRow[];
    readonly errors?: number;
    readonly warnings?: number;
    readonly range?: LogsRange;
    readonly logsFailed?: boolean;
    readonly rosterFailed?: boolean;
}): string {
    const range = options.range ?? '1h';
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    if (options.rosterFailed === true) {
        const query = client.getQueryCache().build(client, { queryKey: SERVICES_QUERY_KEY });
        query.setState({
            ...query.state,
            status: 'error',
            error: new Error('the roster could not be read'),
            fetchStatus: 'idle',
        });
    }
    if (options.logsFailed === true) {
        for (const level of ['error', 'warn'] as const) {
            const query = client.getQueryCache().build(client, {
                queryKey: logCountKey(level, range),
            });
            query.setState({
                ...query.state,
                status: 'error',
                error: new Error('the logs could not be read'),
                fetchStatus: 'idle',
            });
        }
    }
    if (options.roster !== undefined) {
        client.setQueryData(SERVICES_QUERY_KEY, options.roster);
    }
    if (options.errors !== undefined) {
        client.setQueryData(logCountKey('error', range), options.errors);
    }
    if (options.warnings !== undefined) {
        client.setQueryData(logCountKey('warn', range), options.warnings);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <InstallationFigures range={range} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the roster in counts', () => {
    it('counts the services that run, are lost and are missing, and leaves the runners out', () => {
        expect(summariseRoster(roster)).toMatchObject({
            expected: 5,
            running: 3,
            lost: 1,
            missing: 1,
            newest: '0.0.27',
            servicesBehind: 0,
        });
    });

    it('counts a service on an older release once, however many instances it has', () => {
        const summary = summariseRoster([
            row({ service_name: 'ores.iam.service', version: '0.0.26' }),
            row({ service_name: 'ores.iam.service', slot: 2, version: '0.0.26' }),
            row({ service_name: 'ores.dq.service', version: '0.0.27' }),
        ]);

        expect(summary.servicesBehind).toBe(1);
        expect(summary.newest).toBe('0.0.27');
    });

    it('does not call a lost service behind, because it is not running', () => {
        const summary = summariseRoster([
            row({ service_name: 'ores.iam.service', version: '0.0.27' }),
            row({ service_name: 'ores.dq.service', state: 'lost', version: '0.0.20' }),
        ]);

        expect(summary.servicesBehind).toBe(0);
    });
});

describe('the release as it is written', () => {
    it('adds the v the services leave out', () => {
        expect(releaseLabel('0.0.27')).toBe('v0.0.27');
    });

    it('does not add a second one', () => {
        expect(releaseLabel('v0.0.27')).toBe('v0.0.27');
    });

    it('leaves a word that is not a number alone', () => {
        expect(releaseLabel('unknown')).toBe('unknown');
    });
});

describe('the figures', () => {
    it('states the services running of those expected, lost, missing, errors and warnings', () => {
        const html = render({ roster, errors: 4, warnings: 12 });

        expect(html).toContain('>3 of 5<');
        expect(html).toContain('Services running');
        expect(html).toContain('>Lost<');
        expect(html).toContain('>Missing<');
        expect(html).toContain('>4<');
        expect(html).toContain('Errors, Last hour');
        expect(html).toContain('>12<');
        expect(html).toContain('Warnings, Last hour');
        expect(html).toContain('>v0.0.27<');
    });

    it('leads each figure to the screen that explains it', () => {
        const html = render({ roster, errors: 0, warnings: 0 });

        expect(html).toContain('href="/operations/services"');
        expect(html).toContain('href="/operations/logs"');
    });

    it('says how many services run an older release', () => {
        const html = render({
            roster: [
                row({ service_name: 'ores.iam.service', version: '0.0.26' }),
                row({ service_name: 'ores.dq.service', version: '0.0.27' }),
            ],
            errors: 0,
            warnings: 0,
        });

        expect(html).toContain('1 service on an older release');
    });

    it('keeps the roster figures when the logs could not be read', () => {
        const html = render({ roster });

        expect(html).toContain('>3 of 5<');
        expect(html).toContain('>—<');
    });

    it('reads the counts of the range it was given', () => {
        const html = render({ roster, errors: 9, warnings: 2, range: '6h' });

        expect(html).toContain('Errors, Last 6 hours');
        expect(html).toContain('>9<');
        expect(html).not.toContain('>—<');
    });

    it('says the logs could not be read only when the read failed', () => {
        expect(render({ roster, logsFailed: true })).toContain('Could not be read');
        expect(render({ roster })).not.toContain('Could not be read');
    });

    it('says the services could not be read, and shows no figure it made up', () => {
        const html = render({ rosterFailed: true, errors: 3, warnings: 1 });

        expect(html).toContain('The services could not be read.');
        expect(html).not.toContain('Services running');
        expect(html).not.toContain(' of ');
    });

    it('does not paint an installation that expects nothing as healthy', () => {
        const html = render({ roster: [], errors: 0, warnings: 0 });

        expect(html).toContain('>0 of 0<');
        expect(html).not.toContain('text-up');
    });

    it('states nothing it has not read', () => {
        const html = render({});

        expect(html).not.toContain(' of ');
        expect(html).toContain('>—<');
    });
});

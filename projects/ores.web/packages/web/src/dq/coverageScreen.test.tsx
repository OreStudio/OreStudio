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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { Coverage } from '../api/dq.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { CoveragePage } from './CoveragePage.js';

/**
 * The coverage screen, called the way a person reaches it: four collections in
 * the query cache, drawn as the bundles of each level, the members of one
 * bundle, and the reason any member cannot publish.
 */

function datasetRow(code: string, artefactType: string) {
    return {
        version: 1,
        tenant_id: 't',
        id: code,
        code,
        catalog_name: null,
        subject_area_name: '',
        domain_name: '',
        coding_scheme_code: null,
        origin_code: '',
        nature_code: '',
        treatment_code: '',
        methodology_id: null,
        name: `${code} dataset`,
        description: '',
        source_system_id: '',
        business_context: '',
        upstream_derivation_id: null,
        lineage_depth: 0,
        as_of_date: '',
        ingestion_timestamp: '',
        license_info: null,
        artefact_type: artefactType,
        modified_by: '',
        performed_by: '',
        change_reason_code: '',
        change_commentary: '',
        recorded_at: '',
    };
}

function artefactTypeRow(code: string, targetSubject: string | null) {
    return {
        version: 1,
        tenant_id: 't',
        code,
        name: code,
        description: null,
        artefact_table: `${code}_artefact_tbl`,
        target_table: `${code}_tbl`,
        target_subject: targetSubject,
        display_order: 0,
        modified_by: '',
        performed_by: '',
        change_reason_code: '',
        change_commentary: '',
        recorded_at: '',
    };
}

function bundleRow(code: string, name: string, level: string) {
    return {
        version: 1,
        tenant_id: 't',
        id: code,
        code,
        name,
        level,
        description: '',
        modified_by: '',
        performed_by: '',
        change_reason_code: '',
        change_commentary: '',
        recorded_at: '',
    };
}

function memberRow(bundleCode: string, datasetCode: string, order: number) {
    return {
        version: 1,
        tenant_id: 't',
        bundle_code: bundleCode,
        dataset_code: datasetCode,
        display_order: order,
        optional: false,
        modified_by: '',
        performed_by: '',
        change_reason_code: '',
        change_commentary: '',
        recorded_at: '',
    };
}

const WORLD: Coverage = {
    bundles: [bundleRow('world', 'World reference data', 'world')],
    members: [
        memberRow('world', 'currencies', 1),
        memberRow('world', 'calendars', 2),
        memberRow('world', 'lei_entities', 3),
    ],
    datasets: [datasetRow('currencies', 'currency'), datasetRow('calendars', 'calendar')],
    artefactTypes: [artefactTypeRow('currency', 'refdata_currencies')],
};

const EMPTY: Coverage = {
    bundles: [],
    members: [],
    datasets: [],
    artefactTypes: [],
};

function render(coverage: Coverage): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['dq-coverage'], coverage);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/data-quality/coverage']}>
                    <CoveragePage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the coverage screen', () => {
    it('draws the bundles grouped by the level they publish at', () => {
        const html = render(WORLD);

        expect(html).toContain('Reference data coverage');
        expect(html).toContain('world');
        expect(html).toContain('World reference data');
        expect(html).toContain('data-quality');
    });

    it('states the reason each member cannot publish, and who owns it', () => {
        const html = render(WORLD);

        expect(html).toContain('Metadata complete');
        expect(html).toContain('No artefact type');
        expect(html).toContain('No dataset row');
        expect(html).toContain('Data quality fixes it');
    });

    it('draws a dash where the staged and published counts are not served', () => {
        const html = render(WORLD);

        expect(html).toContain('—');
        expect(html).toContain('The staged and published counts are not served yet');
        expect(html).toContain('Not served yet');
    });

    it('draws a fresh database as a sentence and not as a failure', () => {
        const html = render(EMPTY);

        expect(html).toContain('This database holds no bundle yet');
        expect(html).not.toContain('Readiness');
    });
});

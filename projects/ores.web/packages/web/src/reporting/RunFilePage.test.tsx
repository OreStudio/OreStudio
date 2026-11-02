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

import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { ReportRunFileInfoView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { RunFilePage } from './RunFilePage.js';

/**
 * The page of one file of a run. What is checked is what a person opening a
 * file relies on: the page says what the file is and which group it belongs
 * to, gives its facts and a download, and says plainly when the catalogue does
 * not know the file.
 */

const RUN = '22222222-2222-4222-8222-222222222222';
const FILE = 'output:r:npv.csv';

const file: ReportRunFileInfoView['file'] = {
    id: FILE,
    direction: 'output',
    kind: 'file',
    name: 'npv.csv',
    family: 'npv_report',
    bytes: 1441,
    sha256: '75823c6d',
    job: '33333333-3333-4333-8333-333333333333',
    node: 'output:r',
    ore_file_code: 'npv_report',
    ore_file_name: 'NPV Report',
    category_code: 'valuation_report',
};

function render(info: ReportRunFileInfoView): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['reporting-run-file', RUN, FILE], info);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter
                    initialEntries={[`/reporting/runs/${RUN}/files/${encodeURIComponent(FILE)}`]}
                >
                    <Routes>
                        <Route path="/reporting/runs/:id/files/:file" element={<RunFilePage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the page of a run file', () => {
    it('says what the file is, the group it belongs to, and offers it for download', () => {
        const html = render({
            file,
            ore_file: {
                code: 'npv_report',
                name: 'NPV Report',
                category_code: 'valuation_report',
                category_name: 'Valuation Reports',
                category_description: 'The value of each trade today.',
                configuration_type_code: null,
                guide_section: null,
                origin: 'produced',
                format: 'csv',
                xsd_file: null,
                typical_filename: 'npv.csv',
                filename_regex: '^npv\\.csv$',
                description: 'The NPV report. It holds the value of each trade.',
            },
            parameters: [],
        });

        expect(html).toContain('NPV Report');
        expect(html).toContain('Valuation Reports');
        expect(html).toContain('The NPV report. It holds the value of each trade.');
        expect(html).toContain('Comma separated values');
        expect(html).toContain('Written by ORE');
        expect(html).toContain(`/api/reporting/runs/${RUN}/files/${encodeURIComponent(FILE)}`);
        expect(html).not.toContain('No ORE file in the catalogue recognises this name');
    });

    it('says so when no ORE file recognises the name', () => {
        const html = render({
            file: { ...file, ore_file_code: null, ore_file_name: null, category_code: null },
            ore_file: null,
            parameters: [],
        });

        expect(html).toContain('No ORE file in the catalogue recognises this name');
        expect(html).not.toContain('Valuation Reports');
    });
});

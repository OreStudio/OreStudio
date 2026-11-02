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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'node:zlib';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { schemaWithIncludes, tarEntries } from './report-run-files.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The files of a report run: every file the engine read and every file it wrote.
 *
 * What is checked is what a person picking a file relies on: the input package is
 * listed with each file inside it, the output archive with each file its manifest
 * names and the size and digest it states, and a file is served by its id in that
 * list, as the engine wrote it, so no storage key travels from the browser.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

const ENVIRONMENT_ID = 'eager_maxwell';
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: ENVIRONMENT_ID,
    });
}

const TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';
const RUN = '22222222-2222-4222-8222-222222222222';
const BATCH = '55555555-5555-4555-8555-555555555555';
const WORKUNIT = '33333333-3333-4333-8333-333333333333';
const RESULT = '44444444-4444-4444-8444-444444444444';
const OK = { outcome: 'ok', code: '', message: '', fields: [] };
const APP_VERSION = '66666666-6666-4666-8666-666666666666';
const ORE_XSD = '<xs:schema><xs:include schemaLocation="ore_types.xsd"/></xs:schema>';
const TYPES_XSD = '<xs:schema><xs:simpleType name="currencyCode"/></xs:schema>';
const INPUT = `/api/v1/storage/ores/ore/packages/${RUN}/ore_package.tar.gz`;
const OUTPUT = `/api/v1/storage/ores/compute/output/${RESULT}.tar.gz`;
const MANIFEST = `/api/v1/storage/ores/compute/output/${RESULT}/manifest.json.gz`;
const NPV_KEY = `compute/output/${RESULT}/npv.csv.gz`;
const NPV = '#TradeId,NPV\nT1,100\n';

/** A tar archive of the entries, the way a packer writes regular files and a directory. */
function tar(entries: readonly { name: string; body?: string; type?: string }[]): Buffer {
    const blocks: Buffer[] = [];
    for (const { name, body = '', type = '0' } of entries) {
        const header = Buffer.alloc(512);
        header.write(name, 0, 'utf8');
        header.write(`${Buffer.byteLength(body).toString(8).padStart(11, '0')}\0`, 124, 'ascii');
        header.write(type, 156, 'ascii');
        blocks.push(header);
        const data = Buffer.from(body, 'utf8');
        blocks.push(Buffer.concat([data, Buffer.alloc((512 - (data.length % 512)) % 512)]));
    }
    return Buffer.concat([...blocks, Buffer.alloc(1024)]);
}

const PACKAGE = gzipSync(
    tar([
        { name: 'Input/', type: '5' },
        { name: 'Input/ore.xml', body: '<ORE/>' },
        { name: 'Input/portfolio.xml', body: '<Portfolio/>' },
    ]),
);

const STORED: Readonly<Record<string, Buffer>> = {
    [INPUT]: PACKAGE,
    [OUTPUT]: gzipSync(Buffer.from('archive')),
    [MANIFEST]: gzipSync(
        JSON.stringify({
            result_id: RESULT,
            files: [
                {
                    name: 'npv.csv',
                    family: 'npv',
                    bytes: 20,
                    compressed_bytes: 9,
                    sha256: 'ab12',
                    key: NPV_KEY,
                },
                {
                    name: 'log.txt',
                    family: '',
                    bytes: 5,
                    compressed_bytes: 4,
                    sha256: 'cd34',
                    key: `compute/output/${RESULT}/log.txt.gz`,
                },
            ],
        }),
    ),
    [`/api/v1/storage/ores/${NPV_KEY}`]: gzipSync(NPV),
};

const oreFile = (
    code: string,
    name: string,
    category: string,
    regex: string,
    extra: Record<string, unknown> = {},
) => ({
    tenant_id: TENANT,
    code,
    name,
    category_code: category,
    configuration_type_code: null,
    guide_section: null,
    origin: category === 'master_input' ? 'supplied' : 'produced',
    format: 'csv',
    schema_ore_file_code: null,
    typical_filename: name,
    filename_regex: regex,
    description: `What ${name} holds.`,
    display_order: 1,
    recorded_at: '2026-11-02 00:00:00.000000Z',
    ...extra,
});

const ANSWERS: Readonly<Record<string, unknown>> = {
    'reporting.v1.report_instances.list': {
        result: OK,
        instances: [{ id: RUN, name: 'nightly', definition_id: RUN, party_id: TENANT }],
        total: 1,
    },
    'compute.v1.batches.list': {
        result: OK,
        batches: [
            {
                version: 1,
                tenant_id: TENANT,
                id: BATCH,
                party_id: null,
                external_ref: RUN,
                status: 'closed',
                recorded_at: '2026-10-23 07:00:01.000000Z',
            },
        ],
        total: 1,
    },
    'compute.v1.workunits.list_by_batch_id': {
        result: OK,
        workunits: [
            {
                tenant_id: TENANT,
                id: WORKUNIT,
                batch_id: BATCH,
                app_version_id: RUN,
                input_uri: INPUT,
                canonical_result_id: RESULT,
                recorded_at: '2026-10-23 07:00:01.000000Z',
            },
        ],
        total: 1,
    },
    'reporting.v1.ore_files.list': {
        result: OK,
        files: [
            oreFile('master_input', 'Analytics', 'master_input', '^ore\\.xml$', {
                format: 'xml',
                schema_ore_file_code: 'master_input_schema',
            }),
            oreFile('master_input_schema', 'Analytics Schema', 'schemas', '^ore\\.xsd$', {
                format: 'xsd',
                typical_filename: 'ore.xsd',
            }),
            oreFile('ore_types_schema', 'ORE Types Schema', 'schemas', '^ore_types\\.xsd$', {
                format: 'xsd',
                typical_filename: 'ore_types.xsd',
            }),
            oreFile('npv_report', 'NPV Report', 'valuation_report', '^npv\\.csv$'),
        ],
        total: 4,
    },
    'reporting.v1.ore_file_categories.list': {
        result: OK,
        categories: [
            {
                tenant_id: TENANT,
                code: 'master_input',
                name: 'Master Input',
                origin: 'supplied',
                description: 'The one file a run starts with.',
                display_order: 1,
            },
            {
                tenant_id: TENANT,
                code: 'valuation_report',
                name: 'Valuation Reports',
                origin: 'produced',
                description: 'The value of each trade.',
                display_order: 2,
            },
        ],
        total: 2,
    },
    'reporting.v1.file_types.list_by_ore_file_code': {
        result: OK,
        types: [
            {
                tenant_id: TENANT,
                code: 'npv.outputFileName',
                analytic_type_code: 'npv',
                ore_parameter: 'outputFileName',
                direction: 'output',
                kind: 'output_file',
                ore_file_code: 'npv_report',
                name_pattern: 'npv.csv',
                is_default: true,
                description: 'The NPV report of the npv analytic.',
            },
        ],
        total: 1,
    },
    'compute.v1.workunits.get': {
        result: OK,
        workunit: {
            tenant_id: TENANT,
            id: WORKUNIT,
            batch_id: BATCH,
            app_version_id: APP_VERSION,
            input_uri: INPUT,
            canonical_result_id: RESULT,
            recorded_at: '2026-10-23 07:00:01.000000Z',
        },
    },
    'compute.v1.app_versions.get': {
        result: OK,
        app_version: {
            tenant_id: TENANT,
            id: APP_VERSION,
            app_id: APP_VERSION,
            engine_version: '1.8.17.0',
            recorded_at: '2026-10-23 07:00:01.000000Z',
        },
    },
    'reporting.v1.ore_file_versions.list': {
        result: OK,
        versions: [
            {
                tenant_id: TENANT,
                id: RUN,
                ore_file_code: 'master_input_schema',
                engine_version: '1.8.17.0',
                content: ORE_XSD,
                sha256: 'a'.repeat(64),
                bytes: ORE_XSD.length,
            },
            {
                tenant_id: TENANT,
                id: BATCH,
                ore_file_code: 'ore_types_schema',
                engine_version: '1.8.17.0',
                content: TYPES_XSD,
                sha256: 'a'.repeat(64),
                bytes: TYPES_XSD.length,
            },
        ],
        total: 2,
    },
    'compute.v1.results.get': {
        result: OK,
        result_value: {
            tenant_id: TENANT,
            id: RESULT,
            workunit_id: WORKUNIT,
            output_uri: OUTPUT,
            received_at: '2026-10-23 07:01:00.000000Z',
            recorded_at: '2026-10-23 07:01:00.000000Z',
        },
    },
};

function buildTestServer(answers: Readonly<Record<string, unknown>> = ANSWERS) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        token: 'token-123',
        async discoverHttpBaseUrl(): Promise<string | null> {
            return 'http://files.test:20200';
        },
        async callAuthenticated(
            subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            if (!(subject in answers)) {
                throw new Error(`unexpected call ${subject}`);
            }
            return schema.parse(answers[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'sysadmin',
        email: 'sysadmin@acme.example',
        accountId: '11111111-1111-4111-8111-111111111111',
        tenantId: TENANT,
        tenantName: 'System',
        mode: 'application',
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: TENANT,
                name: 'System',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-4777-8777-777777777777',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id } };
}

function stubStorage(): void {
    vi.stubGlobal(
        'fetch',
        vi.fn(async (url: URL) => {
            const body = STORED[url.pathname];
            return body === undefined ? new Response(null, { status: 404 }) : new Response(body);
        }),
    );
}

async function get(url: string, answers?: Readonly<Record<string, unknown>>) {
    stubStorage();
    const { server, cookies } = buildTestServer(answers);
    const response = await server.inject({ method: 'GET', url, cookies });
    await server.close();
    return response;
}

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('tarEntries', () => {
    it('lists the regular files of an archive with their bytes, and passes over a directory', () => {
        const archive = tar([
            { name: 'Input/', type: '5' },
            { name: 'Input/ore.xml', body: '<ORE/>' },
            {
                name: '././@LongLink',
                body: 'Input/a_name_longer_than_the_header_holds.xml',
                type: 'L',
            },
            { name: 'Input/a_name_longer', body: 'long' },
        ]);

        const entries = tarEntries(archive);

        expect(entries.map((entry) => entry.name)).toEqual([
            'Input/ore.xml',
            'Input/a_name_longer_than_the_header_holds.xml',
        ]);
        const first = entries[0]!;
        expect(archive.subarray(first.offset, first.offset + first.size).toString()).toBe('<ORE/>');
    });
});

describe('GET /api/reporting/runs/:id/files', () => {
    it('lists the input package and its files, then the output archive and its manifest', async () => {
        const response = await get(`/api/reporting/runs/${RUN}/files`);

        expect(response.statusCode).toBe(200);
        const { files } = response.json<{
            files: {
                direction: string;
                kind: string;
                name: string;
                family: string;
                bytes: number | null;
                sha256: string | null;
                job: string | null;
                node: string;
            }[];
        }>();
        expect(files.map((file) => [file.direction, file.kind, file.name])).toEqual([
            ['input', 'archive', 'ore_package.tar.gz'],
            ['input', 'file', 'Input/ore.xml'],
            ['input', 'file', 'Input/portfolio.xml'],
            ['output', 'archive', `${RESULT}.tar.gz`],
            ['output', 'file', 'npv.csv'],
            ['output', 'file', 'log.txt'],
        ]);
        expect(files[0]).toMatchObject({ bytes: PACKAGE.length, job: WORKUNIT });
        expect(files[1]).toMatchObject({ bytes: 6, sha256: null });
        expect(files[4]).toMatchObject({ family: 'npv', bytes: 20, sha256: 'ab12' });
        expect(files[5]).toMatchObject({ family: '' });
        expect(files.map((file) => file.node)).toEqual([
            ...Array<string>(3).fill(`input:${WORKUNIT}`),
            ...Array<string>(3).fill(`output:${RESULT}`),
        ]);
    });

    it('hangs the files in a tree of the run, its batch, the job and its input and output', async () => {
        const response = await get(`/api/reporting/runs/${RUN}/files`);

        expect(response.json<{ tree: unknown[] }>().tree).toEqual([
            { id: '', parent: '', kind: 'run', ref: RUN, count: 6 },
            { id: `batch:${BATCH}`, parent: '', kind: 'batch', ref: BATCH, count: 6 },
            {
                id: `job:${WORKUNIT}`,
                parent: `batch:${BATCH}`,
                kind: 'job',
                ref: WORKUNIT,
                count: 6,
            },
            {
                id: `input:${WORKUNIT}`,
                parent: `job:${WORKUNIT}`,
                kind: 'input',
                ref: WORKUNIT,
                count: 3,
            },
            {
                id: `output:${RESULT}`,
                parent: `job:${WORKUNIT}`,
                kind: 'output',
                ref: RESULT,
                count: 3,
            },
        ]);
    });

    it('answers 404 when the session sees no such run', async () => {
        const response = await get(`/api/reporting/runs/${RUN}/files`, {
            ...ANSWERS,
            'reporting.v1.report_instances.list': { result: OK, instances: [], total: 0 },
        });

        expect(response.statusCode).toBe(404);
    });

    it('lists nothing for a run no batch names yet', async () => {
        const response = await get(`/api/reporting/runs/${RUN}/files`, {
            ...ANSWERS,
            'compute.v1.batches.list': { result: OK, batches: [], total: 0 },
        });

        expect(response.json()).toEqual({
            tree: [{ id: '', parent: '', kind: 'run', ref: RUN, count: 0 }],
            files: [],
        });
    });
});

describe('the ORE file of each run file', () => {
    it('names each file by the ORE file its name is, and leaves a name none matches unnamed', async () => {
        const response = await get(`/api/reporting/runs/${RUN}/files`);
        const files = response.json<{
            files: {
                name: string;
                ore_file_code: string | null;
                ore_file_name: string | null;
                category_code: string | null;
            }[];
        }>().files;
        const of = (name: string) => files.find((file) => file.name === name);

        expect(of('Input/ore.xml')).toMatchObject({
            ore_file_code: 'master_input',
            ore_file_name: 'Analytics',
            category_code: 'master_input',
        });
        expect(of('npv.csv')).toMatchObject({
            ore_file_code: 'npv_report',
            category_code: 'valuation_report',
        });
        expect(of('log.txt')).toMatchObject({
            ore_file_code: null,
            ore_file_name: null,
            category_code: null,
        });
    });

    it('leaves out an ORE file whose expression does not compile, and still names the others', async () => {
        const catalogue = ANSWERS['reporting.v1.ore_files.list'] as { files: unknown[] };
        const response = await get(`/api/reporting/runs/${RUN}/files`, {
            ...ANSWERS,
            'reporting.v1.ore_files.list': {
                ...catalogue,
                files: [oreFile('broken', 'Broken', 'valuation_report', '(['), ...catalogue.files],
            },
        });
        const files = response.json<{ files: { name: string; ore_file_code: string | null }[] }>()
            .files;

        expect(files.find((file) => file.name === 'npv.csv')?.ore_file_code).toBe('npv_report');
    });

    it('states the ORE file, its category and the parameters that name it', async () => {
        const id = encodeURIComponent(`output:${RESULT}:npv.csv`);
        const response = await get(`/api/reporting/runs/${RUN}/files/${id}/info`);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            file: { name: 'npv.csv', family: 'npv' },
            ore_file: {
                code: 'npv_report',
                name: 'NPV Report',
                category_name: 'Valuation Reports',
                description: 'What NPV Report holds.',
            },
            parameters: [
                {
                    code: 'npv.outputFileName',
                    analytic_type_code: 'npv',
                    direction: 'output',
                    is_default: true,
                },
            ],
        });
    });

    it('serves a file no ORE file recognises, saying so', async () => {
        const id = encodeURIComponent(`output:${RESULT}:log.txt`);
        const response = await get(`/api/reporting/runs/${RUN}/files/${id}/info`);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ ore_file: null, parameters: [] });
    });
});

describe('GET /api/reporting/runs/:id/files/:file', () => {
    it('serves a file of the input package as the engine read it', async () => {
        const id = encodeURIComponent(`input:${WORKUNIT}:Input/portfolio.xml`);
        const response = await get(`/api/reporting/runs/${RUN}/files/${id}`);

        expect(response.statusCode).toBe(200);
        expect(response.body).toBe('<Portfolio/>');
        expect(response.headers['content-disposition']).toContain('filename="portfolio.xml"');
    });

    it('serves an output file as the engine wrote it, not as stored', async () => {
        const id = encodeURIComponent(`output:${RESULT}:npv.csv`);
        const response = await get(`/api/reporting/runs/${RUN}/files/${id}`);

        expect(response.statusCode).toBe(200);
        expect(response.body).toBe(NPV);
    });

    it('refuses a file the run does not list, whatever key it names', async () => {
        const id = encodeURIComponent(`output:${RESULT}:../../secrets`);
        const response = await get(`/api/reporting/runs/${RUN}/files/${id}`);

        expect(response.statusCode).toBe(404);
    });
});

describe('the schema of an xml file', () => {
    it('names the schema of the file and says it is stored for the engine that ran the job', async () => {
        const id = encodeURIComponent(`input:${WORKUNIT}:Input/ore.xml`);
        const response = await get(`/api/reporting/runs/${RUN}/files/${id}/info`);

        expect(response.json()).toMatchObject({
            engine_version: '1.8.17.0',
            schema: { code: 'master_input_schema', name: 'Analytics Schema', stored: true },
        });
    });

    it('serves the schema with every schema it includes, as one zip', async () => {
        const response = await get(
            '/api/reporting/ore-files/master_input_schema/schema?version=1.8.17.0',
        );

        expect(response.statusCode).toBe(200);
        expect(response.headers['content-type']).toBe('application/zip');
        expect(response.headers['content-disposition']).toContain(
            'master_input_schema-1.8.17.0.zip',
        );
        const body = response.rawPayload;
        expect(body.readUInt32LE(0)).toBe(0x04034b50);
        expect(body.includes(Buffer.from('ore.xsd'))).toBe(true);
        expect(body.includes(Buffer.from('ore_types.xsd'))).toBe(true);
    });

    it('names an include the engine version does not store rather than serve half a schema', async () => {
        const response = await get(
            '/api/reporting/ore-files/master_input_schema/schema?version=1.8.17.0',
            {
                ...ANSWERS,
                'reporting.v1.ore_file_versions.list': {
                    result: OK,
                    versions: [
                        {
                            tenant_id: TENANT,
                            id: RUN,
                            ore_file_code: 'master_input_schema',
                            engine_version: '1.8.17.0',
                            content: ORE_XSD,
                            sha256: 'a'.repeat(64),
                            bytes: ORE_XSD.length,
                        },
                    ],
                    total: 1,
                },
            },
        );

        expect(response.statusCode).toBe(404);
        expect(response.json<{ message: string }>().message).toContain('ore_types.xsd');
    });

    it('refuses a code that is not a schema', async () => {
        const response = await get('/api/reporting/ore-files/master_input/schema?version=1.8.17.0');

        expect(response.statusCode).toBe(404);
    });
});

describe('schemaWithIncludes', () => {
    it('follows includes through other schemas and reads each schema once', () => {
        const contents = new Map([
            [
                'a.xsd',
                Buffer.from(
                    '<xs:include schemaLocation="b.xsd"/><xs:include schemaLocation="c.xsd"/>',
                ),
            ],
            ['b.xsd', Buffer.from('<xs:include schemaLocation="c.xsd"/>')],
            ['c.xsd', Buffer.from('<xs:include schemaLocation="a.xsd"/>')],
        ]);

        const { files, missing } = schemaWithIncludes('a.xsd', contents);

        expect(files.map((file) => file.name)).toEqual(['a.xsd', 'b.xsd', 'c.xsd']);
        expect(missing).toEqual([]);
    });
});

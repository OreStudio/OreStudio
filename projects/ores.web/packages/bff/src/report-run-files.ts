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

/**
 * The files of one report run: every file the engine read, and every file it
 * wrote, each with the size and digest its record states.
 *
 * A run's files live in compute, not in reporting. The run's batch names its
 * workunits; each workunit's input is the package the engine read, a gzipped
 * tar; each workunit's accepted result is the archive the engine wrote, with a
 * manifest that names every file in it. This module reads those records once,
 * so the screen asks for the run's files and never learns where compute keeps
 * them. A file is served by its id in that list, so the browser never names a
 * storage key.
 */

import { gunzipSync } from 'node:zlib';
import type { FastifyBaseLogger, FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    readAppVersion,
    readResult,
    readWorkunit,
    readWorkunitsOfBatch,
    oreFileViewSchema,
    reportRunFileInfoViewSchema,
    reportRunFileNodeViewSchema,
    reportRunFileViewSchema,
    type OreFileView,
    type ReportRunFileInfoView,
    type ReportRunFileNodeView,
    type ReportRunFileView,
    type ReportRunFilesView,
} from '@ores/wire-protocol';
import { subjects as categorySubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_file_category_protocol';
import type { ListOreFileCategoriesResponse } from '@ores/wire-protocol/generated/reporting/protocol/ore_file_category_protocol';
import { subjects as oreFileSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_file_protocol';
import type { ListOreFilesResponse } from '@ores/wire-protocol/generated/reporting/protocol/ore_file_protocol';
import { subjects as fileTypeSubjects } from '@ores/wire-protocol/generated/reporting/protocol/file_type_protocol';
import type { ListByOreFileCodeFileTypesResponse } from '@ores/wire-protocol/generated/reporting/protocol/file_type_protocol';
import { invalidRequest, notFound } from './errors.js';
import { readAll, text } from './refdata-calls.js';
import { zipOf, type ZipEntry } from './zip.js';
import { subjects as versionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_file_version_protocol';
import type { ListOreFileVersionsResponse } from '@ores/wire-protocol/generated/reporting/protocol/ore_file_version_protocol';
import { dispositionFor, downloadName, fetchStored } from './grid-downloads.js';
import { readBatchId, readRun } from './grid-report-runs.js';
import type { LiveSession } from './sessions.js';

const runParamsSchema = z.object({
    id: z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i),
});

const fileParamsSchema = runParamsSchema.extend({ file: z.string().min(1).max(1024) });

const schemaParamsSchema = z.object({ code: z.string().regex(/^[a-z][a-z0-9_]*$/) });

const schemaQuerySchema = z.object({ version: z.string().regex(/^[0-9]+(\.[0-9]+)*$/) });

const nullableText = (value: unknown): string | null => {
    const stated = text(value);
    return stated === '' ? null : stated;
};

const NIL_UUID = '00000000-0000-0000-0000-000000000000';

/** The manifest every accepted result leaves beside its files. */
const manifestKeyOf = (resultId: string): string => `compute/output/${resultId}/manifest.json.gz`;

/** What a file states before it is matched to the ORE file it is. */
const UNIDENTIFIED = { ore_file_code: null, ore_file_name: null, category_code: null } as const;

/** The most workunits a run's file list reads; a report run has a handful. */
const WORKUNIT_LIMIT = 100;

const manifestSchema = z.object({
    files: z
        .array(
            z.looseObject({
                name: z.string(),
                family: z.string().default(''),
                bytes: z.number().nullable().default(null),
                sha256: z.string().default(''),
                key: z.string(),
            }),
        )
        .default([]),
});

/** One entry of a tar archive: its path inside the archive and where its bytes are. */
export interface TarEntry {
    readonly name: string;
    readonly offset: number;
    readonly size: number;
}

const BLOCK = 512;

function field(block: Buffer, start: number, length: number): string {
    const raw = block.subarray(start, start + length);
    const end = raw.indexOf(0);
    return raw.subarray(0, end === -1 ? raw.length : end).toString('utf8');
}

/**
 * The regular files of a tar archive, in order. A GNU long name and a pax path
 * replace the name of the entry that follows them; every other kind of entry,
 * a directory among them, is passed over.
 */
export function tarEntries(archive: Buffer): readonly TarEntry[] {
    const entries: TarEntry[] = [];
    let offset = 0;
    let longName: string | null = null;
    while (offset + BLOCK <= archive.length) {
        const header = archive.subarray(offset, offset + BLOCK);
        if (header.every((byte) => byte === 0)) {
            break;
        }
        const size = parseInt(field(header, 124, 12).trim() || '0', 8);
        const type = String.fromCharCode(header[156] ?? 0);
        const body = offset + BLOCK;
        const next = body + Math.ceil(size / BLOCK) * BLOCK;
        if (type === 'L') {
            longName = field(archive, body, size);
        } else if (type === 'x') {
            const path = /\d+ path=([^\n]*)\n/.exec(
                archive.subarray(body, body + size).toString('utf8'),
            );
            longName = path?.[1] ?? longName;
        } else {
            if (type === '0' || type === '\0') {
                const prefix = field(header, 345, 155);
                const name =
                    longName ?? (prefix === '' ? '' : `${prefix}/`) + field(header, 0, 100);
                entries.push({ name: name.replace(/^\.\//, ''), offset: body, size });
            }
            longName = null;
        }
        offset = next;
    }
    return entries;
}

/** Where one listed file's bytes come from, kept on the server beside its row. */
type Source =
    | { readonly from: 'object'; readonly uri: string; readonly gzip: boolean }
    | { readonly from: 'entry'; readonly uri: string; readonly entry: string };

interface RunFile {
    readonly view: ReportRunFileView;
    readonly source: Source;
}

/** The input package of one workunit, and every file inside it. */
async function inputsOf(
    session: LiveSession,
    workunitId: string,
    uri: string,
    log: FastifyBaseLogger,
): Promise<readonly RunFile[]> {
    const archive = (bytes: number | null): RunFile => ({
        view: reportRunFileViewSchema.parse({
            id: `input:${workunitId}`,
            direction: 'input',
            kind: 'archive',
            name: downloadName(uri),
            family: '',
            bytes,
            sha256: null,
            job: workunitId,
            node: `input:${workunitId}`,
            ...UNIDENTIFIED,
        }),
        source: { from: 'object', uri, gzip: false },
    });
    let packed: Buffer;
    let entries: readonly TarEntry[];
    try {
        packed = await fetchStored(session, uri);
        entries = tarEntries(gunzipSync(packed));
    } catch (error) {
        log.warn({ err: error }, 'The input package of a report run could not be listed');
        return [archive(null)];
    }
    return [
        archive(packed.length),
        ...entries.map((entry) => ({
            view: reportRunFileViewSchema.parse({
                id: `input:${workunitId}:${entry.name}`,
                direction: 'input',
                kind: 'file',
                name: entry.name,
                family: '',
                bytes: entry.size,
                sha256: null,
                job: workunitId,
                node: `input:${workunitId}`,
                ...UNIDENTIFIED,
            }),
            source: { from: 'entry', uri, entry: entry.name } as const,
        })),
    ];
}

/** The output archive of one accepted result, and every file its manifest names. */
async function outputsOf(
    session: LiveSession,
    workunitId: string,
    resultId: string,
    uri: string,
    log: FastifyBaseLogger,
): Promise<readonly RunFile[]> {
    const archive: RunFile = {
        view: reportRunFileViewSchema.parse({
            id: `output:${resultId}`,
            direction: 'output',
            kind: 'archive',
            name: downloadName(uri),
            family: '',
            bytes: null,
            sha256: null,
            job: workunitId,
            node: `output:${resultId}`,
            ...UNIDENTIFIED,
        }),
        source: { from: 'object', uri, gzip: false },
    };
    let manifest: z.infer<typeof manifestSchema>;
    try {
        const packed = await fetchStored(session, manifestKeyOf(resultId));
        manifest = manifestSchema.parse(JSON.parse(gunzipSync(packed).toString('utf8')));
    } catch (error) {
        log.warn({ err: error }, 'The output manifest of a report run could not be read');
        return [archive];
    }
    return [
        archive,
        ...manifest.files.map((file) => ({
            view: reportRunFileViewSchema.parse({
                id: `output:${resultId}:${file.name}`,
                direction: 'output',
                kind: 'file',
                name: file.name,
                family: file.family,
                bytes: file.bytes,
                sha256: file.sha256 === '' ? null : file.sha256,
                job: workunitId,
                node: `output:${resultId}`,
                ...UNIDENTIFIED,
            }),
            source: { from: 'object', uri: file.key, gzip: true } as const,
        })),
    ];
}

/** The files of a run, with the tree they hang in. */
interface RunFiles {
    readonly tree: readonly ReportRunFileNodeView[];
    readonly files: readonly RunFile[];
}

type TreeNode = Omit<ReportRunFileNodeView, 'count'>;

/** The tree with each node's count of the files under it, its descendants included. */
function counted(
    nodes: readonly TreeNode[],
    files: readonly RunFile[],
): readonly ReportRunFileNodeView[] {
    const parentOf = new Map(nodes.map((node) => [node.id, node.parent]));
    const counts = new Map<string, number>();
    for (const file of files) {
        let at: string | undefined = file.view.node;
        while (at !== undefined) {
            counts.set(at, (counts.get(at) ?? 0) + 1);
            at = at === '' ? undefined : parentOf.get(at);
        }
    }
    return nodes.map((node) =>
        reportRunFileNodeViewSchema.parse({ ...node, count: counts.get(node.id) ?? 0 }),
    );
}

/** The ORE files, each with its category and the expression its names match. */
interface Catalogue {
    readonly files: readonly (OreFileView & { readonly pattern: RegExp })[];
}

/**
 * The ORE files and their categories, as the reporting catalogue holds them. A
 * read that fails leaves every file unidentified rather than failing the page.
 */
async function readCatalogue(session: LiveSession, log: FastifyBaseLogger): Promise<Catalogue> {
    try {
        const [files, categories] = await Promise.all([
            readAll<ListOreFilesResponse>(
                session,
                oreFileSubjects.list_ore_files_request,
                'files',
                {
                    filter: null,
                    as_of: null,
                },
            ),
            readAll<ListOreFileCategoriesResponse>(
                session,
                categorySubjects.list_ore_file_categories_request,
                'categories',
                { filter: null, as_of: null },
            ),
        ]);
        const categoryOf = new Map(
            categories.map((category) => [text(category['code']), category]),
        );
        return {
            files: files.map((file) => {
                const category = categoryOf.get(text(file['category_code']));
                const view = oreFileViewSchema.parse({
                    code: text(file['code']),
                    name: text(file['name']),
                    category_code: text(file['category_code']),
                    category_name: text(category?.['name']),
                    category_description: text(category?.['description']),
                    configuration_type_code: nullableText(file['configuration_type_code']),
                    guide_section: nullableText(file['guide_section']),
                    origin: text(file['origin']),
                    format: text(file['format']),
                    schema_ore_file_code: nullableText(file['schema_ore_file_code']),
                    typical_filename: text(file['typical_filename']),
                    filename_regex: text(file['filename_regex']),
                    description: text(file['description']),
                });
                return { ...view, pattern: new RegExp(view.filename_regex) };
            }),
        };
    } catch (error) {
        log.warn({ err: error }, 'The ORE file catalogue could not be read');
        return { files: [] };
    }
}

/** The ORE file a name is: the one whose expression matches the file's own name. */
function oreFileOf(name: string, catalogue: Catalogue): OreFileView | null {
    const base = name.slice(name.lastIndexOf('/') + 1);
    const found = catalogue.files.find((file) => file.pattern.test(base));
    if (found === undefined) {
        return null;
    }
    const { pattern: _pattern, ...view } = found;
    return view;
}

function identified(name: string, catalogue: Catalogue) {
    const file = oreFileOf(name, catalogue);
    return file === null
        ? UNIDENTIFIED
        : { ore_file_code: file.code, ore_file_name: file.name, category_code: file.category_code };
}

/** The ORE engine version a job ran on, from the app version the workunit names. */
async function engineVersionOf(
    session: LiveSession,
    workunitId: string | null,
    log: FastifyBaseLogger,
): Promise<string | null> {
    if (workunitId === null) {
        return null;
    }
    try {
        const workunit = await readWorkunit(session.client, workunitId);
        if (workunit === null || workunit.app_version_id === '') {
            return null;
        }
        const version = await readAppVersion(session.client, workunit.app_version_id);
        return version === null || version.engine_version === '' ? null : version.engine_version;
    } catch (error) {
        log.warn({ err: error }, 'The engine version of a job could not be read');
        return null;
    }
}

/** The stored contents of every schema of one engine version, by the schema's file name. */
async function readSchemaContents(
    session: LiveSession,
    engineVersion: string,
): Promise<ReadonlyMap<string, Buffer>> {
    const [versions, files] = await Promise.all([
        readAll<ListOreFileVersionsResponse>(
            session,
            versionSubjects.list_ore_file_versions_request,
            'versions',
            {
                filter: { engine_version: engineVersion },
                as_of: null,
            },
        ),
        readAll<ListOreFilesResponse>(session, oreFileSubjects.list_ore_files_request, 'files', {
            filter: { category_code: 'schemas' },
            as_of: null,
        }),
    ]);
    const nameOf = new Map(
        files.map((file) => [text(file['code']), text(file['typical_filename'])]),
    );
    const contents = new Map<string, Buffer>();
    for (const version of versions) {
        const name = nameOf.get(text(version['ore_file_code']));
        if (name !== undefined) {
            contents.set(name, Buffer.from(text(version['content']), 'utf8'));
        }
    }
    return contents;
}

const INCLUDE = /<xs:include\s+schemaLocation="([^"]+)"/g;

/**
 * A schema and every schema it includes, at once or through another, as the
 * engine version ships them. A schema is complete only with its includes, so a
 * download that left one out could not validate a document. An include the
 * version does not store is named rather than left out.
 */
export function schemaWithIncludes(
    root: string,
    contents: ReadonlyMap<string, Buffer>,
): { readonly files: readonly ZipEntry[]; readonly missing: readonly string[] } {
    const files: ZipEntry[] = [];
    const missing: string[] = [];
    const seen = new Set<string>();
    const queue = [root];
    while (queue.length > 0) {
        const name = queue.shift() as string;
        if (seen.has(name)) {
            continue;
        }
        seen.add(name);
        const content = contents.get(name);
        if (content === undefined) {
            missing.push(name);
            continue;
        }
        files.push({ name, content });
        for (const match of content.toString('utf8').matchAll(INCLUDE)) {
            queue.push(match[1] as string);
        }
    }
    return { files, missing };
}

/**
 * Every file of one run, inputs first, and the tree they hang in: the run, its
 * batch, each job of the batch, and each job's input package and output. A run
 * the session cannot read is not found; a run with no batch yet has no files; a
 * package or manifest that cannot be read leaves its archive listed without the
 * files inside it.
 */
async function filesOfRun(
    session: LiveSession,
    runId: string,
    log: FastifyBaseLogger,
): Promise<RunFiles> {
    const run = await readRun(session, runId);
    if (run === null) {
        throw notFound('No report run has this id.');
    }
    const root: TreeNode = { id: '', parent: '', kind: 'run', ref: runId };
    const batchId = await readBatchId(session, runId, log);
    if (batchId === null) {
        return { tree: counted([root], []), files: [] };
    }
    const batchNode = `batch:${batchId}`;
    const { workunits } = await readWorkunitsOfBatch(session.client, batchId, {
        offset: 0,
        limit: WORKUNIT_LIMIT,
    });
    const groups = await Promise.all(
        workunits.map(async (workunit) => {
            const jobNode = `job:${workunit.id}`;
            const nodes: TreeNode[] = [
                { id: jobNode, parent: batchNode, kind: 'job', ref: workunit.id },
            ];
            let inputs: readonly RunFile[] = [];
            if (workunit.input_uri !== '') {
                nodes.push({
                    id: `input:${workunit.id}`,
                    parent: jobNode,
                    kind: 'input',
                    ref: workunit.id,
                });
                inputs = await inputsOf(session, workunit.id, workunit.input_uri, log);
            }
            const resultId = workunit.canonical_result_id;
            if (resultId === '' || resultId === NIL_UUID) {
                return { nodes, inputs, outputs: [] };
            }
            const result = await readResult(session.client, resultId);
            if (result === null || result.output_uri === '') {
                return { nodes, inputs, outputs: [] };
            }
            nodes.push({
                id: `output:${resultId}`,
                parent: jobNode,
                kind: 'output',
                ref: resultId,
            });
            const outputs = await outputsOf(session, workunit.id, resultId, result.output_uri, log);
            return { nodes, inputs, outputs };
        }),
    );
    const catalogue = await readCatalogue(session, log);
    const files = [
        ...groups.flatMap((group) => group.inputs),
        ...groups.flatMap((group) => group.outputs),
    ].map((file) => ({
        ...file,
        view: { ...file.view, ...identified(file.view.name, catalogue) },
    }));
    const nodes: TreeNode[] = [
        root,
        { id: batchNode, parent: '', kind: 'batch', ref: batchId },
        ...groups.flatMap((group) => group.nodes),
    ];
    return { tree: counted(nodes, files), files };
}

function paramsOf<Shape extends z.ZodType>(schema: Shape, request: FastifyRequest): z.infer<Shape> {
    const parsed = schema.safeParse(request.params);
    if (!parsed.success) {
        throw invalidRequest('The address names a run that is not a uuid or a file name that is not valid.');
    }
    return parsed.data;
}

export function registerReportRunFileRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/reporting/runs/:id/files', async (request): Promise<ReportRunFilesView> => {
        const session = requireSession(request);
        const { id } = paramsOf(runParamsSchema, request);
        const { tree, files } = await filesOfRun(session, id, request.log);
        return { tree: [...tree], files: files.map((file) => file.view) };
    });

    server.get(
        '/api/reporting/runs/:id/files/:file/info',
        async (request): Promise<ReportRunFileInfoView> => {
            const session = requireSession(request);
            const { id, file } = paramsOf(fileParamsSchema, request);
            const found = (await filesOfRun(session, id, request.log)).files.find(
                (candidate) => candidate.view.id === file,
            );
            if (found === undefined) {
                throw notFound('This run has no such file.');
            }
            const catalogue = await readCatalogue(session, request.log);
            const oreFile = oreFileOf(found.view.name, catalogue);
            const parameters =
                oreFile === null
                    ? []
                    : await readAll<ListByOreFileCodeFileTypesResponse>(
                          session,
                          fileTypeSubjects.list_by_ore_file_code_file_types_request,
                          'types',
                          { ore_file_code: oreFile.code, scope: 'direct', filter: null },
                      );
            const engineVersion = await engineVersionOf(session, found.view.job, request.log);
            const schemaFile =
                oreFile?.schema_ore_file_code === null || oreFile === null
                    ? undefined
                    : catalogue.files.find((one) => one.code === oreFile.schema_ore_file_code);
            const schema =
                schemaFile === undefined
                    ? null
                    : {
                          code: schemaFile.code,
                          name: schemaFile.name,
                          stored:
                              engineVersion !== null &&
                              (await readSchemaContents(session, engineVersion)).has(
                                  schemaFile.typical_filename,
                              ),
                      };
            return reportRunFileInfoViewSchema.parse({
                engine_version: engineVersion,
                schema,
                file: found.view,
                ore_file: oreFile,
                parameters: parameters.map((type) => ({
                    code: text(type['code']),
                    analytic_type_code: nullableText(type['analytic_type_code']),
                    ore_parameter: text(type['ore_parameter']),
                    direction: text(type['direction']),
                    kind: text(type['kind']),
                    name_pattern: text(type['name_pattern']),
                    is_default: type['is_default'] === true,
                    description: text(type['description']),
                })),
            });
        },
    );

    server.get('/api/reporting/ore-files/:code/schema', async (request, reply) => {
        const session = requireSession(request);
        const params = schemaParamsSchema.safeParse(request.params);
        const query = schemaQuerySchema.safeParse(request.query);
        if (!params.success || !query.success) {
            throw invalidRequest('The address names no schema and engine version.');
        }
        const { code } = params.data;
        const { version } = query.data;
        const schema = (await readCatalogue(session, request.log)).files.find(
            (file) => file.code === code && file.format === 'xsd',
        );
        if (schema === undefined) {
            throw notFound('No ORE file is a schema with this code.');
        }
        const { files, missing } = schemaWithIncludes(
            schema.typical_filename,
            await readSchemaContents(session, version),
        );
        if (files.length === 0) {
            throw notFound(`ORE ${version} has no stored copy of ${schema.typical_filename}.`);
        }
        if (missing.length > 0) {
            throw notFound(
                `ORE ${version} has no stored copy of ${missing.join(', ')}, which ${schema.typical_filename} includes.`,
            );
        }
        return reply
            .header('Content-Type', 'application/zip')
            .header('Content-Disposition', dispositionFor(`${code}-${version}.zip`))
            .header('Cache-Control', 'private, no-store')
            .send(zipOf(files));
    });

    server.get('/api/reporting/runs/:id/files/:file', async (request, reply) => {
        const session = requireSession(request);
        const { id, file } = paramsOf(fileParamsSchema, request);
        const found = (await filesOfRun(session, id, request.log)).files.find(
            (candidate) => candidate.view.id === file,
        );
        if (found === undefined) {
            throw notFound('This run has no such file.');
        }
        const { source, view } = found;
        const stored = await fetchStored(session, source.uri);
        let body: Buffer;
        if (source.from === 'entry') {
            const unpacked = gunzipSync(stored);
            const entry = tarEntries(unpacked).find((candidate) => candidate.name === source.entry);
            if (entry === undefined) {
                throw notFound('The input package no longer holds this file.');
            }
            body = unpacked.subarray(entry.offset, entry.offset + entry.size);
        } else {
            body = source.gzip ? gunzipSync(stored) : stored;
        }
        const name = downloadName(view.name);
        return reply
            .header(
                'Content-Type',
                view.kind === 'archive' ? 'application/gzip' : 'application/octet-stream',
            )
            .header('Content-Disposition', dispositionFor(name))
            .header('Cache-Control', 'private, no-store')
            .send(body);
    });
}

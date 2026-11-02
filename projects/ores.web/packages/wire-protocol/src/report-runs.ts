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

import { z } from 'zod';
import { reportHealthWordSchema } from './reporting.js';
import { nullableWireTimestampSchema, wireTimestampSchema } from './time.js';

/**
 * One report run, as the run page reads it: what the run is, the state it
 * reached, how long it took, the compute batch it dispatched and where its
 * output is.
 *
 * The batch names the run in its External Ref, and the run does not name the batch,
 * so `batch_id` is found by reading the batches for that reference and is `null`
 * when the session sees none. `health` is the word the reporting service derives
 * from the run's state, and is `null` when the service states none or a word the
 * screens do not know. `priced_at` is the date the run actually priced at, which
 * the packaging step records on the run, and is `null` until the run reaches that
 * step; `price_note` says why the date moved when it did, and is empty otherwise.
 * `duration_ms` is the time from start to completion, `null` until both are known.
 * `output_storage_key` is where the run's output archive is, empty until the
 * results are collected.
 */
export const reportRunDetailViewSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string(),
    definition_id: z.string(),
    definition_name: z.string().nullable(),
    party_id: z.string(),
    party_name: z.string().nullable(),
    kind: z.string(),
    state_name: z.string(),
    health: reportHealthWordSchema.nullable(),
    output_message: z.string(),
    output_storage_key: z.string(),
    started_at: nullableWireTimestampSchema,
    completed_at: nullableWireTimestampSchema,
    duration_ms: z.number().nullable(),
    priced_at: nullableWireTimestampSchema,
    price_note: z.string(),
    batch_id: z.string().nullable(),
    modified_by: z.string(),
    recorded_at: wireTimestampSchema,
});
export type ReportRunDetailView = z.infer<typeof reportRunDetailViewSchema>;

/** One run in a report's list of runs, newest first. */
export const reportRunRowViewSchema = z.object({
    id: z.string(),
    name: z.string(),
    state_name: z.string(),
    health: reportHealthWordSchema.nullable(),
    started_at: nullableWireTimestampSchema,
    completed_at: nullableWireTimestampSchema,
    duration_ms: z.number().nullable(),
    priced_at: nullableWireTimestampSchema,
});
export type ReportRunRowView = z.infer<typeof reportRunRowViewSchema>;

/**
 * One problem a run recorded: how severe it is, what part of the run it is
 * about, the thing it names and what the engine said.
 *
 * `scope` is the part of the run the problem belongs to: a trade, a curve, an
 * analytic, the report or its configuration. `about` is the one thing the
 * problem names, as a person reads it: the curve a curve failure is about, the
 * trade a trade failure is about, empty when the problem names nothing.
 */
export const reportRunErrorViewSchema = z.object({
    id: z.string(),
    severity: z.string(),
    scope: z.string(),
    about: z.string(),
    message: z.string(),
    subject: z.string().nullable(),
    subject_id: z.string().nullable(),
    cause: z.string().nullable(),
    time: z.string(),
});
export type ReportRunErrorView = z.infer<typeof reportRunErrorViewSchema>;

/**
 * One file a run read or wrote, with what a person needs to pick it.
 *
 * `direction` says whether the engine read the file or wrote it. `kind` says
 * what it is: a whole archive, or one file inside the run's input package or
 * among its outputs. `family` is the output family the catalogue names, empty
 * when no family covers the file. `bytes` and `sha256` are what the record
 * states, null when it states nothing. `job` is the workunit the file belongs
 * to, null for a file of the whole run. `node` is the place in the run's file
 * tree the file hangs under. `ore_file_code` and `ore_file_name` say which ORE
 * file it is, and `category_code` its category; all three are null for a name no
 * ORE file recognises.
 */
export const reportRunFileViewSchema = z.object({
    id: z.string(),
    direction: z.enum(['input', 'output']),
    kind: z.enum(['archive', 'file']),
    name: z.string(),
    family: z.string(),
    bytes: z.number().nullable(),
    sha256: z.string().nullable(),
    job: z.string().nullable(),
    node: z.string(),
    ore_file_code: z.string().nullable(),
    ore_file_name: z.string().nullable(),
    category_code: z.string().nullable(),
});
export type ReportRunFileView = z.infer<typeof reportRunFileViewSchema>;

/**
 * One place in the tree of a run's files: the run, its batch, each job of the
 * batch, and the input package and the output each job has.
 *
 * `kind` says which; `ref` is the id of the record the node stands for, the
 * run, the batch, the workunit or the result; `count` is the number of files
 * under the node, its descendants included. The root is the run, with id ''.
 */
export const reportRunFileNodeViewSchema = z.object({
    id: z.string(),
    parent: z.string(),
    kind: z.enum(['run', 'batch', 'job', 'input', 'output']),
    ref: z.string(),
    count: z.number(),
});
export type ReportRunFileNodeView = z.infer<typeof reportRunFileNodeViewSchema>;

/** The files of a run, and the tree they hang in. */
export const reportRunFilesViewSchema = z.object({
    tree: z.array(reportRunFileNodeViewSchema),
    files: z.array(reportRunFileViewSchema),
});
export type ReportRunFilesView = z.infer<typeof reportRunFilesViewSchema>;

/** The ORE file a run's file is, as the reporting catalogue describes it. */
export const oreFileViewSchema = z.object({
    code: z.string(),
    name: z.string(),
    category_code: z.string(),
    category_name: z.string(),
    category_description: z.string(),
    configuration_type_code: z.string().nullable(),
    guide_section: z.string().nullable(),
    origin: z.string(),
    format: z.string(),
    schema_ore_file_code: z.string().nullable(),
    typical_filename: z.string(),
    filename_regex: z.string(),
    description: z.string(),
});
export type OreFileView = z.infer<typeof oreFileViewSchema>;

/** One parameter of a run document that names the file, with what it is for. */
export const oreFileParameterViewSchema = z.object({
    code: z.string(),
    analytic_type_code: z.string().nullable(),
    ore_parameter: z.string(),
    direction: z.string(),
    kind: z.string(),
    name_pattern: z.string(),
    is_default: z.boolean(),
    description: z.string(),
});
export type OreFileParameterView = z.infer<typeof oreFileParameterViewSchema>;

/**
 * One file of a run with everything a person needs to understand it: the file
 * as the run holds it, the ORE file it is, and the parameters that can name it.
 * `ore_file` is null for a name no ORE file recognises. `engine_version` is the
 * ORE version of the job the file belongs to, null when no job or version is
 * known. `schema` is the ORE file of the XSD an xml file validates against, with
 * whether its contents are stored for that engine version.
 */
export const reportRunFileInfoViewSchema = z.object({
    file: reportRunFileViewSchema,
    ore_file: oreFileViewSchema.nullable(),
    parameters: z.array(oreFileParameterViewSchema),
    engine_version: z.string().nullable(),
    schema: z
        .object({
            code: z.string(),
            name: z.string(),
            stored: z.boolean(),
        })
        .nullable(),
});
export type ReportRunFileInfoView = z.infer<typeof reportRunFileInfoViewSchema>;

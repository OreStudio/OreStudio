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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_ui.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * Labels and headers are translation keys, not English. The English words
 * are in the locale catalogue under the same keys.
 *
 * The key shapes are derived, so regeneration cannot invent a key:
 * <entity>.fld<Member> for a field label, <entity>.col<Member> for a
 * column header, <entity>.<member>Ph for a placeholder, and
 * <entity>.type.<value> for a combo option.
 *
 * Field grouping into tabs is deliberately absent: it is a domain
 * judgement the model does not carry, so regeneration cannot invent it. The
 * model cannot express a grouping yet, so every field renders in one group.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a workunit, in the order the model declares them.
 *
 * `input_uri` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const workunitFields: readonly FieldMeta[] = [
    {
        name: 'input_uri',
        labelKey: 'workunit.fldInputUri',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'workunit.inputUriPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const workunitColumns: readonly ColumnMeta[] = [
    {
        name: 'batch_id',
        headerKey: 'workunit.colBatchId',
        style: 'mono_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'app_version_id',
        headerKey: 'workunit.colAppVersionId',
        style: 'mono_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'input_uri',
        headerKey: 'workunit.colInputUri',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'priority',
        headerKey: 'workunit.colPriority',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'target_redundancy',
        headerKey: 'workunit.colTargetRedundancy',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'workunit.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'modified_by',
        headerKey: 'workunit.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const workunitMeta = {
    entity: 'workunit',
    collection: 'workunits',
    displayField: 'input_uri',
    keyField: 'input_uri',
    columns: workunitColumns,
    fields: workunitFields,
} as const;
/**
 * The entity's own words, in English, keyed the way the catalogue is.
 *
 * The model states them: the detail field's label, the column's header, the
 * placeholder, the title and the brief. They are emitted here rather than
 * written into a catalogue by hand, so a label the model changes changes in
 * one place, and a language that has no translation yet falls back to these
 * rather than to a key nobody can read.
 */
export const workunitMessages = {
        workunit: {
            title: 'Workunits',
            singular: 'workunit',
            newTitle: 'New workunit',
            description: 'Defines the problem to be solved: which app version to use, where the input data lives, and how many times to run it for redundancy. The BOINC equivalent of \'workunit\'. Does not track execution state — that belongs to results.',
            fldInputUri: 'Input Uri',
            inputUriPh: 'Enter workunit input uri',
            colBatchId: 'Batch ID',
            colAppVersionId: 'App Version ID',
            colInputUri: 'Input URI',
            colPriority: 'Priority',
            colTargetRedundancy: 'Redundancy',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};

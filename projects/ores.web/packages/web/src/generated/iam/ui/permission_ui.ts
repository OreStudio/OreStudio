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
 * The fields of a permission, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const permissionFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'permission.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'permission.codePh',
    },
    {
        name: 'description',
        labelKey: 'permission.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'permission.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const permissionColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'permission.colCode',
        style: 'text_left',
        hidden: false,
        width: 260,
    },
    {
        name: 'description',
        headerKey: 'permission.colDescription',
        style: 'text_left',
        hidden: true,
        width: 480,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const permissionMeta = {
    entity: 'permission',
    collection: 'permissions',
    displayField: 'code',
    keyField: 'code',
    columns: permissionColumns,
    fields: permissionFields,
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
export const permissionMessages = {
        permission: {
            title: 'Permissions',
            singular: 'permission',
            newTitle: 'New permission',
            description: 'An atomic permission that can be granted to roles. Permissions follow the hierarchical naming convention component::resource:action (for example iam::accounts:create; "*" grants everything and component::* grants every action within one component). The table is temporal (see projects/ores.sql/create/iam/iam_permissions_create.sql): it carries valid_from/valid_to, the GIST exclusion and the delete rule, but it has no version column and no audit tail -- permissions are system-defined constants seeded from bootstrap data, not user-editable records, so they need no change tracking. The :no_audit_columns: flag in the * SQL ** Flags drawer selects exactly that shape: it drops the version column and the four audit columns while keeping the transaction-time window. The :skip_uuid_check: suppression on id drops the nil-UUID check the hand-written table never had.',
            fldCode: 'Code',
            codePh: 'Enter a permission code',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            colCode: 'Code',
            colDescription: 'Description',
        }
};

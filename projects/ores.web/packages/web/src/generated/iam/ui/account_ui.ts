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
 * The fields of a account, in the order the model declares them.
 *
 * `username` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const accountFields: readonly FieldMeta[] = [
    {
        name: 'username',
        labelKey: 'account.fldUsername',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'account.usernamePh',
    },
    {
        name: 'full_name',
        labelKey: 'account.fldFullName',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account.fullNamePh',
    },
    {
        name: 'email',
        labelKey: 'account.fldEmail',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'account.emailPh',
    },
    {
        name: 'job_title',
        labelKey: 'account.fldJobTitle',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account.jobTitlePh',
    },
    {
        name: 'account_type',
        labelKey: 'account.fldAccountType',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'account.accountTypePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const accountColumns: readonly ColumnMeta[] = [
    {
        name: 'username',
        headerKey: 'account.colUsername',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'full_name',
        headerKey: 'account.colFullName',
        style: 'text_left',
        hidden: false,
        width: 240,
    },
    {
        name: 'email',
        headerKey: 'account.colEmail',
        style: 'text_left',
        hidden: false,
        width: 260,
    },
    {
        name: 'job_title',
        headerKey: 'account.colJobTitle',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'account_type',
        headerKey: 'account.colAccountType',
        style: 'text_left',
        hidden: false,
        width: 140,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const accountMeta = {
    entity: 'account',
    collection: 'accounts',
    displayField: 'username',
    keyField: 'username',
    columns: accountColumns,
    fields: accountFields,
} as const;

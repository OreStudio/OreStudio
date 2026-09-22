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
 * The fields of a account_party, in the order the model declares them.
 */
export const accountPartyFields: readonly FieldMeta[] = [
    {
        name: 'account_id',
        labelKey: 'account_party.fldAccountId',
        control: 'dynamic_combo',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'accounts', valueField: 'id', labelField: 'username' },
    },
    {
        name: 'party_id',
        labelKey: 'account_party.fldPartyId',
        control: 'dynamic_combo',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'parties', valueField: 'id', labelField: 'name' },
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const accountPartyColumns: readonly ColumnMeta[] = [
    {
        name: 'account_id',
        headerKey: 'account_party.colAccountId',
        style: 'text_left',
        hidden: false,
        width: 320,
    },
    {
        name: 'party_id',
        headerKey: 'account_party.colPartyId',
        style: 'text_left',
        hidden: false,
        width: 320,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const accountPartyMeta = {
    entity: 'account_party',
    collection: 'account_parties',
    displayField: '',
    keyField: '',
    columns: accountPartyColumns,
    fields: accountPartyFields,
} as const;

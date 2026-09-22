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
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
    },
    {
        name: 'party_id',
        labelKey: 'account_party.fldPartyId',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
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
/**
 * The entity's own words, in English, keyed the way the catalogue is.
 *
 * The model states them: the detail field's label, the column's header, the
 * placeholder, the title and the brief. They are emitted here rather than
 * written into a catalogue by hand, so a label the model changes changes in
 * one place, and a language that has no translation yet falls back to these
 * rather than to a key nobody can read.
 */
export const accountPartyMessages = {
        account_party: {
            title: 'Account Parties',
            singular: 'account party',
            newTitle: 'New account party',
            description: 'Junction table linking IAM accounts to parties. Each account can be associated with one or more parties, controlling which parties a user can act on behalf of.',
            fldAccountId: 'Account',
            fldPartyId: 'Party',
            colAccountId: 'Account',
            colPartyId: 'Party',
        }
};

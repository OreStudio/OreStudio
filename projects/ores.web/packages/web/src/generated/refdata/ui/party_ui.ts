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
 * The fields of a party, in the order the model declares them.
 *
 * `short_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const partyFields: readonly FieldMeta[] = [
    {
        name: 'short_code',
        labelKey: 'party.fldShortCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'party.shortCodePh',
    },
    {
        name: 'full_name',
        labelKey: 'party.fldFullName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'party.fullNamePh',
    },
    {
        name: 'party_type',
        labelKey: 'party.fldPartyType',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
        codeDomain: 'party_type',
    },
    {
        name: 'status',
        labelKey: 'party.fldStatus',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'statuses', valueField: 'code', labelField: 'description' },
        codeDomain: 'party_status',
    },
    {
        name: 'business_center_code',
        labelKey: 'party.fldBusinessCenterCode',
        control: 'flagged_combo',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'parent_party_id',
        labelKey: 'party.fldParentPartyId',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: true,
        lookup: { collection: 'parties', valueField: 'id', labelField: 'full_name' },
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const partyColumns: readonly ColumnMeta[] = [
    {
        name: 'short_code',
        headerKey: 'party.colShortCode',
        style: 'icon_text_left',
        hidden: false,
        width: 120,
        flag: true,
    },
    {
        name: 'full_name',
        headerKey: 'party.colFullName',
        style: 'text_left',
        hidden: false,
        width: 250,
    },
    {
        name: 'party_type',
        headerKey: 'party.colPartyType',
        style: 'badge_centered',
        hidden: false,
        width: 120,
        codeDomain: 'party_type',
    },
    {
        name: 'status',
        headerKey: 'party.colStatus',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'party_status',
    },
    {
        name: 'business_center_code',
        headerKey: 'party.colBusinessCenterCode',
        style: 'icon_text_left',
        hidden: false,
        width: 130,
    },
    {
        name: 'version',
        headerKey: 'party.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'party.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'party.colRecordedAt',
        style: 'mono_left',
        hidden: true,
        width: 150,
        temporal: true,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 *
 * The image member is present because the model declares a flag, and the
 * shared screen renders its picker from this member rather than from a
 * field, because no form control edits an image.
 */
export const partyMeta = {
    entity: 'party',
    collection: 'parties',
    displayField: 'short_code',
    keyField: 'short_code',
    columns: partyColumns,
    fields: partyFields,
    image: { field: 'image_id', kind: 'flag' },
} as const;

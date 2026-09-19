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
 * judgement the model does not carry. See currencyPair_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a currency_pair, in the order the model declares them.
 *
 * `pair_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const currencyPairFields: readonly FieldMeta[] = [
    {
        name: 'pair_code',
        labelKey: 'currency_pair.fldPairCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'currency_pair.pairCodePh',
    },
    {
        name: 'base_currency',
        labelKey: 'currency_pair.fldBaseCurrency',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
    },
    {
        name: 'quote_currency',
        labelKey: 'currency_pair.fldQuoteCurrency',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
    },
    {
        name: 'classification',
        labelKey: 'currency_pair.fldClassification',
        control: 'static_combo',
        required: false,
        isKey: false,
        nullable: false,
        options: [
            { value: 'major', labelKey: 'currency_pair.type.major' },
            { value: 'minor', labelKey: 'currency_pair.type.minor' },
            { value: 'exotic', labelKey: 'currency_pair.type.exotic' },
            { value: 'commodity', labelKey: 'currency_pair.type.commodity' },
        ],
        codeDomain: 'currency_pair_classification',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const currencyPairColumns: readonly ColumnMeta[] = [
    {
        name: 'pair_code',
        headerKey: 'currency_pair.colPairCode',
        style: 'icon_text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'base_currency',
        headerKey: 'currency_pair.colBaseCurrency',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'quote_currency',
        headerKey: 'currency_pair.colQuoteCurrency',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'classification',
        headerKey: 'currency_pair.colClassification',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'currency_pair_classification',
    },
    {
        name: 'version',
        headerKey: 'currency_pair.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'currency_pair.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'currency_pair.colRecordedAt',
        style: 'mono_left',
        hidden: true,
        temporal: true,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const currencyPairMeta = {
    entity: 'currency_pair',
    collection: 'pairs',
    displayField: 'pair_code',
    keyField: 'pair_code',
    columns: currencyPairColumns,
    fields: currencyPairFields,
} as const;

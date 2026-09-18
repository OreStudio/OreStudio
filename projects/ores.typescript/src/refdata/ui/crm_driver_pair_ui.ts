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
 * judgement the model does not carry. See crmDriverPair_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a crm_driver_pair, in the order the model declares them.
 */
export const crmDriverPairFields: readonly FieldMeta[] = [
    {
        name: 'config_id',
        labelKey: 'crm_driver_pair.fldConfigId',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'crm_topology_configs', valueField: 'id', labelField: 'name' },
    },
    {
        name: 'base_currency_code',
        labelKey: 'crm_driver_pair.fldBaseCurrencyCode',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'quote_currency_code',
        labelKey: 'crm_driver_pair.fldQuoteCurrencyCode',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'enabled',
        labelKey: 'crm_driver_pair.fldEnabled',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const crmDriverPairColumns: readonly ColumnMeta[] = [
    {
        name: 'config_id',
        headerKey: 'crm_driver_pair.colConfigId',
        style: 'mono_left',
        hidden: true,
    },
    {
        name: 'base_currency_code',
        headerKey: 'crm_driver_pair.colBaseCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'quote_currency_code',
        headerKey: 'crm_driver_pair.colQuoteCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'enabled',
        headerKey: 'crm_driver_pair.colEnabled',
        style: 'badge_centered',
        hidden: false,
        width: 80,
        codeDomain: 'crm_enabled',
    },
    {
        name: 'version',
        headerKey: 'crm_driver_pair.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'crm_driver_pair.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'crm_driver_pair.colRecordedAt',
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
export const crmDriverPairMeta = {
    entity: 'crm_driver_pair',
    collection: 'crm_driver_pairs',
    displayField: '',
    keyField: 'id',
    columns: crmDriverPairColumns,
    fields: crmDriverPairFields,
} as const;

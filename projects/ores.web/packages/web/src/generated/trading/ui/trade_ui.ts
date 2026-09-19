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
 * judgement the model does not carry. See trade_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a trade, in the order the model declares them.
 *
 * `external_id` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const tradeFields: readonly FieldMeta[] = [
    {
        name: 'external_id',
        labelKey: 'trade.fldExternalId',
        control: 'line_edit',
        required: false,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: true,
        placeholderKey: 'trade.externalIdPh',
    },
    {
        name: 'trade_type',
        labelKey: 'trade.fldTradeType',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'trade.tradeTypePh',
    },
    {
        name: 'netting_set_id',
        labelKey: 'trade.fldNettingSetId',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'trade.nettingSetIdPh',
    },
    {
        name: 'trade_date',
        labelKey: 'trade.fldTradeDate',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: true,
        placeholderKey: 'trade.tradeDatePh',
    },
    {
        name: 'effective_date',
        labelKey: 'trade.fldEffectiveDate',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: true,
        placeholderKey: 'trade.effectiveDatePh',
    },
    {
        name: 'termination_date',
        labelKey: 'trade.fldTerminationDate',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: true,
        placeholderKey: 'trade.terminationDatePh',
    },
    {
        name: 'execution_timestamp',
        labelKey: 'trade.fldExecutionTimestamp',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'trade.executionTimestampPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const tradeColumns: readonly ColumnMeta[] = [
    {
        name: 'external_id',
        headerKey: 'trade.colExternalId',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'trade_type',
        headerKey: 'trade.colTradeType',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'trade_date',
        headerKey: 'trade.colTradeDate',
        style: 'text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'effective_date',
        headerKey: 'trade.colEffectiveDate',
        style: 'text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'termination_date',
        headerKey: 'trade.colTerminationDate',
        style: 'text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'netting_set_id',
        headerKey: 'trade.colNettingSetId',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'version',
        headerKey: 'trade.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'trade.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'trade.colRecordedAt',
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
 */
export const tradeMeta = {
    entity: 'trade',
    collection: 'trades',
    displayField: 'external_id',
    keyField: 'external_id',
    columns: tradeColumns,
    fields: tradeFields,
} as const;

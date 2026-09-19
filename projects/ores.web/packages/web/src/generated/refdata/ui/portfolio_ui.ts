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
 * judgement the model does not carry. See portfolio_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a portfolio, in the order the model declares them.
 *
 * `name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const portfolioFields: readonly FieldMeta[] = [
    {
        name: 'name',
        labelKey: 'portfolio.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'portfolio.namePh',
    },
    {
        name: 'purpose_type',
        labelKey: 'portfolio.fldPurposeType',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
    },
    {
        name: 'status',
        labelKey: 'portfolio.fldStatus',
        control: 'static_combo',
        required: false,
        isKey: false,
        nullable: false,
        options: [
            { value: 'Active', labelKey: 'portfolio.type.Active' },
            { value: 'Inactive', labelKey: 'portfolio.type.Inactive' },
            { value: 'Closed', labelKey: 'portfolio.type.Closed' },
            { value: 'Frozen', labelKey: 'portfolio.type.Frozen' },
            { value: 'Pending', labelKey: 'portfolio.type.Pending' },
        ],
    },
    {
        name: 'is_virtual',
        labelKey: 'portfolio.fldIsVirtual',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'aggregation_ccy',
        labelKey: 'portfolio.fldAggregationCcy',
        control: 'flagged_combo',
        required: false,
        isKey: false,
        nullable: true,
    },
    {
        name: 'description',
        labelKey: 'portfolio.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'portfolio.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const portfolioColumns: readonly ColumnMeta[] = [
    {
        name: 'name',
        headerKey: 'portfolio.colName',
        style: 'text_left',
        hidden: false,
        width: 250,
    },
    {
        name: 'purpose_type',
        headerKey: 'portfolio.colPurposeType',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'status',
        headerKey: 'portfolio.colStatus',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'portfolio_status',
    },
    {
        name: 'aggregation_ccy',
        headerKey: 'portfolio.colAggregationCcy',
        style: 'icon_text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'is_virtual',
        headerKey: 'portfolio.colIsVirtual',
        style: 'badge_centered',
        hidden: false,
        width: 70,
        codeDomain: 'is_virtual',
    },
    {
        name: 'version',
        headerKey: 'portfolio.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'portfolio.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'portfolio.colRecordedAt',
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
export const portfolioMeta = {
    entity: 'portfolio',
    collection: 'portfolios',
    displayField: 'name',
    keyField: 'name',
    columns: portfolioColumns,
    fields: portfolioFields,
} as const;

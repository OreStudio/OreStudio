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
 * judgement the model does not carry. See marketDataGenerationConfig_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a market_data_generation_config, in the order the model declares them.
 */
export const marketDataGenerationConfigFields: readonly FieldMeta[] = [
    {
        name: 'name',
        labelKey: 'market_data_generation_config.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'market_data_generation_config.namePh',
    },
    {
        name: 'description',
        labelKey: 'market_data_generation_config.fldDescription',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'market_data_generation_config.descriptionPh',
    },
    {
        name: 'enabled',
        labelKey: 'market_data_generation_config.fldEnabled',
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
export const marketDataGenerationConfigColumns: readonly ColumnMeta[] = [
    {
        name: 'name',
        headerKey: 'market_data_generation_config.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'description',
        headerKey: 'market_data_generation_config.colDescription',
        style: 'text_left',
        hidden: true,
        width: 300,
    },
    {
        name: 'scope',
        headerKey: 'market_data_generation_config.colScope',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'binding_mode',
        headerKey: 'market_data_generation_config.colBindingMode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'enabled',
        headerKey: 'market_data_generation_config.colEnabled',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'market_data_generation_config.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'market_data_generation_config.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'market_data_generation_config.colRecordedAt',
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
export const marketDataGenerationConfigMeta = {
    entity: 'market_data_generation_config',
    collection: 'market_data_generation_configs',
    displayField: 'name',
    keyField: 'id',
    columns: marketDataGenerationConfigColumns,
    fields: marketDataGenerationConfigFields,
} as const;

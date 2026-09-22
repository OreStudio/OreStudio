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
/**
 * The entity's own words, in English, keyed the way the catalogue is.
 *
 * The model states them: the detail field's label, the column's header, the
 * placeholder, the title and the brief. They are emitted here rather than
 * written into a catalogue by hand, so a label the model changes changes in
 * one place, and a language that has no translation yet falls back to these
 * rather than to a key nobody can read.
 */
export const marketDataGenerationConfigMessages = {
        market_data_generation_config: {
            title: 'Market Data Generation Configs',
            singular: 'market data generation config',
            newTitle: 'New market data generation config',
            description: 'A top-level container that owns one or more typed sub-configurations (FX spot now; vol surface, interest-rate curves later). It is the recipe for how synthetic market data is produced. Carries two orthogonal axes: scope (system/tenant/party) decides the sharing radius -- who consumes the same generated data -- and binding_mode (bound/sandboxed) decides whether that data is authoritative for real feed consumers or reachable only by explicit selection. tenant_id and party_id are populated per scope level: system leaves both null, tenant sets tenant_id only, party sets both.',
            fldName: 'Name',
            namePh: 'Enter config name',
            fldDescription: 'Description',
            descriptionPh: 'Enter description',
            fldEnabled: 'Enabled',
            colName: 'Name',
            colDescription: 'Description',
            colScope: 'Scope',
            colBindingMode: 'Binding Mode',
            colEnabled: 'Enabled',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};

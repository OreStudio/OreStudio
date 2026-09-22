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
 * The fields of a crm_topology_config, in the order the model declares them.
 *
 * `name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const crmTopologyConfigFields: readonly FieldMeta[] = [
    {
        name: 'name',
        labelKey: 'crm_topology_config.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'crm_topology_config.namePh',
    },
    {
        name: 'pivot_currency_code',
        labelKey: 'crm_topology_config.fldPivotCurrencyCode',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'enabled',
        labelKey: 'crm_topology_config.fldEnabled',
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
export const crmTopologyConfigColumns: readonly ColumnMeta[] = [
    {
        name: 'name',
        headerKey: 'crm_topology_config.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'pivot_currency_code',
        headerKey: 'crm_topology_config.colPivotCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'enabled',
        headerKey: 'crm_topology_config.colEnabled',
        style: 'badge_centered',
        hidden: false,
        width: 80,
        codeDomain: 'crm_enabled',
    },
    {
        name: 'version',
        headerKey: 'crm_topology_config.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'crm_topology_config.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'crm_topology_config.colRecordedAt',
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
export const crmTopologyConfigMeta = {
    entity: 'crm_topology_config',
    collection: 'crm_topology_configs',
    displayField: 'name',
    keyField: 'name',
    columns: crmTopologyConfigColumns,
    fields: crmTopologyConfigFields,
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
export const crmTopologyConfigMessages = {
        crm_topology_config: {
            title: 'CRM Topology Configs',
            singular: 'crm topology config',
            newTitle: 'New crm topology config',
            description: 'A top-level container that owns the [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][spanning-tree topology]]\'s pivot currency and the set of [[id:B38B3869-02FD-4CC7-99BD-9A77904ACA19][CRM]] driver pairs (crm_driver_pair, a separate, config_id-referencing entity) that hang off it. ores.marketdata.service reads the enabled config per (tenant, party) and its enabled driver pairs, feeds them through ores.analytics.quant::topology_builder::build, and keeps one service::rate_engine per (tenant, party) up to date -- see [[file:../../../doc/agile/versions/v0/sprint_23/crm_implementation/task_wire_crm_into_marketdata_ingest.org][the wiring task]]. UI metadata is deferred to a later task, as it is for market_series. Scoped to a tenant and a party so each party runs its own, independent CRM -- two parties in the same tenant may have entirely different topologies and never share an engine.',
            fldName: 'Name',
            namePh: 'e.g. primary',
            fldPivotCurrencyCode: 'Pivot Currency',
            fldEnabled: 'Enabled',
            colName: 'Name',
            colPivotCurrencyCode: 'Pivot Currency',
            colEnabled: 'Enabled',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};

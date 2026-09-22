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
 * The fields of a crm_enabled_derived_pair, in the order the model declares them.
 */
export const crmEnabledDerivedPairFields: readonly FieldMeta[] = [
    {
        name: 'config_id',
        labelKey: 'crm_enabled_derived_pair.fldConfigId',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'crm_topology_configs', valueField: 'id', labelField: 'name' },
    },
    {
        name: 'base_currency_code',
        labelKey: 'crm_enabled_derived_pair.fldBaseCurrencyCode',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'quote_currency_code',
        labelKey: 'crm_enabled_derived_pair.fldQuoteCurrencyCode',
        control: 'flagged_combo',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'enabled',
        labelKey: 'crm_enabled_derived_pair.fldEnabled',
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
export const crmEnabledDerivedPairColumns: readonly ColumnMeta[] = [
    {
        name: 'config_id',
        headerKey: 'crm_enabled_derived_pair.colConfigId',
        style: 'mono_left',
        hidden: true,
    },
    {
        name: 'base_currency_code',
        headerKey: 'crm_enabled_derived_pair.colBaseCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'quote_currency_code',
        headerKey: 'crm_enabled_derived_pair.colQuoteCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'enabled',
        headerKey: 'crm_enabled_derived_pair.colEnabled',
        style: 'badge_centered',
        hidden: false,
        width: 80,
        codeDomain: 'crm_enabled',
    },
    {
        name: 'version',
        headerKey: 'crm_enabled_derived_pair.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'crm_enabled_derived_pair.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'crm_enabled_derived_pair.colRecordedAt',
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
export const crmEnabledDerivedPairMeta = {
    entity: 'crm_enabled_derived_pair',
    collection: 'crm_enabled_derived_pairs',
    displayField: '',
    keyField: 'id',
    columns: crmEnabledDerivedPairColumns,
    fields: crmEnabledDerivedPairFields,
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
export const crmEnabledDerivedPairMessages = {
        crm_enabled_derived_pair: {
            title: 'CRM Enabled Derived Pairs',
            singular: 'crm enabled derived pair',
            newTitle: 'New crm enabled derived pair',
            description: 'A currency pair that is *not* a [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][spanning-tree]] edge (see crm_driver_pair) but is nonetheless curated as available for on-demand triangulation via marketdata.v1.crm.rate -- see [[id:DC08D216-348D-4511-A42D-4016EBBF38F7][the architecture decision]] to never broadcast the full reachable derived set, only serve a config-driven list on request. Owned by a crm_topology_config alongside its driver pairs. Scoped to a tenant and a party, matching its parent config.',
            fldConfigId: 'Config',
            fldBaseCurrencyCode: 'Base',
            fldQuoteCurrencyCode: 'Quote',
            fldEnabled: 'Enabled',
            colConfigId: 'Config Id',
            colBaseCurrencyCode: 'Base',
            colQuoteCurrencyCode: 'Quote',
            colEnabled: 'Enabled',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};

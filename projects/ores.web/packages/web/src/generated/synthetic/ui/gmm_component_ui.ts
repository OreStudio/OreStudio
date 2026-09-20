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
 * judgement the model does not carry, so regeneration cannot invent it.
 * An entity that needs tabs states them as its descriptor's `fieldGroups`.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a gmm_component, in the order the model declares them.
 */
export const gmmComponentFields: readonly FieldMeta[] = [
    {
        name: 'component_index',
        labelKey: 'gmm_component.fldComponentIndex',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'gmm_component.componentIndexPh',
        min: 0,
        max: 9999,
    },
    {
        name: 'description',
        labelKey: 'gmm_component.fldDescription',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'gmm_component.descriptionPh',
    },
    {
        name: 'mean',
        labelKey: 'gmm_component.fldMean',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'gmm_component.meanPh',
    },
    {
        name: 'stdev',
        labelKey: 'gmm_component.fldStdev',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'gmm_component.stdevPh',
    },
    {
        name: 'weight',
        labelKey: 'gmm_component.fldWeight',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'gmm_component.weightPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const gmmComponentColumns: readonly ColumnMeta[] = [
    {
        name: 'component_index',
        headerKey: 'gmm_component.colComponentIndex',
        style: 'mono_center',
        hidden: false,
        width: 120,
    },
    {
        name: 'description',
        headerKey: 'gmm_component.colDescription',
        style: 'text_left',
        hidden: true,
        width: 200,
    },
    {
        name: 'mean',
        headerKey: 'gmm_component.colMean',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'stdev',
        headerKey: 'gmm_component.colStdev',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'weight',
        headerKey: 'gmm_component.colWeight',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'gmm_component.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'gmm_component.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'gmm_component.colRecordedAt',
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
export const gmmComponentMeta = {
    entity: 'gmm_component',
    collection: 'gmm_components',
    displayField: '',
    keyField: 'id',
    columns: gmmComponentColumns,
    fields: gmmComponentFields,
} as const;

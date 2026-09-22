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
 * The fields of a role, in the order the model declares them.
 *
 * `name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const roleFields: readonly FieldMeta[] = [
    {
        name: 'name',
        labelKey: 'role.fldName',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'role.namePh',
    },
    {
        name: 'description',
        labelKey: 'role.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'role.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const roleColumns: readonly ColumnMeta[] = [
    {
        name: 'name',
        headerKey: 'role.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'description',
        headerKey: 'role.colDescription',
        style: 'text_left',
        hidden: true,
        width: 480,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const roleMeta = {
    entity: 'role',
    collection: 'roles',
    displayField: 'name',
    keyField: 'name',
    columns: roleColumns,
    fields: roleFields,
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
export const roleMessages = {
        role: {
            title: 'Roles',
            singular: 'role',
            newTitle: 'New role',
            description: 'A named collection of permissions that can be assigned to accounts. Roles group related permissions for easier management: a "Trading" role might include permissions to read and execute trades, while a "Support" role might have read-only access to most resources. The table is bi-temporal and audited (see projects/ores.sql/create/iam/iam_roles_create.sql): it carries version, the four audit columns and the valid_from/valid_to pair with the GIST exclusion and the delete rule, so the model takes the ordinary audited shape and needs no shape flag. The model describes the table alone. The hand-written domain struct also carried a std::vector<std::string> permission_codes that no column backs -- it is denormalised from ores_iam_role_permissions_tbl by an RBAC join. A joined shape is a message or a query result, never an entity member, so the member is not modelled and the generated role.hpp replaces it; the join itself stays in the hand-written authorization layer. The entity\'s CRUD handler and sub-registrar are switched off below: the hand-written role_handler already owns the iam.v1.roles.* subjects for the authorization protocol, and the generated role_handler.hpp would overwrite it. The generated role_protocol.hpp still declares the entity CRUD messages; only the competing handler is suppressed.',
            fldName: 'Name',
            namePh: 'Enter a role name',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            colName: 'Name',
            colDescription: 'Description',
        }
};

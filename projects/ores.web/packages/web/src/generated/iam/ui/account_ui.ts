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
 * The fields of a account, in the order the model declares them.
 *
 * `username` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const accountFields: readonly FieldMeta[] = [
    {
        name: 'username',
        labelKey: 'account.fldUsername',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'account.usernamePh',
    },
    {
        name: 'full_name',
        labelKey: 'account.fldFullName',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account.fullNamePh',
    },
    {
        name: 'email',
        labelKey: 'account.fldEmail',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'account.emailPh',
    },
    {
        name: 'job_title',
        labelKey: 'account.fldJobTitle',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account.jobTitlePh',
    },
    {
        name: 'account_type',
        labelKey: 'account.fldAccountType',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'account.accountTypePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const accountColumns: readonly ColumnMeta[] = [
    {
        name: 'username',
        headerKey: 'account.colUsername',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'full_name',
        headerKey: 'account.colFullName',
        style: 'text_left',
        hidden: false,
        width: 240,
    },
    {
        name: 'email',
        headerKey: 'account.colEmail',
        style: 'text_left',
        hidden: false,
        width: 260,
    },
    {
        name: 'job_title',
        headerKey: 'account.colJobTitle',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'account_type',
        headerKey: 'account.colAccountType',
        style: 'text_left',
        hidden: false,
        width: 140,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const accountMeta = {
    entity: 'account',
    collection: 'accounts',
    displayField: 'username',
    keyField: 'username',
    columns: accountColumns,
    fields: accountFields,
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
export const accountMessages = {
        account: {
            title: 'Accounts',
            singular: 'account',
            newTitle: 'New account',
            description: 'An account that can authenticate against the system: one row per user, service, algorithm or LLM identity, carrying the password material, the TOTP secret, the email address and the optional profile and reporting links. The table is bi-temporal and audited (see projects/ores.sql/create/iam/iam_accounts_create.sql): it carries version, the four audit columns and the valid_from/valid_to pair with the GIST exclusion, so the model takes the ordinary audited shape and needs no shape flag. The table is a composite parent: ores_iam_accounts_touch_version_fn lets a child entity (account contact information, party association) bump this account\'s own version when the child is written. The model declares :generate_touch_function: true, which renders that function under its existing name rather than leaving it hand-written. The model describes the table and nothing else. Two columns need care: - service_password_hash is a real column with no domain member: it is reached only by check_service_credentials and never travels on the wire, so it is declared :sql_only: true and the generated domain struct omits it while the entity struct and the mapper keep it. - image_id and reports_to_account_id are nullable UUID soft references. The hand-written domain struct represented both as a plain boost::uuids::uuid with a nil sentinel, on the claim that a second std::optional<boost::uuids::uuid> member corrupts reflect-cpp aggregate serialisation for multi-element vectors. Re-verified under the generated estate: all three nullable UUIDs are modelled as std::optional<boost::uuids::uuid>, and the api suite\'s multi-element JSON and table tests plus the core repository\'s five-account round trip pass, so the workaround is not needed here. Two behavioural facets are switched off, each with a reason: - The entity\'s CRUD handler and sub-registrar, because the hand-written account_operations_handler already owns every iam.v1.accounts.* subject. - The generated CRUD service, because the hand-written account_operations_service is the authentication surface (login, lock, unlock, password change and reset, party selection, service-credential check) and the generated service\'s get_account_history(id) collides in name and signature with the hand-written get_account_history(username) while meaning a different read. The generated account_protocol.hpp is suppressed by the same one-owner gate that the operation model already satisfies.',
            fldUsername: 'Username',
            usernamePh: 'Enter the account name',
            fldFullName: 'Full Name',
            fullNamePh: 'Enter the full name',
            fldEmail: 'Email',
            emailPh: 'Enter the email address',
            fldJobTitle: 'Job Title',
            jobTitlePh: 'Enter the job title',
            fldAccountType: 'Type',
            accountTypePh: 'Enter the account type',
            colUsername: 'Username',
            colFullName: 'Full Name',
            colEmail: 'Email',
            colJobTitle: 'Job Title',
            colAccountType: 'Type',
        }
};

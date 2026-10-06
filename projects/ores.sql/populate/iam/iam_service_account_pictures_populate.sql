/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: sql_service_account_pictures_populate.mustache
 *
 * Service Account Pictures
 *
 * Saves each service account's picture, the system avatar coded by its
 * registry name. Must run after the system avatars are published.
 *
 * This script is idempotent.
 */

\echo '--- Service Account Pictures ---'

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_iam_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'iam_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_refdata_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'refdata_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_workspace_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'workspace_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_dq_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'dq_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_variability_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'variability_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_assets_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'assets_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_scheduler_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'scheduler_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_reporting_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'reporting_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_telemetry_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'telemetry_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_trading_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'trading_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_compute_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'compute_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_synthetic_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'synthetic_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_workflow_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'workflow_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_ore_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'ore_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_marketdata_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'marketdata_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_analytics_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'analytics_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_http_server'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'http_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_storage_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'storage_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_inbox_service'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'inbox_service_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, username, account_type, account_status, full_name,
    password_hash, password_salt, service_password_hash, totp_secret, email,
    default_party_id, image_id, job_title, reports_to_account_id,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select a.id, a.tenant_id, a.version, a.username, a.account_type, a.account_status,
       a.full_name, a.password_hash, a.password_salt, a.service_password_hash,
       a.totp_secret, a.email, a.default_party_id, i.id, a.job_title,
       a.reports_to_account_id, current_user, current_user, 'system.initial_load',
       'Attached the service''s picture', current_timestamp,
       ores_utility_infinity_timestamp_fn()
from ores_iam_accounts_tbl a
join ores_assets_images_tbl i
  on i.tenant_id = a.tenant_id
 and i.code = 'ores_compute_wrapper'
 and i.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = ores_utility_system_tenant_id_fn()
  and a.username = :'compute_wrapper_user'
  and a.image_id is null
  and a.valid_to = ores_utility_infinity_timestamp_fn();

-- Summary
select 'Service Account Pictures' as entity, count(*) as count
from ores_iam_accounts_tbl
where account_type = 'service'
  and image_id is not null
  and valid_to = ores_utility_infinity_timestamp_fn();

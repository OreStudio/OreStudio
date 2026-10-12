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
 * Publication of the analytics configuration documents from DQ datasets
 *
 * Each function publishes one document for the party a publish names. The copy,
 * the re-keying per party and the reporting configuration are the shared
 * ores_dq_publish_configuration_document_fn; the function here names the document
 * and its tables.
 */

-- =============================================================================
-- Pricing engines: analytics.v1.ops.publish_pricing_engines_from_dq
-- =============================================================================

create or replace function ores_analytics_publish_pricing_engines_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_party_id uuid;
begin
    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    return query
    select * from ores_dq_publish_configuration_document_fn(
        p_dataset_id, p_target_tenant_id, v_party_id,
        'pricing_engines', 'ores.analytics', 'ores_analytics_',
        array['pricing_model_configs', 'pricing_model_products',
              'pricing_model_product_parameters']);
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_analytics_publish_pricing_engines_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the pricing engines document a dataset stages for the target party, with
 its reporting configuration. Idempotent per row. A publish only inserts, so the
 mode has no effect.';

-- =============================================================================
-- Today's market: analytics.v1.ops.publish_todays_market_from_dq
-- =============================================================================

create or replace function ores_analytics_publish_todays_market_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_party_id uuid;
begin
    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    return query
    select * from ores_dq_publish_configuration_document_fn(
        p_dataset_id, p_target_tenant_id, v_party_id,
        'todays_market', 'ores.analytics', 'ores_analytics_',
        array['todays_market_configs', 'todays_market_collections', 'todays_market_entries',
              'todays_market_configurations', 'todays_market_configuration_bindings']);
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_analytics_publish_todays_market_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the today''s market document a dataset stages for the target party, with
 its reporting configuration. Idempotent per row. A publish only inserts, so the
 mode has no effect.';

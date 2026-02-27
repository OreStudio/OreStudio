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
 * Books Publication Function
 *
 * Copies book artefact data into the refdata table for a target tenant.
 * Books have mandatory FKs to portfolios (parent_portfolio_id) and parties
 * (party_id). Portfolio references are resolved by matching names against
 * already-published refdata. The party is resolved from p_params or by
 * querying the tenant's operational root party.
 *
 * Books have no self-referencing hierarchy so a single bulk INSERT suffices.
 * Portfolios must be published before books.
 *
 * @param p_dataset_id       The DQ dataset containing book artefacts
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode (only 'upsert' supported)
 * @param p_params           Optional: {"party_id": "<uuid>"} to override root party
 */

-- =============================================================================
-- Books Publication
-- =============================================================================

create or replace function ores_dq_books_publish_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_root_party_id uuid;
    v_portfolio_dataset_id uuid;
    v_inserted bigint := 0;
begin
    -- Idempotency: skip if tenant already has books
    if exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Resolve root party: explicit param or query tenant's operational root
    v_root_party_id := coalesce(
        (p_params ->> 'party_id')::uuid,
        (select id from ores_refdata_parties_tbl
         where tenant_id = p_target_tenant_id
           and parent_party_id is null
           and party_category <> 'System'
           and valid_to = ores_utility_infinity_timestamp_fn()
         limit 1)
    );

    if v_root_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    -- Find the portfolios artefact dataset for parent_portfolio_id mapping
    select id into v_portfolio_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.portfolios'
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- Build portfolio reference map: artefact portfolio ID -> published portfolio ID
    -- Joined on name since IDs differ between artefact and published data.
    -- Also carries owner_unit_id from the published portfolio for book FK.
    create temp table portfolio_ref_map (
        artefact_id uuid primary key,
        published_id uuid not null,
        published_owner_unit_id uuid
    ) on commit drop;

    if v_portfolio_dataset_id is not null then
        insert into portfolio_ref_map (artefact_id, published_id, published_owner_unit_id)
        select a.id, r.id, r.owner_unit_id
        from ores_dq_portfolios_artefact_tbl a
        join ores_refdata_portfolios_tbl r
            on r.name = a.name
            and r.tenant_id = p_target_tenant_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
        where a.dataset_id = v_portfolio_dataset_id;
    end if;

    -- Bulk insert all books (no self-referencing hierarchy)
    insert into ores_refdata_books_tbl (
        tenant_id, id, version, party_id, name,
        parent_portfolio_id, ledger_ccy, gl_account_ref, cost_center,
        book_status, is_trading_book, owner_unit_id,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, v_root_party_id, a.name,
        pmap.published_id, a.ledger_ccy, a.gl_account_ref, a.cost_center,
        a.book_status, a.is_trading_book, pmap.published_owner_unit_id,
        coalesce(ores_iam_current_actor_fn(), current_user), current_user, 'system.external_data_import',
        'Published from organisation dataset'
    from ores_dq_books_artefact_tbl a
    join portfolio_ref_map pmap on pmap.artefact_id = a.parent_portfolio_id
    where a.dataset_id = p_dataset_id;

    get diagnostics v_inserted = row_count;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer;

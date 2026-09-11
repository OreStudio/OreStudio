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
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Trade Envelope Table
 *
 * One row per trade whose document stated an envelope, keyed to the
 * trade.
 *
 * The envelope is the block a document wraps around a trade. It names a
 * counterparty, a netting set, a list of portfolios and a list of further
 * fields the document states in its own vocabulary. The schema states all
 * four as optional and the last as open content.
 *
 * The trade row already carries a counterparty and a netting set, but
 * those are the trade's own attributes and the trade states them
 * unconditionally. The envelope is the document's statement and may omit
 * either. A row here holds what the document said, so a reader
 * distinguishes an omitted member from a stated one.
 *
 * A scalar member needs no flag of its own: a null column says the
 * document omitted it and an empty string says the document stated it
 * empty. The two list members are not like that, because an omitted list
 * and a stated empty list both write no child row. Each therefore carries
 * a flag on this row that says the document stated it.
 */

create table if not exists "ores_trading_trade_envelopes_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "counter_party" text null,
    "netting_set_id" text null,
    "has_portfolio_ids" boolean not null,
    "has_additional_fields" boolean not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn())
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trade_envelopes_version_uniq_idx
on "ores_trading_trade_envelopes_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trade_envelopes_id_uniq_idx
on "ores_trading_trade_envelopes_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_envelopes_tenant_idx
on "ores_trading_trade_envelopes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_trade_envelopes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_trade_envelopes_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_trade_envelopes_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_trade_envelopes_insert_trg
before insert on "ores_trading_trade_envelopes_tbl"
for each row execute function ores_trading_trade_envelopes_insert_fn();

create or replace rule ores_trading_trade_envelopes_delete_rule as
on delete to "ores_trading_trade_envelopes_tbl" do instead (
    update "ores_trading_trade_envelopes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

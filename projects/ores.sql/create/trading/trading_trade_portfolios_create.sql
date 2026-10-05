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
 * Trade Portfolio Table
 *
 * A portfolio a trade is reported in. A trade is filed in one book, and the
 * book sits in one portfolio tree; a trade may also be reported in other
 * portfolios, as ORE's Envelope/PortfolioIds states, each by the portfolio's
 * name. The row resolves such a name to a portfolio of the trade's party, so a
 * name that names no portfolio is refused rather than kept as text. The order is
 * the source's order, and the ordinal preserves it.
 *
 * The row is keyed by the trade and the ordinal, and references the
 * [[id:4304A441-E532-45FB-837A-378F13693CAE][trade anchor]] with a database foreign key. It copies the anchor's party,
 * pinned to the anchor and to the portfolio, because row-level security needs
 * the party on every row and a trade is reported only in its own party's
 * portfolios. It replaces the envelope's portfolio id table.
 */

create table if not exists "ores_trading_trade_portfolios_tbl" (
    "trade_id" uuid not null,
    "sequence_number" integer not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "portfolio_id" uuid not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, sequence_number, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        sequence_number WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("sequence_number" > 0),
    constraint ores_trading_trade_portfolios_trade_id_fk foreign key ("tenant_id", "trade_id") references "ores_trading_trades_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_portfolios_anchor_party_pin foreign key ("tenant_id", "trade_id", "party_id") references "ores_trading_trades_tbl" ("tenant_id", "id", "party_id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trade_portfolios_version_uniq_idx
on "ores_trading_trade_portfolios_tbl" (tenant_id, trade_id, sequence_number, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trade_portfolios_id_uniq_idx
on "ores_trading_trade_portfolios_tbl" (tenant_id, trade_id, sequence_number)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_portfolios_tenant_idx
on "ores_trading_trade_portfolios_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_portfolios_portfolio_idx
on "ores_trading_trade_portfolios_tbl" (tenant_id, portfolio_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_trade_portfolios_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate portfolio_id (soft FK to ores_refdata_portfolios_tbl)
    if not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid portfolio_id: %. No active portfolio found with this id.', NEW.portfolio_id
            using errcode = '23503';
    end if;

    -- Validate the portfolio_party pin to ores_refdata_portfolios_tbl
    if NEW.portfolio_id is not null and NEW.party_id is not null and not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.portfolio_id
          and party_id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid portfolio_id: %. The portfolio must be the trade''s party''s.', NEW.portfolio_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_trade_portfolios_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id and sequence_number = NEW.sequence_number
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                raise exception
                    'Row already exists: a create cannot replace it. State the version you read to replace the row, or ask for a version replace.'
                    using errcode = '23505';
            end if;
        elsif NEW.version != current_version then
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
        update "ores_trading_trade_portfolios_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id and sequence_number = NEW.sequence_number
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

create or replace trigger ores_trading_trade_portfolios_insert_trg
before insert on "ores_trading_trade_portfolios_tbl"
for each row execute function ores_trading_trade_portfolios_insert_fn();

create or replace rule ores_trading_trade_portfolios_delete_rule as
on delete to "ores_trading_trade_portfolios_tbl" do instead (
    update "ores_trading_trade_portfolios_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id and sequence_number = OLD.sequence_number
      and valid_to = ores_utility_infinity_timestamp_fn();
);

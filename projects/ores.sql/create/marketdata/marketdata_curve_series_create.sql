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
 * Publication of a curve's own market series
 *
 * The curve configuration a party publishes defines the curves that party
 * marks its trades on, and a curve has to exist as a series before anything can
 * name it: a trade's reference curve names one, and the validator that refuses
 * a reference the catalogue does not hold reads this table. Nothing else
 * creates these rows for a curve, because a curve's identity is not a datum any
 * feed publishes -- it is the curve configuration's to state.
 *
 * The series is keyed by the oresmd URI the curve's own three coordinates
 * spell: the currency, the curve id and the day counter, with the type
 * 'series' because a curve's points hang off it. That is the same URI a
 * trade's reference_curve_id carries, so a reference resolves to exactly the
 * series its config published.
 *
 * The rows belong to the party the configuration was published to, so the
 * party's own curve is what its trades resolve, never another entity's curve
 * of the same id. Publishing twice converges on the same rows: the natural key
 * is the party and the URI, and the function only inserts what is absent.
 *
 * @param p_tenant_id the tenant that receives the series.
 * @param p_party_id  the party whose curve configuration was published.
 * @param p_curves    the curve definitions published, as
 *                    {"currency": ..., "curve_id": ..., "day_counter": ...}.
 */
create or replace function ores_marketdata_publish_curve_series_fn(
    p_tenant_id uuid,
    p_party_id uuid,
    p_curves jsonb
)
returns integer as $$
declare
    v_inserted integer := 0;
    v_curve jsonb;
    v_uri text;
    v_id uuid;
begin
    for v_curve in select * from jsonb_array_elements(p_curves)
    loop
        v_uri := format(
            'oresmd://ir/%s?type=series&instrument=zero&quote=rate&curve_id=%s&day_counter=%s',
            v_curve ->> 'currency', v_curve ->> 'curve_id', v_curve ->> 'day_counter');

        if exists (
            select 1 from ores_marketdata_market_series_tbl s
            where s.tenant_id = p_tenant_id
              and s.party_id = p_party_id
              and s.oresmd_uri = v_uri
              and s.valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            continue;
        end if;

        v_id := ores_dq_party_uuid_fn(p_party_id, md5(v_uri)::uuid);

        insert into ores_marketdata_market_series_tbl (
            id, tenant_id, version, party_id, oresmd_uri, series_subclass,
            producer_kind, derivation_kind, derivation_config_id,
            derivation_config_version, modified_by, performed_by,
            change_reason_code, change_commentary
        )
        values (
            v_id, p_tenant_id, 0, p_party_id, v_uri, 'yield',
            'SYNTHETIC', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'Curve configuration of the party it belongs to'
        );

        insert into ores_marketdata_market_series_asset_classes_tbl (
            market_series_id, tenant_id, asset_class_code, version,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        values (
            v_id, p_tenant_id, 'interest_rates', 0,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'A bootstrapped curve is an interest rates series'
        )
        on conflict (tenant_id, market_series_id, asset_class_code)
        where valid_to = ores_utility_infinity_timestamp_fn()
        do nothing;

        v_inserted := v_inserted + 1;
    end loop;

    return v_inserted;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_marketdata_publish_curve_series_fn(uuid, uuid, jsonb) is
'Creates the market series a party''s published curve configuration states, one per
 curve, keyed by the oresmd URI of the curve''s currency, id and day counter. Idempotent
 per series.';

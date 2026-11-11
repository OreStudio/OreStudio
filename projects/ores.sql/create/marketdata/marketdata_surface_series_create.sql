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
 * The market series of a surface blueprint
 *
 * A blueprint names two series: the pair's fx_option series, which the derived
 * source writes and every pillar reads, and the surface's own fx_vol_surface
 * series, which the calibration writes. Each is found by its oresmd URI, so a
 * series the derived feed already created is the one the blueprint names. A
 * missing series is created:
 *
 *  - The fx_option series carries the derived source's own derivation stamp,
 *    derived_fx_method_id() of ores.synthetic's derived_fx_vol.cpp at version 1,
 *    so the feed finds it and writes into it, and does not create a second one.
 *
 *  - The fx_vol_surface series starts OBSERVED, as a bootstrapped curve's series
 *    does, and the calibration claims it on its first run.
 *
 * A new series is keyed by its party and URI, so a rerun converges on the same
 * rows.
 *
 * @param p_tenant_id  the tenant that holds the blueprint.
 * @param p_party_id   the party that holds the blueprint.
 * @param p_base       the pair's base currency.
 * @param p_quote      the pair's quote currency.
 * @param p_surface_id the blueprint's name, which the surface series carries.
 */
create or replace function ores_marketdata_publish_surface_series_fn(
    p_tenant_id uuid,
    p_party_id uuid,
    p_base text,
    p_quote text,
    p_surface_id text,
    out quotes_series_id uuid,
    out surface_series_id uuid
) as $$
declare
    v_quotes_uri text := format(
        'oresmd://fx/%s?type=series&instrument=fx_option&quote=rate_lnvol&ccy=%s',
        p_base, p_quote);
    v_surface_uri text := format(
        'oresmd://fx/%s?type=series&instrument=fx_vol_surface&quote=rate_lnvol&ccy=%s&surface_id=%s',
        p_base, p_quote, p_surface_id);
begin
    select s.id into quotes_series_id
    from ores_marketdata_market_series_tbl s
    where s.tenant_id = p_tenant_id
      and s.party_id = p_party_id
      and s.oresmd_uri = v_quotes_uri
      and s.provider_code = 'derived'
      and s.valid_to = ores_utility_infinity_timestamp_fn();

    if quotes_series_id is null then
        quotes_series_id := ores_dq_party_uuid_fn(p_party_id, md5('derived|' || v_quotes_uri)::uuid);
        insert into ores_marketdata_market_series_tbl (
            id, tenant_id, version, party_id, oresmd_uri, series_subclass,
            provider_code, derivation_kind, derivation_config_id,
            derivation_config_version, modified_by, performed_by,
            change_reason_code, change_commentary
        )
        values (
            quotes_series_id, p_tenant_id, 0, p_party_id, v_quotes_uri, 'volatility',
            'derived', 'FX_VOL_DERIVATION', 'fefdf018-d882-5c4e-bd27-2aeedfcad9c9'::uuid, 1,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'The ' || p_base || '/' || p_quote || ' quotes the derived source writes, which '
                || p_surface_id || ' reads'
        );
    end if;

    select s.id into surface_series_id
    from ores_marketdata_market_series_tbl s
    where s.tenant_id = p_tenant_id
      and s.party_id = p_party_id
      and s.oresmd_uri = v_surface_uri
      and s.provider_code = 'ore'
      and s.valid_to = ores_utility_infinity_timestamp_fn();

    if surface_series_id is null then
        surface_series_id := ores_dq_party_uuid_fn(p_party_id, md5('ore|' || v_surface_uri)::uuid);
        insert into ores_marketdata_market_series_tbl (
            id, tenant_id, version, party_id, oresmd_uri, series_subclass,
            provider_code, derivation_kind, derivation_config_id,
            derivation_config_version, modified_by, performed_by,
            change_reason_code, change_commentary
        )
        values (
            surface_series_id, p_tenant_id, 0, p_party_id, v_surface_uri, 'volatility',
            'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'The calibrated ' || p_surface_id || ' surface: claimed by the calibration on its first run'
        );
    end if;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_marketdata_publish_surface_series_fn(uuid, uuid, text, text, text) is
'Finds or creates the two market series a surface blueprint names: the pair''s
 derived fx_option series and the surface''s own fx_vol_surface series. Idempotent
 per series.';

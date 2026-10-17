/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
// AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
// Generator: projects/ores.codegen/scripts/regenerate_convention_family_registry.py
// To change a call, edit the convention entity model's #+convention_family: or
// #+convention_write: declaration and regenerate. Include this fragment inside
// the anonymous namespace that defines replace_party_rows, replace_rows_by_id
// and add_missing_world_rows.


// Party families: the import replaces the rows the party holds.
void apply_party_families(const ores::database::context& ctx,
                          messaging::conventions_document& v) {
    replace_party_rows(ctx,
                       average_ois_convention_repository(),
                       std::move(v.average_ois));
    replace_party_rows(ctx,
                       bma_basis_swap_convention_repository(),
                       std::move(v.bma_basis_swap));
    replace_party_rows(ctx,
                       bond_yield_convention_repository(),
                       std::move(v.bond_yield));
    replace_party_rows(ctx,
                       cds_convention_repository(),
                       std::move(v.cds));
    replace_party_rows(ctx,
                       cms_spread_option_convention_repository(),
                       std::move(v.cms_spread_option));
    replace_party_rows(ctx,
                       commodity_forward_convention_repository(),
                       std::move(v.commodity_forward));
    replace_party_rows(ctx,
                       commodity_future_convention_repository(),
                       std::move(v.commodity_future));
    replace_party_rows(ctx,
                       cross_currency_basis_convention_repository(),
                       std::move(v.cross_currency_basis));
    replace_party_rows(ctx,
                       cross_currency_fix_float_convention_repository(),
                       std::move(v.cross_currency_fix_float));
    replace_party_rows(ctx,
                       deposit_convention_repository(),
                       std::move(v.deposit));
    replace_party_rows(ctx,
                       fra_convention_repository(),
                       std::move(v.fra));
    replace_party_rows(ctx,
                       future_convention_repository(),
                       std::move(v.future));
    replace_party_rows(ctx,
                       fx_option_convention_repository(),
                       std::move(v.fx_option));
    replace_party_rows(ctx,
                       inflation_swap_convention_repository(),
                       std::move(v.inflation_swap));
    replace_party_rows(ctx,
                       intraday_power_load_convention_repository(),
                       std::move(v.intraday_power_load));
    replace_party_rows(ctx,
                       ois_convention_repository(),
                       std::move(v.ois));
    replace_party_rows(ctx,
                       swap_convention_repository(),
                       std::move(v.swap));
    replace_party_rows(ctx,
                       swap_index_convention_repository(),
                       std::move(v.swap_index));
    replace_party_rows(ctx,
                       tenor_basis_two_swap_convention_repository(),
                       std::move(v.tenor_basis_two_swap));
    replace_party_rows(ctx,
                       zero_convention_repository(),
                       std::move(v.zero));
    replace_party_rows(ctx,
                       zero_inflation_index_convention_repository(),
                       std::move(v.zero_inflation_index));
}

// By-id families: the import replaces the rows the document names.
void apply_families_by_id(const ores::database::context& ctx,
                         messaging::conventions_document& v) {
    replace_rows_by_id(ctx,
                       convention_repository(),
                       std::move(v.convention));
    replace_rows_by_id(ctx,
                       tenor_basis_swap_long_short_repository(),
                       std::move(v.tenor_basis_swap_long_short));
    replace_rows_by_id(ctx,
                       tenor_basis_swap_pay_receive_repository(),
                       std::move(v.tenor_basis_swap_pay_receive));
}

// World families: the import adds only the rows the tenant is missing.
void apply_world_families(const ores::database::context& ctx,
                          messaging::conventions_document& v,
                          conventions_save_result& result) {
    add_missing_world_rows(ctx,
                           ibor_index_convention_repository(),
                           v.ibor_index,
                           result.world_kept);
    add_missing_world_rows(ctx,
                           overnight_index_convention_repository(),
                           v.overnight_index,
                           result.world_kept);
}

// The document reads each mode back from the same registry.
void read_party_families(const ores::database::context& ctx,
                         messaging::conventions_document& v) {
    v.average_ois = average_ois_convention_repository().read_latest(ctx);
    v.bma_basis_swap = bma_basis_swap_convention_repository().read_latest(ctx);
    v.bond_yield = bond_yield_convention_repository().read_latest(ctx);
    v.cds = cds_convention_repository().read_latest(ctx);
    v.cms_spread_option = cms_spread_option_convention_repository().read_latest(ctx);
    v.commodity_forward = commodity_forward_convention_repository().read_latest(ctx);
    v.commodity_future = commodity_future_convention_repository().read_latest(ctx);
    v.cross_currency_basis = cross_currency_basis_convention_repository().read_latest(ctx);
    v.cross_currency_fix_float = cross_currency_fix_float_convention_repository().read_latest(ctx);
    v.deposit = deposit_convention_repository().read_latest(ctx);
    v.fra = fra_convention_repository().read_latest(ctx);
    v.future = future_convention_repository().read_latest(ctx);
    v.fx_option = fx_option_convention_repository().read_latest(ctx);
    v.inflation_swap = inflation_swap_convention_repository().read_latest(ctx);
    v.intraday_power_load = intraday_power_load_convention_repository().read_latest(ctx);
    v.ois = ois_convention_repository().read_latest(ctx);
    v.swap = swap_convention_repository().read_latest(ctx);
    v.swap_index = swap_index_convention_repository().read_latest(ctx);
    v.tenor_basis_two_swap = tenor_basis_two_swap_convention_repository().read_latest(ctx);
    v.zero = zero_convention_repository().read_latest(ctx);
    v.zero_inflation_index = zero_inflation_index_convention_repository().read_latest(ctx);
}

void read_families_by_id(const ores::database::context& ctx,
                         messaging::conventions_document& v) {
    v.convention = convention_repository().read_latest(ctx);
    v.tenor_basis_swap_long_short = tenor_basis_swap_long_short_repository().read_latest(ctx);
    v.tenor_basis_swap_pay_receive = tenor_basis_swap_pay_receive_repository().read_latest(ctx);
}

void read_world_families(const ores::database::context& ctx,
                         messaging::conventions_document& v) {
    v.ibor_index = ibor_index_convention_repository().read_latest(ctx);
    v.overnight_index = overnight_index_convention_repository().read_latest(ctx);
}

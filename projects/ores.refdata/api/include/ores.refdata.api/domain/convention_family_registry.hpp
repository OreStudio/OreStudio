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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generator: projects/ores.codegen/scripts/regenerate_convention_family_registry.py
 * To change an entry, edit the convention entity model's
 * #+convention_family: or #+convention_write: declaration and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_FAMILY_REGISTRY_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_FAMILY_REGISTRY_HPP

#include <cstddef>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief How the conventions document writes a family's rows.
 *
 * party replaces the rows the importing party holds, by_id replaces the rows
 * the document names by key, and world adds only rows the tenant does not
 * already hold.
 */
enum class convention_write_mode { party, by_id, world };

/**
 * @brief One convention family the conventions document carries.
 *
 * key is the family's field name in the document, entity is the entity model
 * that serves it, domain_type is the generated C++ value type, subject_stem is
 * the protocol prefix every verb of that entity shares, party_scoped is true
 * when the family's table carries a party_id column, and write_mode is how the
 * document writes the family.
 */
struct convention_family_entry {
    std::string_view key;
    std::string_view entity;
    std::string_view domain_type;
    std::string_view subject_stem;
    bool party_scoped;
    convention_write_mode write_mode;
};

/**
 * @brief Every convention family, ordered by family key.
 */
inline constexpr convention_family_entry convention_families[] = {
    {"average_ois",
     "average_ois_convention",
     "ores::refdata::domain::average_ois_convention",
     "refdata.v1.average_ois_conventions",
     true,
     convention_write_mode::party},
    {"bma_basis_swap",
     "bma_basis_swap_convention",
     "ores::refdata::domain::bma_basis_swap_convention",
     "refdata.v1.bma_basis_swap_conventions",
     true,
     convention_write_mode::party},
    {"bond_yield",
     "bond_yield_convention",
     "ores::refdata::domain::bond_yield_convention",
     "refdata.v1.bond_yield_conventions",
     true,
     convention_write_mode::party},
    {"cds",
     "cds_convention",
     "ores::refdata::domain::cds_convention",
     "refdata.v1.cds_conventions",
     true,
     convention_write_mode::party},
    {"cms_spread_option",
     "cms_spread_option_convention",
     "ores::refdata::domain::cms_spread_option_convention",
     "refdata.v1.cms_spread_option_conventions",
     true,
     convention_write_mode::party},
    {"commodity_forward",
     "commodity_forward_convention",
     "ores::refdata::domain::commodity_forward_convention",
     "refdata.v1.commodity_forward_conventions",
     true,
     convention_write_mode::party},
    {"commodity_future",
     "commodity_future_convention",
     "ores::refdata::domain::commodity_future_convention",
     "refdata.v1.commodity_future_conventions",
     true,
     convention_write_mode::party},
    {"convention",
     "convention",
     "ores::refdata::domain::convention",
     "refdata.v1.conventions",
     true,
     convention_write_mode::by_id},
    {"cross_currency_basis",
     "cross_currency_basis_convention",
     "ores::refdata::domain::cross_currency_basis_convention",
     "refdata.v1.cross_currency_basis_conventions",
     true,
     convention_write_mode::party},
    {"cross_currency_fix_float",
     "cross_currency_fix_float_convention",
     "ores::refdata::domain::cross_currency_fix_float_convention",
     "refdata.v1.cross_currency_fix_float_conventions",
     true,
     convention_write_mode::party},
    {"deposit",
     "deposit_convention",
     "ores::refdata::domain::deposit_convention",
     "refdata.v1.deposit_conventions",
     true,
     convention_write_mode::party},
    {"fra",
     "fra_convention",
     "ores::refdata::domain::fra_convention",
     "refdata.v1.fra_conventions",
     true,
     convention_write_mode::party},
    {"future",
     "future_convention",
     "ores::refdata::domain::future_convention",
     "refdata.v1.future_conventions",
     true,
     convention_write_mode::party},
    {"fx_option",
     "fx_option_convention",
     "ores::refdata::domain::fx_option_convention",
     "refdata.v1.fx_option_conventions",
     true,
     convention_write_mode::party},
    {"ibor_index",
     "ibor_index_convention",
     "ores::refdata::domain::ibor_index_convention",
     "refdata.v1.ibor_index_conventions",
     false,
     convention_write_mode::world},
    {"inflation_swap",
     "inflation_swap_convention",
     "ores::refdata::domain::inflation_swap_convention",
     "refdata.v1.inflation_swap_conventions",
     true,
     convention_write_mode::party},
    {"intraday_power_load",
     "intraday_power_load_convention",
     "ores::refdata::domain::intraday_power_load_convention",
     "refdata.v1.intraday_power_load_conventions",
     true,
     convention_write_mode::party},
    {"ois",
     "ois_convention",
     "ores::refdata::domain::ois_convention",
     "refdata.v1.ois_conventions",
     true,
     convention_write_mode::party},
    {"overnight_index",
     "overnight_index_convention",
     "ores::refdata::domain::overnight_index_convention",
     "refdata.v1.overnight_index_conventions",
     false,
     convention_write_mode::world},
    {"swap",
     "swap_convention",
     "ores::refdata::domain::swap_convention",
     "refdata.v1.swap_conventions",
     true,
     convention_write_mode::party},
    {"swap_index",
     "swap_index_convention",
     "ores::refdata::domain::swap_index_convention",
     "refdata.v1.swap_index_conventions",
     true,
     convention_write_mode::party},
    {"tenor_basis_swap_long_short",
     "tenor_basis_swap_long_short",
     "ores::refdata::domain::tenor_basis_swap_long_short",
     "refdata.v1.tenor_basis_swap_long_shorts",
     true,
     convention_write_mode::by_id},
    {"tenor_basis_swap_pay_receive",
     "tenor_basis_swap_pay_receive",
     "ores::refdata::domain::tenor_basis_swap_pay_receive",
     "refdata.v1.tenor_basis_swap_pay_receives",
     true,
     convention_write_mode::by_id},
    {"tenor_basis_two_swap",
     "tenor_basis_two_swap_convention",
     "ores::refdata::domain::tenor_basis_two_swap_convention",
     "refdata.v1.tenor_basis_two_swap_conventions",
     true,
     convention_write_mode::party},
    {"zero",
     "zero_convention",
     "ores::refdata::domain::zero_convention",
     "refdata.v1.zero_conventions",
     true,
     convention_write_mode::party},
    {"zero_inflation_index",
     "zero_inflation_index_convention",
     "ores::refdata::domain::zero_inflation_index_convention",
     "refdata.v1.zero_inflation_index_conventions",
     true,
     convention_write_mode::party},
};

inline constexpr std::size_t convention_family_count =
    sizeof(convention_families) / sizeof(convention_families[0]);

}

#endif

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
#ifndef ORES_ORE_CORE_DOMAIN_CONVENTIONS_FAMILY_UNITS_HPP
#define ORES_ORE_CORE_DOMAIN_CONVENTIONS_FAMILY_UNITS_HPP

#include "ores.ore.core/xml/record.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include <cstddef>
#include <string_view>
#include <vector>

/**
 * @file conventions_family_units.hpp
 * @brief What each convention family's unit offers the package.
 *
 * Each family has a translation unit of its own that reads the family's
 * elements out of the codec's record and packs them, and unpacks a row and
 * writes it back. The declarations take records and strings alone and name no
 * family type, so the code that walks all the families reflects none of them.
 *
 * The tenor basis swap is one document element held by three families, and the
 * FX conventions are a projection that no family holds, so both have a unit of
 * their own that is not part of the table.
 */
namespace ores::ore::domain::conventions_family {

using package_row = refdata::service::conventions_bundle_row;

/**
 * @brief How one family packs and joins, as the table in the package states.
 */
struct family_codec {
    std::string_view family;
    void (*pack)(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
    void (*join)(const package_row& row, xml::record_writer& root);
};

void pack_average_ois(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_average_ois(const package_row& row, xml::record_writer& root);

void pack_bma_basis_swap(const xml::record_reader& root,
                         refdata::service::conventions_bundle& bundle);
void join_bma_basis_swap(const package_row& row, xml::record_writer& root);

void pack_bond_yield(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_bond_yield(const package_row& row, xml::record_writer& root);

void pack_cds(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_cds(const package_row& row, xml::record_writer& root);

void pack_cms_spread_option(const xml::record_reader& root,
                            refdata::service::conventions_bundle& bundle);
void join_cms_spread_option(const package_row& row, xml::record_writer& root);

void pack_commodity_forward(const xml::record_reader& root,
                            refdata::service::conventions_bundle& bundle);
void join_commodity_forward(const package_row& row, xml::record_writer& root);

void pack_commodity_future(const xml::record_reader& root,
                           refdata::service::conventions_bundle& bundle);
void join_commodity_future(const package_row& row, xml::record_writer& root);

void pack_cross_currency_basis(const xml::record_reader& root,
                               refdata::service::conventions_bundle& bundle);
void join_cross_currency_basis(const package_row& row, xml::record_writer& root);

void pack_cross_currency_fix_float(const xml::record_reader& root,
                                   refdata::service::conventions_bundle& bundle);
void join_cross_currency_fix_float(const package_row& row, xml::record_writer& root);

void pack_deposit(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_deposit(const package_row& row, xml::record_writer& root);

void pack_fra(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_fra(const package_row& row, xml::record_writer& root);

void pack_future(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_future(const package_row& row, xml::record_writer& root);

void pack_fx_option(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_fx_option(const package_row& row, xml::record_writer& root);

void pack_ibor_index(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_ibor_index(const package_row& row, xml::record_writer& root);

void pack_inflation_swap(const xml::record_reader& root,
                         refdata::service::conventions_bundle& bundle);
void join_inflation_swap(const package_row& row, xml::record_writer& root);

void pack_intraday_power_load(const xml::record_reader& root,
                              refdata::service::conventions_bundle& bundle);
void join_intraday_power_load(const package_row& row, xml::record_writer& root);

void pack_ois(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_ois(const package_row& row, xml::record_writer& root);

void pack_overnight_index(const xml::record_reader& root,
                          refdata::service::conventions_bundle& bundle);
void join_overnight_index(const package_row& row, xml::record_writer& root);

void pack_swap(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_swap(const package_row& row, xml::record_writer& root);

void pack_swap_index(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_swap_index(const package_row& row, xml::record_writer& root);

void pack_tenor_basis_two_swap(const xml::record_reader& root,
                               refdata::service::conventions_bundle& bundle);
void join_tenor_basis_two_swap(const package_row& row, xml::record_writer& root);

void pack_zero(const xml::record_reader& root, refdata::service::conventions_bundle& bundle);
void join_zero(const package_row& row, xml::record_writer& root);

void pack_zero_inflation_index(const xml::record_reader& root,
                               refdata::service::conventions_bundle& bundle);
void join_zero_inflation_index(const package_row& row, xml::record_writer& root);

void pack_tenor_basis_swap(const xml::record_reader& root,
                           refdata::service::conventions_bundle& bundle);

/// Writes the elements of the rows that belong to the three tenor families.
void join_tenor_basis_swap(const std::vector<const package_row*>& rows, xml::record_writer& root);

/// The kind of the central convention row of a tenor basis swap.
inline constexpr std::string_view tenor_basis_swap_kind = "TENOR_BASIS_SWAP";

/// The families that are one document element, the tenor basis swap.
inline constexpr std::string_view tenor_basis_swap_families[] = {
    "convention", "tenor_basis_swap_long_short", "tenor_basis_swap_pay_receive"};

constexpr bool is_tenor_basis_swap_family(std::string_view family) {
    for (const auto f : tenor_basis_swap_families)
        if (f == family)
            return true;
    return false;
}

std::vector<refdata::messaging::fx_convention> project_fx(const xml::record_reader& root);

void join_fx(const std::vector<refdata::messaging::fx_convention>& fx, xml::record_writer& root);

}

#endif

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
 * Template: cpp_convention_family_dispatch.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "convention_family_apply.hpp"
#include "convention_family_units.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include <cstddef>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::refdata::service {

namespace {

struct family_unit {
    std::string_view key;
    void (*apply)(const ores::database::context&, const std::string&, conventions_apply_result&);
    std::vector<std::string> (*read)(const ores::database::context&);
};

// One row per registry family, in the registry's order. A family with no row
// fails the first assertion, and a row out of order fails the second.
constexpr family_unit units[] = {
    {"average_ois",
     &convention_family::apply_average_ois,
     &convention_family::read_average_ois_payloads},
    {"bma_basis_swap",
     &convention_family::apply_bma_basis_swap,
     &convention_family::read_bma_basis_swap_payloads},
    {"bond_yield",
     &convention_family::apply_bond_yield,
     &convention_family::read_bond_yield_payloads},
    {"cds", &convention_family::apply_cds, &convention_family::read_cds_payloads},
    {"cms_spread_option",
     &convention_family::apply_cms_spread_option,
     &convention_family::read_cms_spread_option_payloads},
    {"commodity_forward",
     &convention_family::apply_commodity_forward,
     &convention_family::read_commodity_forward_payloads},
    {"commodity_future",
     &convention_family::apply_commodity_future,
     &convention_family::read_commodity_future_payloads},
    {"convention",
     &convention_family::apply_convention,
     &convention_family::read_convention_payloads},
    {"cross_currency_basis",
     &convention_family::apply_cross_currency_basis,
     &convention_family::read_cross_currency_basis_payloads},
    {"cross_currency_fix_float",
     &convention_family::apply_cross_currency_fix_float,
     &convention_family::read_cross_currency_fix_float_payloads},
    {"deposit", &convention_family::apply_deposit, &convention_family::read_deposit_payloads},
    {"fra", &convention_family::apply_fra, &convention_family::read_fra_payloads},
    {"future", &convention_family::apply_future, &convention_family::read_future_payloads},
    {"fx_option", &convention_family::apply_fx_option, &convention_family::read_fx_option_payloads},
    {"ibor_index",
     &convention_family::apply_ibor_index,
     &convention_family::read_ibor_index_payloads},
    {"inflation_swap",
     &convention_family::apply_inflation_swap,
     &convention_family::read_inflation_swap_payloads},
    {"intraday_power_load",
     &convention_family::apply_intraday_power_load,
     &convention_family::read_intraday_power_load_payloads},
    {"ois", &convention_family::apply_ois, &convention_family::read_ois_payloads},
    {"overnight_index",
     &convention_family::apply_overnight_index,
     &convention_family::read_overnight_index_payloads},
    {"swap", &convention_family::apply_swap, &convention_family::read_swap_payloads},
    {"swap_index",
     &convention_family::apply_swap_index,
     &convention_family::read_swap_index_payloads},
    {"tenor_basis_swap_long_short",
     &convention_family::apply_tenor_basis_swap_long_short,
     &convention_family::read_tenor_basis_swap_long_short_payloads},
    {"tenor_basis_swap_pay_receive",
     &convention_family::apply_tenor_basis_swap_pay_receive,
     &convention_family::read_tenor_basis_swap_pay_receive_payloads},
    {"tenor_basis_two_swap",
     &convention_family::apply_tenor_basis_two_swap,
     &convention_family::read_tenor_basis_two_swap_payloads},
    {"zero", &convention_family::apply_zero, &convention_family::read_zero_payloads},
    {"zero_inflation_index",
     &convention_family::apply_zero_inflation_index,
     &convention_family::read_zero_inflation_index_payloads},
};

static_assert(std::size(units) == domain::convention_family_count,
              "Every convention family needs a unit.");

constexpr bool units_follow_the_registry() {
    for (std::size_t i = 0; i < std::size(units); ++i)
        if (units[i].key != domain::convention_families[i].key)
            return false;
    return true;
}

static_assert(units_follow_the_registry(), "The units follow the registry's order.");

const family_unit* unit_of(const std::string& family) {
    for (const auto& unit : units)
        if (unit.key == family)
            return &unit;
    return nullptr;
}

}

// Staged families: the unit for one family decodes its payload into its own
// type, so this unit reflects none.
bool apply_staged_family(const ores::database::context& ctx,
                         const std::string& family,
                         const std::string& payload,
                         conventions_apply_result& result) {
    const auto* unit = unit_of(family);
    if (!unit)
        return false;
    unit->apply(ctx, payload, result);
    return true;
}

// Payload reads: each unit reads one family's live rows and serialises each as
// its own type, so an in-process caller sees the bytes the wire carries.
std::vector<std::string> read_family_payloads(const ores::database::context& ctx,
                                              const std::string& family) {
    const auto* unit = unit_of(family);
    if (!unit)
        throw std::logic_error("The registry does not know the convention family '" + family +
                               "'.");
    return unit->read(ctx);
}

}

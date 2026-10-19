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
#include "ores.ore.core/domain/conventions_package.hpp"
#include "conventions_family_units.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include <algorithm>
#include <format>
#include <iterator>
#include <stdexcept>
#include <vector>

namespace ores::ore::domain {

namespace xg = xml::generated::conventions;

namespace conventions_family {

/// Every family that is a document element of its own.
constexpr family_codec family_codecs[] = {
    {"average_ois", &pack_average_ois, &join_average_ois},
    {"bma_basis_swap", &pack_bma_basis_swap, &join_bma_basis_swap},
    {"bond_yield", &pack_bond_yield, &join_bond_yield},
    {"cds", &pack_cds, &join_cds},
    {"cms_spread_option", &pack_cms_spread_option, &join_cms_spread_option},
    {"commodity_forward", &pack_commodity_forward, &join_commodity_forward},
    {"commodity_future", &pack_commodity_future, &join_commodity_future},
    {"cross_currency_basis", &pack_cross_currency_basis, &join_cross_currency_basis},
    {"cross_currency_fix_float", &pack_cross_currency_fix_float, &join_cross_currency_fix_float},
    {"deposit", &pack_deposit, &join_deposit},
    {"fra", &pack_fra, &join_fra},
    {"future", &pack_future, &join_future},
    {"fx_option", &pack_fx_option, &join_fx_option},
    {"ibor_index", &pack_ibor_index, &join_ibor_index},
    {"inflation_swap", &pack_inflation_swap, &join_inflation_swap},
    {"intraday_power_load", &pack_intraday_power_load, &join_intraday_power_load},
    {"ois", &pack_ois, &join_ois},
    {"overnight_index", &pack_overnight_index, &join_overnight_index},
    {"swap", &pack_swap, &join_swap},
    {"swap_index", &pack_swap_index, &join_swap_index},
    {"tenor_basis_two_swap", &pack_tenor_basis_two_swap, &join_tenor_basis_two_swap},
    {"zero", &pack_zero, &join_zero},
    {"zero_inflation_index", &pack_zero_inflation_index, &join_zero_inflation_index},
};

constexpr bool codecs_cover_the_registry() {
    for (const auto& family : refdata::domain::convention_families) {
        if (is_tenor_basis_swap_family(family.key))
            continue;
        std::size_t found = 0;
        for (const auto& codec : family_codecs)
            if (codec.family == family.key)
                ++found;
        if (found != 1)
            return false;
    }
    return std::size(family_codecs) + std::size(tenor_basis_swap_families) ==
           std::size(refdata::domain::convention_families);
}

// A family added to the modelling tree has no way to be read from or written
// to a document until it has a unit and a row here, so the build stops on it.
static_assert(codecs_cover_the_registry(),
              "Every convention family needs a unit in conventions_family_units.hpp and a "
              "row in family_codecs, and every row there needs a family in the registry.");

}

conventions_package split_conventions(const conventions& v) {
    conventions_package p;
    const xml::record_reader root(*xg::conventions_document().root, &v);

    for (const auto& codec : conventions_family::family_codecs)
        codec.pack(root, p.bundle);
    conventions_family::pack_tenor_basis_swap(root, p.bundle);
    p.fx = conventions_family::project_fx(root);
    return p;
}

conventions join_conventions(const conventions_package& p) {
    using conventions_family::family_codec;

    conventions r;
    xml::record_writer root(*xg::conventions_document().root, &r);

    std::vector<const conventions_family::package_row*> tenor;
    for (const auto& row : p.bundle.rows) {
        if (conventions_family::is_tenor_basis_swap_family(row.family)) {
            tenor.push_back(&row);
            continue;
        }

        const auto codec =
            std::ranges::find(conventions_family::family_codecs, row.family, &family_codec::family);
        if (codec == std::end(conventions_family::family_codecs))
            throw std::runtime_error(std::format(
                "Conventions payload names family \"{}\", which no ORE document element carries.",
                row.family));
        codec->join(row, root);
    }

    conventions_family::join_tenor_basis_swap(tenor, root);
    conventions_family::join_fx(p.fx, root);
    return r;
}

}

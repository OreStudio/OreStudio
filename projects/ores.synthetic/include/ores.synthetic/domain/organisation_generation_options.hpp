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
#ifndef ORES_SYNTHETIC_DOMAIN_ORGANISATION_GENERATION_OPTIONS_HPP
#define ORES_SYNTHETIC_DOMAIN_ORGANISATION_GENERATION_OPTIONS_HPP

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>

namespace ores::synthetic::domain {

/**
 * @brief Options for controlling synthetic organisation generation.
 *
 * Controls the size, depth and composition of the generated organisational
 * hierarchy including parties, counterparties, business units, portfolios,
 * books, addresses and identifiers.
 *
 * The country field drives locale-specific naming conventions, address
 * formats and business centre codes. Currently supports "GB" and "US".
 */
struct organisation_generation_options final {
    /**
     * @brief Optional seed for reproducible generation.
     *
     * If not set, a random seed will be used.
     */
    std::optional<std::uint64_t> seed;

    /**
     * @brief Country code driving naming conventions and addresses.
     *
     * "GB" for UK-style names (Plc, Ltd, LLP) and addresses.
     * "US" for US-style names (Inc, LLC, Corp) and addresses.
     */
    std::string country = "GB";

    /**
     * @brief Total number of operational parties to generate.
     *
     * Does not include the system party (which is auto-created by the
     * provisioner). These form a tree rooted at a single legal entity.
     */
    std::size_t party_count = 5;

    /**
     * @brief Maximum depth of the party hierarchy.
     *
     * 1 = flat (root only), 2 = root + divisions, 3 = root + divisions +
     * branches.
     */
    std::size_t party_max_depth = 3;

    /**
     * @brief Total number of counterparties to generate.
     */
    std::size_t counterparty_count = 10;

    /**
     * @brief Maximum depth of the counterparty hierarchy.
     */
    std::size_t counterparty_max_depth = 3;

    /**
     * @brief Number of leaf (non-virtual, trading) portfolios.
     *
     * Virtual aggregation portfolios are added automatically to form
     * the tree above the leaves.
     */
    std::size_t portfolio_leaf_count = 8;

    /**
     * @brief Maximum depth of the portfolio tree.
     *
     * Typically 4: Global -> Region -> Asset Class -> Leaf.
     */
    std::size_t portfolio_max_depth = 4;

    /**
     * @brief Number of trading books per leaf portfolio.
     */
    std::size_t books_per_leaf_portfolio = 2;

    /**
     * @brief Total number of business units to generate.
     */
    std::size_t business_unit_count = 10;

    /**
     * @brief Maximum depth of the business unit hierarchy.
     *
     * Capped at 2 to match the three-level BU type scheme:
     * depth 0 = DIVISION (level 0), depth 1 = BUSINESS_AREA (level 1),
     * depth 2 = DESK (level 2). Deeper nodes would share DESK's level and
     * violate the parent-level < child-level constraint on the BU type table.
     */
    std::size_t business_unit_max_depth = 2;

    /**
     * @brief Whether to generate contact information with addresses.
     */
    bool generate_addresses = true;

    /**
     * @brief Number of contact records per party.
     *
     * Contact types cycle through Legal, Operations, Settlement, Billing.
     */
    std::size_t contacts_per_party = 2;

    /**
     * @brief Number of contact records per counterparty.
     */
    std::size_t contacts_per_counterparty = 1;

    /**
     * @brief Whether to generate identifiers (LEI, BIC).
     */
    bool generate_identifiers = true;
};

}

#endif

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
#ifndef ORES_SYNTHETIC_MESSAGING_GENERATE_ORGANISATION_PROTOCOL_HPP
#define ORES_SYNTHETIC_MESSAGING_GENERATE_ORGANISATION_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <cstdint>
#include <optional>

namespace ores::synthetic::messaging {

struct generate_organisation_request {
    using response_type = struct generate_organisation_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.organisation.generate";

    /**
     * @brief Country code driving naming conventions and addresses.
     *
     * "GB" for UK-style names (Plc, Ltd, LLP) and addresses.
     * "US" for US-style names (Inc, LLC, Corp) and addresses.
     */
    std::string country = "GB";

    /**
     * @brief Total number of operational parties to generate.
     */
    std::uint32_t party_count = 5;

    /**
     * @brief Maximum depth of the party hierarchy.
     *
     * 1 = flat (root only), 2 = root + divisions, 3 = root + divisions +
     * branches.
     */
    std::uint32_t party_max_depth = 3;

    /**
     * @brief Total number of counterparties to generate.
     */
    std::uint32_t counterparty_count = 10;

    /**
     * @brief Maximum depth of the counterparty hierarchy.
     */
    std::uint32_t counterparty_max_depth = 3;

    /**
     * @brief Number of leaf (non-virtual, trading) portfolios.
     */
    std::uint32_t portfolio_leaf_count = 8;

    /**
     * @brief Maximum depth of the portfolio tree.
     *
     * Typically 4: Global -> Region -> Asset Class -> Leaf.
     */
    std::uint32_t portfolio_max_depth = 4;

    /**
     * @brief Number of trading books per leaf portfolio.
     */
    std::uint32_t books_per_leaf_portfolio = 2;

    /**
     * @brief Total number of business units to generate.
     */
    std::uint32_t business_unit_count = 10;

    /**
     * @brief Maximum depth of the business unit hierarchy.
     *
     * Capped at 2: depth 0 = DIVISION, depth 1 = BUSINESS_AREA, depth 2 =
     * DESK.
     */
    std::uint32_t business_unit_max_depth = 2;

    /**
     * @brief Whether to generate contact information with addresses.
     */
    bool generate_addresses = true;

    /**
     * @brief Number of contact records per party.
     */
    std::uint32_t contacts_per_party = 2;

    /**
     * @brief Number of contact records per counterparty.
     */
    std::uint32_t contacts_per_counterparty = 1;

    /**
     * @brief Whether to generate identifiers (LEI, BIC).
     */
    bool generate_identifiers = true;

    /**
     * @brief Optional seed for reproducible generation.
     *
     * If not set, a random seed will be used and the actual seed is returned
     * in the response.
     */
    std::optional<std::uint64_t> seed;
};

struct generate_organisation_response {
    bool success = false;
    std::string error_message;

    /**
     * @brief The seed used for generation.
     *
     * Can be passed back in a subsequent request to reproduce the same
     * organisation exactly.
     */
    std::uint64_t seed = 0;

    int parties_count = 0;
    int counterparties_count = 0;
    int business_unit_types_count = 0;
    int business_units_count = 0;
    int portfolios_count = 0;
    int books_count = 0;
    int contacts_count = 0;
    int identifiers_count = 0;
};

}

#endif

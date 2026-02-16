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
#ifndef ORES_SYNTHETIC_SERVICE_ORGANISATION_GENERATOR_SERVICE_HPP
#define ORES_SYNTHETIC_SERVICE_ORGANISATION_GENERATOR_SERVICE_HPP

#include "ores.synthetic/domain/organisation_generation_options.hpp"
#include "ores.synthetic/domain/generated_organisation.hpp"

namespace ores::synthetic::service {

/**
 * @brief Service for generating complete organisational hierarchies.
 *
 * Generates a realistic, interconnected organisational structure including
 * parties with addresses and identifiers, counterparties with addresses
 * and identifiers, business units, portfolios and trading books.
 *
 * All generated entities are properly cross-referenced:
 * - Parties form a hierarchy via parent_party_id
 * - Counterparties are linked to the root party via junction table
 * - Business units reference the root party
 * - Portfolios form a tree with books at leaf nodes
 * - Books reference both their parent portfolio and the root party
 *
 * The country option drives locale-specific naming conventions:
 * - "GB": UK-style names (Plc, Ltd, LLP) with UK addresses
 * - "US": US-style names (Inc, LLC, Corp) with US addresses
 *
 * Example usage:
 * @code
 * organisation_generator_service service;
 *
 * // Generate with defaults (GB, 5 parties, 10 counterparties)
 * auto org = service.generate();
 *
 * // Generate US organisation with custom options
 * organisation_generation_options opts;
 * opts.seed = 42;
 * opts.country = "US";
 * opts.party_count = 3;
 * opts.counterparty_count = 20;
 * opts.portfolio_leaf_count = 12;
 * auto org2 = service.generate(opts);
 * @endcode
 */
class organisation_generator_service final {
public:
    /**
     * @brief Generates a complete organisational hierarchy.
     *
     * @param options Configuration for the generation process.
     * @return A complete organisation with proper cross-references.
     */
    domain::generated_organisation generate(
        const domain::organisation_generation_options& options);

    /**
     * @brief Generates an organisation with default options.
     *
     * Uses GB naming conventions with:
     * - 5 parties (max depth 3)
     * - 10 counterparties (max depth 3)
     * - 8 leaf portfolios (max depth 4)
     * - 2 books per leaf portfolio
     * - 10 business units (max depth 3)
     * - Addresses and identifiers enabled
     *
     * @return A complete organisation with default sizes.
     */
    domain::generated_organisation generate();
};

}

#endif

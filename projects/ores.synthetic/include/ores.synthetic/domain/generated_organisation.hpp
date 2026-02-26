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
#ifndef ORES_SYNTHETIC_DOMAIN_GENERATED_ORGANISATION_HPP
#define ORES_SYNTHETIC_DOMAIN_GENERATED_ORGANISATION_HPP

#include <cstdint>
#include <vector>
#include "ores.refdata/domain/party.hpp"
#include "ores.refdata/domain/party_contact_information.hpp"
#include "ores.refdata/domain/party_identifier.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.refdata/domain/counterparty_contact_information.hpp"
#include "ores.refdata/domain/counterparty_identifier.hpp"
#include "ores.refdata/domain/party_counterparty.hpp"
#include "ores.refdata/domain/business_unit_type.hpp"
#include "ores.refdata/domain/business_unit.hpp"
#include "ores.refdata/domain/portfolio.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::synthetic::domain {

/**
 * @brief A complete, interconnected organisational hierarchy.
 *
 * Contains all entities needed to represent an organisation: parties with
 * addresses and identifiers, counterparties with addresses and identifiers,
 * business units, portfolios and trading books. All entities are properly
 * cross-referenced (e.g. books reference leaf portfolios, business units
 * reference the root party, counterparties are linked via junction table).
 *
 * All vectors are ordered parent-first for safe insertion into databases
 * with foreign key constraints.
 *
 * This structure is a pure data container with no I/O dependencies, making
 * it suitable for serialisation to any format (SQL, JSON, CSV) by external
 * tools such as a future synthetic CLI.
 */
struct generated_organisation final {
    /**
     * @brief The seed used to generate this organisation.
     *
     * Can be used to recreate the exact same organisation by passing the
     * same seed to the generator service.
     */
    std::uint64_t seed = 0;

    /**
     * @brief Operational parties in parent-first order.
     *
     * The first party is the root legal entity. Subsequent parties are
     * divisions and branches forming a hierarchy via parent_party_id.
     */
    std::vector<refdata::domain::party> parties;

    /**
     * @brief Contact information (addresses) for parties.
     */
    std::vector<refdata::domain::party_contact_information> party_contacts;

    /**
     * @brief External identifiers (LEI, BIC) for parties.
     */
    std::vector<refdata::domain::party_identifier> party_identifiers;

    /**
     * @brief Counterparties in parent-first order.
     */
    std::vector<refdata::domain::counterparty> counterparties;

    /**
     * @brief Contact information (addresses) for counterparties.
     */
    std::vector<refdata::domain::counterparty_contact_information>
        counterparty_contacts;

    /**
     * @brief External identifiers (LEI, BIC) for counterparties.
     */
    std::vector<refdata::domain::counterparty_identifier>
        counterparty_identifiers;

    /**
     * @brief Links between the root party and counterparties.
     */
    std::vector<refdata::domain::party_counterparty> party_counterparties;

    /**
     * @brief Canonical business unit types for the tenant's ORES-ORG scheme.
     *
     * Must be inserted before business_units (FK dependency).
     */
    std::vector<refdata::domain::business_unit_type> business_unit_types;

    /**
     * @brief Business units in parent-first order.
     */
    std::vector<refdata::domain::business_unit> business_units;

    /**
     * @brief Portfolios in parent-first order.
     *
     * Virtual portfolios are aggregation nodes; non-virtual portfolios
     * are leaf nodes that parent trading books.
     */
    std::vector<refdata::domain::portfolio> portfolios;

    /**
     * @brief Trading books, each referencing a leaf portfolio.
     */
    std::vector<refdata::domain::book> books;
};

}

#endif

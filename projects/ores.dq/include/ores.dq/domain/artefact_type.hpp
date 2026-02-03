/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_DOMAIN_ARTEFACT_TYPE_HPP
#define ORES_DQ_DOMAIN_ARTEFACT_TYPE_HPP

#include <string>
#include <optional>

namespace ores::dq::domain {

/**
 * @brief Maps artefact type codes to their population functions and tables.
 *
 * Artefact types define the mapping between dataset artefact codes and the
 * database infrastructure needed to publish them:
 * - The artefact table where staging data is stored
 * - The target production table where data is published
 * - The populate function that performs the publication
 *
 * This is static configuration data - no bitemporal support required.
 */
struct artefact_type final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Unique code identifying this artefact type.
     *
     * This is the natural key for the type.
     * Examples: "countries", "currencies", "images".
     */
    std::string code;

    /**
     * @brief Human-readable name for display purposes.
     */
    std::string name;

    /**
     * @brief Detailed description of this artefact type.
     */
    std::optional<std::string> description;

    /**
     * @brief Name of the artefact (staging) table for this type.
     *
     * This is where data is staged before publication.
     * Example: "dq_countries_artefact_tbl".
     */
    std::optional<std::string> artefact_table;

    /**
     * @brief Name of the target (production) table for this type.
     *
     * This is where data is published to.
     * Example: "refdata_countries_tbl".
     */
    std::optional<std::string> target_table;

    /**
     * @brief Name of the database function that populates the target table.
     *
     * This function is called during publication to move data from
     * the artefact table to the target table.
     * Example: "dq_countries_publish_fn".
     */
    std::optional<std::string> populate_function;

    /**
     * @brief Display order for UI sorting.
     */
    int display_order = 0;
};

}

#endif

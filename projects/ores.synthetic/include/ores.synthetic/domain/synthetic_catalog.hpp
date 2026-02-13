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
#ifndef ORES_SYNTHETIC_DOMAIN_SYNTHETIC_CATALOG_HPP
#define ORES_SYNTHETIC_DOMAIN_SYNTHETIC_CATALOG_HPP

#include <string>
#include <vector>
#include "ores.dq/domain/catalog.hpp"
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/subject_area.hpp"
#include "ores.dq/domain/data_domain.hpp"
#include "ores.dq/domain/origin_dimension.hpp"
#include "ores.dq/domain/nature_dimension.hpp"
#include "ores.dq/domain/treatment_dimension.hpp"
#include "ores.iam/domain/account.hpp"

namespace ores::synthetic::domain {

/**
 * @brief A coherent catalog of synthetic data with proper relationships.
 *
 * This structure contains all related entities that form a complete
 * synthetic data catalog. All entities are properly linked: datasets reference
 * their catalogs, subject areas, domains, and dimensions; all entities
 * have consistent modified_by values from the accounts collection.
 *
 * The catalog includes:
 * - IAM accounts for user references
 * - DQ catalogs for organizing datasets
 * - Data domains and subject areas for classification
 * - Dimension values (origin, nature, treatment)
 * - DQ datasets with proper inter-entity references
 *
 * Note: Methodologies are not generated - datasets should reference the
 * well-known "Synthetic Data Generation" methodology by ID.
 */
struct synthetic_catalog final {
    /**
     * @brief The seed used to generate this catalog.
     *
     * Can be used to recreate the exact same catalog by passing the
     * same seed to the generator service.
     */
    std::uint64_t seed = 0;

    /**
     * @brief IAM accounts for user references.
     *
     * These accounts are used as modified_by values across all entities.
     */
    std::vector<iam::domain::account> accounts;

    /**
     * @brief DQ catalogs for organizing datasets.
     */
    std::vector<dq::domain::catalog> catalogs;

    /**
     * @brief Data domains for categorization.
     */
    std::vector<dq::domain::data_domain> data_domains;

    /**
     * @brief Subject areas within data domains.
     */
    std::vector<dq::domain::subject_area> subject_areas;

    /**
     * @brief Origin dimensions for data lineage.
     */
    std::vector<dq::domain::origin_dimension> origin_dimensions;

    /**
     * @brief Nature dimensions for data classification.
     */
    std::vector<dq::domain::nature_dimension> nature_dimensions;

    /**
     * @brief Treatment dimensions for processing classification.
     */
    std::vector<dq::domain::treatment_dimension> treatment_dimensions;

    /**
     * @brief DQ datasets with proper references to other entities.
     */
    std::vector<dq::domain::dataset> datasets;

    /**
     * @brief Names of catalogs this synthetic catalog depends on.
     *
     * When injecting this catalog, these dependencies must be satisfied
     * first. Each string is a catalog name that will be resolved at
     * injection time.
     */
    std::vector<std::string> dependencies;
};

}

#endif

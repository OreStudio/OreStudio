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
#ifndef ORES_SYNTHETIC_SERVICE_CATALOG_GENERATOR_SERVICE_HPP
#define ORES_SYNTHETIC_SERVICE_CATALOG_GENERATOR_SERVICE_HPP

#include "ores.synthetic/domain/generation_options.hpp"
#include "ores.synthetic/domain/synthetic_catalog.hpp"

namespace ores::synthetic::service {

/**
 * @brief Service for generating coherent synthetic data catalogs.
 *
 * This is the main entry point for the ores.synthetic library. It creates
 * complete synthetic data catalogs with proper relationships between entities.
 * All generated data is consistent, high-quality, and can be used together
 * for testing purposes.
 *
 * Key features:
 * - Reproducible generation with configurable seed
 * - Proper entity relationships (datasets reference catalogs, dimensions, etc.)
 * - Realistic data values for financial/data management domains
 * - Configurable counts for all entity types
 *
 * Example usage:
 * @code
 * catalog_generator_service service;
 *
 * // Generate with defaults
 * auto catalog = service.generate();
 *
 * // Generate with custom options
 * generation_options opts;
 * opts.seed = 42;  // Reproducible
 * opts.dataset_count = 50;
 * auto catalog2 = service.generate(opts);
 *
 * // Regenerate same data
 * opts.seed = catalog.seed;
 * auto same_catalog = service.generate(opts);
 * @endcode
 */
class catalog_generator_service final {
public:
    /**
     * @brief Generates a complete synthetic data catalog.
     *
     * Creates a coherent catalog where all entities reference each other
     * properly. Datasets reference existing catalogs, domains, dimensions,
     * and methodologies. All recorded_by fields reference existing accounts.
     *
     * @param options Configuration for the generation process.
     * @return A complete synthetic catalog with proper relationships.
     */
    domain::synthetic_catalog generate(const domain::generation_options& options);

    /**
     * @brief Generates a synthetic catalog with default options.
     *
     * Uses sensible defaults for all entity counts:
     * - 5 accounts
     * - 3 catalogs
     * - 4 data domains with 3 subject areas each
     * - Standard dimension values
     * - 5 methodologies
     * - 20 datasets
     *
     * @return A complete synthetic catalog with default sizes.
     */
    domain::synthetic_catalog generate();
};

}

#endif

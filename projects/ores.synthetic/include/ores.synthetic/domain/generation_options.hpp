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
#ifndef ORES_SYNTHETIC_DOMAIN_GENERATION_OPTIONS_HPP
#define ORES_SYNTHETIC_DOMAIN_GENERATION_OPTIONS_HPP

#include <cstddef>
#include <cstdint>
#include <optional>

namespace ores::synthetic::domain {

/**
 * @brief Options for controlling synthetic dataset generation.
 *
 * These options allow fine-grained control over the size and composition
 * of the generated synthetic dataset.
 */
struct generation_options final {
    /**
     * @brief Optional seed for reproducible generation.
     *
     * If not set, a random seed will be used.
     */
    std::optional<std::uint64_t> seed;

    /**
     * @brief Number of IAM accounts to generate.
     */
    std::size_t account_count = 5;

    /**
     * @brief Number of DQ catalogs to generate.
     */
    std::size_t catalog_count = 3;

    /**
     * @brief Number of data domains to generate.
     */
    std::size_t data_domain_count = 4;

    /**
     * @brief Number of subject areas per domain to generate.
     */
    std::size_t subject_areas_per_domain = 3;

    /**
     * @brief Number of origin dimensions to generate.
     */
    std::size_t origin_dimension_count = 5;

    /**
     * @brief Number of nature dimensions to generate.
     */
    std::size_t nature_dimension_count = 4;

    /**
     * @brief Number of treatment dimensions to generate.
     */
    std::size_t treatment_dimension_count = 4;

    /**
     * @brief Number of methodologies to generate.
     */
    std::size_t methodology_count = 5;

    /**
     * @brief Number of DQ datasets to generate.
     */
    std::size_t dataset_count = 20;
};

}

#endif

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
#ifndef ORES_SYNTHETIC_GENERATORS_DATASET_GENERATOR_HPP
#define ORES_SYNTHETIC_GENERATORS_DATASET_GENERATOR_HPP

#include <vector>
#include "ores.dq/domain/dataset.hpp"

namespace ores::synthetic::generators {

/**
 * @brief Generates a synthetic DQ dataset.
 *
 * Creates a complete dataset with randomly generated metadata including
 * name, description, source system, business context, and lineage info.
 */
dq::domain::dataset generate_synthetic_dataset();

/**
 * @brief Generates N synthetic DQ datasets.
 *
 * @param n Number of datasets to generate.
 * @return Vector of synthetic datasets.
 */
std::vector<dq::domain::dataset>
generate_synthetic_datasets(std::size_t n);

}

#endif

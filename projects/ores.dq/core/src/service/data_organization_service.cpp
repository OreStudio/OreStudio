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
#include "ores.dq.core/service/data_organization_service.hpp"
#include <algorithm>
#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

data_organization_service::data_organization_service(context ctx)
    : ctx_(ctx)
    , dataset_dependency_repo_(ctx) {}

// ============================================================================
// Dataset Dependency Management
// ============================================================================

std::vector<domain::dataset_dependency> data_organization_service::list_dataset_dependencies() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all dataset dependencies";
    return dataset_dependency_repo_.read_latest();
}

std::vector<domain::dataset_dependency>
data_organization_service::list_dataset_dependencies_by_dataset(const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing dependencies for dataset: " << dataset_code;
    return dataset_dependency_repo_.read_latest_by_dataset(dataset_code);
}

}

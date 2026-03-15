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
#ifndef ORES_DQ_MESSAGING_DATASET_DEPENDENCY_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATASET_DEPENDENCY_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.dq/domain/dataset_dependency.hpp"
#include "ores.dq/domain/dataset.hpp"

namespace ores::dq::messaging {

struct get_dataset_dependencies_request {
    using response_type = struct get_dataset_dependencies_response;
    static constexpr std::string_view nats_subject =
        "ores.dq.v1.dataset-dependencies.list";
    int offset = 0;
    int limit = 100;
};

struct get_dataset_dependencies_response {
    std::vector<ores::dq::domain::dataset_dependency> dependencies;
    int total_available_count = 0;
};

struct get_dataset_dependencies_by_dataset_request {
    using response_type = struct get_dataset_dependencies_by_dataset_response;
    static constexpr std::string_view nats_subject =
        "ores.dq.v1.dataset-dependencies.by-dataset";
    std::string dataset_code;
};

struct get_dataset_dependencies_by_dataset_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::dataset_dependency> dependencies;
};

struct resolve_dependencies_request {
    using response_type = struct resolve_dependencies_response;
    static constexpr std::string_view nats_subject =
        "ores.dq.v1.dataset-dependencies.resolve";
    std::vector<std::string> dataset_ids;
};

struct resolve_dependencies_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::dataset> datasets;
    std::vector<std::string> requested_ids;
};

}

#endif

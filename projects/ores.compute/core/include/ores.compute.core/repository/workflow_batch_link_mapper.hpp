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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_COMPUTE_CORE_REPOSITORY_WORKFLOW_BATCH_LINK_MAPPER_HPP
#define ORES_COMPUTE_CORE_REPOSITORY_WORKFLOW_BATCH_LINK_MAPPER_HPP

#include "ores.compute.api/domain/workflow_batch_link.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/workflow_batch_link_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::compute::repository {

/**
 * @brief Maps workflow_batch_link domain entities to data storage layer and vice-versa.
 */
class ORES_COMPUTE_CORE_EXPORT workflow_batch_link_mapper {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.workflow_batch_link_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::workflow_batch_link map(const workflow_batch_link_entity& v);
    static workflow_batch_link_entity map(const domain::workflow_batch_link& v);

    static std::vector<domain::workflow_batch_link>
    map(const std::vector<workflow_batch_link_entity>& v);
    static std::vector<workflow_batch_link_entity>
    map(const std::vector<domain::workflow_batch_link>& v);
};

}

#endif

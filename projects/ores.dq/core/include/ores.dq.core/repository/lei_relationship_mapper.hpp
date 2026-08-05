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
#ifndef ORES_DQ_CORE_REPOSITORY_LEI_RELATIONSHIP_MAPPER_HPP
#define ORES_DQ_CORE_REPOSITORY_LEI_RELATIONSHIP_MAPPER_HPP

#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/lei_relationship_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::repository {

/**
 * @brief Maps lei_relationship domain entities to data storage layer and vice-versa.
 */
class ORES_DQ_CORE_EXPORT lei_relationship_mapper {
private:
    inline static std::string_view logger_name = "ores.dq.repository.lei_relationship_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::lei_relationship map(const lei_relationship_entity& v);
    static lei_relationship_entity map(const domain::lei_relationship& v);

    static std::vector<domain::lei_relationship> map(const std::vector<lei_relationship_entity>& v);
    static std::vector<lei_relationship_entity> map(const std::vector<domain::lei_relationship>& v);
};

}

#endif

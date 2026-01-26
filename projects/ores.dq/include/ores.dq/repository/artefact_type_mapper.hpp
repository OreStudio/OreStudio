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
#ifndef ORES_DQ_REPOSITORY_ARTEFACT_TYPE_MAPPER_HPP
#define ORES_DQ_REPOSITORY_ARTEFACT_TYPE_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/artefact_type.hpp"
#include "ores.dq/repository/artefact_type_entity.hpp"

namespace ores::dq::repository {

/**
 * @brief Maps artefact_type entities to domain types and vice-versa.
 */
class artefact_type_mapper {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.artefact_type_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::artefact_type map(const artefact_type_entity& v);
    static artefact_type_entity map(const domain::artefact_type& v);

    static std::vector<domain::artefact_type>
    map(const std::vector<artefact_type_entity>& v);
    static std::vector<artefact_type_entity>
    map(const std::vector<domain::artefact_type>& v);
};

}

#endif

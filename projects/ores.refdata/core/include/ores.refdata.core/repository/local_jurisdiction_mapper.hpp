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
#ifndef ORES_REFDATA_CORE_REPOSITORY_LOCAL_JURISDICTION_MAPPER_HPP
#define ORES_REFDATA_CORE_REPOSITORY_LOCAL_JURISDICTION_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/local_jurisdiction.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/local_jurisdiction_entity.hpp"

namespace ores::refdata::repository {

/**
 * @brief Maps local_jurisdiction domain entities to data storage layer and vice-versa.
 */
class ORES_REFDATA_CORE_EXPORT local_jurisdiction_mapper {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.local_jurisdiction_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::local_jurisdiction map(const local_jurisdiction_entity& v);
    static local_jurisdiction_entity map(const domain::local_jurisdiction& v);

    static std::vector<domain::local_jurisdiction>
    map(const std::vector<local_jurisdiction_entity>& v);
    static std::vector<local_jurisdiction_entity>
    map(const std::vector<domain::local_jurisdiction>& v);
};

}

#endif

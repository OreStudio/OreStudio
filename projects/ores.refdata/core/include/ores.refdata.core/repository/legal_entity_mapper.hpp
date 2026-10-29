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
#ifndef ORES_REFDATA_CORE_REPOSITORY_LEGAL_ENTITY_MAPPER_HPP
#define ORES_REFDATA_CORE_REPOSITORY_LEGAL_ENTITY_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/legal_entity.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/legal_entity_entity.hpp"

namespace ores::refdata::repository {

/**
 * @brief Maps legal_entity domain entities to data storage layer and vice-versa.
 */
class ORES_REFDATA_CORE_EXPORT legal_entity_mapper {
private:
    inline static std::string_view logger_name = "ores.refdata.repository.legal_entity_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::legal_entity map(const legal_entity_entity& v);
    static legal_entity_entity map(const domain::legal_entity& v);

    static std::vector<domain::legal_entity> map(const std::vector<legal_entity_entity>& v);
    static std::vector<legal_entity_entity> map(const std::vector<domain::legal_entity>& v);
};

}

#endif

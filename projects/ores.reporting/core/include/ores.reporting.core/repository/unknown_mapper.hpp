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
#ifndef ORES_REPORTING_CORE_REPOSITORY__MAPPER_HPP
#define ORES_REPORTING_CORE_REPOSITORY__MAPPER_HPP

#include "ores.reporting.api/domain/.hpp"
#include "ores.reporting.core/repository/_entity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.core/export.hpp"

namespace ores::reporting::repository {

/**
 * @brief Maps  domain entities to data storage layer and vice-versa.
 */
class ORES_REPORTING_CORE_EXPORT _mapper {
private:
    inline static std::string_view logger_name =
        "ores.reporting.repository._mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain:: map(const _entity& v);
    static _entity map(const domain::& v);

    static std::vector<domain::>
    map(const std::vector<_entity>& v);
    static std::vector<_entity>
    map(const std::vector<domain::>& v);
};

}

#endif

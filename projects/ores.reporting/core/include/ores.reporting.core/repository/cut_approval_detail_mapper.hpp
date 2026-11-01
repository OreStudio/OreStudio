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
#ifndef ORES_REPORTING_CORE_REPOSITORY_CUT_APPROVAL_DETAIL_MAPPER_HPP
#define ORES_REPORTING_CORE_REPOSITORY_CUT_APPROVAL_DETAIL_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/cut_approval_detail.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_entity.hpp"

namespace ores::reporting::repository {

/**
 * @brief Maps cut_approval_detail domain entities to data storage layer and vice-versa.
 */
class ORES_REPORTING_CORE_EXPORT cut_approval_detail_mapper {
private:
    inline static std::string_view logger_name =
        "ores.reporting.repository.cut_approval_detail_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::cut_approval_detail map(const cut_approval_detail_entity& v);
    static cut_approval_detail_entity map(const domain::cut_approval_detail& v);

    static std::vector<domain::cut_approval_detail>
    map(const std::vector<cut_approval_detail_entity>& v);
    static std::vector<cut_approval_detail_entity>
    map(const std::vector<domain::cut_approval_detail>& v);
};

}

#endif

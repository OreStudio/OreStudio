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
#ifndef ORES_REFDATA_REPOSITORY_BUSINESS_UNIT_MAPPER_HPP
#define ORES_REFDATA_REPOSITORY_BUSINESS_UNIT_MAPPER_HPP

#include "ores.refdata/domain/business_unit.hpp"
#include "ores.refdata/repository/business_unit_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::refdata::repository {

/**
 * @brief Maps business_unit domain entities to data storage layer and vice-versa.
 */
class business_unit_mapper {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.business_unit_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::business_unit map(const business_unit_entity& v);
    static business_unit_entity map(const domain::business_unit& v);

    static std::vector<domain::business_unit>
    map(const std::vector<business_unit_entity>& v);
    static std::vector<business_unit_entity>
    map(const std::vector<domain::business_unit>& v);
};

}

#endif

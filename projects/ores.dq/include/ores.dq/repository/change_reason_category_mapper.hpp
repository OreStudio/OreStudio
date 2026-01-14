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
#ifndef ORES_DQ_REPOSITORY_CHANGE_REASON_CATEGORY_MAPPER_HPP
#define ORES_DQ_REPOSITORY_CHANGE_REASON_CATEGORY_MAPPER_HPP

#include "ores.dq/domain/change_reason_category.hpp"
#include "ores.dq/repository/change_reason_category_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::repository {

/**
 * @brief Maps change_reason_category domain entities to data storage layer and vice-versa.
 */
class change_reason_category_mapper {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.change_reason_category_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::change_reason_category map(const change_reason_category_entity& v);
    static change_reason_category_entity map(const domain::change_reason_category& v);

    static std::vector<domain::change_reason_category>
    map(const std::vector<change_reason_category_entity>& v);
    static std::vector<change_reason_category_entity>
    map(const std::vector<domain::change_reason_category>& v);
};

}

#endif

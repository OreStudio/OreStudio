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
#ifndef ORES_REFDATA_REPOSITORY_OIS_CONVENTION_MAPPER_HPP
#define ORES_REFDATA_REPOSITORY_OIS_CONVENTION_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/ois_convention_entity.hpp"

namespace ores::refdata::repository {

/**
 * @brief Maps ois_convention domain entities to data storage layer and vice-versa.
 */
class ORES_REFDATA_CORE_EXPORT ois_convention_mapper {
private:
    inline static std::string_view logger_name = "ores.refdata.repository.ois_convention_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::ois_convention map(const ois_convention_entity& v);
    static ois_convention_entity map(const domain::ois_convention& v);

    static std::vector<domain::ois_convention> map(const std::vector<ois_convention_entity>& v);
    static std::vector<ois_convention_entity> map(const std::vector<domain::ois_convention>& v);
};

}

#endif

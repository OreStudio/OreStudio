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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_CRM_DRIVER_PAIR_MAPPER_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_CRM_DRIVER_PAIR_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/crm_driver_pair.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/crm_driver_pair_entity.hpp"

namespace ores::marketdata::repository {

/**
 * @brief Maps crm_driver_pair domain entities to data storage layer and vice-versa.
 */
class ORES_MARKETDATA_CORE_EXPORT crm_driver_pair_mapper {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.crm_driver_pair_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::crm_driver_pair map(const crm_driver_pair_entity& v);
    static crm_driver_pair_entity map(const domain::crm_driver_pair& v);

    static std::vector<domain::crm_driver_pair> map(const std::vector<crm_driver_pair_entity>& v);
    static std::vector<crm_driver_pair_entity> map(const std::vector<domain::crm_driver_pair>& v);
};

}

#endif

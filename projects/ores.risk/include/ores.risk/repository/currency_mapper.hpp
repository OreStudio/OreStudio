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
#ifndef ORES_RISK_REPOSITORY_CURRENCY_MAPPERP_HPP
#define ORES_RISK_REPOSITORY_CURRENCY_MAPPERP_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.risk/domain/currency.hpp"
#include "ores.risk/repository/currency_entity.hpp"

namespace ores::risk::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class currency_mapper {
private:
    inline static std::string_view logger_name =
        "ores.risk.repository.currency_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::currency map(const currency_entity& v);
    static currency_entity map(const domain::currency& v);

    static std::vector<domain::currency>
    map(const std::vector<currency_entity>& v);
    static std::vector<currency_entity>
    map(const std::vector<domain::currency>& v);
};

}

#endif

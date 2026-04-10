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
#ifndef ORES_TRADING_REPOSITORY_EQUITY_POSITION_INSTRUMENT_MAPPER_HPP
#define ORES_TRADING_REPOSITORY_EQUITY_POSITION_INSTRUMENT_MAPPER_HPP

#include <vector>
#include "ores.trading.api/domain/equity_position_instrument.hpp"
#include "ores.trading.core/repository/equity_position_instrument_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::trading::repository {

/**
 * @brief Maps equity_position_instrument domain entities to data storage layer and vice-versa.
 */
class equity_position_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.equity_position_instrument_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::equity_position_instrument map(const equity_position_instrument_entity& v);
    static equity_position_instrument_entity map(const domain::equity_position_instrument& v);

    static std::vector<domain::equity_position_instrument>
    map(const std::vector<equity_position_instrument_entity>& v);
    static std::vector<equity_position_instrument_entity>
    map(const std::vector<domain::equity_position_instrument>& v);
};

}

#endif

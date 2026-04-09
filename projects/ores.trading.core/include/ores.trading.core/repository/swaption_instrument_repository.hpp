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
#ifndef ORES_TRADING_REPOSITORY_SWAPTION_INSTRUMENT_REPOSITORY_HPP
#define ORES_TRADING_REPOSITORY_SWAPTION_INSTRUMENT_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"

namespace ores::trading::repository {

/**
 * @brief Reads and writes swaption instruments to data storage.
 */
class swaption_instrument_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.swaption_instrument_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::swaption_instrument& v);
    void write(context ctx, const std::vector<domain::swaption_instrument>& v);

    std::vector<domain::swaption_instrument> read_latest(context ctx);
    std::vector<domain::swaption_instrument>
    read_latest(context ctx, const std::string& instrument_id);
    std::vector<domain::swaption_instrument>
    read_all(context ctx, const std::string& instrument_id);

    void remove(context ctx, const std::string& instrument_id);
};

}

#endif

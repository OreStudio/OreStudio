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
#ifndef ORES_TRADING_REPOSITORY_TRADE_ID_TYPE_REPOSITORY_HPP
#define ORES_TRADING_REPOSITORY_TRADE_ID_TYPE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading/domain/trade_id_type.hpp"

namespace ores::trading::repository {

/**
 * @brief Reads and writes trade ID types to data storage.
 */
class trade_id_type_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.trade_id_type_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::trade_id_type& v);
    void write(context ctx, const std::vector<domain::trade_id_type>& v);

    std::vector<domain::trade_id_type> read_latest(context ctx);
    std::vector<domain::trade_id_type>
    read_latest(context ctx, const std::string& code);
    std::vector<domain::trade_id_type>
    read_all(context ctx, const std::string& code);

    void remove(context ctx, const std::string& code);
};

}

#endif

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
#ifndef ORES_TRADING_SERVICE_TRADE_ID_TYPE_SERVICE_HPP
#define ORES_TRADING_SERVICE_TRADE_ID_TYPE_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading/domain/trade_id_type.hpp"
#include "ores.trading/repository/trade_id_type_repository.hpp"

namespace ores::trading::service {

/**
 * @brief Service for managing trade ID types.
 */
class trade_id_type_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.trade_id_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit trade_id_type_service(context ctx);

    std::vector<domain::trade_id_type> list_id_types();

    std::optional<domain::trade_id_type>
    find_id_type(const std::string& code);

    void save_id_type(const domain::trade_id_type& v);

    void save_id_types(const std::vector<domain::trade_id_type>& id_types);

    void remove_id_type(const std::string& code);

    std::vector<domain::trade_id_type>
    get_id_type_history(const std::string& code);

private:
    context ctx_;
    repository::trade_id_type_repository repo_;
};

}

#endif

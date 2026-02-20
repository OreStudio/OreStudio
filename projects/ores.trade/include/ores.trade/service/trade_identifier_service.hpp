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
#ifndef ORES_TRADE_SERVICE_TRADE_IDENTIFIER_SERVICE_HPP
#define ORES_TRADE_SERVICE_TRADE_IDENTIFIER_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trade/domain/trade_identifier.hpp"
#include "ores.trade/repository/trade_identifier_repository.hpp"

namespace ores::trade::service {

/**
 * @brief Service for managing trade identifiers.
 */
class trade_identifier_service {
private:
    inline static std::string_view logger_name =
        "ores.trade.service.trade_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit trade_identifier_service(context ctx);

    std::vector<domain::trade_identifier> list_identifiers();

    std::optional<domain::trade_identifier>
    find_identifier(const std::string& id);

    void save_identifier(const domain::trade_identifier& v);

    void remove_identifier(const std::string& id);

    std::vector<domain::trade_identifier>
    get_identifier_history(const std::string& id);

private:
    context ctx_;
    repository::trade_identifier_repository repo_;
};

}

#endif

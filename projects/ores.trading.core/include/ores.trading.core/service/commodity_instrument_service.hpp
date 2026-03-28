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
#ifndef ORES_TRADING_SERVICE_COMMODITY_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_SERVICE_COMMODITY_INSTRUMENT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.core/repository/commodity_instrument_repository.hpp"

namespace ores::trading::service {

/**
 * @brief Service for managing commodity instruments.
 */
class commodity_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.commodity_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit commodity_instrument_service(context ctx);

    std::vector<domain::commodity_instrument> list_commodity_instruments();

    std::vector<domain::commodity_instrument>
    list_commodity_instruments(std::uint32_t offset, std::uint32_t limit);

    std::uint32_t count_commodity_instruments();

    std::optional<domain::commodity_instrument>
    find_commodity_instrument(const std::string& id);

    void save_commodity_instrument(const domain::commodity_instrument& v);

    void remove_commodity_instrument(const std::string& id);

    std::vector<domain::commodity_instrument>
    get_commodity_instrument_history(const std::string& id);

private:
    context ctx_;
    repository::commodity_instrument_repository repo_;
};

}

#endif

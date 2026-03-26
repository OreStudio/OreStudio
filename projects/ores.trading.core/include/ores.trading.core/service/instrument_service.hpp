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
#ifndef ORES_TRADING_SERVICE_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_SERVICE_INSTRUMENT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.core/repository/instrument_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"

namespace ores::trading::service {

/**
 * @brief Service for managing instruments.
 */
class instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit instrument_service(context ctx);

    std::vector<domain::instrument> list_instruments();

    std::vector<domain::instrument>
    list_instruments(std::uint32_t offset, std::uint32_t limit);

    std::uint32_t count_instruments();

    std::optional<domain::instrument>
    find_instrument(const std::string& id);

    std::vector<domain::swap_leg>
    get_legs(const std::string& instrument_id);

    void save_instrument(const domain::instrument& v);

    void save_instrument(const domain::instrument& v,
        const std::vector<domain::swap_leg>& legs);

    void remove_instrument(const std::string& id);

    std::vector<domain::instrument>
    get_instrument_history(const std::string& id);

private:
    context ctx_;
    repository::instrument_repository repo_;
    repository::swap_leg_repository leg_repo_;
};

}

#endif

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
#ifndef ORES_TRADING_SERVICE_FRA_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_SERVICE_FRA_INSTRUMENT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.core/repository/fra_instrument_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"

namespace ores::trading::service {

/**
 * @brief Service for managing FRA instruments.
 */
class fra_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.fra_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit fra_instrument_service(context ctx);

    std::vector<domain::fra_instrument> list_fra_instruments();

    std::vector<domain::fra_instrument>
    list_fra_instruments(std::uint32_t offset, std::uint32_t limit);

    std::uint32_t count_fra_instruments();

    std::optional<domain::fra_instrument>
    get_fra_instrument(const std::string& id);

    void save_fra_instrument(const domain::fra_instrument& v);

    void remove_fra_instrument(const std::string& id);

    std::vector<domain::fra_instrument>
    get_fra_instrument_history(const std::string& id);

    /**
     * @brief Fetches the swap legs for any rates instrument by instrument_id.
     *
     * All rates instrument families share the swap_legs table. This method is
     * the shared access point used by handlers that need legs for any type.
     */
    std::vector<domain::swap_leg>
    get_swap_legs(const std::string& instrument_id);

private:
    context ctx_;
    repository::fra_instrument_repository repo_;
    repository::swap_leg_repository leg_repo_;
};

}

#endif

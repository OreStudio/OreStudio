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
#ifndef ORES_TRADING_SERVICE_VANILLA_SWAP_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_SERVICE_VANILLA_SWAP_INSTRUMENT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_repository.hpp"

namespace ores::trading::service {

/**
 * @brief Service for managing vanilla swap instruments.
 */
class vanilla_swap_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.vanilla_swap_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit vanilla_swap_instrument_service(context ctx);

    std::vector<domain::vanilla_swap_instrument> list_vanilla_swap_instruments();

    std::vector<domain::vanilla_swap_instrument>
    list_vanilla_swap_instruments(std::uint32_t offset, std::uint32_t limit);

    std::uint32_t count_vanilla_swap_instruments();

    std::optional<domain::vanilla_swap_instrument>
    find_vanilla_swap_instrument(const std::string& id);

    void save_vanilla_swap_instrument(const domain::vanilla_swap_instrument& v);

    void remove_vanilla_swap_instrument(const std::string& id);

    std::vector<domain::vanilla_swap_instrument>
    get_vanilla_swap_instrument_history(const std::string& id);

private:
    context ctx_;
    repository::vanilla_swap_instrument_repository repo_;
};

}

#endif

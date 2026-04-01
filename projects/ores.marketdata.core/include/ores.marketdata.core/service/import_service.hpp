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
#ifndef ORES_MARKETDATA_CORE_SERVICE_IMPORT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_IMPORT_SERVICE_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/import_protocol.hpp"

namespace ores::marketdata::service {

/**
 * @brief Imports ORE market data and fixings files into the database.
 *
 * Parses market.txt and fixings.txt content, decomposes keys via the series
 * key registry, upserts market series catalog entries, and bulk-inserts
 * observations and fixings into the TimescaleDB hypertables.
 */
class import_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.import_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit import_service(context ctx);

    messaging::import_market_data_response
    import(const messaging::import_market_data_request& req);

private:
    context ctx_;
};

}

#endif

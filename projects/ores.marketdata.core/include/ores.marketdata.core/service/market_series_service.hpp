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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_SERIES_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_SERIES_SERVICE_HPP

#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"

namespace ores::marketdata::service {

/**
 * @brief Service for managing market data series catalog entries.
 */
class market_series_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.market_series_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit market_series_service(context ctx);

    std::vector<domain::market_series> list();
    std::vector<domain::market_series>
    find_by_type(const std::string& series_type, const std::string& metric,
        const std::string& qualifier);

    void save(const domain::market_series& v);
    void save(const std::vector<domain::market_series>& v);

    void remove(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::market_series_repository repo_;
};

}

#endif

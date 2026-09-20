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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_ASSET_CLASS_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_ASSET_CLASS_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes asset classes to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT market_series_asset_class_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.market_series_asset_class_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit market_series_asset_class_repository(context ctx);

    std::string sql();

    void write(const domain::market_series_asset_class& asset_class);
    void write(const std::vector<domain::market_series_asset_class>& asset_classes);

    std::vector<domain::market_series_asset_class> read_latest();
    std::vector<domain::market_series_asset_class> read_latest(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active asset classes.
     */
    std::uint32_t get_total_asset_class_count();
    std::vector<domain::market_series_asset_class>
    read_latest_by_series(const boost::uuids::uuid& market_series_id);

    /**
     * @brief Gets the total count of active asset classes filtered by market_series_id.
     */
    std::uint32_t get_total_asset_class_count_by_series(const boost::uuids::uuid& market_series_id);

    std::vector<domain::market_series_asset_class>
    read_latest_by_asset_class(const std::string& asset_class_code);

    /**
     * @brief Gets the total count of active asset classes filtered by asset_class_code.
     */
    std::uint32_t get_total_asset_class_count_by_asset_class(const std::string& asset_class_code);

    void remove(const boost::uuids::uuid& market_series_id, const std::string& asset_class_code);
    void remove_by_series(const boost::uuids::uuid& market_series_id);

private:
    context ctx_;
};

}

#endif

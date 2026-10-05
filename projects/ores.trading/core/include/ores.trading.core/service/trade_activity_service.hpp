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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_ACTIVITY_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_ACTIVITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_activity.hpp"
#include "ores.trading.api/messaging/trade_activity_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade activities.
 *
 * Provides a higher-level interface for trade activity operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_activity_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_activity_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_activity_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_activity_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_trade_activities_response
    list_trade_activities(const messaging::list_trade_activities_request& request);
    messaging::get_trade_activity_response
    get_trade_activity(const messaging::get_trade_activity_request& request);
    messaging::get_many_trade_activities_response
    get_many_trade_activities(const messaging::get_many_trade_activities_request& request);
    /**@}*/

    /**
     * @brief Lists trade activities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade activities for the requested page.
     */
    std::vector<domain::trade_activity> list_activities(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade activities.
     *
     * @return Total number of active trade activities.
     */
    std::uint32_t count_activities();


    /**
     * @brief Retrieves a single trade activity by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade activity if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_activity> get_activity(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade activities by primary key.
     */
    std::vector<domain::trade_activity> get_activities(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade activity (creates or updates).
     *
     * @param activity The trade activity to save.
     * @throws std::exception on failure.
     */
    void save_activity(const domain::trade_activity& activity);

    /**
     * @brief Saves a batch of trade activities.
     *
     * @param activities The trade activities to save.
     * @throws std::exception on failure.
     */
    void save_activities(const std::vector<domain::trade_activity>& activities);

    /**
     * @brief Deletes a trade activity by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_activity(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade activities by their primary keys.
     */
    void delete_activities(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::trade_activity_repository repo_;
};

}

#endif

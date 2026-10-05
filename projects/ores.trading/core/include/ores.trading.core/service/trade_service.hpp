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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade anchors.
 *
 * Provides a higher-level interface for trade anchor operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_service(context ctx);

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
    messaging::list_trades_response list_trades(const messaging::list_trades_request& request);
    messaging::get_trade_response get_trade(const messaging::get_trade_request& request);
    messaging::get_many_trades_response
    get_many_trades(const messaging::get_many_trades_request& request);
    /**@}*/

    /**
     * @brief Lists trade anchors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade anchors for the requested page.
     */
    std::vector<domain::trade> list_trades(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade anchors.
     *
     * @return Total number of active trade anchors.
     */
    std::uint32_t count_trades();


    /**
     * @brief Retrieves a single trade anchor by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade anchor if found, std::nullopt otherwise.
     */
    std::optional<domain::trade> get_trade(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade anchors by primary key.
     */
    std::vector<domain::trade> get_trades(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade anchor (creates or updates).
     *
     * @param trade The trade anchor to save.
     * @throws std::exception on failure.
     */
    void save_trade(const domain::trade& trade);

    /**
     * @brief Saves a batch of trade anchors.
     *
     * @param trades The trade anchors to save.
     * @throws std::exception on failure.
     */
    void save_trades(const std::vector<domain::trade>& trades);

    /**
     * @brief Deletes a trade anchor by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade anchors by their primary keys.
     */
    void delete_trades(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::trade_repository repo_;
};

}

#endif

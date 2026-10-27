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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_SUMMARY_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_SUMMARY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade summaries.
 *
 * Provides a higher-level interface for trade summary operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_summary_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_summary_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_summary_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_summary_service(context ctx);

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
    ::ores::trading::messaging::list_trade_summaries_response
    list_trade_summaries(const ::ores::trading::messaging::list_trade_summaries_request& request);
    ::ores::trading::messaging::get_trade_summary_response
    get_trade_summary(const ::ores::trading::messaging::get_trade_summary_request& request);
    ::ores::trading::messaging::get_many_trade_summaries_response get_many_trade_summaries(
        const ::ores::trading::messaging::get_many_trade_summaries_request& request);
    /**@}*/

    /**
     * @brief Lists trade summaries with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade summaries for the requested page.
     */
    std::vector<domain::trade_summary> list_trade_summaries(std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade summaries.
     *
     * @return Total number of active trade summaries.
     */
    std::uint32_t count_trade_summaries();


    /**
     * @brief Retrieves a single trade summary by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade summary if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_summary> get_trade_summary(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade summaries by primary key.
     */
    std::vector<domain::trade_summary> get_trade_summaries(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade summary (creates or updates).
     *
     * @param trade_summary The trade summary to save.
     * @throws std::exception on failure.
     */
    void save_trade_summary(const domain::trade_summary& trade_summary);

    /**
     * @brief Saves a batch of trade summaries.
     *
     * @param trade_summaries The trade summaries to save.
     * @throws std::exception on failure.
     */
    void save_trade_summaries(const std::vector<domain::trade_summary>& trade_summaries);

    /**
     * @brief Deletes a trade summary by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_summary(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade summaries by their primary keys.
     */
    void delete_trade_summaries(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::trade_summary_repository repo_;
};

}

#endif

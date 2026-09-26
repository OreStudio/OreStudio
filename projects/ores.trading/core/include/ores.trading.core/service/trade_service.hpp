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
 * @brief Service for managing trades.
 *
 * Provides a higher-level interface for trade operations,
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
    messaging::put_trade_response put_trade(const messaging::put_trade_request& request);
    messaging::put_many_trades_response
    put_many_trades(const messaging::put_many_trades_request& request);
    messaging::delete_trade_response delete_trade(const messaging::delete_trade_request& request);
    messaging::delete_many_trades_response
    delete_many_trades(const messaging::delete_many_trades_request& request);
    messaging::list_trade_versions_response
    list_trade_versions(const messaging::list_trade_versions_request& request);
    messaging::get_trade_version_response
    get_trade_version(const messaging::get_trade_version_request& request);
    /**@}*/

    /**
     * @brief Lists trades with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trades for the requested page.
     */
    std::vector<domain::trade> list_trades(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trades.
     *
     * @return Total number of active trades.
     */
    std::uint32_t count_trades();

    /**
     * @brief Lists a page of trades under one node_id.
     *
     * An empty node_id covers every trade the
     * tenant can see, so the caller need not special-case the unfiltered
     * listing.
     */
    std::vector<domain::trade>
    list_trades(std::uint32_t offset, std::uint32_t limit, const std::string& node_id);

    /**
     * @brief Counts the trades under one node_id,
     * for the pager the page itself cannot supply.
     */
    std::uint32_t count_trades(const std::string& node_id);


    /**
     * @brief Retrieves a single trade as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade> get_trade_at_version(const boost::uuids::uuid& id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single trade by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade if found, std::nullopt otherwise.
     */
    std::optional<domain::trade> get_trade(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single trade by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The trade if found, std::nullopt otherwise.
     */
    std::optional<domain::trade> get_trade_by_external_id(const std::string& external_id);

    /**
     * @brief Retrieves a batch of trades by primary key.
     */
    std::vector<domain::trade> get_trades(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade (creates or updates).
     *
     * @param trade The trade to save.
     * @throws std::exception on failure.
     */
    void save_trade(const domain::trade& trade);

    /**
     * @brief Saves a batch of trades.
     *
     * @param trades The trades to save.
     * @throws std::exception on failure.
     */
    void save_trades(const std::vector<domain::trade>& trades);

    /**
     * @brief Deletes a trade by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trades by their primary keys.
     */
    void delete_trades(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::trade> get_trade_history(const std::string& key);

private:
    context ctx_;
    repository::trade_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::trade_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::trade& out);
};

}

#endif

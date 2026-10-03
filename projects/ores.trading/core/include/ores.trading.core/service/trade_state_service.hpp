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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_STATE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_STATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_state.hpp"
#include "ores.trading.api/messaging/trade_state_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade states.
 *
 * Provides a higher-level interface for trade state operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_state_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_state_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_state_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_state_service(context ctx);

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
    messaging::list_trade_states_response
    list_trade_states(const messaging::list_trade_states_request& request);
    messaging::get_trade_state_response
    get_trade_state(const messaging::get_trade_state_request& request);
    messaging::get_many_trade_states_response
    get_many_trade_states(const messaging::get_many_trade_states_request& request);
    messaging::put_trade_state_response
    put_trade_state(const messaging::put_trade_state_request& request);
    messaging::put_many_trade_states_response
    put_many_trade_states(const messaging::put_many_trade_states_request& request);
    messaging::delete_trade_state_response
    delete_trade_state(const messaging::delete_trade_state_request& request);
    messaging::delete_many_trade_states_response
    delete_many_trade_states(const messaging::delete_many_trade_states_request& request);
    messaging::list_trade_state_versions_response
    list_trade_state_versions(const messaging::list_trade_state_versions_request& request);
    messaging::get_trade_state_version_response
    get_trade_state_version(const messaging::get_trade_state_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade states with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade states for the requested page.
     */
    std::vector<domain::trade_state> list_trade_states(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade states.
     *
     * @return Total number of active trade states.
     */
    std::uint32_t count_trade_states();


    /**
     * @brief Retrieves a single trade state as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade state at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_state>
    get_trade_state_at_version(const boost::uuids::uuid& trade_id, std::uint32_t version);

    /**
     * @brief Retrieves a single trade state by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade state if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_state> get_trade_state(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of trade states by primary key.
     */
    std::vector<domain::trade_state> get_trade_states(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a trade state (creates or updates).
     *
     * @param trade_state The trade state to save.
     * @throws std::exception on failure.
     */
    void save_trade_state(const domain::trade_state& trade_state);

    /**
     * @brief Saves a batch of trade states.
     *
     * @param trade_states The trade states to save.
     * @throws std::exception on failure.
     */
    void save_trade_states(const std::vector<domain::trade_state>& trade_states);

    /**
     * @brief Deletes a trade state by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_state(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes trade states by their primary keys.
     */
    void delete_trade_states(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a trade state.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_state> get_trade_state_history(const std::string& trade_id);

private:
    context ctx_;
    repository::trade_state_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::trade_state_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::trade_state& out);
};

}

#endif

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
#ifndef ORES_TRADING_CORE_SERVICE_SWAP_LEG_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SWAP_LEG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/messaging/swap_leg_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing swap legs.
 *
 * Provides a higher-level interface for swap leg operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT swap_leg_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.swap_leg_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a swap_leg_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit swap_leg_service(context ctx);

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
    messaging::list_swap_legs_response
    list_swap_legs(const messaging::list_swap_legs_request& request);
    messaging::get_swap_leg_response get_swap_leg(const messaging::get_swap_leg_request& request);
    messaging::get_many_swap_legs_response
    get_many_swap_legs(const messaging::get_many_swap_legs_request& request);
    messaging::put_swap_leg_response put_swap_leg(const messaging::put_swap_leg_request& request);
    messaging::put_many_swap_legs_response
    put_many_swap_legs(const messaging::put_many_swap_legs_request& request);
    messaging::delete_swap_leg_response
    delete_swap_leg(const messaging::delete_swap_leg_request& request);
    messaging::delete_many_swap_legs_response
    delete_many_swap_legs(const messaging::delete_many_swap_legs_request& request);
    messaging::list_by_trade_id_swap_legs_response
    list_by_trade_id_swap_legs(const messaging::list_by_trade_id_swap_legs_request& request);
    messaging::list_swap_leg_versions_response
    list_swap_leg_versions(const messaging::list_swap_leg_versions_request& request);
    messaging::get_swap_leg_version_response
    get_swap_leg_version(const messaging::get_swap_leg_version_request& request);
    /**@}*/

    /**
     * @brief Lists swap legs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of swap legs for the requested page.
     */
    std::vector<domain::swap_leg> list_swap_legs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active swap legs.
     *
     * @return Total number of active swap legs.
     */
    std::uint32_t count_swap_legs();


    /**
     * @brief Lists swap legs filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching swap legs for the requested page.
     */
    std::vector<domain::swap_leg> list_swap_legs_by_trade_id(const std::string& trade_id,
                                                             std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active swap legs filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching swap legs.
     */
    std::uint32_t count_swap_legs_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single swap leg as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The swap leg at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::swap_leg> get_swap_leg_at_version(const std::string& trade_id,
                                                            const std::string& leg_number,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single swap leg by its primary key.
     *
     * @return The swap leg if found, std::nullopt otherwise.
     */
    std::optional<domain::swap_leg> get_swap_leg(const std::string& trade_id,
                                                 const std::string& leg_number);

    /**
     * @brief Retrieves a batch of swap legs by primary key.
     */
    std::vector<domain::swap_leg> get_swap_legs(const std::vector<std::string>& trade_ids,
                                                const std::vector<std::string>& leg_numbers);

    /**
     * @brief Saves a swap leg (creates or updates).
     *
     * @param swap_leg The swap leg to save.
     * @throws std::exception on failure.
     */
    void save_swap_leg(const domain::swap_leg& swap_leg);

    /**
     * @brief Saves a batch of swap legs.
     *
     * @param swap_legs The swap legs to save.
     * @throws std::exception on failure.
     */
    void save_swap_legs(const std::vector<domain::swap_leg>& swap_legs);

    /**
     * @brief Deletes a swap leg by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_swap_leg(const std::string& trade_id, const std::string& leg_number);

    /**
     * @brief Deletes swap legs by their primary keys.
     */
    void delete_swap_legs(const std::vector<std::string>& trade_ids,
                          const std::vector<std::string>& leg_numbers);

    /**
     * @brief Retrieves all historical versions of a swap leg.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::swap_leg> get_swap_leg_history(const std::string& trade_id,
                                                       const std::string& leg_number);

private:
    context ctx_;
    repository::swap_leg_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::swap_leg_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::swap_leg& out);
};

}

#endif

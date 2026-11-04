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
#ifndef ORES_TRADING_CORE_SERVICE_SWAP_LEG_INDEXING_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SWAP_LEG_INDEXING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/swap_leg_indexing.hpp"
#include "ores.trading.api/messaging/swap_leg_indexing_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/swap_leg_indexing_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing swap leg indexings.
 *
 * Provides a higher-level interface for swap leg indexing operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT swap_leg_indexing_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.swap_leg_indexing_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a swap_leg_indexing_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit swap_leg_indexing_service(context ctx);

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
    ::ores::trading::messaging::list_swap_leg_indexings_response list_swap_leg_indexings(
        const ::ores::trading::messaging::list_swap_leg_indexings_request& request);
    ::ores::trading::messaging::get_swap_leg_indexing_response
    get_swap_leg_indexing(const ::ores::trading::messaging::get_swap_leg_indexing_request& request);
    ::ores::trading::messaging::get_many_swap_leg_indexings_response get_many_swap_leg_indexings(
        const ::ores::trading::messaging::get_many_swap_leg_indexings_request& request);
    ::ores::trading::messaging::put_swap_leg_indexing_response
    put_swap_leg_indexing(const ::ores::trading::messaging::put_swap_leg_indexing_request& request);
    ::ores::trading::messaging::put_many_swap_leg_indexings_response put_many_swap_leg_indexings(
        const ::ores::trading::messaging::put_many_swap_leg_indexings_request& request);
    ::ores::trading::messaging::delete_swap_leg_indexing_response delete_swap_leg_indexing(
        const ::ores::trading::messaging::delete_swap_leg_indexing_request& request);
    ::ores::trading::messaging::delete_many_swap_leg_indexings_response
    delete_many_swap_leg_indexings(
        const ::ores::trading::messaging::delete_many_swap_leg_indexings_request& request);
    ::ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response
    list_by_trade_id_swap_leg_indexings(
        const ::ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request& request);
    ::ores::trading::messaging::list_swap_leg_indexing_versions_response
    list_swap_leg_indexing_versions(
        const ::ores::trading::messaging::list_swap_leg_indexing_versions_request& request);
    ::ores::trading::messaging::get_swap_leg_indexing_version_response
    get_swap_leg_indexing_version(
        const ::ores::trading::messaging::get_swap_leg_indexing_version_request& request);
    /**@}*/

    /**
     * @brief Lists swap leg indexings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of swap leg indexings for the requested page.
     */
    std::vector<domain::swap_leg_indexing> list_swap_leg_indexings(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active swap leg indexings.
     *
     * @return Total number of active swap leg indexings.
     */
    std::uint32_t count_swap_leg_indexings();


    /**
     * @brief Lists swap leg indexings filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching swap leg indexings for the requested page.
     */
    std::vector<domain::swap_leg_indexing> list_swap_leg_indexings_by_trade_id(
        const std::string& trade_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active swap leg indexings filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching swap leg indexings.
     */
    std::uint32_t count_swap_leg_indexings_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single swap leg indexing as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The swap leg indexing at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::swap_leg_indexing>
    get_swap_leg_indexing_at_version(const std::string& trade_id,
                                     const std::string& leg_number,
                                     const std::string& sequence_number,
                                     std::uint32_t version);

    /**
     * @brief Retrieves a single swap leg indexing by its primary key.
     *
     * @return The swap leg indexing if found, std::nullopt otherwise.
     */
    std::optional<domain::swap_leg_indexing>
    get_swap_leg_indexing(const std::string& trade_id,
                          const std::string& leg_number,
                          const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of swap leg indexings by primary key.
     */
    std::vector<domain::swap_leg_indexing>
    get_swap_leg_indexings(const std::vector<std::string>& trade_ids,
                           const std::vector<std::string>& leg_numbers,
                           const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a swap leg indexing (creates or updates).
     *
     * @param swap_leg_indexing The swap leg indexing to save.
     * @throws std::exception on failure.
     */
    void save_swap_leg_indexing(const domain::swap_leg_indexing& swap_leg_indexing);

    /**
     * @brief Saves a batch of swap leg indexings.
     *
     * @param swap_leg_indexings The swap leg indexings to save.
     * @throws std::exception on failure.
     */
    void save_swap_leg_indexings(const std::vector<domain::swap_leg_indexing>& swap_leg_indexings);

    /**
     * @brief Deletes a swap leg indexing by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_swap_leg_indexing(const std::string& trade_id,
                                  const std::string& leg_number,
                                  const std::string& sequence_number);

    /**
     * @brief Deletes swap leg indexings by their primary keys.
     */
    void delete_swap_leg_indexings(const std::vector<std::string>& trade_ids,
                                   const std::vector<std::string>& leg_numbers,
                                   const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a swap leg indexing.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::swap_leg_indexing>
    get_swap_leg_indexing_history(const std::string& trade_id,
                                  const std::string& leg_number,
                                  const std::string& sequence_number);

private:
    context ctx_;
    repository::swap_leg_indexing_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::trading::messaging::swap_leg_indexing_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::swap_leg_indexing& out);
};

}

#endif

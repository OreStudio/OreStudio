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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_BARRIER_LEVEL_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_BARRIER_LEVEL_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_barrier_level.hpp"
#include "ores.trading.api/messaging/instrument_barrier_level_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_barrier_level_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument barrier levels.
 *
 * Provides a higher-level interface for instrument barrier level operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_barrier_level_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.instrument_barrier_level_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_barrier_level_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_barrier_level_service(context ctx);

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
    ::ores::trading::messaging::list_instrument_barrier_levels_response
    list_instrument_barrier_levels(
        const ::ores::trading::messaging::list_instrument_barrier_levels_request& request);
    ::ores::trading::messaging::get_instrument_barrier_level_response get_instrument_barrier_level(
        const ::ores::trading::messaging::get_instrument_barrier_level_request& request);
    ::ores::trading::messaging::get_many_instrument_barrier_levels_response
    get_many_instrument_barrier_levels(
        const ::ores::trading::messaging::get_many_instrument_barrier_levels_request& request);
    ::ores::trading::messaging::put_instrument_barrier_level_response put_instrument_barrier_level(
        const ::ores::trading::messaging::put_instrument_barrier_level_request& request);
    ::ores::trading::messaging::put_many_instrument_barrier_levels_response
    put_many_instrument_barrier_levels(
        const ::ores::trading::messaging::put_many_instrument_barrier_levels_request& request);
    ::ores::trading::messaging::delete_instrument_barrier_level_response
    delete_instrument_barrier_level(
        const ::ores::trading::messaging::delete_instrument_barrier_level_request& request);
    ::ores::trading::messaging::delete_many_instrument_barrier_levels_response
    delete_many_instrument_barrier_levels(
        const ::ores::trading::messaging::delete_many_instrument_barrier_levels_request& request);
    ::ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_response
    list_by_trade_id_instrument_barrier_levels(
        const ::ores::trading::messaging::list_by_trade_id_instrument_barrier_levels_request&
            request);
    ::ores::trading::messaging::list_instrument_barrier_level_versions_response
    list_instrument_barrier_level_versions(
        const ::ores::trading::messaging::list_instrument_barrier_level_versions_request& request);
    ::ores::trading::messaging::get_instrument_barrier_level_version_response
    get_instrument_barrier_level_version(
        const ::ores::trading::messaging::get_instrument_barrier_level_version_request& request);
    /**@}*/

    /**
     * @brief Lists instrument barrier levels with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument barrier levels for the requested page.
     */
    std::vector<domain::instrument_barrier_level>
    list_instrument_barrier_levels(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument barrier levels.
     *
     * @return Total number of active instrument barrier levels.
     */
    std::uint32_t count_instrument_barrier_levels();


    /**
     * @brief Lists instrument barrier levels filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching instrument barrier levels for the requested page.
     */
    std::vector<domain::instrument_barrier_level> list_instrument_barrier_levels_by_trade_id(
        const std::string& trade_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument barrier levels filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching instrument barrier levels.
     */
    std::uint32_t count_instrument_barrier_levels_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single instrument barrier level as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument barrier level at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_barrier_level>
    get_instrument_barrier_level_at_version(const std::string& trade_id,
                                            const std::string& barrier_role,
                                            const std::string& barrier_sequence_number,
                                            const std::string& sequence_number,
                                            std::uint32_t version);

    /**
     * @brief Retrieves a single instrument barrier level by its primary key.
     *
     * @return The instrument barrier level if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_barrier_level>
    get_instrument_barrier_level(const std::string& trade_id,
                                 const std::string& barrier_role,
                                 const std::string& barrier_sequence_number,
                                 const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of instrument barrier levels by primary key.
     */
    std::vector<domain::instrument_barrier_level>
    get_instrument_barrier_levels(const std::vector<std::string>& trade_ids,
                                  const std::vector<std::string>& barrier_roles,
                                  const std::vector<std::string>& barrier_sequence_numbers,
                                  const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a instrument barrier level (creates or updates).
     *
     * @param instrument_barrier_level The instrument barrier level to save.
     * @throws std::exception on failure.
     */
    void
    save_instrument_barrier_level(const domain::instrument_barrier_level& instrument_barrier_level);

    /**
     * @brief Saves a batch of instrument barrier levels.
     *
     * @param instrument_barrier_levels The instrument barrier levels to save.
     * @throws std::exception on failure.
     */
    void save_instrument_barrier_levels(
        const std::vector<domain::instrument_barrier_level>& instrument_barrier_levels);

    /**
     * @brief Deletes a instrument barrier level by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_barrier_level(const std::string& trade_id,
                                         const std::string& barrier_role,
                                         const std::string& barrier_sequence_number,
                                         const std::string& sequence_number);

    /**
     * @brief Deletes instrument barrier levels by their primary keys.
     */
    void delete_instrument_barrier_levels(const std::vector<std::string>& trade_ids,
                                          const std::vector<std::string>& barrier_roles,
                                          const std::vector<std::string>& barrier_sequence_numbers,
                                          const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a instrument barrier level.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::instrument_barrier_level>
    get_instrument_barrier_level_history(const std::string& trade_id,
                                         const std::string& barrier_role,
                                         const std::string& barrier_sequence_number,
                                         const std::string& sequence_number);

private:
    context ctx_;
    repository::instrument_barrier_level_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::instrument_barrier_level_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::instrument_barrier_level& out);
};

}

#endif

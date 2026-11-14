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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_UNDERLYING_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_UNDERLYING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_underlying.hpp"
#include "ores.trading.api/messaging/instrument_underlying_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_underlying_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument underlyings.
 *
 * Provides a higher-level interface for instrument underlying operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_underlying_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.instrument_underlying_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_underlying_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_underlying_service(const context& ctx);

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
    ::ores::trading::messaging::list_instrument_underlyings_response list_instrument_underlyings(
        const ::ores::trading::messaging::list_instrument_underlyings_request& request);
    ::ores::trading::messaging::get_instrument_underlying_response get_instrument_underlying(
        const ::ores::trading::messaging::get_instrument_underlying_request& request);
    ::ores::trading::messaging::get_many_instrument_underlyings_response
    get_many_instrument_underlyings(
        const ::ores::trading::messaging::get_many_instrument_underlyings_request& request);
    ::ores::trading::messaging::put_instrument_underlying_response put_instrument_underlying(
        const ::ores::trading::messaging::put_instrument_underlying_request& request);
    ::ores::trading::messaging::put_many_instrument_underlyings_response
    put_many_instrument_underlyings(
        const ::ores::trading::messaging::put_many_instrument_underlyings_request& request);
    ::ores::trading::messaging::delete_instrument_underlying_response delete_instrument_underlying(
        const ::ores::trading::messaging::delete_instrument_underlying_request& request);
    ::ores::trading::messaging::delete_many_instrument_underlyings_response
    delete_many_instrument_underlyings(
        const ::ores::trading::messaging::delete_many_instrument_underlyings_request& request);
    ::ores::trading::messaging::list_by_trade_id_instrument_underlyings_response
    list_by_trade_id_instrument_underlyings(
        const ::ores::trading::messaging::list_by_trade_id_instrument_underlyings_request& request);
    ::ores::trading::messaging::list_instrument_underlying_versions_response
    list_instrument_underlying_versions(
        const ::ores::trading::messaging::list_instrument_underlying_versions_request& request);
    ::ores::trading::messaging::get_instrument_underlying_version_response
    get_instrument_underlying_version(
        const ::ores::trading::messaging::get_instrument_underlying_version_request& request);
    /**@}*/

    /**
     * @brief Lists instrument underlyings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument underlyings for the requested page.
     */
    std::vector<domain::instrument_underlying> list_instrument_underlyings(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument underlyings.
     *
     * @return Total number of active instrument underlyings.
     */
    std::uint32_t count_instrument_underlyings();


    /**
     * @brief Lists instrument underlyings filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching instrument underlyings for the requested page.
     */
    std::vector<domain::instrument_underlying> list_instrument_underlyings_by_trade_id(
        const std::string& trade_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument underlyings filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching instrument underlyings.
     */
    std::uint32_t count_instrument_underlyings_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single instrument underlying as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument underlying at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_underlying>
    get_instrument_underlying_at_version(const std::string& trade_id,
                                         const std::string& underlying_role,
                                         const std::string& sequence_number,
                                         std::uint32_t version);

    /**
     * @brief Retrieves a single instrument underlying by its primary key.
     *
     * @return The instrument underlying if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_underlying>
    get_instrument_underlying(const std::string& trade_id,
                              const std::string& underlying_role,
                              const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of instrument underlyings by primary key.
     */
    std::vector<domain::instrument_underlying>
    get_instrument_underlyings(const std::vector<std::string>& trade_ids,
                               const std::vector<std::string>& underlying_roles,
                               const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a instrument underlying (creates or updates).
     *
     * @param instrument_underlying The instrument underlying to save.
     * @throws std::exception on failure.
     */
    void save_instrument_underlying(const domain::instrument_underlying& instrument_underlying);

    /**
     * @brief Saves a batch of instrument underlyings.
     *
     * @param instrument_underlyings The instrument underlyings to save.
     * @throws std::exception on failure.
     */
    void save_instrument_underlyings(
        const std::vector<domain::instrument_underlying>& instrument_underlyings);

    /**
     * @brief Deletes a instrument underlying by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_underlying(const std::string& trade_id,
                                      const std::string& underlying_role,
                                      const std::string& sequence_number);

    /**
     * @brief Deletes instrument underlyings by their primary keys.
     */
    void delete_instrument_underlyings(const std::vector<std::string>& trade_ids,
                                       const std::vector<std::string>& underlying_roles,
                                       const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a instrument underlying.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::instrument_underlying>
    get_instrument_underlying_history(const std::string& trade_id,
                                      const std::string& underlying_role,
                                      const std::string& sequence_number);

private:
    context ctx_;
    repository::instrument_underlying_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::instrument_underlying_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::instrument_underlying& out);
};

}

#endif

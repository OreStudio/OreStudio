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
#ifndef ORES_TRADING_CORE_SERVICE_EQUITY_POSITION_OPTION_UNDERLYING_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_EQUITY_POSITION_OPTION_UNDERLYING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.trading.api/messaging/equity_position_option_underlying_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/equity_position_option_underlying_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing equity position option underlyings.
 *
 * Provides a higher-level interface for equity position option underlying operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT equity_position_option_underlying_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.equity_position_option_underlying_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a equity_position_option_underlying_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit equity_position_option_underlying_service(context ctx);

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
    messaging::list_equity_position_option_underlyings_response
    list_equity_position_option_underlyings(
        const messaging::list_equity_position_option_underlyings_request& request);
    messaging::get_equity_position_option_underlying_response get_equity_position_option_underlying(
        const messaging::get_equity_position_option_underlying_request& request);
    messaging::get_many_equity_position_option_underlyings_response
    get_many_equity_position_option_underlyings(
        const messaging::get_many_equity_position_option_underlyings_request& request);
    messaging::put_equity_position_option_underlying_response put_equity_position_option_underlying(
        const messaging::put_equity_position_option_underlying_request& request);
    messaging::put_many_equity_position_option_underlyings_response
    put_many_equity_position_option_underlyings(
        const messaging::put_many_equity_position_option_underlyings_request& request);
    messaging::delete_equity_position_option_underlying_response
    delete_equity_position_option_underlying(
        const messaging::delete_equity_position_option_underlying_request& request);
    messaging::delete_many_equity_position_option_underlyings_response
    delete_many_equity_position_option_underlyings(
        const messaging::delete_many_equity_position_option_underlyings_request& request);
    messaging::list_equity_position_option_underlying_versions_response
    list_equity_position_option_underlying_versions(
        const messaging::list_equity_position_option_underlying_versions_request& request);
    messaging::get_equity_position_option_underlying_version_response
    get_equity_position_option_underlying_version(
        const messaging::get_equity_position_option_underlying_version_request& request);
    /**@}*/

    /**
     * @brief Lists equity position option underlyings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of equity position option underlyings for the requested page.
     */
    std::vector<domain::equity_position_option_underlying>
    list_equity_position_option_underlyings(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active equity position option underlyings.
     *
     * @return Total number of active equity position option underlyings.
     */
    std::uint32_t count_equity_position_option_underlyings();


    /**
     * @brief Retrieves a single equity position option underlying as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The equity position option underlying at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::equity_position_option_underlying>
    get_equity_position_option_underlying_at_version(const std::string& instrument_id,
                                                     const std::string& sequence_number,
                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single equity position option underlying by its primary key.
     *
     * @return The equity position option underlying if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_position_option_underlying>
    get_equity_position_option_underlying(const std::string& instrument_id,
                                          const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of equity position option underlyings by primary key.
     */
    std::vector<domain::equity_position_option_underlying>
    get_equity_position_option_underlyings(const std::vector<std::string>& instrument_ids,
                                           const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a equity position option underlying (creates or updates).
     *
     * @param equity_position_option_underlying The equity position option underlying to save.
     * @throws std::exception on failure.
     */
    void save_equity_position_option_underlying(
        const domain::equity_position_option_underlying& equity_position_option_underlying);

    /**
     * @brief Saves a batch of equity position option underlyings.
     *
     * @param equity_position_option_underlyings The equity position option underlyings to save.
     * @throws std::exception on failure.
     */
    void save_equity_position_option_underlyings(
        const std::vector<domain::equity_position_option_underlying>&
            equity_position_option_underlyings);

    /**
     * @brief Deletes a equity position option underlying by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_equity_position_option_underlying(const std::string& instrument_id,
                                                  const std::string& sequence_number);

    /**
     * @brief Deletes equity position option underlyings by their primary keys.
     */
    void
    delete_equity_position_option_underlyings(const std::vector<std::string>& instrument_ids,
                                              const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a equity position option underlying.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::equity_position_option_underlying>
    get_equity_position_option_underlying_history(const std::string& instrument_id,
                                                  const std::string& sequence_number);

private:
    context ctx_;
    repository::equity_position_option_underlying_repository repo_;

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
    prepare_change(const messaging::equity_position_option_underlying_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::equity_position_option_underlying& out);
};

}

#endif

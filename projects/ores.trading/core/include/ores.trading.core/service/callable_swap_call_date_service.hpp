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
#ifndef ORES_TRADING_CORE_SERVICE_CALLABLE_SWAP_CALL_DATE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_CALLABLE_SWAP_CALL_DATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/callable_swap_call_date.hpp"
#include "ores.trading.api/messaging/callable_swap_call_date_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/callable_swap_call_date_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing callable swap call dates.
 *
 * Provides a higher-level interface for callable swap call date operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT callable_swap_call_date_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.callable_swap_call_date_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a callable_swap_call_date_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit callable_swap_call_date_service(context ctx);

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
    messaging::list_callable_swap_call_dates_response
    list_callable_swap_call_dates(const messaging::list_callable_swap_call_dates_request& request);
    messaging::get_callable_swap_call_date_response
    get_callable_swap_call_date(const messaging::get_callable_swap_call_date_request& request);
    messaging::get_many_callable_swap_call_dates_response get_many_callable_swap_call_dates(
        const messaging::get_many_callable_swap_call_dates_request& request);
    messaging::put_callable_swap_call_date_response
    put_callable_swap_call_date(const messaging::put_callable_swap_call_date_request& request);
    messaging::put_many_callable_swap_call_dates_response put_many_callable_swap_call_dates(
        const messaging::put_many_callable_swap_call_dates_request& request);
    messaging::delete_callable_swap_call_date_response delete_callable_swap_call_date(
        const messaging::delete_callable_swap_call_date_request& request);
    messaging::delete_many_callable_swap_call_dates_response delete_many_callable_swap_call_dates(
        const messaging::delete_many_callable_swap_call_dates_request& request);
    messaging::list_callable_swap_call_date_versions_response list_callable_swap_call_date_versions(
        const messaging::list_callable_swap_call_date_versions_request& request);
    messaging::get_callable_swap_call_date_version_response get_callable_swap_call_date_version(
        const messaging::get_callable_swap_call_date_version_request& request);
    /**@}*/

    /**
     * @brief Lists callable swap call dates with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of callable swap call dates for the requested page.
     */
    std::vector<domain::callable_swap_call_date> list_callable_swap_call_dates(std::uint32_t offset,
                                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active callable swap call dates.
     *
     * @return Total number of active callable swap call dates.
     */
    std::uint32_t count_callable_swap_call_dates();


    /**
     * @brief Retrieves a single callable swap call date as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The callable swap call date at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::callable_swap_call_date> get_callable_swap_call_date_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single callable swap call date by its primary key.
     *
     * @return The callable swap call date if found, std::nullopt otherwise.
     */
    std::optional<domain::callable_swap_call_date>
    get_callable_swap_call_date(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of callable swap call dates by primary key.
     */
    std::vector<domain::callable_swap_call_date>
    get_callable_swap_call_dates(const std::vector<std::string>& trade_ids,
                                 const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a callable swap call date (creates or updates).
     *
     * @param callable_swap_call_date The callable swap call date to save.
     * @throws std::exception on failure.
     */
    void
    save_callable_swap_call_date(const domain::callable_swap_call_date& callable_swap_call_date);

    /**
     * @brief Saves a batch of callable swap call dates.
     *
     * @param callable_swap_call_dates The callable swap call dates to save.
     * @throws std::exception on failure.
     */
    void save_callable_swap_call_dates(
        const std::vector<domain::callable_swap_call_date>& callable_swap_call_dates);

    /**
     * @brief Deletes a callable swap call date by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_callable_swap_call_date(const std::string& trade_id,
                                        const std::string& sequence_number);

    /**
     * @brief Deletes callable swap call dates by their primary keys.
     */
    void delete_callable_swap_call_dates(const std::vector<std::string>& trade_ids,
                                         const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a callable swap call date.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::callable_swap_call_date>
    get_callable_swap_call_date_history(const std::string& trade_id,
                                        const std::string& sequence_number);

private:
    context ctx_;
    repository::callable_swap_call_date_repository repo_;

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
    prepare_change(const messaging::callable_swap_call_date_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::callable_swap_call_date& out);
};

}

#endif

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
#ifndef ORES_TRADING_CORE_SERVICE_RANGE_BOUND_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_RANGE_BOUND_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.trading.api/messaging/range_bound_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/range_bound_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing range bounds.
 *
 * Provides a higher-level interface for range bound operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT range_bound_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.range_bound_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a range_bound_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit range_bound_service(context ctx);

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
    ::ores::trading::messaging::list_range_bounds_response
    list_range_bounds(const ::ores::trading::messaging::list_range_bounds_request& request);
    ::ores::trading::messaging::get_range_bound_response
    get_range_bound(const ::ores::trading::messaging::get_range_bound_request& request);
    ::ores::trading::messaging::get_many_range_bounds_response
    get_many_range_bounds(const ::ores::trading::messaging::get_many_range_bounds_request& request);
    ::ores::trading::messaging::put_range_bound_response
    put_range_bound(const ::ores::trading::messaging::put_range_bound_request& request);
    ::ores::trading::messaging::put_many_range_bounds_response
    put_many_range_bounds(const ::ores::trading::messaging::put_many_range_bounds_request& request);
    ::ores::trading::messaging::delete_range_bound_response
    delete_range_bound(const ::ores::trading::messaging::delete_range_bound_request& request);
    ::ores::trading::messaging::delete_many_range_bounds_response delete_many_range_bounds(
        const ::ores::trading::messaging::delete_many_range_bounds_request& request);
    ::ores::trading::messaging::list_by_trade_id_range_bounds_response
    list_by_trade_id_range_bounds(
        const ::ores::trading::messaging::list_by_trade_id_range_bounds_request& request);
    ::ores::trading::messaging::list_range_bound_versions_response list_range_bound_versions(
        const ::ores::trading::messaging::list_range_bound_versions_request& request);
    ::ores::trading::messaging::get_range_bound_version_response get_range_bound_version(
        const ::ores::trading::messaging::get_range_bound_version_request& request);
    /**@}*/

    /**
     * @brief Lists range bounds with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of range bounds for the requested page.
     */
    std::vector<domain::range_bound> list_range_bounds(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active range bounds.
     *
     * @return Total number of active range bounds.
     */
    std::uint32_t count_range_bounds();


    /**
     * @brief Lists range bounds filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching range bounds for the requested page.
     */
    std::vector<domain::range_bound> list_range_bounds_by_trade_id(const std::string& trade_id,
                                                                   std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active range bounds filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching range bounds.
     */
    std::uint32_t count_range_bounds_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single range bound as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The range bound at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::range_bound> get_range_bound_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single range bound by its primary key.
     *
     * @return The range bound if found, std::nullopt otherwise.
     */
    std::optional<domain::range_bound> get_range_bound(const std::string& trade_id,
                                                       const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of range bounds by primary key.
     */
    std::vector<domain::range_bound>
    get_range_bounds(const std::vector<std::string>& trade_ids,
                     const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a range bound (creates or updates).
     *
     * @param range_bound The range bound to save.
     * @throws std::exception on failure.
     */
    void save_range_bound(const domain::range_bound& range_bound);

    /**
     * @brief Saves a batch of range bounds.
     *
     * @param range_bounds The range bounds to save.
     * @throws std::exception on failure.
     */
    void save_range_bounds(const std::vector<domain::range_bound>& range_bounds);

    /**
     * @brief Deletes a range bound by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_range_bound(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Deletes range bounds by their primary keys.
     */
    void delete_range_bounds(const std::vector<std::string>& trade_ids,
                             const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a range bound.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::range_bound> get_range_bound_history(const std::string& trade_id,
                                                             const std::string& sequence_number);

private:
    context ctx_;
    repository::range_bound_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::range_bound_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::range_bound& out);
};

}

#endif

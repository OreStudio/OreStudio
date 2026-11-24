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
#ifndef ORES_MARKETDATA_CORE_SERVICE_DELIVERY_COUNT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_DELIVERY_COUNT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/domain/delivery_count.hpp"
#include "ores.marketdata.api/messaging/delivery_count_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/delivery_count_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing delivery counts.
 *
 * Provides a higher-level interface for delivery count operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT delivery_count_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.delivery_count_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a delivery_count_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit delivery_count_service(const context& ctx);

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
    ::ores::marketdata::messaging::list_delivery_counts_response list_delivery_counts(
        const ::ores::marketdata::messaging::list_delivery_counts_request& request);
    ::ores::marketdata::messaging::get_delivery_count_response
    get_delivery_count(const ::ores::marketdata::messaging::get_delivery_count_request& request);
    ::ores::marketdata::messaging::get_many_delivery_counts_response get_many_delivery_counts(
        const ::ores::marketdata::messaging::get_many_delivery_counts_request& request);
    ::ores::marketdata::messaging::put_delivery_count_response
    put_delivery_count(const ::ores::marketdata::messaging::put_delivery_count_request& request);
    ::ores::marketdata::messaging::put_many_delivery_counts_response put_many_delivery_counts(
        const ::ores::marketdata::messaging::put_many_delivery_counts_request& request);
    ::ores::marketdata::messaging::delete_delivery_count_response delete_delivery_count(
        const ::ores::marketdata::messaging::delete_delivery_count_request& request);
    ::ores::marketdata::messaging::delete_many_delivery_counts_response delete_many_delivery_counts(
        const ::ores::marketdata::messaging::delete_many_delivery_counts_request& request);
    /**@}*/

    /**
     * @brief Lists delivery counts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of delivery counts for the requested page.
     */
    std::vector<domain::delivery_count> list_delivery_counts(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active delivery counts.
     *
     * @return Total number of active delivery counts.
     */
    std::uint32_t count_delivery_counts();


    /**
     * @brief Retrieves a single delivery count by its primary key.
     *
     * @return The delivery count if found, std::nullopt otherwise.
     */
    std::optional<domain::delivery_count> get_delivery_count(const std::string& party_id,
                                                             const std::string& provider_code,
                                                             const std::string& feed_id,
                                                             const std::string& oresmd_uri,
                                                             const std::string& period_start);

    /**
     * @brief Retrieves a single delivery count by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * @return The delivery count if found, std::nullopt otherwise.
     */
    std::optional<domain::delivery_count>
    get_delivery_count_by_oresmd_uri(const std::string& oresmd_uri);

    /**
     * @brief Retrieves a batch of delivery counts by primary key.
     */
    std::vector<domain::delivery_count>
    get_delivery_counts(const std::vector<std::string>& party_ids,
                        const std::vector<std::string>& provider_codes,
                        const std::vector<std::string>& feed_ids,
                        const std::vector<std::string>& oresmd_uris,
                        const std::vector<std::string>& period_starts);

    /**
     * @brief Saves a delivery count (creates or updates).
     *
     * @param delivery_count The delivery count to save.
     * @throws std::exception on failure.
     */
    void save_delivery_count(const domain::delivery_count& delivery_count);

    /**
     * @brief Saves a batch of delivery counts.
     *
     * @param delivery_counts The delivery counts to save.
     * @throws std::exception on failure.
     */
    void save_delivery_counts(const std::vector<domain::delivery_count>& delivery_counts);

    /**
     * @brief Deletes a delivery count by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_delivery_count(const std::string& party_id,
                               const std::string& provider_code,
                               const std::string& feed_id,
                               const std::string& oresmd_uri,
                               const std::string& period_start);

    /**
     * @brief Deletes delivery counts by their primary keys.
     */
    void delete_delivery_counts(const std::vector<std::string>& party_ids,
                                const std::vector<std::string>& provider_codes,
                                const std::vector<std::string>& feed_ids,
                                const std::vector<std::string>& oresmd_uris,
                                const std::vector<std::string>& period_starts);


private:
    context ctx_;
    repository::delivery_count_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::marketdata::messaging::delivery_count_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::delivery_count>& current,
                   domain::delivery_count& out);
};

}

#endif

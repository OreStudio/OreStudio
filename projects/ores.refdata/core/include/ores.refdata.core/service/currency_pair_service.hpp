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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_PAIR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_PAIR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_pair.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_pair_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency pairs.
 *
 * Provides a higher-level interface for currency pair operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_pair_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.currency_pair_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_pair_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_pair_service(context ctx);

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
    messaging::list_currency_pairs_response
    list_currency_pairs(const messaging::list_currency_pairs_request& request);
    messaging::get_currency_pair_response
    get_currency_pair(const messaging::get_currency_pair_request& request);
    messaging::get_many_currency_pairs_response
    get_many_currency_pairs(const messaging::get_many_currency_pairs_request& request);
    messaging::put_currency_pair_response
    put_currency_pair(const messaging::put_currency_pair_request& request);
    messaging::put_many_currency_pairs_response
    put_many_currency_pairs(const messaging::put_many_currency_pairs_request& request);
    messaging::delete_currency_pair_response
    delete_currency_pair(const messaging::delete_currency_pair_request& request);
    messaging::delete_many_currency_pairs_response
    delete_many_currency_pairs(const messaging::delete_many_currency_pairs_request& request);
    messaging::list_currency_pair_versions_response
    list_currency_pair_versions(const messaging::list_currency_pair_versions_request& request);
    messaging::get_currency_pair_version_response
    get_currency_pair_version(const messaging::get_currency_pair_version_request& request);
    /**@}*/

    /**
     * @brief Lists currency pairs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currency pairs for the requested page.
     */
    std::vector<domain::currency_pair> list_pairs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency pairs.
     *
     * @return Total number of active currency pairs.
     */
    std::uint32_t count_pairs();


    /**
     * @brief Retrieves a single currency pair as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The currency pair at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_pair> get_pair_at_version(const std::string& pair_code,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single currency pair by its primary key.
     *
     * @return The currency pair if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_pair> get_pair(const std::string& pair_code);

    /**
     * @brief Retrieves a batch of currency pairs by primary key.
     */
    std::vector<domain::currency_pair> get_pairs(const std::vector<std::string>& pair_codes);

    /**
     * @brief Saves a currency pair (creates or updates).
     *
     * @param pair The currency pair to save.
     * @throws std::exception on failure.
     */
    void save_pair(const domain::currency_pair& pair);

    /**
     * @brief Saves a batch of currency pairs.
     *
     * @param pairs The currency pairs to save.
     * @throws std::exception on failure.
     */
    void save_pairs(const std::vector<domain::currency_pair>& pairs);

    /**
     * @brief Deletes a currency pair by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_pair(const std::string& pair_code);

    /**
     * @brief Deletes currency pairs by their primary keys.
     */
    void delete_pairs(const std::vector<std::string>& pair_codes);

    /**
     * @brief Retrieves all historical versions of a currency pair.
     */
    std::vector<domain::currency_pair> get_pair_history(const std::string& pair_code);

private:
    context ctx_;
    repository::currency_pair_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::currency_pair_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::currency_pair& out);
};

}

#endif

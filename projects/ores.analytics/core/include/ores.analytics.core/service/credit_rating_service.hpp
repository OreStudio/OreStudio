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
#ifndef ORES_ANALYTICS_CORE_SERVICE_CREDIT_RATING_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_CREDIT_RATING_SERVICE_HPP

#include "ores.analytics.api/domain/credit_rating.hpp"
#include "ores.analytics.api/messaging/credit_rating_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/credit_rating_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing credit ratings.
 *
 * Provides a higher-level interface for credit rating operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT credit_rating_service {
private:
    inline static std::string_view logger_name = "ores.analytics.service.credit_rating_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a credit_rating_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit credit_rating_service(context ctx);

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
    messaging::list_credit_ratings_response
    list_credit_ratings(const messaging::list_credit_ratings_request& request);
    messaging::get_credit_rating_response
    get_credit_rating(const messaging::get_credit_rating_request& request);
    messaging::get_many_credit_ratings_response
    get_many_credit_ratings(const messaging::get_many_credit_ratings_request& request);
    messaging::put_credit_rating_response
    put_credit_rating(const messaging::put_credit_rating_request& request);
    messaging::put_many_credit_ratings_response
    put_many_credit_ratings(const messaging::put_many_credit_ratings_request& request);
    messaging::delete_credit_rating_response
    delete_credit_rating(const messaging::delete_credit_rating_request& request);
    messaging::delete_many_credit_ratings_response
    delete_many_credit_ratings(const messaging::delete_many_credit_ratings_request& request);
    messaging::list_credit_rating_versions_response
    list_credit_rating_versions(const messaging::list_credit_rating_versions_request& request);
    messaging::get_credit_rating_version_response
    get_credit_rating_version(const messaging::get_credit_rating_version_request& request);
    /**@}*/

    /**
     * @brief Lists credit ratings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of credit ratings for the requested page.
     */
    std::vector<domain::credit_rating> list_ratings(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active credit ratings.
     *
     * @return Total number of active credit ratings.
     */
    std::uint32_t count_ratings();


    /**
     * @brief Retrieves a single credit rating as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The credit rating at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::credit_rating> get_rating_at_version(const std::string& code,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single credit rating by its primary key.
     *
     * @return The credit rating if found, std::nullopt otherwise.
     */
    std::optional<domain::credit_rating> get_rating(const std::string& code);

    /**
     * @brief Retrieves a batch of credit ratings by primary key.
     */
    std::vector<domain::credit_rating> get_ratings(const std::vector<std::string>& codes);

    /**
     * @brief Saves a credit rating (creates or updates).
     *
     * @param rating The credit rating to save.
     * @throws std::exception on failure.
     */
    void save_rating(const domain::credit_rating& rating);

    /**
     * @brief Saves a batch of credit ratings.
     *
     * @param ratings The credit ratings to save.
     * @throws std::exception on failure.
     */
    void save_ratings(const std::vector<domain::credit_rating>& ratings);

    /**
     * @brief Deletes a credit rating by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_rating(const std::string& code);

    /**
     * @brief Deletes credit ratings by their primary keys.
     */
    void delete_ratings(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a credit rating.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::credit_rating> get_rating_history(const std::string& code);

private:
    context ctx_;
    repository::credit_rating_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::credit_rating_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::credit_rating& out);
};

}

#endif

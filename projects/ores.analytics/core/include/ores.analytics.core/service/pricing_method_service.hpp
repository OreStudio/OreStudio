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
#ifndef ORES_ANALYTICS_CORE_SERVICE_PRICING_METHOD_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_PRICING_METHOD_SERVICE_HPP

#include "ores.analytics.api/domain/pricing_method.hpp"
#include "ores.analytics.api/messaging/pricing_method_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/pricing_method_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing pricing methods.
 *
 * Provides a higher-level interface for pricing method operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT pricing_method_service {
private:
    inline static std::string_view logger_name = "ores.analytics.service.pricing_method_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a pricing_method_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit pricing_method_service(context ctx);

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
    messaging::list_pricing_methods_response
    list_pricing_methods(const messaging::list_pricing_methods_request& request);
    messaging::get_pricing_method_response
    get_pricing_method(const messaging::get_pricing_method_request& request);
    messaging::get_many_pricing_methods_response
    get_many_pricing_methods(const messaging::get_many_pricing_methods_request& request);
    messaging::put_pricing_method_response
    put_pricing_method(const messaging::put_pricing_method_request& request);
    messaging::put_many_pricing_methods_response
    put_many_pricing_methods(const messaging::put_many_pricing_methods_request& request);
    messaging::delete_pricing_method_response
    delete_pricing_method(const messaging::delete_pricing_method_request& request);
    messaging::delete_many_pricing_methods_response
    delete_many_pricing_methods(const messaging::delete_many_pricing_methods_request& request);
    messaging::list_pricing_method_versions_response
    list_pricing_method_versions(const messaging::list_pricing_method_versions_request& request);
    messaging::get_pricing_method_version_response
    get_pricing_method_version(const messaging::get_pricing_method_version_request& request);
    /**@}*/

    /**
     * @brief Lists pricing methods with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of pricing methods for the requested page.
     */
    std::vector<domain::pricing_method> list_methods(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active pricing methods.
     *
     * @return Total number of active pricing methods.
     */
    std::uint32_t count_methods();


    /**
     * @brief Retrieves a single pricing method as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The pricing method at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_method> get_method_at_version(const std::string& code,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single pricing method by its primary key.
     *
     * @return The pricing method if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_method> find_method(const std::string& code);

    /**
     * @brief Retrieves a batch of pricing methods by primary key.
     */
    std::vector<domain::pricing_method> get_methods(const std::vector<std::string>& codes);

    /**
     * @brief Saves a pricing method (creates or updates).
     *
     * @param method The pricing method to save.
     * @throws std::exception on failure.
     */
    void save_method(const domain::pricing_method& method);

    /**
     * @brief Saves a batch of pricing methods.
     *
     * @param methods The pricing methods to save.
     * @throws std::exception on failure.
     */
    void save_methods(const std::vector<domain::pricing_method>& methods);

    /**
     * @brief Deletes a pricing method by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_method(const std::string& code);

    /**
     * @brief Deletes pricing methods by their primary keys.
     */
    void delete_methods(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a pricing method.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::pricing_method> get_method_history(const std::string& code);

private:
    context ctx_;
    repository::pricing_method_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::pricing_method_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::pricing_method& out);
};

}

#endif

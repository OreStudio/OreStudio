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
#ifndef ORES_TRADING_CORE_SERVICE_ACTIVITY_CATEGORY_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_ACTIVITY_CATEGORY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/activity_category.hpp"
#include "ores.trading.api/messaging/activity_category_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/activity_category_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing activity categorys.
 *
 * Provides a higher-level interface for activity category operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT activity_category_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.activity_category_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a activity_category_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit activity_category_service(context ctx);

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
    messaging::list_activity_categories_response
    list_activity_categories(const messaging::list_activity_categories_request& request);
    messaging::get_activity_category_response
    get_activity_category(const messaging::get_activity_category_request& request);
    messaging::get_many_activity_categories_response
    get_many_activity_categories(const messaging::get_many_activity_categories_request& request);
    messaging::put_activity_category_response
    put_activity_category(const messaging::put_activity_category_request& request);
    messaging::put_many_activity_categories_response
    put_many_activity_categories(const messaging::put_many_activity_categories_request& request);
    messaging::delete_activity_category_response
    delete_activity_category(const messaging::delete_activity_category_request& request);
    messaging::delete_many_activity_categories_response delete_many_activity_categories(
        const messaging::delete_many_activity_categories_request& request);
    messaging::list_activity_category_versions_response list_activity_category_versions(
        const messaging::list_activity_category_versions_request& request);
    messaging::get_activity_category_version_response
    get_activity_category_version(const messaging::get_activity_category_version_request& request);
    /**@}*/

    /**
     * @brief Lists activity categorys with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of activity categorys for the requested page.
     */
    std::vector<domain::activity_category> list_activity_categories(std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active activity categorys.
     *
     * @return Total number of active activity categorys.
     */
    std::uint32_t count_activity_categories();


    /**
     * @brief Retrieves a single activity category as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The activity category at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::activity_category>
    get_activity_category_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single activity category by its primary key.
     *
     * @return The activity category if found, std::nullopt otherwise.
     */
    std::optional<domain::activity_category> get_activity_category(const std::string& code);

    /**
     * @brief Retrieves a batch of activity categorys by primary key.
     */
    std::vector<domain::activity_category>
    get_activity_categories(const std::vector<std::string>& codes);

    /**
     * @brief Saves a activity category (creates or updates).
     *
     * @param activity_category The activity category to save.
     * @throws std::exception on failure.
     */
    void save_activity_category(const domain::activity_category& activity_category);

    /**
     * @brief Saves a batch of activity categorys.
     *
     * @param activity_categories The activity categorys to save.
     * @throws std::exception on failure.
     */
    void
    save_activity_categories(const std::vector<domain::activity_category>& activity_categories);

    /**
     * @brief Deletes a activity category by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_activity_category(const std::string& code);

    /**
     * @brief Deletes activity categorys by their primary keys.
     */
    void delete_activity_categories(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a activity category.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::activity_category> get_activity_category_history(const std::string& code);

private:
    context ctx_;
    repository::activity_category_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::activity_category_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::activity_category& out);
};

}

#endif

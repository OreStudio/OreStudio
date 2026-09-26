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
#ifndef ORES_DQ_CORE_SERVICE_CHANGE_REASON_CATEGORY_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CHANGE_REASON_CATEGORY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/change_reason_category.hpp"
#include "ores.dq.api/messaging/change_reason_category_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/change_reason_category_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing change reason categories.
 *
 * Provides a higher-level interface for change reason category operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT change_reason_category_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.change_reason_category_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a change_reason_category_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit change_reason_category_service(context ctx);

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
    messaging::list_change_reason_categories_response
    list_change_reason_categories(const messaging::list_change_reason_categories_request& request);
    messaging::get_change_reason_category_response
    get_change_reason_category(const messaging::get_change_reason_category_request& request);
    messaging::get_many_change_reason_categories_response get_many_change_reason_categories(
        const messaging::get_many_change_reason_categories_request& request);
    messaging::put_change_reason_category_response
    put_change_reason_category(const messaging::put_change_reason_category_request& request);
    messaging::put_many_change_reason_categories_response put_many_change_reason_categories(
        const messaging::put_many_change_reason_categories_request& request);
    messaging::delete_change_reason_category_response
    delete_change_reason_category(const messaging::delete_change_reason_category_request& request);
    messaging::delete_many_change_reason_categories_response delete_many_change_reason_categories(
        const messaging::delete_many_change_reason_categories_request& request);
    messaging::list_change_reason_category_versions_response list_change_reason_category_versions(
        const messaging::list_change_reason_category_versions_request& request);
    messaging::get_change_reason_category_version_response get_change_reason_category_version(
        const messaging::get_change_reason_category_version_request& request);
    /**@}*/

    /**
     * @brief Lists change reason categories with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of change reason categories for the requested page.
     */
    std::vector<domain::change_reason_category> list_categories(std::uint32_t offset,
                                                                std::uint32_t limit);

    /**
     * @brief Gets the total count of active change reason categories.
     *
     * @return Total number of active change reason categories.
     */
    std::uint32_t count_categories();


    /**
     * @brief Retrieves a single change reason category as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The change reason category at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::change_reason_category> get_category_at_version(const std::string& code,
                                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single change reason category by its primary key.
     *
     * @return The change reason category if found, std::nullopt otherwise.
     */
    std::optional<domain::change_reason_category> get_category(const std::string& code);

    /**
     * @brief Retrieves a batch of change reason categories by primary key.
     */
    std::vector<domain::change_reason_category>
    get_categories(const std::vector<std::string>& codes);

    /**
     * @brief Saves a change reason category (creates or updates).
     *
     * @param category The change reason category to save.
     * @throws std::exception on failure.
     */
    void save_category(const domain::change_reason_category& category);

    /**
     * @brief Saves a batch of change reason categories.
     *
     * @param categories The change reason categories to save.
     * @throws std::exception on failure.
     */
    void save_categories(const std::vector<domain::change_reason_category>& categories);

    /**
     * @brief Deletes a change reason category by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_category(const std::string& code);

    /**
     * @brief Deletes change reason categories by their primary keys.
     */
    void delete_categories(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a change reason category.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::change_reason_category> get_category_history(const std::string& code);

private:
    context ctx_;
    repository::change_reason_category_repository repo_;

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
    prepare_change(const messaging::change_reason_category_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::change_reason_category& out);
};

}

#endif

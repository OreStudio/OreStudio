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
#ifndef ORES_ANALYTICS_CORE_SERVICE_SENSITIVITY_ASSET_CLASS_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_SENSITIVITY_ASSET_CLASS_SERVICE_HPP

#include "ores.analytics.api/domain/sensitivity_asset_class.hpp"
#include "ores.analytics.api/messaging/sensitivity_asset_class_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/sensitivity_asset_class_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing sensitivity asset classes.
 *
 * Provides a higher-level interface for sensitivity asset class operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT sensitivity_asset_class_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.sensitivity_asset_class_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a sensitivity_asset_class_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit sensitivity_asset_class_service(context ctx);

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
    ::ores::analytics::messaging::list_sensitivity_asset_classes_response
    list_sensitivity_asset_classes(
        const ::ores::analytics::messaging::list_sensitivity_asset_classes_request& request);
    ::ores::analytics::messaging::get_sensitivity_asset_class_response get_sensitivity_asset_class(
        const ::ores::analytics::messaging::get_sensitivity_asset_class_request& request);
    ::ores::analytics::messaging::get_many_sensitivity_asset_classes_response
    get_many_sensitivity_asset_classes(
        const ::ores::analytics::messaging::get_many_sensitivity_asset_classes_request& request);
    ::ores::analytics::messaging::put_sensitivity_asset_class_response put_sensitivity_asset_class(
        const ::ores::analytics::messaging::put_sensitivity_asset_class_request& request);
    ::ores::analytics::messaging::put_many_sensitivity_asset_classes_response
    put_many_sensitivity_asset_classes(
        const ::ores::analytics::messaging::put_many_sensitivity_asset_classes_request& request);
    ::ores::analytics::messaging::delete_sensitivity_asset_class_response
    delete_sensitivity_asset_class(
        const ::ores::analytics::messaging::delete_sensitivity_asset_class_request& request);
    ::ores::analytics::messaging::delete_many_sensitivity_asset_classes_response
    delete_many_sensitivity_asset_classes(
        const ::ores::analytics::messaging::delete_many_sensitivity_asset_classes_request& request);
    ::ores::analytics::messaging::list_sensitivity_asset_class_versions_response
    list_sensitivity_asset_class_versions(
        const ::ores::analytics::messaging::list_sensitivity_asset_class_versions_request& request);
    ::ores::analytics::messaging::get_sensitivity_asset_class_version_response
    get_sensitivity_asset_class_version(
        const ::ores::analytics::messaging::get_sensitivity_asset_class_version_request& request);
    /**@}*/

    /**
     * @brief Lists sensitivity asset classes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of sensitivity asset classes for the requested page.
     */
    std::vector<domain::sensitivity_asset_class> list_asset_classes(std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active sensitivity asset classes.
     *
     * @return Total number of active sensitivity asset classes.
     */
    std::uint32_t count_asset_classes();


    /**
     * @brief Retrieves a single sensitivity asset class as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The sensitivity asset class at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::sensitivity_asset_class>
    get_asset_class_at_version(const std::string& asset_class_code, std::uint32_t version);

    /**
     * @brief Retrieves a single sensitivity asset class by its primary key.
     *
     * @return The sensitivity asset class if found, std::nullopt otherwise.
     */
    std::optional<domain::sensitivity_asset_class>
    get_asset_class(const std::string& asset_class_code);

    /**
     * @brief Retrieves a batch of sensitivity asset classes by primary key.
     */
    std::vector<domain::sensitivity_asset_class>
    get_asset_classes(const std::vector<std::string>& asset_class_codes);

    /**
     * @brief Saves a sensitivity asset class (creates or updates).
     *
     * @param asset_class The sensitivity asset class to save.
     * @throws std::exception on failure.
     */
    void save_asset_class(const domain::sensitivity_asset_class& asset_class);

    /**
     * @brief Saves a batch of sensitivity asset classes.
     *
     * @param asset_classes The sensitivity asset classes to save.
     * @throws std::exception on failure.
     */
    void save_asset_classes(const std::vector<domain::sensitivity_asset_class>& asset_classes);

    /**
     * @brief Deletes a sensitivity asset class by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_asset_class(const std::string& asset_class_code);

    /**
     * @brief Deletes sensitivity asset classes by their primary keys.
     */
    void delete_asset_classes(const std::vector<std::string>& asset_class_codes);

    /**
     * @brief Retrieves all historical versions of a sensitivity asset class.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::sensitivity_asset_class>
    get_asset_class_history(const std::string& asset_class_code);

private:
    context ctx_;
    repository::sensitivity_asset_class_repository repo_;

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
    prepare_change(const ::ores::analytics::messaging::sensitivity_asset_class_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::sensitivity_asset_class& out);
};

}

#endif

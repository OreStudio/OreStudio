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
#ifndef ORES_VARIABILITY_CORE_SERVICE_SYSTEM_SETTING_SERVICE_HPP
#define ORES_VARIABILITY_CORE_SERVICE_SYSTEM_SETTING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/messaging/system_setting_protocol.hpp"
#include "ores.variability.core/export.hpp"
#include "ores.variability.core/repository/system_setting_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::variability::service {

/**
 * @brief Service for managing system settings.
 *
 * Provides a higher-level interface for system setting operations,
 * wrapping the underlying repository.
 */
class ORES_VARIABILITY_CORE_EXPORT system_setting_service {
private:
    inline static std::string_view logger_name = "ores.variability.service.system_setting_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a system_setting_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit system_setting_service(context ctx);

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
    messaging::list_system_settings_response
    list_system_settings(const messaging::list_system_settings_request& request);
    messaging::get_system_setting_response
    get_system_setting(const messaging::get_system_setting_request& request);
    messaging::get_many_system_settings_response
    get_many_system_settings(const messaging::get_many_system_settings_request& request);
    messaging::put_system_setting_response
    put_system_setting(const messaging::put_system_setting_request& request);
    messaging::put_many_system_settings_response
    put_many_system_settings(const messaging::put_many_system_settings_request& request);
    messaging::delete_system_setting_response
    delete_system_setting(const messaging::delete_system_setting_request& request);
    messaging::delete_many_system_settings_response
    delete_many_system_settings(const messaging::delete_many_system_settings_request& request);
    messaging::list_system_setting_versions_response
    list_system_setting_versions(const messaging::list_system_setting_versions_request& request);
    messaging::get_system_setting_version_response
    get_system_setting_version(const messaging::get_system_setting_version_request& request);
    /**@}*/

    /**
     * @brief Lists system settings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of system settings for the requested page.
     */
    std::vector<domain::system_setting> list_settings(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active system settings.
     *
     * @return Total number of active system settings.
     */
    std::uint32_t count_settings();


    /**
     * @brief Retrieves a single system setting as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The system setting at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::system_setting> get_setting_at_version(const boost::uuids::uuid& id,
                                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single system setting by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The system setting if found, std::nullopt otherwise.
     */
    std::optional<domain::system_setting> get_setting(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single system setting by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The system setting if found, std::nullopt otherwise.
     */
    std::optional<domain::system_setting> get_setting_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of system settings by primary key.
     */
    std::vector<domain::system_setting> get_settings(const std::vector<std::string>& ids);

    /**
     * @brief Saves a system setting (creates or updates).
     *
     * @param setting The system setting to save.
     * @throws std::exception on failure.
     */
    void save_setting(const domain::system_setting& setting);

    /**
     * @brief Saves a batch of system settings.
     *
     * @param settings The system settings to save.
     * @throws std::exception on failure.
     */
    void save_settings(const std::vector<domain::system_setting>& settings);

    /**
     * @brief Deletes a system setting by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_setting(const boost::uuids::uuid& id);

    /**
     * @brief Deletes system settings by their primary keys.
     */
    void delete_settings(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a system setting.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::system_setting> get_setting_history(const std::string& key);

private:
    context ctx_;
    repository::system_setting_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::system_setting_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::system_setting& out);
};

}

#endif

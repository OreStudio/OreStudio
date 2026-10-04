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
#ifndef ORES_INBOX_CORE_SERVICE_NOTIFICATION_PREFERENCE_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_NOTIFICATION_PREFERENCE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/notification_preference.hpp"
#include "ores.inbox.api/messaging/notification_preference_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/notification_preference_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing notification preferences.
 *
 * Provides a higher-level interface for notification preference operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT notification_preference_service {
private:
    inline static std::string_view logger_name =
        "ores.inbox.service.notification_preference_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a notification_preference_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit notification_preference_service(context ctx);

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
    messaging::list_notification_preferences_response
    list_notification_preferences(const messaging::list_notification_preferences_request& request);
    messaging::get_notification_preference_response
    get_notification_preference(const messaging::get_notification_preference_request& request);
    messaging::get_many_notification_preferences_response get_many_notification_preferences(
        const messaging::get_many_notification_preferences_request& request);
    messaging::put_notification_preference_response
    put_notification_preference(const messaging::put_notification_preference_request& request);
    messaging::put_many_notification_preferences_response put_many_notification_preferences(
        const messaging::put_many_notification_preferences_request& request);
    messaging::delete_notification_preference_response delete_notification_preference(
        const messaging::delete_notification_preference_request& request);
    messaging::delete_many_notification_preferences_response delete_many_notification_preferences(
        const messaging::delete_many_notification_preferences_request& request);
    messaging::list_by_account_id_notification_preferences_response
    list_by_account_id_notification_preferences(
        const messaging::list_by_account_id_notification_preferences_request& request);
    messaging::list_notification_preference_versions_response list_notification_preference_versions(
        const messaging::list_notification_preference_versions_request& request);
    messaging::get_notification_preference_version_response get_notification_preference_version(
        const messaging::get_notification_preference_version_request& request);
    /**@}*/

    /**
     * @brief Lists notification preferences with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of notification preferences for the requested page.
     */
    std::vector<domain::notification_preference> list_preferences(std::uint32_t offset,
                                                                  std::uint32_t limit);

    /**
     * @brief Gets the total count of active notification preferences.
     *
     * @return Total number of active notification preferences.
     */
    std::uint32_t count_preferences();


    /**
     * @brief Lists notification preferences filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching notification preferences for the requested page.
     */
    std::vector<domain::notification_preference> list_preferences_by_account_id(
        const std::string& account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active notification preferences filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching notification preferences.
     */
    std::uint32_t count_preferences_by_account_id(const std::string& account_id);


    /**
     * @brief Retrieves a single notification preference as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The notification preference at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::notification_preference>
    get_preference_at_version(const std::string& account_id,
                              const std::string& kind_code,
                              const std::string& channel_code,
                              std::uint32_t version);

    /**
     * @brief Retrieves a single notification preference by its primary key.
     *
     * @return The notification preference if found, std::nullopt otherwise.
     */
    std::optional<domain::notification_preference> get_preference(const std::string& account_id,
                                                                  const std::string& kind_code,
                                                                  const std::string& channel_code);

    /**
     * @brief Retrieves a batch of notification preferences by primary key.
     */
    std::vector<domain::notification_preference>
    get_preferences(const std::vector<std::string>& account_ids,
                    const std::vector<std::string>& kind_codes,
                    const std::vector<std::string>& channel_codes);

    /**
     * @brief Saves a notification preference (creates or updates).
     *
     * @param preference The notification preference to save.
     * @throws std::exception on failure.
     */
    void save_preference(const domain::notification_preference& preference);

    /**
     * @brief Saves a batch of notification preferences.
     *
     * @param preferences The notification preferences to save.
     * @throws std::exception on failure.
     */
    void save_preferences(const std::vector<domain::notification_preference>& preferences);

    /**
     * @brief Deletes a notification preference by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_preference(const std::string& account_id,
                           const std::string& kind_code,
                           const std::string& channel_code);

    /**
     * @brief Deletes notification preferences by their primary keys.
     */
    void delete_preferences(const std::vector<std::string>& account_ids,
                            const std::vector<std::string>& kind_codes,
                            const std::vector<std::string>& channel_codes);

    /**
     * @brief Retrieves all historical versions of a notification preference.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::notification_preference>
    get_preference_history(const std::string& account_id,
                           const std::string& kind_code,
                           const std::string& channel_code);

private:
    context ctx_;
    repository::notification_preference_repository repo_;

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
    prepare_change(const messaging::notification_preference_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::notification_preference& out);
};

}

#endif

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
#ifndef ORES_INBOX_CORE_SERVICE_NOTIFICATION_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_NOTIFICATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/notification.hpp"
#include "ores.inbox.api/messaging/notification_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/notification_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing notifications.
 *
 * Provides a higher-level interface for notification operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT notification_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.notification_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a notification_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit notification_service(context ctx);

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
    messaging::list_notifications_response
    list_notifications(const messaging::list_notifications_request& request);
    messaging::get_notification_response
    get_notification(const messaging::get_notification_request& request);
    messaging::get_many_notifications_response
    get_many_notifications(const messaging::get_many_notifications_request& request);
    messaging::put_notification_response
    put_notification(const messaging::put_notification_request& request);
    messaging::put_many_notifications_response
    put_many_notifications(const messaging::put_many_notifications_request& request);
    messaging::delete_notification_response
    delete_notification(const messaging::delete_notification_request& request);
    messaging::delete_many_notifications_response
    delete_many_notifications(const messaging::delete_many_notifications_request& request);
    messaging::list_by_kind_code_notifications_response list_by_kind_code_notifications(
        const messaging::list_by_kind_code_notifications_request& request);
    messaging::list_notification_versions_response
    list_notification_versions(const messaging::list_notification_versions_request& request);
    messaging::get_notification_version_response
    get_notification_version(const messaging::get_notification_version_request& request);
    /**@}*/

    /**
     * @brief Lists notifications with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of notifications for the requested page.
     */
    std::vector<domain::notification> list_notifications(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active notifications.
     *
     * @return Total number of active notifications.
     */
    std::uint32_t count_notifications();


    /**
     * @brief Lists notifications filtered by kind_code, with pagination.
     *
     * @param kind_code The kind_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching notifications for the requested page.
     */
    std::vector<domain::notification> list_notifications_by_kind_code(const std::string& kind_code,
                                                                      std::uint32_t offset,
                                                                      std::uint32_t limit);

    /**
     * @brief Gets the total count of active notifications filtered by kind_code.
     *
     * @param kind_code The kind_code to filter by.
     * @return Total number of matching notifications.
     */
    std::uint32_t count_notifications_by_kind_code(const std::string& kind_code);


    /**
     * @brief Retrieves a single notification as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The notification at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::notification> get_notification_at_version(const boost::uuids::uuid& id,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single notification by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The notification if found, std::nullopt otherwise.
     */
    std::optional<domain::notification> get_notification(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of notifications by primary key.
     */
    std::vector<domain::notification> get_notifications(const std::vector<std::string>& ids);

    /**
     * @brief Saves a notification (creates or updates).
     *
     * @param notification The notification to save.
     * @throws std::exception on failure.
     */
    void save_notification(const domain::notification& notification);

    /**
     * @brief Saves a batch of notifications.
     *
     * @param notifications The notifications to save.
     * @throws std::exception on failure.
     */
    void save_notifications(const std::vector<domain::notification>& notifications);

    /**
     * @brief Deletes a notification by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_notification(const boost::uuids::uuid& id);

    /**
     * @brief Deletes notifications by their primary keys.
     */
    void delete_notifications(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a notification.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::notification> get_notification_history(const std::string& id);

private:
    context ctx_;
    repository::notification_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::notification_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::notification& out);
};

}

#endif

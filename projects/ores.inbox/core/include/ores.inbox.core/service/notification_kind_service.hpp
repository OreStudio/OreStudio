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
#ifndef ORES_INBOX_CORE_SERVICE_NOTIFICATION_KIND_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_NOTIFICATION_KIND_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/notification_kind.hpp"
#include "ores.inbox.api/messaging/notification_kind_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/notification_kind_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing notification kinds.
 *
 * Provides a higher-level interface for notification kind operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT notification_kind_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.notification_kind_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a notification_kind_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit notification_kind_service(context ctx);

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
    messaging::list_notification_kinds_response
    list_notification_kinds(const messaging::list_notification_kinds_request& request);
    messaging::get_notification_kind_response
    get_notification_kind(const messaging::get_notification_kind_request& request);
    messaging::get_many_notification_kinds_response
    get_many_notification_kinds(const messaging::get_many_notification_kinds_request& request);
    messaging::put_notification_kind_response
    put_notification_kind(const messaging::put_notification_kind_request& request);
    messaging::put_many_notification_kinds_response
    put_many_notification_kinds(const messaging::put_many_notification_kinds_request& request);
    messaging::delete_notification_kind_response
    delete_notification_kind(const messaging::delete_notification_kind_request& request);
    messaging::delete_many_notification_kinds_response delete_many_notification_kinds(
        const messaging::delete_many_notification_kinds_request& request);
    messaging::list_notification_kind_versions_response list_notification_kind_versions(
        const messaging::list_notification_kind_versions_request& request);
    messaging::get_notification_kind_version_response
    get_notification_kind_version(const messaging::get_notification_kind_version_request& request);
    /**@}*/

    /**
     * @brief Lists notification kinds with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of notification kinds for the requested page.
     */
    std::vector<domain::notification_kind> list_notification_kinds(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active notification kinds.
     *
     * @return Total number of active notification kinds.
     */
    std::uint32_t count_notification_kinds();


    /**
     * @brief Retrieves a single notification kind as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The notification kind at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::notification_kind>
    get_notification_kind_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single notification kind by its primary key.
     *
     * @return The notification kind if found, std::nullopt otherwise.
     */
    std::optional<domain::notification_kind> find_notification_kind(const std::string& code);

    /**
     * @brief Retrieves a batch of notification kinds by primary key.
     */
    std::vector<domain::notification_kind>
    get_notification_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Saves a notification kind (creates or updates).
     *
     * @param notification_kind The notification kind to save.
     * @throws std::exception on failure.
     */
    void save_notification_kind(const domain::notification_kind& notification_kind);

    /**
     * @brief Saves a batch of notification kinds.
     *
     * @param notification_kinds The notification kinds to save.
     * @throws std::exception on failure.
     */
    void save_notification_kinds(const std::vector<domain::notification_kind>& notification_kinds);

    /**
     * @brief Deletes a notification kind by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_notification_kind(const std::string& code);

    /**
     * @brief Deletes notification kinds by their primary keys.
     */
    void delete_notification_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a notification kind.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::notification_kind> get_notification_kind_history(const std::string& code);

private:
    context ctx_;
    repository::notification_kind_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::notification_kind_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::notification_kind& out);
};

}

#endif

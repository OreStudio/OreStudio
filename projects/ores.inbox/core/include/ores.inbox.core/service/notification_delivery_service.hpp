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
#ifndef ORES_INBOX_CORE_SERVICE_NOTIFICATION_DELIVERY_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_NOTIFICATION_DELIVERY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/notification_delivery.hpp"
#include "ores.inbox.api/messaging/notification_delivery_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/notification_delivery_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing notification deliveries.
 *
 * Provides a higher-level interface for notification delivery operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT notification_delivery_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.notification_delivery_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a notification_delivery_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit notification_delivery_service(context ctx);

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
    messaging::list_notification_deliveries_response
    list_notification_deliveries(const messaging::list_notification_deliveries_request& request);
    messaging::get_notification_delivery_response
    get_notification_delivery(const messaging::get_notification_delivery_request& request);
    messaging::get_many_notification_deliveries_response get_many_notification_deliveries(
        const messaging::get_many_notification_deliveries_request& request);
    messaging::put_notification_delivery_response
    put_notification_delivery(const messaging::put_notification_delivery_request& request);
    messaging::put_many_notification_deliveries_response put_many_notification_deliveries(
        const messaging::put_many_notification_deliveries_request& request);
    messaging::delete_notification_delivery_response
    delete_notification_delivery(const messaging::delete_notification_delivery_request& request);
    messaging::delete_many_notification_deliveries_response delete_many_notification_deliveries(
        const messaging::delete_many_notification_deliveries_request& request);
    messaging::list_by_notification_id_notification_deliveries_response
    list_by_notification_id_notification_deliveries(
        const messaging::list_by_notification_id_notification_deliveries_request& request);
    messaging::list_notification_delivery_versions_response list_notification_delivery_versions(
        const messaging::list_notification_delivery_versions_request& request);
    messaging::get_notification_delivery_version_response get_notification_delivery_version(
        const messaging::get_notification_delivery_version_request& request);
    /**@}*/

    /**
     * @brief Lists notification deliveries with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of notification deliveries for the requested page.
     */
    std::vector<domain::notification_delivery> list_deliveries(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active notification deliveries.
     *
     * @return Total number of active notification deliveries.
     */
    std::uint32_t count_deliveries();


    /**
     * @brief Lists notification deliveries filtered by notification_id, with pagination.
     *
     * @param notification_id The notification_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching notification deliveries for the requested page.
     */
    std::vector<domain::notification_delivery> list_deliveries_by_notification_id(
        const std::string& notification_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active notification deliveries filtered by notification_id.
     *
     * @param notification_id The notification_id to filter by.
     * @return Total number of matching notification deliveries.
     */
    std::uint32_t count_deliveries_by_notification_id(const std::string& notification_id);


    /**
     * @brief Retrieves a single notification delivery as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The notification delivery at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::notification_delivery>
    get_delivery_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single notification delivery by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The notification delivery if found, std::nullopt otherwise.
     */
    std::optional<domain::notification_delivery> get_delivery(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of notification deliveries by primary key.
     */
    std::vector<domain::notification_delivery> get_deliveries(const std::vector<std::string>& ids);

    /**
     * @brief Saves a notification delivery (creates or updates).
     *
     * @param delivery The notification delivery to save.
     * @throws std::exception on failure.
     */
    void save_delivery(const domain::notification_delivery& delivery);

    /**
     * @brief Saves a batch of notification deliveries.
     *
     * @param deliveries The notification deliveries to save.
     * @throws std::exception on failure.
     */
    void save_deliveries(const std::vector<domain::notification_delivery>& deliveries);

    /**
     * @brief Deletes a notification delivery by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_delivery(const boost::uuids::uuid& id);

    /**
     * @brief Deletes notification deliveries by their primary keys.
     */
    void delete_deliveries(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a notification delivery.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::notification_delivery> get_delivery_history(const std::string& id);

private:
    context ctx_;
    repository::notification_delivery_repository repo_;

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
    prepare_change(const messaging::notification_delivery_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::notification_delivery& out);
};

}

#endif

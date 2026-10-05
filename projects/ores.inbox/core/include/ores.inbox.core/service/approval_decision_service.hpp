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
#ifndef ORES_INBOX_CORE_SERVICE_APPROVAL_DECISION_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_APPROVAL_DECISION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_decision.hpp"
#include "ores.inbox.api/messaging/approval_decision_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/approval_decision_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing approval decisions.
 *
 * Provides a higher-level interface for approval decision operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT approval_decision_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.approval_decision_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a approval_decision_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit approval_decision_service(context ctx);

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
    messaging::list_approval_decisions_response
    list_approval_decisions(const messaging::list_approval_decisions_request& request);
    messaging::get_approval_decision_response
    get_approval_decision(const messaging::get_approval_decision_request& request);
    messaging::get_many_approval_decisions_response
    get_many_approval_decisions(const messaging::get_many_approval_decisions_request& request);
    messaging::put_approval_decision_response
    put_approval_decision(const messaging::put_approval_decision_request& request);
    messaging::put_many_approval_decisions_response
    put_many_approval_decisions(const messaging::put_many_approval_decisions_request& request);
    messaging::delete_approval_decision_response
    delete_approval_decision(const messaging::delete_approval_decision_request& request);
    messaging::delete_many_approval_decisions_response delete_many_approval_decisions(
        const messaging::delete_many_approval_decisions_request& request);
    messaging::list_by_request_id_approval_decisions_response list_by_request_id_approval_decisions(
        const messaging::list_by_request_id_approval_decisions_request& request);
    messaging::list_approval_decision_versions_response list_approval_decision_versions(
        const messaging::list_approval_decision_versions_request& request);
    messaging::get_approval_decision_version_response
    get_approval_decision_version(const messaging::get_approval_decision_version_request& request);
    /**@}*/

    /**
     * @brief Lists approval decisions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of approval decisions for the requested page.
     */
    std::vector<domain::approval_decision> list_decisions(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active approval decisions.
     *
     * @return Total number of active approval decisions.
     */
    std::uint32_t count_decisions();


    /**
     * @brief Lists approval decisions filtered by request_id, with pagination.
     *
     * @param request_id The request_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching approval decisions for the requested page.
     */
    std::vector<domain::approval_decision> list_decisions_by_request_id(
        const std::string& request_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active approval decisions filtered by request_id.
     *
     * @param request_id The request_id to filter by.
     * @return Total number of matching approval decisions.
     */
    std::uint32_t count_decisions_by_request_id(const std::string& request_id);


    /**
     * @brief Retrieves a single approval decision as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The approval decision at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::approval_decision> get_decision_at_version(const boost::uuids::uuid& id,
                                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single approval decision by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The approval decision if found, std::nullopt otherwise.
     */
    std::optional<domain::approval_decision> get_decision(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of approval decisions by primary key.
     */
    std::vector<domain::approval_decision> get_decisions(const std::vector<std::string>& ids);

    /**
     * @brief Saves a approval decision (creates or updates).
     *
     * @param decision The approval decision to save.
     * @throws std::exception on failure.
     */
    void save_decision(const domain::approval_decision& decision);

    /**
     * @brief Saves a batch of approval decisions.
     *
     * @param decisions The approval decisions to save.
     * @throws std::exception on failure.
     */
    void save_decisions(const std::vector<domain::approval_decision>& decisions);

    /**
     * @brief Deletes a approval decision by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_decision(const boost::uuids::uuid& id);

    /**
     * @brief Deletes approval decisions by their primary keys.
     */
    void delete_decisions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a approval decision.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::approval_decision> get_decision_history(const std::string& id);

private:
    context ctx_;
    repository::approval_decision_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::approval_decision_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::approval_decision& out);
};

}

#endif

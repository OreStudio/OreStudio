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
#ifndef ORES_INBOX_CORE_SERVICE_HOLD_REASON_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_HOLD_REASON_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/hold_reason.hpp"
#include "ores.inbox.api/messaging/hold_reason_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/hold_reason_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing hold reasons.
 *
 * Provides a higher-level interface for hold reason operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT hold_reason_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.hold_reason_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a hold_reason_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit hold_reason_service(context ctx);

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
    ::ores::inbox::messaging::list_hold_reasons_response
    list_hold_reasons(const ::ores::inbox::messaging::list_hold_reasons_request& request);
    ::ores::inbox::messaging::get_hold_reason_response
    get_hold_reason(const ::ores::inbox::messaging::get_hold_reason_request& request);
    ::ores::inbox::messaging::get_many_hold_reasons_response
    get_many_hold_reasons(const ::ores::inbox::messaging::get_many_hold_reasons_request& request);
    ::ores::inbox::messaging::put_hold_reason_response
    put_hold_reason(const ::ores::inbox::messaging::put_hold_reason_request& request);
    ::ores::inbox::messaging::put_many_hold_reasons_response
    put_many_hold_reasons(const ::ores::inbox::messaging::put_many_hold_reasons_request& request);
    ::ores::inbox::messaging::delete_hold_reason_response
    delete_hold_reason(const ::ores::inbox::messaging::delete_hold_reason_request& request);
    ::ores::inbox::messaging::delete_many_hold_reasons_response delete_many_hold_reasons(
        const ::ores::inbox::messaging::delete_many_hold_reasons_request& request);
    ::ores::inbox::messaging::list_hold_reason_versions_response list_hold_reason_versions(
        const ::ores::inbox::messaging::list_hold_reason_versions_request& request);
    ::ores::inbox::messaging::get_hold_reason_version_response get_hold_reason_version(
        const ::ores::inbox::messaging::get_hold_reason_version_request& request);
    /**@}*/

    /**
     * @brief Lists hold reasons with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of hold reasons for the requested page.
     */
    std::vector<domain::hold_reason> list_reasons(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active hold reasons.
     *
     * @return Total number of active hold reasons.
     */
    std::uint32_t count_reasons();


    /**
     * @brief Retrieves a single hold reason as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The hold reason at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::hold_reason> get_reason_at_version(const std::string& code,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single hold reason by its primary key.
     *
     * @return The hold reason if found, std::nullopt otherwise.
     */
    std::optional<domain::hold_reason> find_reason(const std::string& code);

    /**
     * @brief Retrieves a batch of hold reasons by primary key.
     */
    std::vector<domain::hold_reason> get_reasons(const std::vector<std::string>& codes);

    /**
     * @brief Saves a hold reason (creates or updates).
     *
     * @param reason The hold reason to save.
     * @throws std::exception on failure.
     */
    void save_reason(const domain::hold_reason& reason);

    /**
     * @brief Saves a batch of hold reasons.
     *
     * @param reasons The hold reasons to save.
     * @throws std::exception on failure.
     */
    void save_reasons(const std::vector<domain::hold_reason>& reasons);

    /**
     * @brief Deletes a hold reason by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_reason(const std::string& code);

    /**
     * @brief Deletes hold reasons by their primary keys.
     */
    void delete_reasons(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a hold reason.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::hold_reason> get_reason_history(const std::string& code);

private:
    context ctx_;
    repository::hold_reason_repository repo_;

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
    prepare_change(const ::ores::inbox::messaging::hold_reason_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::hold_reason& out);
};

}

#endif

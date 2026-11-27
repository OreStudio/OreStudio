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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_ADDRESS_MARKET_READINESS_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_ADDRESS_MARKET_READINESS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.reporting.api/domain/report_address_market_readiness.hpp"
#include "ores.reporting.api/messaging/report_address_market_readiness_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_address_market_readiness_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing address readinesses.
 *
 * Provides a higher-level interface for address readiness operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_address_market_readiness_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_address_market_readiness_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_address_market_readiness_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_address_market_readiness_service(const context& ctx);

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
    ::ores::reporting::messaging::list_report_address_market_readinesses_response
    list_report_address_market_readinesses(
        const ::ores::reporting::messaging::list_report_address_market_readinesses_request&
            request);
    ::ores::reporting::messaging::get_report_address_market_readiness_response
    get_report_address_market_readiness(
        const ::ores::reporting::messaging::get_report_address_market_readiness_request& request);
    ::ores::reporting::messaging::get_many_report_address_market_readinesses_response
    get_many_report_address_market_readinesses(
        const ::ores::reporting::messaging::get_many_report_address_market_readinesses_request&
            request);
    ::ores::reporting::messaging::put_report_address_market_readiness_response
    put_report_address_market_readiness(
        const ::ores::reporting::messaging::put_report_address_market_readiness_request& request);
    ::ores::reporting::messaging::put_many_report_address_market_readinesses_response
    put_many_report_address_market_readinesses(
        const ::ores::reporting::messaging::put_many_report_address_market_readinesses_request&
            request);
    ::ores::reporting::messaging::delete_report_address_market_readiness_response
    delete_report_address_market_readiness(
        const ::ores::reporting::messaging::delete_report_address_market_readiness_request&
            request);
    ::ores::reporting::messaging::delete_many_report_address_market_readinesses_response
    delete_many_report_address_market_readinesses(
        const ::ores::reporting::messaging::delete_many_report_address_market_readinesses_request&
            request);
    ::ores::reporting::messaging::list_report_address_market_readiness_versions_response
    list_report_address_market_readiness_versions(
        const ::ores::reporting::messaging::list_report_address_market_readiness_versions_request&
            request);
    ::ores::reporting::messaging::get_report_address_market_readiness_version_response
    get_report_address_market_readiness_version(
        const ::ores::reporting::messaging::get_report_address_market_readiness_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists address readinesses with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of address readinesses for the requested page.
     */
    std::vector<domain::report_address_market_readiness>
    list_address_readinesses(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active address readinesses.
     *
     * @return Total number of active address readinesses.
     */
    std::uint32_t count_address_readinesses();


    /**
     * @brief Retrieves a single address readiness as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The address readiness at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_address_market_readiness>
    get_address_readiness_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single address readiness by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The address readiness if found, std::nullopt otherwise.
     */
    std::optional<domain::report_address_market_readiness>
    get_address_readiness(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of address readinesses by primary key.
     */
    std::vector<domain::report_address_market_readiness>
    get_address_readinesses(const std::vector<std::string>& ids);

    /**
     * @brief Saves a address readiness (creates or updates).
     *
     * @param address_readiness The address readiness to save.
     * @throws std::exception on failure.
     */
    void save_address_readiness(const domain::report_address_market_readiness& address_readiness);

    /**
     * @brief Saves a batch of address readinesses.
     *
     * @param address_readinesses The address readinesses to save.
     * @throws std::exception on failure.
     */
    void save_address_readinesses(
        const std::vector<domain::report_address_market_readiness>& address_readinesses);

    /**
     * @brief Deletes a address readiness by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_address_readiness(const boost::uuids::uuid& id);

    /**
     * @brief Deletes address readinesses by their primary keys.
     */
    void delete_address_readinesses(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a address readiness.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_address_market_readiness>
    get_address_readiness_history(const std::string& id);

private:
    context ctx_;
    repository::report_address_market_readiness_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(
        const ::ores::reporting::messaging::report_address_market_readiness_change& change,
        const ores::utility::domain::change_intent& intent,
        const std::vector<domain::report_address_market_readiness>& current,
        domain::report_address_market_readiness& out);
};

}

#endif

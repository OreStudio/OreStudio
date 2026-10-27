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
#ifndef ORES_REPORTING_CORE_SERVICE_ENGINE_SETTINGS_FIXINGS_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ENGINE_SETTINGS_FIXINGS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings.hpp"
#include "ores.reporting.api/messaging/engine_settings_fixings_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/engine_settings_fixings_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing engine settings fixings.
 *
 * Provides a higher-level interface for engine settings fixings operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT engine_settings_fixings_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.engine_settings_fixings_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a engine_settings_fixings_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit engine_settings_fixings_service(context ctx);

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
    ::ores::reporting::messaging::list_engine_settings_fixings_response
    list_engine_settings_fixings(
        const ::ores::reporting::messaging::list_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::get_engine_settings_fixings_response get_engine_settings_fixings(
        const ::ores::reporting::messaging::get_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::get_many_engine_settings_fixings_response
    get_many_engine_settings_fixings(
        const ::ores::reporting::messaging::get_many_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::put_engine_settings_fixings_response put_engine_settings_fixings(
        const ::ores::reporting::messaging::put_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::put_many_engine_settings_fixings_response
    put_many_engine_settings_fixings(
        const ::ores::reporting::messaging::put_many_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::delete_engine_settings_fixings_response
    delete_engine_settings_fixings(
        const ::ores::reporting::messaging::delete_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::delete_many_engine_settings_fixings_response
    delete_many_engine_settings_fixings(
        const ::ores::reporting::messaging::delete_many_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_response
    list_by_engine_settings_id_engine_settings_fixings(
        const ::ores::reporting::messaging::
            list_by_engine_settings_id_engine_settings_fixings_request& request);
    ::ores::reporting::messaging::list_engine_settings_fixings_versions_response
    list_engine_settings_fixings_versions(
        const ::ores::reporting::messaging::list_engine_settings_fixings_versions_request& request);
    ::ores::reporting::messaging::get_engine_settings_fixings_version_response
    get_engine_settings_fixings_version(
        const ::ores::reporting::messaging::get_engine_settings_fixings_version_request& request);
    /**@}*/

    /**
     * @brief Lists engine settings fixings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of engine settings fixings for the requested page.
     */
    std::vector<domain::engine_settings_fixings> list_engine_settings_fixings(std::uint32_t offset,
                                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings fixings.
     *
     * @return Total number of active engine settings fixings.
     */
    std::uint32_t count_engine_settings_fixings();


    /**
     * @brief Lists engine settings fixings filtered by engine_settings_id, with pagination.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching engine settings fixings for the requested page.
     */
    std::vector<domain::engine_settings_fixings> list_engine_settings_fixings_by_engine_settings_id(
        const std::string& engine_settings_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings fixings filtered by engine_settings_id.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @return Total number of matching engine settings fixings.
     */
    std::uint32_t
    count_engine_settings_fixings_by_engine_settings_id(const std::string& engine_settings_id);

    /**
     * @brief Lists engine settings fixings filtered by engine_settings_id, with pagination.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching engine settings fixings for the requested page.
     */
    std::vector<domain::engine_settings_fixings> list_engine_settings_fixings_by_engine_settings_id(
        const boost::uuids::uuid& engine_settings_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings fixings filtered by engine_settings_id.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @return Total number of matching engine settings fixings.
     */
    std::uint32_t count_engine_settings_fixings_by_engine_settings_id(
        const boost::uuids::uuid& engine_settings_id);


    /**
     * @brief Lists engine settings fixings filtered by engine_settings_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching engine settings fixings.
     */
    std::vector<domain::engine_settings_fixings>
    list_engine_settings_fixings_by_engine_settings_id_as_of(
        const std::string& engine_settings_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single engine settings fixings as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The engine settings fixings at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::engine_settings_fixings>
    get_engine_settings_fixings_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single engine settings fixings by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The engine settings fixings if found, std::nullopt otherwise.
     */
    std::optional<domain::engine_settings_fixings>
    get_engine_settings_fixings(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of engine settings fixings by primary key.
     */
    std::vector<domain::engine_settings_fixings>
    get_engine_settings_fixings(const std::vector<std::string>& ids);

    /**
     * @brief Saves a engine settings fixings (creates or updates).
     *
     * @param engine_settings_fixings The engine settings fixings to save.
     * @throws std::exception on failure.
     */
    void
    save_engine_settings_fixings(const domain::engine_settings_fixings& engine_settings_fixings);

    /**
     * @brief Saves a batch of engine settings fixings.
     *
     * @param engine_settings_fixings The engine settings fixings to save.
     * @throws std::exception on failure.
     */
    void save_engine_settings_fixings(
        const std::vector<domain::engine_settings_fixings>& engine_settings_fixings);

    /**
     * @brief Deletes a engine settings fixings by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_engine_settings_fixings(const boost::uuids::uuid& id);

    /**
     * @brief Deletes engine settings fixings by their primary keys.
     */
    void delete_engine_settings_fixings(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a engine settings fixings.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::engine_settings_fixings>
    get_engine_settings_fixings_history(const std::string& id);

private:
    context ctx_;
    repository::engine_settings_fixings_repository repo_;

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
    prepare_change(const ::ores::reporting::messaging::engine_settings_fixings_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::engine_settings_fixings& out);
};

}

#endif

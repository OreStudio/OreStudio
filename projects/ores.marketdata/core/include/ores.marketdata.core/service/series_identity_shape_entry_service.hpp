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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_IDENTITY_SHAPE_ENTRY_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_IDENTITY_SHAPE_ENTRY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/domain/series_identity_shape_entry.hpp"
#include "ores.marketdata.api/messaging/series_identity_shape_entry_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/series_identity_shape_entry_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing series identity shape entries.
 *
 * Provides a higher-level interface for series identity shape entry operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT series_identity_shape_entry_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.series_identity_shape_entry_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a series_identity_shape_entry_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit series_identity_shape_entry_service(const context& ctx);

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
    ::ores::marketdata::messaging::list_series_identity_shape_entries_response
    list_series_identity_shape_entries(
        const ::ores::marketdata::messaging::list_series_identity_shape_entries_request& request);
    ::ores::marketdata::messaging::get_series_identity_shape_entry_response
    get_series_identity_shape_entry(
        const ::ores::marketdata::messaging::get_series_identity_shape_entry_request& request);
    ::ores::marketdata::messaging::get_many_series_identity_shape_entries_response
    get_many_series_identity_shape_entries(
        const ::ores::marketdata::messaging::get_many_series_identity_shape_entries_request&
            request);
    ::ores::marketdata::messaging::put_series_identity_shape_entry_response
    put_series_identity_shape_entry(
        const ::ores::marketdata::messaging::put_series_identity_shape_entry_request& request);
    ::ores::marketdata::messaging::put_many_series_identity_shape_entries_response
    put_many_series_identity_shape_entries(
        const ::ores::marketdata::messaging::put_many_series_identity_shape_entries_request&
            request);
    ::ores::marketdata::messaging::delete_series_identity_shape_entry_response
    delete_series_identity_shape_entry(
        const ::ores::marketdata::messaging::delete_series_identity_shape_entry_request& request);
    ::ores::marketdata::messaging::delete_many_series_identity_shape_entries_response
    delete_many_series_identity_shape_entries(
        const ::ores::marketdata::messaging::delete_many_series_identity_shape_entries_request&
            request);
    ::ores::marketdata::messaging::list_by_shape_code_series_identity_shape_entries_response
    list_by_shape_code_series_identity_shape_entries(
        const ::ores::marketdata::messaging::
            list_by_shape_code_series_identity_shape_entries_request& request);
    ::ores::marketdata::messaging::list_series_identity_shape_entry_versions_response
    list_series_identity_shape_entry_versions(
        const ::ores::marketdata::messaging::list_series_identity_shape_entry_versions_request&
            request);
    ::ores::marketdata::messaging::get_series_identity_shape_entry_version_response
    get_series_identity_shape_entry_version(
        const ::ores::marketdata::messaging::get_series_identity_shape_entry_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists series identity shape entries with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of series identity shape entries for the requested page.
     */
    std::vector<domain::series_identity_shape_entry> list_entries(std::uint32_t offset,
                                                                  std::uint32_t limit);

    /**
     * @brief Gets the total count of active series identity shape entries.
     *
     * @return Total number of active series identity shape entries.
     */
    std::uint32_t count_entries();


    /**
     * @brief Lists series identity shape entries filtered by shape_code, with pagination.
     *
     * @param shape_code The shape_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching series identity shape entries for the requested page.
     */
    std::vector<domain::series_identity_shape_entry> list_entries_by_shape_code(
        const std::string& shape_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active series identity shape entries filtered by shape_code.
     *
     * @param shape_code The shape_code to filter by.
     * @return Total number of matching series identity shape entries.
     */
    std::uint32_t count_entries_by_shape_code(const std::string& shape_code);


    /**
     * @brief Lists series identity shape entries filtered by shape_code that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param shape_code The shape_code to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching series identity shape entries.
     */
    std::vector<domain::series_identity_shape_entry>
    list_entries_by_shape_code_as_of(const std::string& shape_code,
                                     std::chrono::system_clock::time_point valid_from_bound,
                                     std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single series identity shape entry as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The series identity shape entry at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::series_identity_shape_entry>
    get_entry_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single series identity shape entry by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The series identity shape entry if found, std::nullopt otherwise.
     */
    std::optional<domain::series_identity_shape_entry> get_entry(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of series identity shape entries by primary key.
     */
    std::vector<domain::series_identity_shape_entry>
    get_entries(const std::vector<std::string>& ids);

    /**
     * @brief Saves a series identity shape entry (creates or updates).
     *
     * @param entry The series identity shape entry to save.
     * @throws std::exception on failure.
     */
    void save_entry(const domain::series_identity_shape_entry& entry);

    /**
     * @brief Saves a batch of series identity shape entries.
     *
     * @param entries The series identity shape entries to save.
     * @throws std::exception on failure.
     */
    void save_entries(const std::vector<domain::series_identity_shape_entry>& entries);

    /**
     * @brief Deletes a series identity shape entry by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_entry(const boost::uuids::uuid& id);

    /**
     * @brief Deletes series identity shape entries by their primary keys.
     */
    void delete_entries(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a series identity shape entry.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::series_identity_shape_entry> get_entry_history(const std::string& id);

private:
    context ctx_;
    repository::series_identity_shape_entry_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::marketdata::messaging::series_identity_shape_entry_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::series_identity_shape_entry>& current,
                   domain::series_identity_shape_entry& out);
};

}

#endif

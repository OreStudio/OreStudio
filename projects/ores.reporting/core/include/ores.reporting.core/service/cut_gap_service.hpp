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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_GAP_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_GAP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/cut_gap.hpp"
#include "ores.reporting.api/messaging/cut_gap_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/cut_gap_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing cut gaps.
 *
 * Provides a higher-level interface for cut gap operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT cut_gap_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.cut_gap_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cut_gap_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cut_gap_service(context ctx);

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
    ::ores::reporting::messaging::list_cut_gaps_response
    list_cut_gaps(const ::ores::reporting::messaging::list_cut_gaps_request& request);
    ::ores::reporting::messaging::get_cut_gap_response
    get_cut_gap(const ::ores::reporting::messaging::get_cut_gap_request& request);
    ::ores::reporting::messaging::get_many_cut_gaps_response
    get_many_cut_gaps(const ::ores::reporting::messaging::get_many_cut_gaps_request& request);
    ::ores::reporting::messaging::put_cut_gap_response
    put_cut_gap(const ::ores::reporting::messaging::put_cut_gap_request& request);
    ::ores::reporting::messaging::put_many_cut_gaps_response
    put_many_cut_gaps(const ::ores::reporting::messaging::put_many_cut_gaps_request& request);
    ::ores::reporting::messaging::delete_cut_gap_response
    delete_cut_gap(const ::ores::reporting::messaging::delete_cut_gap_request& request);
    ::ores::reporting::messaging::delete_many_cut_gaps_response
    delete_many_cut_gaps(const ::ores::reporting::messaging::delete_many_cut_gaps_request& request);
    ::ores::reporting::messaging::list_by_cut_id_cut_gaps_response list_by_cut_id_cut_gaps(
        const ::ores::reporting::messaging::list_by_cut_id_cut_gaps_request& request);
    ::ores::reporting::messaging::list_cut_gap_versions_response list_cut_gap_versions(
        const ::ores::reporting::messaging::list_cut_gap_versions_request& request);
    ::ores::reporting::messaging::get_cut_gap_version_response
    get_cut_gap_version(const ::ores::reporting::messaging::get_cut_gap_version_request& request);
    /**@}*/

    /**
     * @brief Lists cut gaps with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cut gaps for the requested page.
     */
    std::vector<domain::cut_gap> list_gaps(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cut gaps.
     *
     * @return Total number of active cut gaps.
     */
    std::uint32_t count_gaps();


    /**
     * @brief Lists cut gaps filtered by cut_id, with pagination.
     *
     * @param cut_id The cut_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cut gaps for the requested page.
     */
    std::vector<domain::cut_gap>
    list_gaps_by_cut_id(const std::string& cut_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cut gaps filtered by cut_id.
     *
     * @param cut_id The cut_id to filter by.
     * @return Total number of matching cut gaps.
     */
    std::uint32_t count_gaps_by_cut_id(const std::string& cut_id);


    /**
     * @brief Retrieves a single cut gap as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cut gap at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cut_gap> get_gap_at_version(const boost::uuids::uuid& id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single cut gap by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The cut gap if found, std::nullopt otherwise.
     */
    std::optional<domain::cut_gap> get_gap(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of cut gaps by primary key.
     */
    std::vector<domain::cut_gap> get_gaps(const std::vector<std::string>& ids);

    /**
     * @brief Saves a cut gap (creates or updates).
     *
     * @param gap The cut gap to save.
     * @throws std::exception on failure.
     */
    void save_gap(const domain::cut_gap& gap);

    /**
     * @brief Saves a batch of cut gaps.
     *
     * @param gaps The cut gaps to save.
     * @throws std::exception on failure.
     */
    void save_gaps(const std::vector<domain::cut_gap>& gaps);

    /**
     * @brief Deletes a cut gap by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_gap(const boost::uuids::uuid& id);

    /**
     * @brief Deletes cut gaps by their primary keys.
     */
    void delete_gaps(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a cut gap.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cut_gap> get_gap_history(const std::string& id);

private:
    context ctx_;
    repository::cut_gap_repository repo_;

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
    prepare_change(const ::ores::reporting::messaging::cut_gap_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::cut_gap& out);
};

}

#endif

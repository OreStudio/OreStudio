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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/cut.hpp"
#include "ores.reporting.api/messaging/cut_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing cuts.
 *
 * Provides a higher-level interface for cut operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT cut_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.cut_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cut_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cut_service(context ctx);

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
    ::ores::reporting::messaging::list_cuts_response
    list_cuts(const ::ores::reporting::messaging::list_cuts_request& request);
    ::ores::reporting::messaging::get_cut_response
    get_cut(const ::ores::reporting::messaging::get_cut_request& request);
    ::ores::reporting::messaging::get_many_cuts_response
    get_many_cuts(const ::ores::reporting::messaging::get_many_cuts_request& request);
    ::ores::reporting::messaging::put_cut_response
    put_cut(const ::ores::reporting::messaging::put_cut_request& request);
    ::ores::reporting::messaging::put_many_cuts_response
    put_many_cuts(const ::ores::reporting::messaging::put_many_cuts_request& request);
    ::ores::reporting::messaging::delete_cut_response
    delete_cut(const ::ores::reporting::messaging::delete_cut_request& request);
    ::ores::reporting::messaging::delete_many_cuts_response
    delete_many_cuts(const ::ores::reporting::messaging::delete_many_cuts_request& request);
    ::ores::reporting::messaging::list_by_kind_code_cuts_response list_by_kind_code_cuts(
        const ::ores::reporting::messaging::list_by_kind_code_cuts_request& request);
    ::ores::reporting::messaging::list_cut_versions_response
    list_cut_versions(const ::ores::reporting::messaging::list_cut_versions_request& request);
    ::ores::reporting::messaging::get_cut_version_response
    get_cut_version(const ::ores::reporting::messaging::get_cut_version_request& request);
    /**@}*/

    /**
     * @brief Lists cuts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cuts for the requested page.
     */
    std::vector<domain::cut> list_cuts(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cuts.
     *
     * @return Total number of active cuts.
     */
    std::uint32_t count_cuts();


    /**
     * @brief Lists cuts filtered by kind_code, with pagination.
     *
     * @param kind_code The kind_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cuts for the requested page.
     */
    std::vector<domain::cut>
    list_cuts_by_kind_code(const std::string& kind_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cuts filtered by kind_code.
     *
     * @param kind_code The kind_code to filter by.
     * @return Total number of matching cuts.
     */
    std::uint32_t count_cuts_by_kind_code(const std::string& kind_code);


    /**
     * @brief Retrieves a single cut as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cut at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cut> get_cut_at_version(const boost::uuids::uuid& id,
                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single cut by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The cut if found, std::nullopt otherwise.
     */
    std::optional<domain::cut> get_cut(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of cuts by primary key.
     */
    std::vector<domain::cut> get_cuts(const std::vector<std::string>& ids);

    /**
     * @brief Saves a cut (creates or updates).
     *
     * @param cut The cut to save.
     * @throws std::exception on failure.
     */
    void save_cut(const domain::cut& cut);

    /**
     * @brief Saves a batch of cuts.
     *
     * @param cuts The cuts to save.
     * @throws std::exception on failure.
     */
    void save_cuts(const std::vector<domain::cut>& cuts);

    /**
     * @brief Deletes a cut by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cut(const boost::uuids::uuid& id);

    /**
     * @brief Deletes cuts by their primary keys.
     */
    void delete_cuts(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a cut.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cut> get_cut_history(const std::string& id);

private:
    context ctx_;
    repository::cut_repository repo_;

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
    prepare_change(const ::ores::reporting::messaging::cut_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::cut& out);
};

}

#endif
